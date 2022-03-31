# Subset stats module
 
# subset_stats_ui
# Arguments
# id: The id to use for the namespace created for this module.
# tab: String giving the tab this module applies to. This should be 
# either "dge" or "corr".
# metadata_config: the metadata sub-list defined within the config list 
# meta_categories: a vector of all metadata categories included 
# in the config file
# gene_selected: In the corr tab, the gene selected by the user for computation.
subset_stats_ui <- function(id,
                            tab=c("dge","corr"),
                            metadata_config,
                            meta_categories,
                            subset_selections,
                            gene_selected=NULL
                            ){
  # Namespace function: prevents conflicts with 
  # inputs/outputs defined in other modules
  ns <- NS(id)
  
  if (tab == "dge"){
    #UI for stats in dge tab
    tagList(
      #Test statistics
      div(
        tags$strong("Test selected", class="x-large inline-block"),
        textOutput(outputId = ns("print_mode"), 
                   inline=FALSE)
        ),
      # Metadata-specific subset statistics
      tags$strong("Subset Used for Test", 
                  class="x-large inline-block space-top"),
      # Loop through the metadata categories read on app startup
      # One container is created for each category in the subset using lapply.
      # Unique values for each category will display inline with labels
      lapply(
        X = meta_categories(),
        # Category is looped through by lapply
        # metadata_config and ns are "constants" that must be passed as 
        # additional arguments to lapply 
        FUN = function(category, metadata_config, ns){
          subset_stats_metadata_output(
            category = category,
            metadata_config = metadata_config, 
            ns = ns
            )
          },
        # Constants
        metadata_config,
        ns),
      # General subset statistics
      div(
        tags$strong("Number of cells in subset: ",
                    class = "space-top inline-block"),
        textOutput(outputId = ns("n_cells"), 
                   inline = TRUE)
        ),
      div(
        tags$strong("Number of cells per class: "),
        verbatimTextOutput(outputId = ns("n_by_class")),
        # Applies CSS from www/other.css to verbatimTextOutput
        class="n_by_class_style"
        )
      )# End tagList
      
  } else if (tab == "corr"){
    # UI for stats in correlations tab
    ui <- tagList(
      tags$strong("Subset Summary and Quality Statistics", 
                  class = "x-large inline-block space-top"),
      # Restriction criteria
      # Create outputs for each subset menu
      lapply(
        # Loop through the metadata categories read on app startup
        X = meta_categories(), 
        FUN = function(category, metadata_config){
          # Get label for the metadata category used in the
          # current subset selection 
          # menu, defined in config file
          label <- metadata_config()[[category]]$label
        
          # Return HTML
          # Use of div creates a new line between each entry
          div(
            tags$strong(glue("{label}:")),
            # Text output to report values in selected category 
            # found in the subset
            textOutput(
              # Output ID: uses the category name 
              outputId = ns(glue("selected_{category}")),
              inline = TRUE)
            )
          },
        # metadata_config must be passed to the function above after it is 
        # defined (see ... argument in ?lapply)
        metadata_config
        ), # End lapply
      
      # Number of cells in subset
      div(
        tags$strong("Number of cells in subset: ",
                      class = "space-top inline-block"),
        textOutput(outputId = ns("n_cells"), inline = TRUE)
        ),
      
      # Number and percentage of cells with nonzero reads for the selected gene
      div(
        tags$strong(glue("Cells with non-zero reads for {gene_selected()}:")),
        textOutput(outputId = ns("n_nonzero_and_percent"), inline = TRUE)
        )
      ) # End tagList
    
    # Return UI from module to the parent UI 
    return(ui)
  }
  
}

# subset_stats_server
# Arguments
# id: The id to use for the namespace created for this module.
# tab: String giving the tab this module applies to. This should be either 
# "dge" or " corr".
# subset (reactive): a subsetted Seurat object for which stats will be computed.
# meta_categories: a vector of all metadata categories included in the config file
# event_expr: a reactive variable used to control execution of reactive 
# expressions. All eventReacive and observeEvent statements will respond to this 
# variable.
# gene_selected: In the corr tab, the gene selected by the user for computation.
# nonzero_threshold: In the corr tab, the minimum acceptable proportion of nonzero 
# reads. A warning notification will be displayed to the user if the proportion 
# is below this threshold.
# group_by_category (reactive): For the DGE tab, the group by category currently 
# chosen for the test. 
subset_stats_server <- function(id,
                                tab=c("dge","corr"), #Non-reactive
                                subset, # Reactive
                                event_expr, # Reactive
                                meta_categories, # Reactive
                                gene_selected=NULL,
                                nonzero_threshold=NULL,
                                group_by_category=NULL
                                ){
  moduleServer(id, 
               function(input,output,session){
                 # Server namespace function (used for renderUI and 
                 # JavaScript ID references)
                 ns <- session$ns
                 
                 # 1. Compute stats for subset ---------------------------------
                 # Cells in subset (computed for both dge and corr tabs)
                 n_cells <- eventReactive(
                   event_expr(),
                   {
                     length(Cells(subset()))
                     })
                 
                 # Nonzero reads, proportion of nonzero reads, and percentage
                 # Computed for the correlations tab only
                 if (tab=="corr"){
                   # Cells with nonzero reads
                   n_nonzero <- 
                     eventReactive(
                       event_expr(),
                       {
                         sum(subset()@assays$RNA@counts[gene_selected(),] != 0)
                         })
                   
                   # Proportion of nonzero reads
                   prop_nonzero <- 
                     eventReactive(
                       event_expr(),
                       {
                         n_nonzero() / n_cells()
                         })
                   
                   # Percentage of nonzero reads
                   percent_nonzero <- 
                     eventReactive(
                       event_expr(),
                       {
                         format(
                           prop_nonzero() * 100,
                           # Display at least three sig figs in percentage
                           digits = 3,
                           # Display at least two digits after decimal point
                           nsmall = 2,
                           scientific = FALSE
                           )
                         })
                 }
                 
                 # For DGE tab only: stats on selected DE/marker classes, test 
                 # mode, and number of cells by class
                 if(tab == "dge"){
                   # classes: unique values in the selected group by metadata 
                   # category (not displayed but used downstream)
                   classes <- 
                     eventReactive(
                       event_expr(),
                       {
                         unique(subset()@meta.data[,group_by_category()])
                       })
                   
                   # Number of classes of the group_by metadata 
                   # category in subset
                   n_classes <- 
                     eventReactive(
                       event_expr(),
                       {
                         length(classes())
                       })
                   
                   # Print the type of test (DE or marker identification) and a 
                   # brief description of the classes selected
                   mode_description <- 
                     eventReactive(
                       event_expr(),
                       {
                         ifelse(
                           # Conditional: TRUE when differential expression 
                           # is selected
                           n_classes() == 2,
                           # Differential expression: print the two groups
                           # classes() contains the identities of both groups
                           glue("Differential Expression ({classes()[1]} vs. 
                                {classes()[2]})"),
                           # Marker identification: print the number of
                           # classes selected
                           glue("Marker Identification ({n_classes()}  classes)")
                           )
                         })
                   
                   # Number of cells in subset by class
                   n_by_class <- 
                     eventReactive(
                       event_expr(),
                       {
                         # Number of cells by class (tibble format)
                         n_cells_tibble <- 
                           subset()@meta.data |>
                           # Group by the specified metadata variable 
                           group_by(.data[[group_by_category()]]) |>
                           # Calculate number of cells per group
                           summarise(n = n()) 
                         
                         # Extract information from tibble
                         # Class names in first column of tibble
                         class_names <- as.character(n_cells_tibble[[1]])
                         # Cell counts are in second column of tibble
                         n_cells <- n_cells_tibble[[2]]
                         
                         # Print list of classes and the number of cells in each
                         n_cells_list = list()
                         for (i in 1:nrow(n_cells_tibble)){
                           n_cells_list[[i]] <- 
                             glue("{class_names[i]}: {n_cells[i]}")
                         }
                         
                         # Collapse list of class-count pairs into a string
                         # \n is the separator (will be read by 
                         # verbatimTextOutput())
                         n_by_class <- paste(n_cells_list, collapse = "\n")
                         
                         return(n_by_class)
                       })
                   }
                 
                 # 2. Notifications --------------------------------------------
                 # 2.1. Nonzero proportion is below the defined threshold 
                 # Use observe statement to reactively check prop_nonzero()
                 # Applies to correlations tab only
                 if (tab=="corr"){
                   observeEvent(
                     event_expr(),
                     ignoreNULL=FALSE,
                     {
                       if (prop_nonzero() < nonzero_threshold){
                         # Define notification UI (warning icon plus text)
                         notification_ui <- span(
                           # Warning icon (inline and enlarged)
                           icon("exclamation-triangle",
                                style="display: inline-block; 
                                font-size: 1.7em;"),
                           # Notification text with proportion and 
                           # number of non-zero cells
                           span(glue("Low gene coverage: the selected 
                                    feature was detected in {percent_nonzero()}%
                                    of cells within the selection restriction 
                                    criteria ({n_nonzero()}/{n_cells()} cells). 
                                    Correlation results may be inaccurate."),
                                # Font size of notification text
                                style="font-size: 1.17em;")#End span
                           ) # End notification_ui span
                         
                         # Display notification UI
                         showNotification(
                           ui=notification_ui,
                           # Duration=NULL will make the message persist 
                           # until dismissed
                           duration = NULL,
                           id = ns("high_zero_content"),
                           session=session
                           )
                         }# End if statement
                     })# End observeEvent
                   }
                 
                 # 3. Display stats --------------------------------------------
                 ## 3.1. Number of cells (both tabs)
                 output$n_cells <- renderText({
                   # Use isolate to keep values from updating before the submit 
                   # button is pressed
                   n_cells()
                 })
                 
                 ## 3.2 Cells with nonzero reads (correlations tab only)
                 if (tab == "corr"){
                   output$n_nonzero_and_percent <-
                     renderText({
                       glue("{n_nonzero()} ({percent_nonzero()}%)")
                       })
                 }
                 
                 ## 3.3. - 3.4 Outputs specific to DGE tab
                 if (tab == "dge"){
                   ## 3.3 Print description of test selected
                   output$print_mode <- 
                     renderText({
                       mode_description()
                       })
                   
                   ## 3.4 Print number of cells by class
                   output$n_by_class <- 
                     renderText({
                       n_by_class()
                       })
                 }
                 
                 ## 3.5. Summary of unique metadata in subset (both tabs)
                 # lapply creates an output for each metadata category
                 lapply(
                   X = meta_categories(), 
                   FUN = function(category){
                     output[[glue("selected_{category}")]] <-
                       renderText({
                         # Display unique values appearing in the subset 
                         # for the category
                         unique(subset()@meta.data[[category]]) |> 
                           # Sort unique values alphanumerically
                           # May add support for custom order later
                           str_sort(numeric=TRUE) |> 
                           vector_to_text()
                       }) # End renderText
                     }
                   )

                 # 4. Return Stats from Server ---------------------------------
                 # For dge tab: return n_cells, classes, and n_classes
                 # Return a list of reactives, as opposed to a reactive list
                 if (tab=="dge"){
                   return(
                     list(
                       `n_cells` = reactive({n_cells()}),
                       `classes` = reactive({classes()}),
                       `n_classes` = reactive({n_classes()})
                       )
                     )
                   } # Currently no need to return values for correlations tab
                 })
}
