#Subset stats module

#subset_stats_ui
#Arguments
#id: The id to use for the namespace created for this module.
#tab: String giving the tab this module applies to. This should be either "dge" or "corr".
#metadata_config: the metadata sub-list defined within the config list 
#subset_selections: A reactive list giving the metadata values included in the subset.
#gene_selected: In the corr tab, the gene selected by the user for computation.
subset_stats_ui <- function(id,
                            tab=c("dge","corr"),
                            metadata_config,
                            subset_selections,
                            gene_selected=NULL
                            ){
  #Namespace function: prevents conflicts with inputs/outputs defined in other modules
  ns <- NS(id)
  
  if (tab=="dge"){
    #UI for stats in dge tab
    
  } else if (tab=="corr"){
    #UI for stats in correlations tab
    ui <- tagList(
      tags$strong("Subset Summary and Quality Statistics", 
                  class="x-large inline-block space-top"),
      #Restriction criteria
      #Create outputs for each subset menu
      lapply(
        #Loop through the category names associated with subset criteria
        X = names(subset_selections()), 
        FUN = function(category, metadata_config){
          #Get label for the metadata category used in the current subset selection 
          #menu, defined in config file
          label<- metadata_config[[category]]$label
        
          #Return HTML
          #Use of div creates a new line between each entry
          div(
            tags$strong(glue("{label}:")),
            #Text output to report values in selected category found in the subset
            textOutput(
              #Output ID: uses the category name 
              outputId = ns(glue("selected_{category}")),
              inline=TRUE)
            )
          },
        #metadata_config must be passed to the function above after it is 
        #defined (see ... argument in ?lapply)
        metadata_config
        ), #End lapply
      
      #Number of cells in subset
      div(
        tags$strong("Number of cells in subset: ",
                      class="space-top inline-block"),
        textOutput(outputId = ns("n_cells"), inline = TRUE)
        ),
      
      #Number and percentage of cells with nonzero reads for the selected gene
      div(
        tags$strong(glue("Cells with non-zero reads for {gene_selected()}:")),
        textOutput(outputId = ns("n_nonzero_and_percent"), inline = TRUE)
        )
      )#End tagList
    
    #Return UI from module to the parent UI 
    return(ui)
  }
  
}

#subset_stats_server
#Arguments
#id: The id to use for the namespace created for this module.
#tab: String giving the tab this module applies to. This should be either "dge" or "corr".
#subset: A subsetted Seurat object for which stats will be computed (reactive).
#subset_selections: A reactive list giving the metadata values included in the subset.
#submit_button: the value of the "Submit" action button in the options panel. 
#Used to control reactivity of outputs.
#gene_selected: In the corr tab, the gene selected by the user for computation.
#nonzero_threshold: In the corr tab, the minimum acceptable proportion of nonzero 
#reads. A warning notification will be displayed to the user if the proportion 
#is below this threshold.
subset_stats_server <- function(id,
                                tab=c("dge","corr"),
                                subset,
                                subset_selections,
                                submit_button,
                                gene_selected=NULL,
                                nonzero_threshold=NULL
                                ){
  moduleServer(id, 
               function(input,output,session){
                 #Server namespace function (used for renderUI and JavaScript ID references)
                 ns <- session$ns
                 
                 # 1. Compute stats for subset ---------------------------------
                 #Cells in subset
                 n_cells <- reactive({
                   length(Cells(subset()))
                 })
                 
                 #Nonzero reads, proportion of nonzero reads, and percentage
                 #Computed for the correlations tab only
                 if (tab=="corr"){
                   #Cells with nonzero reads
                   n_nonzero <- reactive({
                     sum(subset()@assays$RNA@counts[gene_selected(),] != 0)
                   })
                   
                   #Proportion of nonzero reads
                   prop_nonzero <- reactive({
                     n_nonzero()/n_cells()
                   })
                   
                   #Percentage of nonzero reads
                   percent_nonzero <- reactive({
                     format(
                       prop_nonzero()*100,
                       #Display at least three sig figs in percentage
                       digits=3,
                       #Display at least two digits after decimal point
                       nsmall=2,
                       scientific=FALSE
                     )
                   })
                 }
                 
                 # 2. Notifications --------------------------------------------
                 # 2.1. Nonzero proportion is below the defined threshold 
                 #Use observe statement to reactively check prop_nonzero()
                 observeEvent(submit_button(),
                              ignoreNULL=FALSE,
                              {
                                if (prop_nonzero() < nonzero_threshold){
                                  #Define notification UI (warning icon plus text)
                                  notification_ui <- span(
                                    #Warning icon (inline and enlarged)
                                    icon("exclamation-triangle",
                                         style="display: inline-block; font-size: 1.7em;"),
                                    
                                    #Notification text with proportion and number of non-zero cells
                                    span(glue("Low gene coverage: the selected 
                                    feature was detected in {percent_nonzero()}%
                                    of cells within the selection restriction 
                                    criteria ({n_nonzero()}/{n_cells()} cells). 
                                    Correlation results may be inaccurate."),
                                    #Font size of notification text
                                    style="font-size: 1.17em;")#End span
                                    )#End notification_ui span
                     
                                  #Display notification UI
                                  showNotification(ui=notification_ui,
                                                   #Duration=NULL will make the 
                                                   #message persist until dismissed
                                                   duration = NULL,
                                                   id = ns("high_zero_content"),
                                                   session=session)
                                  }#End if statement
                                })
                 
                 # 3. Display stats --------------------------------------------
                 ## 3.1. Number of cells
                 output$n_cells <- renderText({
                   #Use isolate to keep values from updating before the submit 
                   #button is pressed
                   isolate(n_cells())
                 })
                 
                 ## 3.2 Cells with nonzero reads (correlations tab only)
                 if (tab=="corr"){
                   output$n_nonzero_and_percent <- renderText({
                     isolate(glue("{n_nonzero()} ({percent_nonzero()}%)"))
                   })
                 }
                 
                 ## 3.3. Summary of unique metadata in subset
                 #Create an output for each metadata category used in creation 
                 #of the subset
                 lapply(X = names(subset_selections()), 
                        FUN = function(category){
                          output[[glue("selected_{category}")]] <-
                            renderText({
                              isolate(
                                #Display unique values appearing in the subset 
                                #for the category
                                unique(subset()@meta.data[[category]]) |> 
                                  #Sort unique values alphanumerically
                                  #May add support for custom order later
                                  str_sort(numeric=TRUE) |> 
                                  vector_to_text()
                                ) #End isolate
                            }) #End renderText
                        })
                 
               })
}
