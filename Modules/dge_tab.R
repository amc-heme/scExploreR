#Differential Expression Tab Module

#dge_tab_ui
#Arguments
#id: ID to use for module elements. Should be equal to "dge".
#unique_metadata: a list of the unique metadata values for each of the metadata 
#categories listed in the config file. This is generated in the main server 
#function at startup.
#metadata_config: the metadata section of the config file imported in the 
#main server function
dge_tab_ui <- function(id,
                       unique_metadata,
                       metadata_config){
  #Namespace function: prevents conflicts with 
  #inputs/outputs defined in other modules 
  ns <- NS(id)
  
  #UI for DGE Tab
  fluidPage(
    sidebarLayout(
      #1. Options Panel
      sidebarPanel(
        #Add a container to display a waiter over when the options are updating
        div(id = ns("sidebar"),
            tags$h3("Differential Gene Expression"),
            tags$p("Use the dropdown menus below to select the desired test, 
                   the groups to use, and the subset on which to perform the test."),
            
            #Menus to choose test (DGE or marker identification and 
            #classes/groups to include). Uses dge_test_selection module
            dge_test_selection_ui(id=ns("test_selections"),
                                  meta_choices = meta_choices),
            
            #Menus to choose subset (placed within collapsible panel)
            collapsible_panel(
              inputId = ns("subset_selections"), 
              label = "Subset Options", 
              active = TRUE, 
              {
                subset_selections_ui(id=ns("subset_selections"),
                                     unique_metadata = unique_metadata,
                                     metadata_config = metadata_config)
                }),
            
            #UMAP options panel (conditional UI)
            uiOutput(outputId = ns("umap_options")),
            
            #Checkbox to return positive markers only (shown for both modes)
            checkboxInput(inputId = ns("pos"),
                          label="Positive Markers Only",
                          value = TRUE),
            #Submit button
            actionButton(inputId = ns("submit"),
                         label = "Update"),
            
            #Download Button
            uiOutput(outputId = ns("downloads_ui"))
        )#End dge-sidebar div
      ),#End sidebarPanel (1.3.1)
      
      #2. Main Panel
      mainPanel(
        div(id = ns("main_panel"), 
            class = "spinner-container-main",
            
            # # TEMP: display reactive variables used in this tab while developing
            # "Test Selection Output",
            # verbatimTextOutput(outputId = ns("test_selection_output")),
            # "Subset Selection Output",
            # verbatimTextOutput(outputId = ns("subset_selection_output")),
            # "Subset information:",
            # verbatimTextOutput(outputId = ns("subset_info_output")),
            # "Subset Stats Output:",
            # verbatimTextOutput(outputId = ns("subset_stats_output")),
            
            #Div added to contain Waiter spinner (forces the spinner to cover 
            #the full main panel)
            uiOutput(outputId = ns("main_panel_ui"))
        )#End dge_main_panel
      )#End MainPanel
    )#End sidebarLayout
  )#End fluidPage
}


#dge_tab_server
dge_tab_server <- function(id,
                           sobj,
                           metadata_config,
                           unique_metadata,
                           meta_choices){
  moduleServer(id,
               function(input,output,session){
                 #Namespace function: for dynamic UI and modules
                 ns <- session$ns
                 
                 #This will become TRUE when the subset button is pressed at 
                 #least once
                 # submit_pressed <- reactive({FALSE})
                 #submit_pressed <- FALSE

                 #1. Process Selections for DGE Test ---------------------------
                 test_selections <- 
                   dge_test_selection_server(
                     id = "test_selections",
                     sobj = sobj,
                     unique_metadata = unique_metadata,
                     metadata_config = metadata_config, 
                     meta_choices = meta_choices
                     )
                 
                 #2. Process Subset Selection Options --------------------------
                 ##2.1. Process group_by category from test_selections
                 #The metadata chosen as the group by category in the test 
                 #selections menu must be hidden from the subset selections 
                 #inputs, since the subset must be equal to the classes or 
                 #groups chosen, which are already selected by the user in the
                 #test selections module. 
                 group_by_category <- 
                   eventReactive(test_selections(),
                                 label = "DGE Tab: Process Group by Selection",
                                 {
                                   test_selections()$group_by
                                 })

                 ##2.2. Call subset_selections module
                 #Use observer so subset_selections module reacts to the 
                 #selected group by variable 
                 observe({
                   subset_selections <<- 
                     subset_selections_server(
                       id = "subset_selections",
                       sobj = sobj,
                       unique_metadata = unique_metadata,
                       metadata_config = metadata_config,
                       hide_menu = group_by_category
                       )
                   })
                 
                 # 3. Calculations ran after submit button is pressed ----------
                 #Includes table, stats, and UMAP
                 #Subset criteria (3.1) processed first, UMAP (3.6) processed last
                 ## 3.1. Process Submit button input
                 #Pass value of action button to nested modules to control reactivity
                 submit_button <- reactive({
                   input$submit
                 })
                 
                 ## 3.2. Define subset criteria
                 #Combines outputs from test selection and subset selection 
                 #modules. The subset criteria are used both in the 
                 #make_subset() function and the subset stats module (the 
                 #latter of which is designed to take a reactive variable
                 #as input)
                 dge_subset_criteria <- 
                   eventReactive(
                     submit_button(),
                     label = "DGE: Subset Criteria",
                     ignoreNULL = FALSE,
                     #All reactives in 3. must run at startup for output to be
                     #properly generated (endless spinner results otherwise)
                     ignoreInit = FALSE,
                     {
                       print("3.2 Process subset criteria")
                       #Retrieve information from test_selections
                       #Category
                       group_by_category <- test_selections()$group_by
                       #Chosen groups/classes (store in a vector)
                       if (test_selections()$dge_mode == "mode_dge"){
                         #For DGE mode: set the vector of choices equal to the 
                         #selections for the two groups
                         choices <- 
                           c(test_selections()$group_1, 
                             test_selections()$group_2)
                       } else if (test_selections()$dge_mode == "mode_marker"){
                         #Marker identification: use vector of selected classes
                         choices <- test_selections()$classes_selected 
                       }
                       
                       #Retrieve list of subset selections 
                       #Must unpack from reactive to avoid modifying the reactive
                       #with the test_selections data above
                       subset_criteria <- subset_selections()
                       #Append test_selections information to selections list 
                       subset_criteria[[group_by_category]] <- choices
                       
                       return(subset_criteria)
                     })
                 
                 ## 3.3. Form subset
                 subset <- eventReactive(
                   dge_subset_criteria(),
                   label = "DGE: Subset",
                   ignoreNULL = FALSE,
                   {
                     print("3.3 make subset")
                     #Create subset from selections and return
                     subset <- make_subset(sobj = sobj,
                                           criteria_list = dge_subset_criteria())
                     return(subset)
                   })
                 
                 ## 3.4. Compute subset stats
                 subset_stats <- 
                   eventReactive(
                     subset(),
                     label = "DGE: Subset Stats",
                     ignoreNULL = FALSE,
                     #Do not run at startup
                     #ignoreInit = TRUE,
                     {
                       print("3.4 subset stats")
                       #Initiate subset_stats server and store output
                       stats <- subset_stats_server(
                         id = "subset_stats",
                         tab = "dge",
                         subset = subset,
                         #Pass dge_subset_criteria() to this argument instead of
                         #subset_selections() (dge_subset_criteria() includes
                         #the group by category)
                         subset_selections = dge_subset_criteria,
                         submit_button = submit_button,
                         group_by_category = group_by_category
                         )
                       
                       #Return computed stats from the server module
                       return(stats())
                   }
                 )
                 
                 ## 3.5. Run Presto
                 dge_table_content <- 
                   eventReactive(
                     #subset(),
                     subset_stats(),
                     label = "DGE: Run Presto",
                     ignoreNULL = FALSE,
                     {
                       print("3.5 Run Presto")
                       dge_table <-
                         #Run presto on the subset, using the group by category
                         wilcoxauc(subset(), group_by = group_by_category()) %>%
                         #Explicitly coerce to tibble
                         as_tibble() %>%
                         #remove stat and auc from the output table
                         select(-c(statistic, auc)) %>%
                         #Using magrittr pipes here because the following 
                         #statement doesn't work with base R pipes
                         #remove negative logFCs if box is checked
                         {if (input$pos) filter(., logFC > 0) else .} %>%
                         #Arrange in ascending order for padj, pval (lower 
                         #values are more "significant"). Ascending order is 
                         #used for the log fold-change
                         arrange(padj, pval, desc(abs(logFC)))
                     
                     return(dge_table)
                   }
                 )
                 
                 ## 3.6. DGE table, as DT for viewing
                 dge_DT_content <-
                   eventReactive(
                     dge_table_content(),
                     label = "DGE: DT Generation",
                     ignoreNULL=FALSE,
                     {
                       datatable(
                         dge_table_content(),
                         class = "compact stripe cell-border hover",
                         selection = "none",
                         filter = "top",
                         rownames = FALSE
                         ) %>%
                         #Use 5 sig figs (3 or more is sufficient)
                         formatSignif(3:8, 5)
                       })
                 
                 ## 3.7. UMAP of DE Selected Groups 
                 dge_umap <- 
                   eventReactive(
                     subset(), 
                     ignoreNULL = FALSE,
                     label="DGE: UMAP", 
                     {
                       print("DGE UMAP")
                       #ncol_argument: number of columns
                       #Based on number of classes being analyzed in the subset. 
                       #Access with double brackets returns a dataframe.
                       #Slice for the first row (the unique values)
                       n_panel <- 
                         unique(subset()[[group_by_category()]])[,1] |>
                         length()
                       
                       #Set ncol to number of panels if less than four
                       #Panels are created
                       if (n_panel<4){
                         ncol=n_panel
                         }
                       
                       #Use three columns for 4-9 panels
                       else if (n_panel>=4 & n_panel<9){
                         ncol=3
                         }
                       
                       #Use four columns for 9+ panels
                       else if (n_panel>=9){
                         ncol=4
                         }
                       
                       #Create UMAP of subsetted object
                       umap <- DimPlot(
                         subset(),
                         #Split by thgroup by category
                         split.by = group_by_category(),
                         #Group by variable set in UMAP options panel
                         group.by = input$umap_group_by,
                         ncol=ncol
                         )
                       
                       #Hide loading screen
                       #UMAP is last time-intensive object calculated
                       #after submit button is pressed 
                       waiter_hide(id = ns("main_panel"))
                       waiter_hide(id = ns("sidebar"))
                       
                       return(umap)
                       })
                 
                 # 4. Dynamic UI for Main Panel --------------------------------
                 dge_ui <- 
                   eventReactive(
                     submit_button(),
                     label = "DGE Main UI (Define Content)",
                     #Do not render main UI at startup to avoid errors
                     #ignoreInit=TRUE,
                     #ignoreNULL = FALSE,
                     {
                       print("4. DGE Main UI")
                       #Spinner for main panel
                       #Displays until the UMAP is calculated (last reactive
                       #computed after subset button is clicked)
                       waiter_show(
                         id = ns("main_panel"),
                         html = spin_loaders(
                           id = 2,
                           color = "#555588"
                         ),
                         color = "#FFFFFF",
                         #Gives manual control of showing/hiding spinner
                         hide_on_render = FALSE
                       )

                       #Spinner for options panel
                       #Keeps user from being able to press download buttons
                       #before content is ready
                       waiter_show(
                         id = ns("sidebar"),
                         html = spin_loaders(id = 2, color = "#555588"),
                         color = "#B1B1B188",
                         #Gives manual control of showing/hiding spinner
                         hide_on_render = FALSE
                       )

                       #User-defined label for group-by variable (for printing
                       #in UI below)
                       #TODO: make sure this updates when a different group
                       #by variable is submitted
                       group_by_label <-
                         metadata_config[[test_selections()$group_by]]$label

                       #UI to display
                       tagList(
                         tags$h2(
                           glue("Differential Expression/Marker Genes by
                                {group_by_label} in Subset"),
                           class="center"
                         ),
                         tags$h3("Test Summary", class="center"),
                         #Subset Stats Module for showing summary stats
                         subset_stats_ui(
                           id = ns("subset_stats"),
                           tab = "dge",
                           metadata_config = metadata_config,
                           #Pass dge_subset_criteria() to this argument instead
                           #of subset_selections() (dge_subset_criteria()
                           #includes the group by category)
                           subset_selections = dge_subset_criteria
                         ),
                         #DGE Table (uses DT data table)
                         tags$h3("DGE Table",
                                 class="center"),
                         #Output container for table
                         DTOutput(outputId = ns("table"),
                                  width = "95%"),

                         #UMAP plot
                         #Title for plot
                         #Depends on mode, tested through n_classes()
                         #Make sure n_classes is defined to avoid errors
                         if (!is.null(subset_stats()$n_classes)){
                           #Use different titles based on the test used
                           if(subset_stats()$n_classes == 2){
                             #Title for differential gene expression
                             tags$h3("UMAP of groups being compared",
                                     class="center")
                           } else {
                             #Title for marker identification
                             tagList(
                               #Center text
                               tags$h3("UMAP by class",
                                       class="center"),
                               tags$p("(Markers are computed for each group shown)",
                                      class="center")
                             )
                           }
                         } else NULL,

                         #UMAP container
                         plotOutput(outputId = ns("umap"),
                                    height = "600px")
                       )#End tagList
                       })
                 
                 # 5. Dynamic UI: Download Buttons for Table and Plots ---------
                 dge_downloads_ui <-
                   eventReactive(
                     c(submit_button(), input$table_rows_selected),
                     label = "DGE Download Buttons UI",
                     ignoreNULL = FALSE,
                     {
                       #Conditional level one, !hasName(): TRUE before table 
                       #is created, FALSE after
                       if (!hasName(input, "dge_table_rows_selected")) {
                         #Display nothing before table is created
                         NULL
                       } else {
                         #!hasName()==FALSE (table created)
                         #Display button to download table after table is created
                         div(
                           downloadButton(
                             outputId = "dge_download_table",
                             label = "Download Table",
                             icon = icon("table")
                           )
                         )
                       }
                     }
                   )
                 
                 # 6. Dynamic UI: Options Panel for UMAP -----------------------
                 umap_options <-
                   eventReactive(
                     dge_umap(),
                     label = "DGE: UMAP Options Panel",
                     ignoreNULL = FALSE,
                     {
                       print("class of dge_umap: {class(dge_umap())}")
                       #Display options panel after the umap is created 
                       #Test: dge_umap is of class 'ggplot'
                       if ("ggplot" %in% class(dge_umap())){
                         collapsible_panel(
                           inputId = ns("umap_options_panel"),
                           label = "UMAP Options",
                           active = TRUE,
                           #group.by selection
                           selectInput(
                             inputId = ns("umap_group_by"),
                             label = "Metadata to Group by:",
                             #Remove "none" from selectable 
                             #options to group by
                             choices=
                               meta_choices[!meta_choices %in% "none"], 
                             #First category in meta_choices is selected 
                             #by default
                             selected = 
                               meta_choices[!meta_choices %in% "none"][1]
                             )
                           )
                       } else {
                         #Return nothing unless the UMAP is created
                         NULL
                       }
                     }
                   )
                 
                 # 7. Render DGE UI, table, and UMAP ---------------------------
                 #Main UI
                 output$main_panel_ui <- renderUI({
                   dge_ui()
                 })
                 
                 #Options panel for UMAP
                 output$umap_options <- renderUI({
                   umap_options()
                 })
                 
                 #Download buttons
                 output$downloads_ui <- renderUI({
                   dge_downloads_ui()
                 })
                 
                 #Table
                 output$table <- renderDT({
                   dge_DT_content()
                 })
                 
                 #UMAP plot
                 output$umap <- renderPlot({
                   dge_umap()
                 })
                 
                 #During development: render outputs of reactive variables in tab
                 # output$test_selection_output <- renderPrint({
                 #   test_selections()
                 # })
                 # output$subset_selection_output <- renderPrint({
                 #   subset_selections()
                 # })
                 # output$subset_info_output <- renderPrint({
                 #   subset()
                 # })
                 # output$subset_stats_output<- renderPrint({
                 #   subset_stats()
                 # })
                 
                 # 8. Download Handler for DGE Table ---------------------------
                 output$dge_download_table <- downloadHandler(
                   filename = function() {
                     glue("DGE_table_{input$dge_group_by}.csv")
                   },
                   content = function(file) {
                     write.csv(dge_table_content(),
                               file = file,
                               row.names = FALSE)
                   },
                   contentType = "text/csv"
                 )#End downloadHandler 
               }
  )
}
  