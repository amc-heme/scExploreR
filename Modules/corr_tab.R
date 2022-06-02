# Correlations tab module
 
# corr_tab_ui
# Arguments
# id: ID to use for module elements. Should be equal to "corr".
# unique_metadata: a list of the unique metadata values for each of the metadata 
# categories listed in the config file. This is generated in the main 
# server function at startup.
# metadata_config: the metadata section of the config file imported 
# in the main server function
corr_tab_ui <- function(id,
                        unique_metadata,
                        metadata_config
                        ){
  # Namespace function: prevents conflicts with inputs/outputs defined in 
  # other modules 
  ns <- NS(id)
  
  fluidPage(
    sidebarLayout(
      # 1.3.1. Options Panel
      sidebarPanel(
        # Add a block to display a waiter over when the options are updating
        div(id = ns("sidebar"),
            # 1.3.1.1. Restrict correlation table by metadata
            tags$h3("Correlation Coefficients"),
            tags$p("Enter one feature to view the top features positively and 
                   negatively correlated with the feature in the data. You may 
                   optionally restrict the correlation analysis by metadata 
                   variables using the dropdown menus below."),
            
            # Feature selection: only one feature can be entered
            selectizeInput(
              inputId = ns("feature_selection"),
              label = "Gene Selection",
              # Feature choices populated in server, as in the plots tab
              choices = NULL,
              selected = character(0),
              options = list("placeholder" = "Enter gene name")),
            
            # Module for subset selections              
            subset_selections_ui(
              # Create separate module instances for each datset to avoid 
              # namespace collisions between datasets. Use key of dataset in id
              id = ns("subset_selections"),
              unique_metadata = unique_metadata,
              metadata_config = metadata_config
              ),
            
            # Submit button
            actionButton(
              inputId = ns("submit"),
              label = "Submit",
              # Display inline with download button when it appears
              class = "inline-block"
              ),
            
            # Download buttons: render after the correlations table 
            # and scatterplots are computed
            uiOutput(
              outputId = ns("downloads_ui"), 
              inline = TRUE
              ),
            
            # Options for scatterplot: a collapsible panel of options
            # appears after a scatterplot is displayed
            uiOutput(outputId = ns("scatter_options_ui"))
        ) # End corr-sidebar div
      ), # End sidebarPanel (1.3.1)
      
      # 1.3.2 Main Panel
      mainPanel(
        div(id = ns("main_panel"), 
            class = "spinner-container-main", 
            # Div added to contain Waiter spinner (forces the spinner to cover 
            # the full main panel)
            uiOutput(outputId = ns("main_panel_ui")))
        
      ) # End MainPanel
    ) # End sidebarLayout
  ) # End fluidPage
}

# corr_tab_server
# Arguments
# id: Used to match server function to ui function. "corr" is the recommended 
# value to use
# object: The Seurat Object defined in the main server function
# metadata_config: metadata section of the config file imported in the main server function
# unique_metadata: a list of the unique metadata values for each of the metadata 
# categories listed in the config file. This is generated in the main server function 
# at startup.
# n_cells_original: number of cells in full Seurat object. Calculated in main 
# server function.
# nonzero_threshold: the minimum acceptable proportion of cells with nonzero reads 
# for the feature used to compute correlations. This is defined in the 
# main server function.
# meta_choices: a named vector giving the metadata categories defined in the 
# config file, with their respective labels for display in dropdown menus. This 
# is defined in the main server function.
# valid features: a list giving the valid features that can be selected from each
# assay. This is generated from the config file in the main server function
# error_list: a list of error messages to print custom notifications for, 
# if they are encountered while the correlations table is being calculated. This 
# is defined in the main server function.
# update_features: a reactive trigger created by the local makeReactiveTrigger() 
# function. The trigger is created in the main server function and ensures that
# the feature selection input is updated after the input is created (input will
# have no choices otherwise.)
# object_trigger: a reactive trigger that invalidates downstream reactives when 
# the object is changed.
corr_tab_server <- function(id,
                            object,
                            metadata_config,
                            # This will replace metadata_config at some point
                            # (It is derived from the config file and is the 
                            # only information used)
                            meta_categories,
                            unique_metadata,
                            n_cells_original,
                            nonzero_threshold,
                            meta_choices,
                            valid_features,
                            error_list,
                            update_features,
                            object_trigger
                            ){

  #Function to initialize module server
  moduleServer(id,
               function(input,output,session){
                 # Server namespace function: required for ids defined within 
                 # renderUI functions, but not for other ids 
                 ns <- session$ns
                 
                 # Correlations Tab Server ###
                 # TODO: Entire correlations tab only supports 
                 # the RNA assay. Should expand to other assays at some point.
                 
                 # Define spinners to display during computation
                 # Spinner for options panel
                 sidebar_spinner <- 
                   Waiter$new(
                     id = ns("sidebar"),
                     html = spin_loaders(id = 2, color = "#555588"),
                     color = "#B1B1B188",
                     #Gives manual control of showing/hiding spinner
                     hide_on_render = FALSE
                   )
                 
                 #Spinner for main panel
                 main_spinner <-
                   Waiter$new(
                     id = ns("main_panel"),
                     html = spin_loaders(id = 2, color = "#555588"),
                     color = "#FFFFFF",
                     #Gives manual control of showing/hiding spinner
                     hide_on_render = FALSE
                   )
                 
                 # 1. Render Choices for Feature selection ---------------------
                 # 1.1. Render Choices ####
                 # Reactive - updates in response to change in dataset 
                 observeEvent(
                   # Responds to loading of update and creation of UI (to ensure
                   # feature updating is always performed after the input is 
                   # created)
                   c(valid_features(), update_features$depend()),
                   label = "Render choices for feature selection",
                   {
                     updateSelectizeInput(
                       session,
                       inputId = "feature_selection",
                       # Include only genes for now (this will be 
                       # the first row of valid_features)
                       choices = valid_features()[[1]],
                       selected = character(0),
                       server = TRUE
                       )
                   })
                 
                 # 2. Process Inputs -------------------------------------------
                 # Inputs are packaged into reactive values for proper 
                 # processing in downstream modules and functions
                 
                 ## 2.1. Gene selection for correlation analysis ####
                 corr_main_gene <- 
                   eventReactive(
                     input$feature_selection,
                     ignoreNULL = FALSE,
                     label = "Corr: Process Selected Feature",
                     {
                       # Only run if a feature is defined 
                       # (avoids downstream errors)
                       # req(input$feature_selection, cancelOutput = TRUE)
                        
                       # HARD CODING: remove "rna_" prefix 
                       # from feature entered. Currently specific 
                       # to RNA assay in this Seurat object
                       return(sub("rna_", "", input$feature_selection))
                       })
                 
                 ## 2.2. Inputs in subset selections menus ####
                 # Module to record selections made in for subsetting based on 
                 # metadata categories
                 
                 ### 2.2.1 Create module instances for each dataset
                 subset_selections <- 
                   subset_selections_server(
                     # Use key of dataset in the id to avoid collisions
                     id = "subset_selections",
                     object = object,
                     unique_metadata = unique_metadata,
                     metadata_config = metadata_config,
                     meta_categories = meta_categories,
                     valid_features = valid_features
                     )
                 
                 # 3. Computation of Correlation Table
                 # A chain of reactive expressions is used, beginning with a 
                 # reactive showing the spinners after the submit button is 
                 # pressed.
                 
                 ## 3.1. Process Submit button input ####
                 submit_button <- 
                   eventReactive(
                     input$submit,
                     label = "Corr: Submit Button (Show Spinners)",
                     ignoreNULL = FALSE,
                     ignoreInit = TRUE,
                     {
                       #print("3.1. Show Spinners")
                       # Show spinners if the submit button is pressed and a 
                       # feature has been selected (requirement to begin 
                       # calculation)
                       
                       # Test if the input is null separately from it being an
                       # empty string (input$feature_selection is not 
                       # initialized when a new dataset is loaded, which will 
                       # cause errors with the conditional testing if it is an 
                       # empty string, which is the value of 
                       # input$feature_selection when it exists and no features 
                       # have been entered)
                       log_session(session)
                       log_info("Corr tab: Submit button pressed")
                       if (isTruthy(input$feature_selection)){
                         log_info("Displaying spinners")
                         sidebar_spinner$show()
                         main_spinner$show()
                       } else {
                         # If the submit button is pressed without specifying
                         # a feature, notify the user 
                         
                       }
                       
                       # Always return value of submit button
                       input$submit
                       }
                     )

                 ## 3.2 Form subset based on chosen criteria ####
                 # Initialize variables to control reset of subset when the 
                 # object is changed
                 
                 # object_init: a reactive value set to TRUE when a new object 
                 # is loaded. The currently saved subset will be cleared when
                 # object_init is set to TRUE
                 object_init <- reactiveVal(FALSE)
                 
                 # Create a reactive trigger (triggers reset of subset when
                 # new object is loaded)
                 subset_trigger <- makeReactiveTrigger()
                 
                 # Upon object change: set object_init to TRUE and trigger 
                 # subset eventReactive
                 observeEvent(
                   # Reacts to object_trigger defined in main server function
                   object_trigger$depend(),
                   label = "DGE: object_init(TRUE)",
                   # Must use ignoreNULL = FALSE for reactive triggers
                   ignoreNULL = FALSE,
                   # Don't want this to run at startup or upon module creation
                   ignoreInit = TRUE,
                   {
                     object_init(TRUE)
                     subset_trigger$trigger()
                   })
                 
                 # Store in reactive variable
                 subset <- 
                   eventReactive(
                     c(submit_button(), subset_trigger$depend()),
                     label = "Corr: Subset",
                     ignoreNULL = FALSE,
                     ignoreInit = TRUE,
                     {
                       #print("3.2 make subset")
                       
                       if (object_init() == TRUE){
                         # If object_init==TRUE, set subset equal to the full 
                         # object
                         return(object())
                       } else {
                         # Otherwise, create subset from selections and return
                         return(
                           make_subset(
                             object = object,
                             criteria_list = subset_selections$selections,
                             user_string = subset_selections$user_string
                             )
                           )
                         }
                       })
                 
                 ## 3.2.a. Check Memory usage after making subset
                 observeEvent(
                   label = "Corr: Subset Memory Query",
                   subset(),
                   {
                     log_session(session)
                     log_info(
                       glue(
                         "Memory used after creating subset in corr tab: {to_GB(mem_used())}"
                         )
                       )
                   })
                 
                 ## 3.3. Correlations Tab Continuation Conditional ####
                 # After subset is computed, downstream computations should only 
                 # proceed if the subset has been created as a result of 
                 # pressing the submit button, and not as a result of the reset 
                 # that ocurrs when switching tabs.
                 
                 # Reactive trigger: proceeds with computation if the subset is 
                 # reset due to switching tabs (when object_init()==TRUE)
                 continue <- makeReactiveTrigger()
                 
                 observeEvent(
                   label = "Corr: Continuation Conditional",
                   subset(),
                   ignoreNULL = FALSE,
                   # Does not work properly when ignoreInit == TRUE
                   ignoreInit = FALSE,
                   {
                     #print("3.3. Continuation Conditional")
                     if (object_init() == FALSE){
                       # If object_init == FALSE,
                       # test if features are entered
                       if (isTruthy(input$feature_selection)){
                         # Continue if a feature is entered
                         continue$trigger()
                       } else {
                         # If a feature is not set, do not continue, 
                         # and hide spinners.
                         sidebar_spinner$hide()
                         main_spinner$hide()
                         
                         # Also, notify the user.
                         showNotification(
                           ui = 
                             icon_notification_ui_2(
                               icon = "exclamation-triangle",
                               # Change to feature when other 
                               # features are supported
                               "A gene must be specified to 
                                 recieve correlation results."
                             ),
                           #Show notification for 3 seconds
                           duration = 3,
                           session=session
                         )
                       }
                     } else {
                       # If object_init == TRUE, do not continue.
                       
                       # Set object_init() back to FALSE in this case
                       object_init(FALSE)
                       
                       # In case spinners are displayed, remove them
                       sidebar_spinner$hide()
                       main_spinner$hide()
                     }
                     
                   })
                 
                 ## 3.4. Determine if the subset created is a subset ####
                 is_subset <- 
                   eventReactive(
                     continue$depend(),
                     ignoreNULL = FALSE,
                     ignoreInit = TRUE,
                     label = "Corr: Determine if Object is a Subset",
                     {
                       #print("3.4 is_subset")
                       # Print an error if the subset does not exist or is NULL
                       validate(
                         need(
                           subset(),
                           message = "subset is NULL"
                         )
                       )
                       
                       # Compute number of cells in subset
                       n_cells_subset <-
                         subset() |>
                         Cells() |>
                         length()
                       
                       # Test if the number of cells in the subset differs from
                       # the number of cells in the original object. If this
                       # conditional is TRUE, then the object read is a subset
                       n_cells_original() != n_cells_subset
                       })
                 
                 ## 3.5. Subset Stats Module ####
                 subset_stats_server(
                   id = "stats",
                   tab = "corr",
                   subset = subset,
                   meta_categories = meta_categories,
                   # Reactive expressions in module will execute after 
                   # is_subset is computed
                   event_expr = is_subset,
                   gene_selected = corr_main_gene,
                   nonzero_threshold = nonzero_threshold
                 )
                 
                 ## 3.6. Compute correlation tables ####
                 # Calculations used depend on whether the object is a subset
                 corr_table_content <-
                   eventReactive(
                     is_subset(),
                     label = "Corr: Corr Table",
                     ignoreNULL = FALSE,
                     # Does not execute properly when ignoreInit == TRUE
                     ignoreInit = FALSE,
                     {
                       log_session(session)
                       log_info("Corr tab: Begin correlation computations")
                       #print("3.6 Compute Correlation Tables")
                       if (is_subset() == TRUE){
                         # Subset is selected: compute tables for full object 
                         # and subset, then merge
                         # Full object
                         table_full <- 
                           compute_correlation(
                             gene_selected = corr_main_gene,
                             object = object,
                             colnames =
                               c("Feature",
                                 "Correlation_Global")
                           )
                         
                         # Subset
                         table_subset <- 
                           compute_correlation(
                             gene_selected = corr_main_gene,
                             object = subset,
                             colnames = 
                               c("Feature",
                                 "Correlation_Subset")
                           )
                         
                         # Merge individual tables and arrange in descending 
                         # order by the subset correlation coefficient
                         corr_table <- 
                           merge(
                             table_full,
                             table_subset,
                             by = "Feature"
                             ) |>
                           arrange(
                             desc(
                               .data[["Correlation_Subset"]]
                             )
                           )
                       } else {
                         # If a subset is not present: compute the table for the 
                         # full data only (which is the "subset" in this case)
                         corr_table <- 
                           compute_correlation(
                             gene_selected = corr_main_gene,
                             object = subset,
                             colnames = c("Feature","Correlation_Subset")
                           )
                       }
                       
                       log_session(session)
                       log_info("Corr tab: Computations complete")
                       
                       # Return the computed table
                       corr_table
                     })

                 ## 3.7. Store table in DT format for display in app ####
                 # Reactive trigger to remove spinners upon completion
                 remove_spinners <- makeReactiveTrigger()
                 corr_DT_content <- 
                   eventReactive(
                     corr_table_content(),
                     label = "Corr: DT Content",
                     ignoreNULL = FALSE,
                     {
                       #print("3.7. DT Table")
                       # Define header for datatable using HTML
                       if (is_subset() == TRUE){
                         # If a subset is selected, the header will have three 
                         # columns for the feature, the global correlation 
                         # coefficients, and the correlation coefficients 
                         # for the subset
                         header <- 
                           tags$table(
                             # center-colnames class: centers the 
                             # column names in the header
                             class = "compact stripe cell-border 
                             hover center-colnames",
                             tags$thead(
                               tags$tr(
                                 tags$th("Feature"),
                                 tags$th(
                                   tagList(
                                     "Correlation",
                                     tags$br(),
                                     "(Global)"
                                     )
                                   ),
                                 tags$th(
                                   tagList(
                                     "Correlation",
                                     tags$br(),
                                     "(Subset)"
                                     )
                                   ) # End th
                                 ) # End tr
                               ) # End thead
                             ) # End table
                         
                         } else {
                           # If the full data is used, display two columns
                           # (feature and correlation coefficient in full data)
                           header <- 
                             tags$table(
                               # center-colnames class: centers the
                               # column names in the header
                               class = "compact stripe cell-border 
                               hover center-colnames",
                               tags$thead(
                                 tags$tr(
                                   tags$th("Feature"),
                                   tags$th(
                                     tagList(
                                       "Correlation Coefficient",
                                       tags$br(),
                                       "(Global)"
                                       )
                                     ) # End th
                                   ) # End tr
                                 ) # End thead
                             ) # End table tag
                           }
                       
                       DT <-
                         datatable(
                           corr_table_content(),
                           class = "compact stripe cell-border hover",
                           selection = "single",
                           filter = "top",
                           rownames = FALSE,
                           container = header
                           ) %>%
                         # Use 5 sig figs for pearson coefficientcolumn(s). If a 
                         # subset is used, this will be columns 2 and 3; 
                         # if not, this will be column 2.
                         formatSignif(
                           columns = if (is_subset() == TRUE) c(2,3) else 2,
                           digits = 5
                           )
                       
                       remove_spinners$trigger()
                       
                       # Return DT datatable
                       DT
                       })
                 
                 # 3.8 Hide spinners after the DT datatable is computed ####
                 observeEvent(
                   remove_spinners$depend(),
                   label="Corr: Hide Spinners",
                   ignoreNULL = FALSE,
                   ignoreInit = TRUE,
                   {
                     #print("3.8. Hide spinners")
                     # Hide loading screen
                     main_spinner$hide()
                     sidebar_spinner$hide()
                     log_session(session)
                     log_info("Corr tab: Hide spinners")
                     # Show content in main panel
                     showElement(id = ns("main_panel_ui"))
                   })
                 
                 # 4. Correlations UI ------------------------------------------
                 ## 4.1. Main Panel UI ####
                 main_panel_ui <- 
                   eventReactive(
                     submit_button(),
                     label = "Corr: Main UI",
                     {
                       #print("C.4.1: Main UI")
                       # UI: if the feature selection menu is empty (default 
                       # state at initialization), prompt user to enter features
                       # isTruthy will cover a variety of possible scenarios
                       # ("" or NULL). This is the same test used by req()
                       if (!isTruthy(input$feature_selection)){
                         NULL
                         } else {
                           # If a feature has been defined, generate table UI
                           tagList(
                             tags$h2(
                             glue("Correlation Analysis for 
                                  {corr_main_gene()}"), 
                             class = "center"),
                             # Subset stats module UI
                             # Prints output containers and text to report the 
                             # metadata included in the subset and the amount of 
                             # nonzero reads for that gene in the subset
                             subset_stats_ui(
                               # Use namespacing for module UI instance
                               # Use key of currently selected dataset to avoid
                               # collisions between inputs of different datasets
                               id = ns("stats"),
                               tab = "corr",
                               metadata_config = metadata_config,
                               meta_categories = meta_categories,
                               subset_selections = subset_selections,
                               gene_selected = corr_main_gene
                               ),
                             
                             # Correlations table and plots
                             tags$h3(
                               "Correlated Genes",
                               class = "center"
                               ),
                             
                             # Table: rendered inline 
                             div(
                               class = "two-column",
                               style = "width: 40%; float: left;",
                               tags$strong(
                                 "Correlation Table", 
                                 class="center single-space-bottom"
                                 ),
                               # Use a DT data table
                               DTOutput(
                                 outputId = ns("corr_table")
                                 )
                               ),
                             
                             # Scatterplot: only appears after the user makes 
                             # a selection on the table
                             div(
                               class = "two-column",
                               style = "width: 60%; float: right;",
                               # UI for scatterplot rendered in 
                               # separate eventReactive function
                               uiOutput(
                                 outputId = ns("scatterplot_ui")
                                 )
                               )
                           )# End tagList
                           }
                       })
                 
                 ## 4.2. Correlations scatterplot UI ####
                 # Computed separately from main UI since it responds to a 
                 # different user input (clicking table)
                 scatterplot_ui <- 
                   eventReactive(
                     c(input$corr_table_rows_selected,
                       is_subset()),
                     label = "Corr: Scatterplot UI",
                     ignoreNULL = FALSE,
                     {
                       if (length(input$corr_table_rows_selected) > 0){
                         # Display the graph if rows are selected
                         if (is_subset() == TRUE){
                           # If a subset is selected, display two plots: 
                           # one for the subset and one for the full data.
                           tagList(
                             tags$strong(
                               "Scatterplot for Subset",
                               class = "center single-space-bottom"),
                             plotOutput(
                               outputId = ns("subset_scatterplot"), 
                               height = "400px", 
                               width = "400px"
                               ),
                             tags$strong(
                               "Scatterplot for Full Data",
                               class="center single-space-bottom"
                               ),
                             plotOutput(
                               outputId = ns("full_data_scatterplot"), 
                               height = "400px", 
                               width = "400px"
                               )
                             )
                           
                           } else {
                             # Otherwise, display only one scatterplot.
                             tagList(
                               tags$strong(
                                 "Scatterplot",
                                 class = "center single-space-bottom"),
                               plotOutput(
                                 outputId = ns("full_data_scatterplot"),
                                 height = "400px", 
                                 width="400px"
                                 )
                             )
                           }
                         }
                       })
                 
                 ## 4.3. UI for customizing the scatterplot ####
                 # This appears in the sidebar and displays a list of options 
                 # used for customizing the scatterplot
                 scatter_options <- 
                   eventReactive(
                     c(input$corr_table_rows_selected, 
                       is_subset()),
                     label="Corr: Scatterplot Options UI",
                     ignoreNULL = FALSE,
                     {
                       # If a selection in the table is made, display a 
                       # collapsible_panel with a list of options for 
                       # customization
                       if (length(input$corr_table_rows_selected) > 0){
                         collapsible_panel(
                           inputId = ns("scatter_options"),
                           label = "Scatterplot Options",
                           active = TRUE,
                           # group.by selection
                           selectInput(
                             inputId = ns("scatter_group_by"),
                             label = "Metadata to Group by:",
                             # Remove "none" from selectable options to group by
                             choices = 
                               meta_choices()[!meta_choices() %in% "none"], 
                             selected = "clusters"
                             ),
                           
                           # Download button for scatterplot (subset)
                           # Displays only if a subset is selected
                           if (is_subset() == TRUE){
                             downloadButton(
                               outputId = ns("download_scatter_subset"),
                               label = "Download Scatterplot (Subset)",
                               # Adds space before button
                               class = "space-top",
                               icon = icon("poll")
                               )
                             } else NULL, # End downloadButton tag
                           
                           # Download button for scatterplot (full data)
                           downloadButton(
                             outputId = ns("download_scatter_global"),
                             # Label changes based on whether 
                             # a subset is selected
                             label = if (is_subset() == TRUE){
                               "Download Scatterplot (Full Data)"
                               } else {
                                 "Download Scatterplot"
                                 },
                             
                             # space-top class: adds space before button 
                             # this is only needed when a subset is 
                             # selected and there are two buttons
                             class = if (is_subset() == TRUE){
                               "space-top"
                               } else NULL,
                             icon = icon("poll")
                             ) # End downloadButton
                           ) # End collapsible_panel  
                         } # End if statement
                       })
                 
                 ## 4.4. Download Button for Table ####
                 downloads_ui <- 
                   eventReactive(
                     c(submit_button(),
                       input$corr_table_rows_selected),
                     label = "Corr: Table Download Button UI",
                     ignoreNULL = FALSE,
                     {
                       # Condition !hasName(): 
                       # TRUE before table is created, FALSE after
                       if (!hasName(input,"corr_table_rows_selected")){
                         # Display nothing before table is created
                         NULL 
                         } else {
                           # Display download button after table is created
                           downloadButton(
                             outputId = ns("download_table"),
                             label = "Download Table",
                             # Add space before button
                             class = "inline-block",
                             icon = icon("table")
                             )
                           } # End else
                       })
                 
                 # 5. Server Value for Rows Selected from Table ----------------
                 # Creates a reactive boolean that is TRUE when the user has 
                 # selected  a gene in the correlations table, and FALSE if not. 
                 # This was created to avoid an error in the display of 
                 # correlation table  plots where an error message flickers in 
                 # the plots before displaying them, which may confuse users.
                 rows_selected <- 
                   eventReactive(
                     input$corr_table_rows_selected,
                     label = "Rows Selected: Server Value",
                     {
                       # Set rows_selected() to TRUE when 
                       # input$corr_table_rows_selected is not NULL, and not 
                       # equal to `character(0)` (value assigned by Shiny when
                       # no rows are selected)
                       if (
                         (!identical(input$corr_table_rows_selected,character(0)))&
                         (!is.null(input$corr_table_rows_selected))
                         ){
                         rows_selected = TRUE
                         } else {
                           # If a row is deselected or the table is re-computed, 
                           # this must be set back to 
                           #FALSE to keep the scatterplot from running 
                           #when a feature is not selected, which will 
                           #cause an error
                           rows_selected = FALSE
                           }
                                   
                       return(rows_selected)
                       })
                 
                 
                 # 6. Plot of feature selected from table ----------------------
                 ## 6.1. Correlation scatterplot for subset
                 # Computes a scatterplot for a secondary gene selected by the 
                 # user from the correlations table.
                 # Row index of user selection from table is stored in 
                 # input$corr_table_rows_selected. eventReactive responds to
                 # input$corr_table_rows_selected and rows_selected()
                 # rows_selected()  prevents the code from running when the 
                 # user has de-selected values 
                 subset_scatterplot <- 
                   eventReactive(
                     c(input$corr_table_rows_selected,
                       rows_selected(),
                       input$scatter_group_by),
                     label = "Correlation Scatterplot Content (Subset)",
                     {
                       row_idx <- input$corr_table_rows_selected
                       # Take action only if a row is selected
                       if (rows_selected() == TRUE){
                         # Record gene name of row selected
                         # Superassignment ensures value is accessible elsewhere in app
                         corr_secondary_gene <<- reactive({
                           as.character(corr_table_content()[row_idx,1])
                           })
                         
                         # Make and store scatterplot
                         FeatureScatter(
                           subset(), 
                           feature1 = corr_main_gene(),
                           feature2 = corr_secondary_gene(),
                           # group.by and split.by 
                           # according to user input
                           group.by = input$scatter_group_by
                           )
                         }
                       })
                 
                 ## 6.2. Correlation plot for full data
                 full_data_scatterplot <- 
                   eventReactive(
                     c(input$corr_table_rows_selected, 
                       rows_selected(),
                       input$scatter_group_by),
                     label = "Correlation Scatterplot Content (Global)",
                     {
                       row_idx <- input$corr_table_rows_selected
                       # Take action only if a row is selected 
                       if (rows_selected() == TRUE){
                         # TODO: REMOVE NESTED REACTIVE
                         # Record gene name of row selected
                         # Superassignment ensures value is 
                         # Accessible elsewhere in app
                         corr_secondary_gene <<- reactive({
                           as.character(corr_table_content()[row_idx,1])
                           })
                         
                         #Make and store scatterplot 
                         #Use full object
                         FeatureScatter(
                           object(), 
                           feature1 = corr_main_gene(),
                           feature2 = corr_secondary_gene(),
                           #group.by and split.by according to user input
                           group.by = input$scatter_group_by
                           )
                         }
                       })
                 
                 # 7. Render Correlation UI, table, scatterplot, and statistics ----
                 # Main panel UI
                 output$main_panel_ui <- renderUI({
                   main_panel_ui()
                 })
                 
                 # Container for scatterplots (main panel)
                 output$scatterplot_ui <- renderUI({
                   scatterplot_ui()
                 })
                 
                 # Scatterplot (main panel, in UI container)
                 observeEvent(input$corr_table_rows_selected, 
                              label = "Render Corr Scatter (Subset)", 
                              {
                                output$subset_scatterplot <- 
                                  renderPlot({
                                    subset_scatterplot()
                                    })
                              })
                 
                 # Scatterplot (main panel, in UI container)
                 observeEvent(input$corr_table_rows_selected, 
                              label = "Render Corr Scatter (Global)", 
                              {
                                output$full_data_scatterplot <- 
                                  renderPlot({
                                    full_data_scatterplot()
                                    })
                              })
                 
                 # Options for scatterplot (sidebar panel)
                 output$scatter_options_ui <- renderUI({
                   scatter_options()
                 })
                 
                 # Download button for Table
                 output$downloads_ui <- renderUI({
                   downloads_ui()
                 })
                 
                 # Table
                 output$corr_table <- renderDT({
                   corr_DT_content()
                 })
                 
                 # 8. Download Handlers ----------------------------------------
                 # Correlations Table
                 output$download_table <- downloadHandler(
                   filename = function(){
                     glue("Corr_table_{corr_main_gene()}.csv")
                   },
                   content=function(file){
                     write.csv(corr_table_content(),
                               file = file,
                               row.names=FALSE)
                   },
                   contentType = "text/csv"
                 ) # End downloadHandler 
                 
                 # Scatterplot (for selected subset)
                 output$download_scatter_subset <- downloadHandler(
                   filename = function(){
                     glue("Corr_scatter_{corr_main_gene()}-vs-{corr_secondary_gene()}_subset.png")
                   },
                   content=function(file){
                     ggsave(file, 
                            plot = subset_scatterplot(), 
                            device = "png",
                            bg = "#FFFFFF")
                   },
                   contentType = "image/png"
                 )# End downloadHandler
                 
                 # Scatterplot (for full data)
                 output$download_scatter_global <- downloadHandler(
                   filename = function(){
                     glue("Corr_scatter_{corr_main_gene()}-vs-{corr_secondary_gene()}_global.png")
                   },
                   content=function(file){
                     ggsave(file, 
                            plot = full_data_scatterplot(), 
                            device = "png",
                            bg = "#FFFFFF")
                   },
                   contentType = "image/png"
                  )# End downloadHandler
               }
               )
}
