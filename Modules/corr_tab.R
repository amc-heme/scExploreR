#Correlations tab module

#corr_tab_ui
#arguments
#id: ID to use for module elements. Should be equal to "corr".
corr_tab_ui <- function(id,
                        unique_metadata,
                        metadata_config){
  #Namespace function: prevents conflicts with inputs/outputs defined in other modules 
  ns <- NS(id)
  
  fluidPage(
    sidebarLayout(
      #1.3.1. Options Panel
      sidebarPanel(
        #Add a block to display a waiter over when the options are updating
        div(id = ns("sidebar"),
            #1.3.1.1. Restrict correlation table by metadata
            tags$h3("Correlation Coefficients"),
            tags$p("Enter one feature to view the top features positively and negatively correlated with the feature in the data. You may optionally restrict the correlation analysis by metadata variables using the dropdown menus below."),
            #Feature selection: only one feature can be entered
            selectizeInput(inputId = ns("feature_selection"),
                           label = "Gene Selection",
                           #Feature choices populated in server, as in the plots tab
                           choices = NULL,
                           selected = character(0),
                           options = list("placeholder"="Enter gene name")),
            #Module for subset selections              
            subset_selections_ui(id = ns("subset_selections"),
                                 unique_metadata = unique_metadata,
                                 metadata_config = metadata_config),
            #Submit button
            actionButton(inputId = ns("submit"),
                         label = "Submit",
                         #Display inline with download button when it appears
                         class = "inline-block"),
            
            #Download buttons: render after the correlations table 
            #and scatterplots are computed
            uiOutput(outputId = ns("downloads_ui"), 
                     inline = TRUE),
            
            #Options for scatterplot: a collapsible panel of options
            #appears after a scatterplot is displayed
            uiOutput(outputId = ns("scatter_options_ui"))
        )#End corr-sidebar div
      ),#End sidebarPanel (1.3.1)
      
      #1.3.2 Main Panel
      mainPanel(
        div(id = ns("main_panel"), 
            class = "spinner-container-main", 
            #Div added to contain Waiter spinner (forces the spinner to cover 
            #the full main panel)
            uiOutput(outputId = ns("main_panel_ui")))
        
      )#End MainPanel
    )#End sidebarLayout
  )#End fluidPage
}

#corr_tab_server
#Arguments
#id: Used to match server function to ui function. "corr" is the recommended 
#value to use
#sobj: The Seurat Object defined in the main server function
#metadata_config: metadata section of the config file imported in the main server function
#n_cells_original: number of cells in full Seurat object. Calculated in main 
#server function.
#nonzero_threshold: the minimum acceptable proportion of cells with nonzero reads 
#for the feature used to compute correlations. This is defined in the 
#main server function.
#meta_choices: a named vector giving the metadata categories defined in the 
#config file, with their respective labels for display in dropdown menus. This 
#is defined in the main server function.
#valid features: a list giving the valid features that can be selected from each
#assay. This is generated from the config file in the main server function
#error_list: a list of error messages to print custom notifications for, 
#if they are encountered while the correlations table is being calculated. This 
#is defined in the main server function.
corr_tab_server <- function(id,
                            sobj,
                            metadata_config,
                            n_cells_original,
                            nonzero_threshold,
                            meta_choices,
                            valid_features,
                            error_list){

  #Function to initialize module server
  moduleServer(id,
               function(input,output,session){
                 #Server namespace function: required for ids defined within 
                 #renderUI functions, but not for other ids 
                 ns <- session$ns
                 
                 #Reactive values: will be deleted and re-implemented in near future
                 rv <- reactiveValues()
                 #rv$corr_is_subset: determines how correlations are computed. 
                 #When rv$corr_is_subset is TRUE, correlations are computed for 
                 #the subset and the full dataset, and when it is FALSE, 
                 #correlations are computed only for the full dataset. 
                 rv$corr_is_subset <- FALSE

                 #Correlations Tab Server ###
                 #HARD CODING: Entire correlations tab only supports the RNA assay.
                 
                 # 1. Feature selection ####
                 # 1.1. Render Choices ####
                 updateSelectizeInput(session,
                                      inputId = "feature_selection",
                                      #Include only genes for now
                                      choices = valid_features[["Genes"]],
                                      selected = character(0),
                                      server = TRUE)
                 
                 ## 1.2 Process Input ####
                 #Creates a reactive value for the selected feature for proper 
                 #processing in downstream functions such as compute_correlation()
                 corr_main_gene <- 
                   eventReactive(input$feature_selection,
                                 ignoreNULL = FALSE,
                                 #No need to run this on startup
                                 ignoreInit = TRUE,
                                 label="Correlations Tab: Process Selected Feature",
                                 {
                                   print("corr process feature selection")
                                   #HARD CODING: remove "rna_" prefix 
                                   #from feature entered. Currently specific 
                                   #to RNA assay in this Seurat object
                                   return(sub("rna_","",input$feature_selection))
                                   })
                 
                 # 2. Process Subset Selections Input ####
                 #Module to record selections made in for subsetting based on metadata categories
                 subset_selections <- 
                   subset_selections_server(id = "subset_selections",
                                            metadata_config = metadata_config)
                 
                 # 3. Reactive dropdown menu for patient ####
                 #TODO: add a more generalized form of this in the subset selections ui
                 #Since patients fall into either the sensitive or resistant 
                 #category, the patients dropdown will need to be updated to keep 
                 #the user from choosing invalid combinations.
                 #Menu will be updated in the future when variables such as treatment 
                 #and time after diagnosis are added (ignoreInit prevents this 
                 #from happening when app is initialized)
                 observeEvent(c(input$response_selection, 
                                input$treatment_selection),
                              #Do not run this code at startup
                              ignoreInit = TRUE,
                              label="Reactive Patient Dropdown",{ 
                                #Show a spinner while the valid patient ID's are calculated
                                waiter_show(
                                  id = ns("sidebar"),
                                  html = spin_loaders(id=2, color = "#555588"),
                                  color = "#B1B1B188",
                                  hide_on_render = FALSE #Gives manual control of showing/hiding spinner
                                )
                                
                                ####Corr dplyr subset ####
                                #Filter object for treatment and response selections, and find valid patients
                                valid_patients <- sobj@meta.data |> 
                                  filter(
                                    (.data[["response"]] %in% input$response_selection)&
                                      (.data[["treatment"]] %in% input$treatment_selection)
                                  ) |> 
                                  #Select patients metadata column
                                  select(.data[["htb"]]) |> 
                                  #Return unique values
                                  unique() |>
                                  #Convert to a character vector
                                  unlist()
                                
                                #Form categorized list of valid patients for display in dropdown menu
                                valid_patients_categories <- 
                                  build_patient_list(valid_patients)
                                #Sort patients categorized list so they appear in order
                                valid_patients_categories <- 
                                  sort_patient_list(valid_patients_categories)
                                #####
                                
                                #Update picker input with valid patient ID's
                                updatePickerInput(session,
                                                  inputId = "htb_selection",
                                                  label = "Restrict by Patient",
                                                  choices = valid_patients_categories,
                                                  selected = valid_patients,
                                                  options = list(
                                                    "selected-text-format" = "count > 3",
                                                    "actions-box"=TRUE
                                                  ))
                                
                                #Hide waiter
                                waiter_hide(id = ns("sidebar"))
                              })
                 
                 # 4. Correlation table for selected feature and restriction criteria ####
                 #Table updates only when the "Submit" button is clicked
                 ## 4.1. Compute table content ####
                 #The table in this function is accessed by the download handler,
                 #and converted to DT format in 4.2. for display in app
                 corr_table_content <- 
                   eventReactive(input$submit,
                                 label="Corelation Table Content",
                                 ignoreInit = FALSE, 
                                 ignoreNULL = FALSE, 
                                 {
                                   print("corr DT content")
                                   #Only run the correlation table code if a feature has been specified
                                   if (corr_main_gene() != ""){
                                     #Hide the main panel UI while calculations are performed
                                     hideElement(id=ns("main_panel_ui"))
                                     #Show loading screen above main panel while table is computed
                                     waiter_show(
                                       id = ns("main_panel"),
                                       html = spin_loaders(id=2, 
                                                           color = "#555588"),
                                       color = "#FFFFFF",
                                       #Gives manual control of showing/hiding spinner
                                       hide_on_render = FALSE
                                       )
                                     
                                     #Error handling: the code in this block must 
                                     #be handled with tryCatch() to capture errors 
                                     #that may arise from selecting subsets with 
                                     #zero cells, or from memory limitations that 
                                     #may be reached with larger datasets.
                                     corr_table <- tryCatch(
                                       #If an error is caught: attempt to determine 
                                       #type of error byinspecting message text 
                                       #with grepl (not recommended, but I currently 
                                       #don't know any other way to catch this error type)
                                       error = function(cnd){
                                         error_handler(
                                           session, 
                                           cnd_message = cnd$message,
                                           #Uses a list of subset-specific errors 
                                           error_list = error_list,
                                           #Id prefix for notification elements
                                           id_prefix = "plots"
                                           )
                                         
                                         #Return nothing for the correlation table 
                                         #if an error occurs
                                         corr_table <- NULL 
                                         return(corr_table)
                                         }, #End error function
                                       #Begin tryCatch code
                                       {
                                         print("corr table: make subset")
                                         #Form subset based on chosen criteria 
                                         #Store in reactive variable
                                         obj_sub <<- reactive({
                                           make_subset(sobj,
                                                       criteria_list = subset_selections) 
                                         })
                                         
                                         #Determine if the subset created is a 
                                         #subset (if it is the full data, use 
                                         #different procedures for creating/rendering 
                                         #the table and plots)
                                         print("corr table: test if subset")
                                         if (n_cells_original!=ncol(rv$s_sub)){
                                           rv$corr_is_subset <- TRUE
                                           } else {
                                             return(FALSE)
                                           }
                                         })
                                         
                                         ###Subset Stats
                                         #Determine the proportion of cells with 
                                         #nonzero reads for the selected gene. 
                                         #If it is below the threshold defined 
                                         #at the top of this script return a 
                                         #warning to the user.
                                         print("corr table: subset stats module")
                                         subset_stats_server(
                                           id="stats",
                                           tab="corr",
                                           subset=obj_sub,
                                           gene_selected=corr_main_gene,
                                           subset_selections=subset_selections,
                                           nonzero_threshold=nonzero_threshold
                                           )
                                         
                                         #Compute correlations
                                         #If a subset has been selected, correlation 
                                         #coefficients between the selected feature 
                                         #and others will be computed for both 
                                         #the full data and the subset, and both 
                                         #will be displayed. If a subset is not 
                                         #selected, correlation coefficients will 
                                         #only be computed for the full data.
                                         
                                         if (corr_is_subset()==TRUE){
                                           print("corr table: compute for full data")
                                           #Subset is selected: compute both 
                                           #tables and merge
                                           table_full <- 
                                             compute_correlation(
                                               gene_selected = corr_main_gene,
                                               object = sobj,
                                               colnames=
                                                 c("Feature",
                                                   "Correlation_Global")
                                               )
                                           
                                           print("corr table: compute for subset")
                                           table_subset <- 
                                             compute_correlation(
                                               gene_selected = corr_main_gene,
                                               object = obj_sub,
                                               colnames = 
                                                 c("Feature",
                                                   "Correlation_Subset")
                                               )
                                           
                                           #Merge individual tables and arrange 
                                           #in descending order by the subset 
                                           #correlation coefficient
                                           print("corr table: merge tables")
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
                                           }else{
                                             print("corr table: compute for full data")
                                             #If a subset is not present: 
                                             #compute the table for the full 
                                             #data only (which is the subset 
                                             #in this case)
                                             corr_table <- 
                                               compute_correlation(
                                                 gene_selected = corr_main_gene,
                                                 object = obj_sub,
                                                 colnames = c("Feature","Correlation_Subset")
                                               )
                                             print("corr table: recieved table from compute_correlation")
                                             }
                                         
                                         print("corr table: return table from tryCatch")
                                         #Return corr_table from tryCatch to 
                                         #eventReactive() function
                                         corr_table
                                         })#End tryCatch
                                     
                                     print("recieved corr_table from tryCatch")
                                     print("corr table: hide waiters")
                                     #Hide loading screen
                                     waiter_hide(id = ns("main_panel"))
                                     waiter_hide(id = ns("sidebar"))
                                     #Show content in main panel
                                     showElement(id = ns("main_panel_ui"))
                                     
                                     #Return table for storage in corr_table_content()
                                     return(corr_table)
                                     }
                                   })
                 
                 ## 4.2. Store table in DT format for display in app ####
                 corr_DT_content <- 
                   eventReactive(c(input$submit,
                                   rv$corr_is_subset),
                                 label = "Corr DT Content",
                                 ignoreNULL = FALSE,
                                 {
                                   #Define header for datatable using HTML
                                   if(corr_is_subset()==TRUE){
                                     #If a subset is selected, the header will 
                                     #have three columns for the feature, the 
                                     #global correlation coefficients, and the 
                                     #correlation coefficients for the subset
                                     header <- tags$table(
                                       #center-colnames class: centers the 
                                       #column names in the header
                                       class = "compact stripe cell-border hover center-colnames",
                                       tags$thead(
                                         tags$tr(
                                           tags$th("Feature"),
                                           tags$th(
                                             tagList("Correlation",
                                                     tags$br(),
                                                     "(Global)"
                                                     )
                                             ),
                                           tags$th(
                                             tagList("Correlation",
                                                     tags$br(),
                                                     "(Subset)"
                                                     )
                                             ) #End th
                                           ) #End tr
                                         ) #End thead
                                       ) #End table
                                     
                                     } else {
                                       header <- 
                                         tags$table(
                                           #center-colnames class: centers the
                                           #column names in the header
                                           class = "compact stripe cell-border hover center-colnames",
                                           tags$thead(
                                             tags$tr(
                                               tags$th("Feature"),
                                               tags$th(
                                                 tagList("Correlation Coefficient",
                                                         tags$br(),
                                                         "(Global)")
                                                 ) #End th
                                               ) #End tr
                                             ) #End thead
                                         ) #End table tag
                                       }
                                   
                                   datatable(
                                     corr_table_content(),
                                     class = "compact stripe cell-border hover",
                                     selection = "single",
                                     filter = "top",
                                     rownames = FALSE,
                                     container = header
                                     ) %>%
                                     #Use 5 sig figs for pearson coefficient 
                                     #column(s). If a subset is used, this 
                                     #will be columns 2 and 3; if not, 
                                     #this will be column 2.
                                     formatSignif(
                                       columns = if(corr_is_subset()==TRUE) c(2,3) else 2,
                                       digits = 5
                                       )
                                   })
                 
                 # 5. Correlations UI ####
                 ## 5.1. Main Panel UI ####
                 #IgnoreNULL set to false to get UI to render at start up
                 main_panel_ui <- eventReactive(input$submit, 
                                          label = "Correlation Main UI (Define Content)",
                                          ignoreNULL = FALSE, 
                                          {
                                            print("Corr UI Function")
                                            #UI: if the feature selection menu 
                                            #is empty (default state at initialization), 
                                            #prompt user to enter features
                                            if (corr_main_gene() == ""){
                                              tags$h3("Enter a feature and press 
                                                      submit to view correlated 
                                                      features. You may also specify 
                                                      restriction criteria using the 
                                                      dropdown menus.")
                                              #After a feature is applied and the 
                                              #submit button is pressed, display the table
                                            } else {
                                              #Display the loading screen (screen 
                                              #will show until the end of the 
                                              #corr_table_content calculation is reached).
                                              waiter_show(
                                                id = ns("main_panel"),
                                                html = spin_loaders(id=2, 
                                                                    color = "#555588"),
                                                color = "#FFFFFF",
                                                #Gives manual control of showing/hiding spinner
                                                hide_on_render = FALSE 
                                              )
                                              
                                              #Also display spinner over the 
                                              #options menu to keep user from being 
                                              #able to click download buttons before 
                                              #content is ready
                                              waiter_show(
                                                id = ns("sidebar"),
                                                html = spin_loaders(id=2, 
                                                                    color = "#555588"),
                                                color = "#B1B1B188",
                                                #Gives manual control of showing/hiding spinner
                                                hide_on_render = FALSE 
                                              )
                                              
                                              #UI to display 
                                              tagList(
                                                tags$h2(
                                                  glue("Correlation Analysis for
                                                       {corr_main_gene()}"), 
                                                  class="center"),
                                                
                                                #Subset stats module UI
                                                #Prints output containers and text
                                                #to report the metadata included 
                                                #in the subset and the amount
                                                #of nonzero reads for that gene
                                                #in the subset
                                                subset_stats_ui(
                                                  #Use namespacing for module 
                                                  #UI components
                                                  id = ns("stats"),
                                                  tab = "corr",
                                                  metadata_config = metadata_config,
                                                  gene_selected = corr_main_gene,
                                                  subset_selections = subset_selections),
                                                
                                                #Correlations table and plots
                                                tags$h3("Correlated Genes", 
                                                        class="center"),
                                                
                                                #Table: rendered inline 
                                                div(
                                                  class="two-column",
                                                  style="width: 40%; float: left;",
                                                  tags$strong(
                                                    "Correlation Table", 
                                                    class="center single-space-bottom"
                                                    ),
                                                  #Use a DT data table
                                                  DTOutput(
                                                    outputId = ns("corr_table")
                                                    )
                                                ),
                                                
                                                #Scatterplot: only appears after 
                                                #the user makes a selection on 
                                                #the table
                                                div(
                                                  class="two-column",
                                                  style="width: 60%; float: right;",
                                                  #UI for scatterplot rendered in 
                                                  #separate eventReactive function
                                                  uiOutput(
                                                    outputId = ns("scatterplot_ui")
                                                    )
                                                )
                                              )#End tagList
                                            }
                                          })
                 
                 ## 5.2. Correlations scatterplot UI ####
                 #Computed separately from main UI since it responds to a 
                 #different user input (clicking table)
                 scatterplot_ui <- 
                   eventReactive(c(input$corr_table_rows_selected,
                                   corr_is_subset()),
                                 label="Correlation Scatterplot UI",
                                 ignoreNULL = FALSE,
                                 {
                                   #Display the graph if rows are selected
                                   if (length(input$corr_table_rows_selected)>0){
                                     #If a subset is selected, display two plots: 
                                     #one for the subset and one for the full data.
                                     if (corr_is_subset()==TRUE){
                                       tagList(
                                         tags$strong("Scatterplot for Subset",
                                                     class = "center single-space-bottom"),
                                         plotOutput(outputId = ns("subset_scatterplot"), 
                                                    height = "400px", 
                                                    width = "400px"),
                                         tags$strong("Scatterplot for Full Data",
                                                     class="center single-space-bottom"),
                                         plotOutput(outputId = ns("full_data_scatterplot"), 
                                                    height = "400px", 
                                                    width = "400px")
                                         )
                                       #Otherwise, display only one scatterplot.
                                       } else {
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
                 
                 ## 5.3. UI for customizing the scatterplot ####
                 #This appears in the sidebar and displays a list of options used for customizing
                 #the scatterplot
                 scatter_options <- 
                   eventReactive(c(input$corr_table_rows_selected, 
                                   corr_is_subset()),
                                 label="Corr. Scatterplot Options UI",
                                 ignoreNULL = FALSE,
                                 {
                                   #If a selection in the table is made, display 
                                   #a collapsible_panel with a list of options 
                                   #for customization
                                   if (length(input$corr_table_rows_selected)>0){
                                     collapsible_panel(
                                       inputId = ns("scatter_options"),
                                       label = "Scatterplot Options",
                                       active = TRUE,
                                       #group.by selection
                                       selectInput(
                                         inputId = ns("scatter_group_by"),
                                         label = "Metadata to Group by:",
                                         #Remove "none" from selectable 
                                         #options to group by
                                         choices=
                                           meta_choices[!meta_choices %in% "none"], 
                                         selected = "clusters"
                                         ),
                                       #Download button for scatterplot (subset)
                                       
                                       #Displays only if a subset is selected
                                       if(corr_is_subset()==TRUE){
                                         downloadButton(
                                           outputId = ns("download_scatter_subset"),
                                           label = "Download Scatterplot (Subset)",
                                           #Adds space before button
                                           class = "space-top",
                                           icon = icon("poll")
                                         )
                                       } else NULL, #End downloadButton tag
                                       
                                       #Download button for scatterplot (full data)
                                       downloadButton(
                                         outputId = ns("download_scatter_global"),
                                         #Label changes based on whether 
                                         #a subset is selected
                                         label = if(corr_is_subset()==TRUE){
                                           "Download Scatterplot (Full Data)"
                                         } else {
                                           "Download Scatterplot"},
                                         
                                         #space-top class: adds space before button 
                                         #this is only needed when a subset is 
                                         #selected and there are two buttons
                                         class = if(corr_is_subset()==TRUE){
                                           "space-top"
                                         } else NULL,
                                         icon = icon("poll")
                                       ) #End downloadButton
                                     ) #End collapsible_panel  
                                   } #End if statement
                                 })
                 
                 ## 5.4. Download Button for Table ####
                 downloads_ui <- 
                   eventReactive(c(input$submit,
                                   input$corr_table_rows_selected),
                                 label = "Correlation Table Download Button UI",
                                 ignoreNULL = FALSE, 
                                 {
                                   #Condition !hasName(): TRUE before table 
                                   #is created, FALSE after
                                   if (!hasName(input,"corr_table_rows_selected")){
                                     #Display nothing before table is created
                                     NULL 
                                     } else {
                                       #Display download button after table is created
                                       downloadButton(
                                         outputId = ns("download_table"),
                                         label = "Download Table",
                                         #Adds space before button
                                         class = "inline-block",
                                         icon = icon("table")
                                         )
                                       } #End else
                                   })
                 
                 # 6. Server Value for Rows Selected from Table ####
                 #Becuase input$corr_table_rows_selected is NULL before the table is clicked,
                 #An error flickers where the correlation plots are before displaying the plots,
                 #giving the user the impression that an error has occurred. 
                 rows_selected <- 
                   eventReactive(input$corr_table_rows_selected,
                                 label = "Rows Selected: Server Value",
                                 {
                                   #If the number of rows selected is not NULL 
                                   #and not equal to `character(0)` (Value assigned 
                                   #by Shiny when no rows are selected), set 
                                   #rv$rows_selected to TRUE
                                   print(glue("Rows are selected: 
                                   {(!identical(input$corr_table_rows_selected,character(0)))&
                                              (!is.null(input$corr_table_rows_selected))}"))
                                   if ((!identical(input$corr_table_rows_selected,character(0)))&
                                       (!is.null(input$corr_table_rows_selected))){
                                     rows_selected=TRUE
                                     } else {
                                       #If a row is deselected or the table is 
                                       #re-computed, this must be set back to 
                                       #FALSE to keep the scatterplot from running 
                                       #when a feature is not selected, which will 
                                       #cause an error
                                       rows_selected=FALSE
                                       }
                                   
                                   return(rows_selected)
                                   })
                 
                 
                 # 7. Plot of feature selected from table ####
                 ## 7.1. Correlation scatterplot for subset
                 #Computes a scatterplot for a secondary gene selected by the 
                 #user from the correlations table.
                 #Row index of user selection from table is stored in 
                 #input$corr_table_rows_selected. eventReactive responds to
                 #input$corr_table_rows_selected and rv$corr_table_rows_selected
                 #input$corr_table_rows_selected is the index of the row selected,
                 #while rows_selected() is the boolean generated in 2.3.4. rows_selected() 
                 #prevents the code from running when the user has de-selected values 
                 subset_scatterplot <- 
                   eventReactive(
                     c(input$corr_table_rows_selected,
                       rows_selected(),
                       input$scatter_group_by),
                     label="Correlation Scatterplot Content (Subset)",
                     {
                       row_idx <- input$corr_table_rows_selected
                       #Take action only if a row is selected
                       if (rows_selected()==TRUE){
                         #Record gene name of row selected
                         #Superassignment ensures value is accessible elsewhere in app
                         corr_secondary_gene <<- reactive({
                           as.character(corr_table_content()[row_idx,1])
                           })
                         
                         #Make and store scatterplot
                         FeatureScatter(obj_sub(), 
                                        feature1 = corr_main_gene(),
                                        feature2 = corr_secondary_gene(),
                                        #group.by and split.by 
                                        #according to user input
                                        group.by = input$scatter_group_by)
                         }
                       })
                 
                 ## 7.2. Correlation plot for full data
                 full_data_scatterplot <- 
                   eventReactive(c(input$corr_table_rows_selected, 
                                   rows_selected(),
                                   input$scatter_group_by),
                                 label="Correlation Scatterplot Content (Global)",
                                 {
                                   row_idx <- input$corr_table_rows_selected
                                   #Take action only if a row is selected 
                                   if (rows_selected()==TRUE){
                                     #Record gene name of row selected
                                     #Superassignment ensures value is 
                                     #Accessible elsewhere in app
                                     corr_secondary_gene <<- reactive({
                                       as.character(corr_table_content()[row_idx,1])
                                                          })
                                     
                                     #Make and store scatterplot 
                                     #Use full object
                                     FeatureScatter(
                                       sobj, 
                                       feature1 = corr_main_gene(),
                                       feature2 = corr_secondary_gene(),
                                       #group.by and split.by according to user input
                                       group.by = input$scatter_group_by
                                       )
                                     }
                                   })
                 
                 # 8. Render Correlation UI, table, scatterplot, and statistics ####
                 #Main panel UI
                 output$main_panel_ui <- renderUI({
                   main_panel_ui()
                 })
                 
                 #Container for scatterplots (main panel)
                 output$scatterplot_ui <- renderUI({
                   scatterplot_ui()
                 })
                 
                 #Scatterplot (main panel, in UI container)
                 observeEvent(input$corr_table_rows_selected, 
                              label = "Render Corr Scatter (Subset)", 
                              {
                                output$subset_scatterplot <- 
                                  renderPlot({
                                    subset_scatterplot()
                                    })
                              })
                 
                 #Scatterplot (main panel, in UI container)
                 observeEvent(input$corr_table_rows_selected, 
                              label = "Render Corr Scatter (Global)", 
                              {
                                output$full_data_scatterplot <- 
                                  renderPlot({
                                    full_data_scatterplot()
                                    })
                              })
                 
                 #Options for scatterplot (sidebar panel)
                 output$scatter_options_ui <- renderUI({
                   scatter_options()
                 })
                 
                 #Download button for Table
                 output$downloads_ui <- renderUI({
                   downloads_ui()
                 })
                 
                 #Table
                 output$corr_table <- renderDT({
                   corr_DT_content()
                 })
                 
                 #Render Statistics
                 observeEvent(input$submit,
                              label = "Render Statistics",{
                                render_statistics(input,output,session,rv)
                              })
                 
                 # 9. Download Handlers ####
                 #Correlations Table
                 output$download_table <- downloadHandler(
                   filename=function(){
                     glue("Corr_table_{corr_main_gene()}.csv")
                   },
                   content=function(file){
                     write.csv(corr_table_content(),
                               file=file,
                               row.names=FALSE)
                   },
                   contentType="text/csv"
                 )#End downloadHandler 
                 
                 #Scatterplot (for selected subset)
                 output$download_scatter_subset <- downloadHandler(
                   filename=function(){
                     glue("Corr_scatter_{corr_main_gene()}-vs-{corr_secondary_gene()}_subset.png")
                   },
                   content=function(file){
                     ggsave(file, 
                            plot=subset_scatterplot(), 
                            device="png",
                            bg="#FFFFFF")
                   },
                   contentType = "image/png"
                 )#End downloadHandler
                 
                 #Scatterplot (for full data)
                 output$download_scatter_global <- downloadHandler(
                   filename=function(){
                     glue("Corr_scatter_{corr_main_gene()}-vs-{corr_secondary_gene()}_global.png")
                   },
                   content=function(file){
                     ggsave(file, 
                            plot=full_data_scatterplot(), 
                            device="png",
                            bg="#FFFFFF")
                   },
                   contentType = "image/png"
                 )#End downloadHandler
               }
               )
}
