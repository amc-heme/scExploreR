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
            
            #Menus to choose subset
            subset_selections_ui(id=ns("subset_selections"),
                                 unique_metadata = unique_metadata,
                                 metadata_config = metadata_config),
            
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
            #TEMP: verbatimTextOutput container for displaying 
            "Test Selection Output",
            verbatimTextOutput(outputId = ns("test_selection_output")),
            "Subset Selection Output",
            verbatimTextOutput(outputId = ns("subset_selection_output")),
            "Subset information:",
            verbatimTextOutput(outputId = ns("subset_info_output")),
            #Div added to contain Waiter spinner (forces the spinner to 
            #cover the full main panel)
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

                 ##2.3. Call subset_selections module
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
                 
              
                 #TEMP: display outputs of test selection server
                 output$test_selection_output <- renderPrint({
                   test_selections()
                 })
                 
                 output$subset_selection_output <- renderPrint({
                   subset_selections()
                 })
                 
                 #3. DGE table for selected metadata and restriction criteria ----
                 ##3.1. Process Submit button input
                 #Pass value of action button to nested modules to control reactivity
                 submit_button <- reactive({input$submit})
                 
                 ##3.2. Define subset criteria
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
                     #Errors occur when this is run at startup
                     ignoreInit = TRUE,
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
                 
                 ##3.3. Form subset
                 subset <- eventReactive(
                   dge_subset_criteria(),
                   label = "DGE: Subset",
                   ignoreNULL = FALSE,
                   #Do not run on startup
                   #ignoreInit = TRUE,
                   {
                     print("3.3 make subset")
                     #Create subset from selections and return
                     subset <- make_subset(sobj = sobj,
                                           criteria_list = dge_subset_criteria())
                     return(subset)
                   })
                 
                 ##3.4. Compute subset stats
                 observeEvent(
                   subset(),
                   label = "DGE: Subset Stats",
                   ignoreNULL = FALSE,
                   #Do not run at startup
                   #ignoreInit = TRUE,
                   {
                     print("3.4 subset stats")
                     #Initiate subset_stats server
                     subset_stats_server(
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
                   }
                 )
                 
                 #TEMP: print information on subset to screen
                 output$subset_info_output <- renderPrint({
                   subset()
                   })
                 
                 ##3.5. Run Presto
                 observeEvent(
                   subset(),
                   ignoreNULL = FALSE,
                   #Do not run at startup
                   #ignoreInit = TRUE,
                   {
                     print("3.5 Run Presto")
                     print(subset())
                     
                   }
                 )
                 
                 #Old Implementation
                 #Table updates only when the "Update" button is clicked
                 #### 2.1. Store table content as reactive value ####
                 #The tibble generated here is converted to a DT in 2.2.2.2. for viewing, and
                 #Is passed to the download handler when the "download table" button is clicked.
                 
                 # dge_table_content <- eventReactive(
                 #   input$submit,
                 #   label = "DGE Table Content",
                 #   ignoreNULL = FALSE,
                 #   {
                 #     #Show loading screen above main panel while table is computed (takes about a minute)
                 #     waiter_show(
                 #       id = "dge_main_panel",
                 #       html = spin_loaders(id = 2, color = "#555588"),
                 #       color = "#FFFFFF",
                 #       hide_on_render = FALSE #Gives manual control of showing/hiding spinner
                 #     )
                 #     
                 #     #Error handling: errors are frequent in this script, often 
                 #     #due to memory limitations, and they will result in the 
                 #     #spinner not disappearing from the main window since 
                 #     #waiter_hide() exists at the end this code block. Therefore, 
                 #     #the code in this block must be handled with tryCatch() to 
                 #     #capture errors. This error handling code has been duplicated
                 #     #verbatim from section 2.3.2.1 - likely isn't necessary here;
                 #     #should be removed if warranted by testing.
                 #     dge_table <- tryCatch(
                 #       #If an error is caught: attempt to determine type of 
                 #       #error by inspecting message text with grepl (not recommended, 
                 #       #but I currently don't know any other way to catch this error type)
                 #       error = function(cnd) {
                 #         #Use error_handler function to display notifications to the 
                 #         #user based on the error message
                 #         error_handler(session,
                 #                       cnd_message=cnd$message,
                 #                       #The error handling function uses a list 
                 #                       #of subset-specific errors 
                 #                       error_list = error_list,
                 #                       #Id prefix for notification elements
                 #                       id_prefix = "dge")
                 #         
                 #         #Return nothing if an error occurs
                 #         dge_table <-
                 #           NULL 
                 #         return(dge_table)
                 #       },
                 #       #End error function
                 #       #Begin tryCatch code
                 #       {
                 #         #Form subset based on chosen criteria
                 #         #Store in reactive variable so it can be accessed by 
                 #         #UMAP eventReactive() function
                 #         
                 #         ## Pick the appropriate input variables bases on the group_by selection
                 #         ## Not clear why the listing and unlisting is required for a character vector
                 #         dge_clusters_int = unlist(ifelse(
                 #           input$dge_group_by == "clusters",
                 #           list(input$dge_clusters_selection_group),
                 #           list(input$dge_clusters_selection)
                 #         ))
                 #         
                 #         dge_response_int = unlist(ifelse(
                 #           input$dge_group_by == "response",
                 #           list(input$dge_response_selection_group),
                 #           list(input$dge_response_selection)
                 #         ))
                 #         
                 #         dge_htb_int = unlist(ifelse(
                 #           input$dge_group_by == "htb",
                 #           list(input$dge_htb_selection_group),
                 #           list(input$dge_htb_selection)
                 #         ))
                 #         
                 #         dge_treatment_int = unlist(ifelse(
                 #           input$dge_group_by == "treatment",
                 #           list(input$dge_treatment_selection_group),
                 #           list(input$dge_treatment_selection)
                 #         ))
                 #         
                 #         #Subset method depends on whether marker identification or
                 #         #dge is selected
                 #         if (input$dge_mode=="mode_marker"){
                 #           #Marker identification: create subset based on the subset 
                 #           #dropdown menus, and include the group_by metadata in the 
                 #           #subset. The group_by dropdown is named reactively according
                 #           #to user choice, such that its id matches one of the four below. 
                 #           rv$dge_s_sub <- subset(
                 #             sobj,
                 #             subset =
                 #               (clusters %in% dge_clusters_int) &
                 #               (response %in% dge_response_int) &
                 #               (htb %in% dge_htb_int) &
                 #               (treatment %in% dge_treatment_int)
                 #           )#End subset
                 #         } else {
                 #           #For DGE, define subset conditionally based on the group by
                 #           #variable selection (VERY messy, but works for now)
                 #           if (input$dge_group_by=="clusters"){
                 #             #Subset will use the subset criteria chosen, as well as the two
                 #             #classes chosen for the group by variable 
                 #             
                 #             #Create a vector with the two classes chosen for DGE
                 #             clusters_selected_vector <- c(input$dge_group_1, input$dge_group_2)
                 #             
                 #             #Subset as usual for all variables except for the group by variable
                 #             rv$dge_s_sub <- subset(
                 #               sobj,
                 #               subset =
                 #                 #Use the vector above for the group_by_variable
                 #                 (clusters %in% clusters_selected_vector) &
                 #                 (response %in% dge_response_int) &
                 #                 (htb %in% dge_htb_int) &
                 #                 (treatment %in% dge_treatment_int)
                 #             )#End subset
                 #             
                 #           } else if (input$dge_group_by=="response"){
                 #             #Create a vector with the two response classes chosen for DGE
                 #             response_selected_vector <- c(input$dge_group_1, input$dge_group_2)
                 #             
                 #             #Subset as usual for all variables except for response
                 #             rv$dge_s_sub <- subset(
                 #               sobj,
                 #               subset =
                 #                 #Use the vector above for the group_by_variable
                 #                 (clusters %in% dge_clusters_int) &
                 #                 (response %in% response_selected_vector) &
                 #                 (htb %in% dge_htb_int) &
                 #                 (treatment %in% dge_treatment_int)
                 #             )#End subset
                 #             
                 #           } else if (input$dge_group_by=="htb"){
                 #             #Create a vector with the two htb classes chosen for DGE
                 #             htb_selected_vector <- c(input$dge_group_1, input$dge_group_2)
                 #             
                 #             #Subset as usual for all variables except for htb
                 #             rv$dge_s_sub <- subset(
                 #               sobj,
                 #               subset =
                 #                 #Use the vector above for the group_by_variable
                 #                 (clusters %in% dge_clusters_int) &
                 #                 (response %in% dge_response_int) &
                 #                 (htb %in% htb_selected_vector) &
                 #                 (treatment %in% dge_treatment_int)
                 #             )#End subset
                 #             
                 #           } else if (input$dge_group_by=="treatment"){
                 #             #Create a vector with the two treatment (timepoint) classes chosen for DGE
                 #             treatment_selected_vector <- c(input$dge_group_1, input$dge_group_2)
                 #             
                 #             #Subset as usual for all variables except for treatment
                 #             rv$dge_s_sub <- subset(
                 #               sobj,
                 #               subset =
                 #                 #Use the vector above for the group_by_variable
                 #                 (clusters %in% dge_clusters_int) &
                 #                 (response %in% dge_response_int) &
                 #                 (htb %in% dge_htb_int) &
                 #                 (treatment %in% treatment_selected_vector)
                 #             )#End subset
                 #           }
                 #           
                 #         }
                 #         
                 #         ###Subset Stats
                 #         #Cells in subset
                 #         rv$dge_n_cells <-
                 #           length(Cells(rv$dge_s_sub))
                 #         
                 #         ## DGE-specific Stats
                 #         #Number of classes in selection
                 #         rv$dge_n_classes <- length(unique(rv$dge_s_sub@meta.data[,input$dge_group_by]))
                 #         #Mode selection
                 #         rv$dge_mode <- ifelse(
                 #           rv$dge_n_classes == 2,
                 #           paste0(
                 #             "Differential Expression (",
                 #             unique(rv$dge_s_sub@meta.data[, input$dge_group_by])[1],
                 #             " vs. ",
                 #             unique(rv$dge_s_sub@meta.data[, input$dge_group_by])[2],
                 #             ")"
                 #           ),
                 #           paste0("Marker Identification (", rv$dge_n_classes, " classes)")
                 #         )
                 #         
                 #         print("n_by_class")
                 #         #Cells per class
                 #         #Calculate number of cells per class using metadata table of subset
                 #         n_by_class <- rv$dge_s_sub@meta.data |>
                 #           #Group by the specified metadata variable (.data and double braces 
                 #           #used so group_by can properly interpret the character vector input)
                 #           group_by(.data[[input$dge_group_by]]) |> 
                 #           summarise(n=n()) #Calculate number of cells per group 
                 #         
                 #         #Print class names and cell counts per class
                 #         #Convert class names and cell counts from tibble to character vector 
                 #         #Class names in first column of tibble
                 #         class_names <- as.character(n_by_class[[1]])
                 #         #Cell counts are in second column of tibble
                 #         n_cells <- n_by_class[[2]]
                 #         
                 #         #Build list of classes and the number of cells in each
                 #         n_list=list()
                 #         for (i in 1:nrow(n_by_class)){
                 #           n_list[[i]]<-glue("{class_names[i]}: {n_cells[i]}")
                 #         }
                 #         #Collapse list of class/count pairs into a string, 
                 #         #and store in reactive variable
                 #         rv$dge_n_by_class<-paste(n_list,collapse = "\n")
                 #         #\n is the separator (will be read by verbatimTextOutput())
                 #         
                 #         #Run Presto
                 #         dge_table <-
                 #           #Run presto on the subsetted object and indicated metadata slot
                 #           wilcoxauc(rv$dge_s_sub, group_by = input$dge_group_by) %>% 
                 #           #Explicitly coerce to tibble
                 #           as_tibble() %>%  
                 #           #remove stat and auc from the output table
                 #           select(-c(statistic, auc)) %>%  
                 #           #Using magrittr pipes here because the following statement 
                 #           #doesn't work with base R pipes
                 #           #remove negative logFCs if box is checked
                 #           {if (input$dge_pos) filter(., logFC > 0) else .} %>%
                 #           #Arrange in ascending order for padj, pval (lower values are more
                 #           #"significant"). Ascending order is used for the log fold-change
                 #           arrange(padj, pval, desc(abs(logFC))) 
                 #       }
                 #     )#End tryCatch
                 #     
                 #     #Hide loading screen
                 #     waiter_hide(id = "dge_main_panel")
                 #     waiter_hide(id = "dge_sidebar")
                 #     
                 #     #Return table for storage in dge_table_content()
                 #     dge_table
                 #   }
                 # )
                 
                 #### 2.2. DGE table, as DT for viewing ####
                 # dge_DT_content <- 
                 #   eventReactive(input$submit, 
                 #                 label = "DGE DT Generation",
                 #                 ignoreNULL=FALSE, 
                 #                 {
                 #                   datatable(
                 #                     dge_table_content(),
                 #                     class = "compact stripe cell-border hover",
                 #                     selection = "none",
                 #                     filter = "top",
                 #                     rownames = FALSE
                 #                     ) %>%
                 #                     #Use 5 sig figs (3 or more is sufficient)
                 #                     formatSignif(3:8, 5) 
                 #                                 })
                 
                 # 4. Dynamic UI for Main Panel --------------------------------
                 dge_ui <- 
                   eventReactive(
                     input$submit,
                     label = "DGE Main UI (Define Content)",
                     #Do not render main UI at startup
                     ignoreInit=TRUE,
                     ignoreNULL = FALSE,
                     {
                       print("DGE UI Function")
                       waiter_show(
                         id = "dge_main_panel",
                         html = spin_loaders(id = 2,
                                             color = "#555588"
                                             ),
                         color = "#FFFFFF",
                         #Gives manual control of showing/hiding spinner
                         hide_on_render = FALSE
                         )
                       
                       #Also display spinner over the options menu to keep user 
                       #from being able to click download buttons before 
                       #content is ready
                       waiter_show(
                         id = "dge_sidebar",
                         html = spin_loaders(id = 2, color = "#555588"),
                         color = "#B1B1B188",
                         #Gives manual control of showing/hiding spinner
                         hide_on_render = FALSE 
                         )
                       
                       #User-defined label for group-by variable (for printing 
                       #in UI below)
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
                         tags$h3("DGE Table", class="center"),
                         #Output container for table
                         DTOutput(outputId = "dge_table"),
                         
                         #UMAP plot
                         #Label: depends on mode
                         
                         #The conditional below is passed as an argument
                         #To the div() function to generate the HTML for the
                         #DGE UI. hasName() is used to avoid errors arising
                         #From the rv$dge_n_classes not yet existing
                         #This happens at startup when the UI is computed
                         #Before the table, where rv$dge_n_classes is undefined.
                         
                         #rv$dge_n_classes no longer exists. Must re-implement this
                         # if(hasName(rv,"dge_n_classes")){
                         #   #Title for differential gene expression mode
                         #   if (rv$dge_n_classes == 2){
                         #     tags$h3("UMAP of groups being compared")
                         #   }else{
                         #     #Title for marker identification mode
                         #     tagList(
                         #       #Center text
                         #       tags$h3("UMAP by class",
                         #               class="center"),
                         #       tags$p("(Markers are computed for each group shown)",
                         #              class="center")
                         #     )
                         #   }
                         # } else {
                         #   #This conditional will be run at startup.
                         #   #The default is to compute markers for each cluster,
                         #   #so the text for marker identification will be shown.
                         #   tagList(
                         #     #Center text
                         #     tags$h3("UMAP by class",
                         #             class="center"),
                         #     tags$p("(Markers are computed for each group shown)",
                         #            class="center")
                         #   )
                         # }, #This comma is very important (conditional is an argument)
                         
                         #UMAP container
                         plotOutput(outputId = "dge_umap",
                                    height = "600px")
                         )#End tagList
                       })
                 
                 #### 2.2.3.2. Conditional Dropdown Menus ####
                 #display based on whether DGE or marker identification is selected
                 #2.2.3.2.1. Main Dropdown interface
                 # dge_conditional_menus <- 
                 #   eventReactive(
                 #     dge_mode(),
                 #     label = "DGE Conditional Menus",
                 #     ignoreNULL = FALSE,
                 #     {
                 #       print("Begin code to compute UI (dge_tab_module)")
                 #       ui <- 
                 #         tagList(
                 #           #Test selection module UI: display separate modules 
                 #           #depending on the chosen mode
                 #           if (dge_mode() == "mode_marker"){
                 #             dge_test_selection_ui(
                 #               id = ns("test_selection_marker"),
                 #               dge_mode = "mode_marker",
                 #               meta_choices = meta_choices
                 #               )
                 #             } else if (dge_mode() == "mode_dge"){
                 #               dge_test_selection_ui(
                 #                 id = ns("test_selection_dge"),
                 #                 dge_mode = "mode_dge",
                 #                 meta_choices = meta_choices
                 #                 )
                 #               },
                 #           #Subset selections module UI: same for both modes
                 #           subset_selections_ui(
                 #             id = ns("subset_selections"),
                 #             unique_metadata = unique_metadata,
                 #             metadata_config = metadata_config
                 #             )
                 #           )
                 #       
                 #       print("End code to compute UI (dge_tab_module)")
                 #       
                 #       return(ui)
                 #    })
                 #####
                 
                 #5. Dynamic UI: Download Buttons for Table and Plots ----------
                 dge_downloads_ui <-
                   eventReactive(
                     c(input$submit, input$dge_table_rows_selected),
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
                 
                 #### 2.2.4. Process Choices made for Test and Subset Options ####
                 #Server code for processing test options
                 # test_selections <- 
                 #   eventReactive(dge_mode(),
                 #                 label = "test selections server",
                 #                 ignoreNULL = FALSE,
                 #                 {
                 #                   print("Code to Run dge_test_selection_server")
                 #                   if (dge_mode()=="mode_marker"){
                 #                     print("Run test selection server (marker)")
                 #                     selections <- 
                 #                       dge_test_selection_server(
                 #                         id = "test_selection_marker",
                 #                         sobj = sobj,
                 #                         unique_metadata = unique_metadata,
                 #                         metadata_config = metadata_config, 
                 #                         meta_choices = meta_choices,
                 #                         dge_mode = "mode_marker"
                 #                       )
                 #                     return(selections)
                 #                   } else if (dge_mode()=="mode_dge") {
                 #                     print("Run test selection server (dge)")
                 #                     selections <- 
                 #                       dge_test_selection_server(
                 #                         id = "test_selection_dge",
                 #                         sobj = sobj,
                 #                         unique_metadata = unique_metadata,
                 #                         metadata_config = metadata_config, 
                 #                         meta_choices = meta_choices,
                 #                         dge_mode = "mode_marker"
                 #                       )
                 #                     return(selections)
                 #                   }
                 #                   })
                 ######
                 
                 #6. UMAP of DE Selected Groups --------------------------------
                 dge_umap <- 
                   eventReactive(input$submit, 
                                 ignoreNULL = FALSE,
                                 label="DGE UMAP", {
                                   print("DGE UMAP")
                                   #ncol_argument: number of columns
                                   #based on number of classes being analyzed in 
                                   #the subset. Use double-bracket means of
                                   #accessing the metadata variable (supports 
                                   #entry of an arbitrary variable)
                                   #This means of access returns a dataframe.
                                   #Slice for the first row (the unique values)
                                   n_panel <- 
                                     unique(rv$dge_s_sub[[input$dge_group_by]])[,1] |>
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
                                   #split by metadata for object calculation, 
                                   #group by cluster
                                   DimPlot(rv$dge_s_sub,
                                           split.by =input$dge_group_by,
                                           group.by = "clusters",
                                           ncol=ncol)
                                   })
                 
                 #7. Render DGE UI, table, and statistics ----------------------
                 #Main UI
                 output$main_panel_ui <- renderUI({
                   dge_ui()
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
                 
                 #Render Statistics
                 observeEvent(input$submit, 
                              ignoreNULL= FALSE,
                              label = "DE Render Statistics", 
                              {
                                #New stats computation: find unique classes 
                                #in the subset created (rather than the user
                                #input)
                                #Render Clusters in subset
                                output$dge_selected_clusters <-
                                  renderText({
                                    rv$dge_s_sub@meta.data |> 
                                      #Get unique clusters in subset
                                      select(clusters) |> 
                                      unique() |>
                                      #Convert to vector (returns a factor)
                                      unlist() |> 
                                      #Remove names from vector 
                                      unname() |> 
                                      #Convert factor of clusters to character vector
                                      as.character() |> 
                                      #Convert to grammatically correct output
                                      vector_to_text()
                                  })
                                
                                #Render Response Criteria
                                output$dge_selected_response <-
                                  renderText({
                                    rv$dge_s_sub@meta.data |> 
                                      #Get unique response criteria in subset
                                      select(response) |> 
                                      unique() |>
                                      #Convert to vector
                                      unlist() |> 
                                      #Remove names from vector 
                                      unname() |> 
                                      #Convert factor of criteria to character vector
                                      as.character() |> 
                                      #Convert to grammatically correct output
                                      vector_to_text()
                                  })
                                
                                #Render Patients
                                output$dge_selected_htb <-
                                  renderText({
                                    rv$dge_s_sub@meta.data |> 
                                      #Get unique response criteria in subset
                                      select(htb) |> 
                                      unique() |>
                                      #Convert to vector
                                      unlist() |> 
                                      #Remove names from vector 
                                      unname() |> 
                                      #Convert factor of criteria to character vector
                                      as.character() |> 
                                      #Sort patient vector 
                                      str_sort(numeric=TRUE) |> 
                                      #Convert to grammatically correct output
                                      vector_to_text()
                                  })
                                
                                #Render timepoints
                                output$dge_selected_treatment <- 
                                  renderText({
                                    rv$dge_s_sub@meta.data |> 
                                      #Get unique treatment (timepoints) 
                                      #in subset
                                      select(treatment) |> 
                                      unique() |>
                                      #Convert to vector
                                      unlist() |> 
                                      #Remove names from vector 
                                      unname() |> 
                                      #Convert factor to character vector
                                      as.character() |> 
                                      #Convert to grammatically correct output
                                      vector_to_text()
                                  })
                                
                                #N cells in subset
                                output$dge_print_n_cells <-
                                  renderText(isolate(rv$dge_n_cells))
                                
                                #N by class
                                output$dge_print_n_by_class <- 
                                  renderText(isolate(rv$dge_n_by_class))
                                
                                #Current Test
                                output$dge_print_mode <-
                                  renderText(isolate(rv$dge_mode))
                              })
                 
                 #8. Download Handler for DGE Table -----------------------------
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
  