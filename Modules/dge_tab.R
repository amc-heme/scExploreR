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
      #1.2.1. Options Panel
      sidebarPanel(
        #Add a block to display a waiter over when the options are updating
        div(id = ns("sidebar"),
            #1.2.1.1. Restrict correlation table by metadata
            tags$h3("Differential Gene Expression"),
            tags$p("Use the dropdown menus below to select the desired test, the groups to use, and the subset on which to perform the test."),
            selectInput(inputId = ns("mode"),
                        label = "Choose test to perform",
                        #User chooses either dge or marker identification.
                        #Human- and computer- readable names given for both options
                        choices = c("Marker Identification" = "mode_marker",
                                    "Differential Expression" = "mode_dge"),
                        #Marker selection on clusters is displayed at startup
                        selected = "mode_marker"),
            #Further dropdown menus are specific to the chosen mode
            uiOutput(outputId = ns("conditional_menus")),
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
      #1.3.2 Main Pane
      mainPanel(
        div(id = ns("main_panel"), 
            class = "spinner-container-main",
            #TEMP: verbatimTextOutput container for displaying 
            verbatimTextOutput(outputId = ns("test_selection_output")),
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
                 
                 #1. Process dge_mode selection --------------------------------
                 #The value of input$dge_mode is stored as a reactive variable 
                 #and will be passed to the subset selections server module
                 dge_mode <- reactive(label = "dge_mode",
                                      {
                                        print("Change observed in dge_mode")
                                        return(input$mode)
                                        })
              
                 
                 ### 2.2.1. Reactive dropdown menus ####
                 #### 2.2.1.1. Reactive dropdown menu for patient ####
                 # The code in this section is duplicated from section 2.3.1, 
                 #and should be rewritten as a function to avoid redundancy.
                 
                 #Since patients fall into either the sensitive or resistant category,
                 #the patients dropdown will need to be updated to keep the user 
                 #from choosing invalid combinations. 
                 #Menu will be updated in the 
                 #future when variables such as treatment and time after diagnosis 
                 #are added (ignoreInit prevents this from happening when app is
                 #initialized. Running of code at startup is disabled with 
                 #"ignoreInit=TRUE"
                 # This (above line) is broken for DGE, but the behaviour is 
                 #tolerable as-is. Should be formalized or fixed.
                 # observeEvent(
                 #   c(input$dge_response_selection, 
                 #     input$dge_treatment_selection),
                 #   ignoreInit = TRUE,
                 #   label = "Reactive Patient Dropdown",
                 #   {
                 #     #Show a spinner while the valid patient ID's are calculated
                 #     waiter_show(
                 #       id = "dge_sidebar",
                 #       html = spin_loaders(id = 2, color = "#555588"),
                 #       color = "#B1B1B188",
                 #       hide_on_render = FALSE #Gives manual control of showing/hiding spinner
                 #     )
                 #     
                 #     ## DGE Metadata Subset 
                 #     #Valid for all modes except dge with response or treatment 
                 #     #as the group.by variable
                 #     #Filter object for treatment and response selections
                 #     valid_patients <- sobj@meta.data |> 
                 #       filter(
                 #         (.data[["response"]] %in% input$dge_response_selection)&
                 #           (.data[["treatment"]] %in% input$dge_treatment_selection)
                 #       ) |> 
                 #       #Select patients metadata column
                 #       select(.data[["htb"]]) |> 
                 #       #Return unique values
                 #       unique() |>
                 #       #Convert to a character vector
                 #       unlist()
                 #     
                 #     #Form categorized list of valid patients for display in 
                 #     #dropdown menu
                 #     valid_patients_categories <- build_patient_list(valid_patients)
                 #     #Sort patients categorized list so they appear in order
                 #     valid_patients_categories <- sort_patient_list(valid_patients_categories)
                 #     
                 #     #Update picker input with valid patient ID's
                 #     updatePickerInput(
                 #       session,
                 #       inputId = "dge_htb_selection",
                 #       #If patients is the group by variable, do not change the label
                 #       #(a special label is used for the group by category)
                 #       label = 
                 #         if (input$dge_group_by=="htb") {NULL}
                 #       else {"Restrict by Patient"},
                 #       choices = valid_patients_categories,
                 #       selected = valid_patients,
                 #       options = list(
                 #         "selected-text-format" = "count > 3",
                 #         "actions-box" = TRUE
                 #       )
                 #     )
                 #     
                 #     #Hide waiter
                 #     waiter_hide(id = "dge_sidebar")
                 #   }
                 # )
                 
                 #### 2.2.1.2. Reactive dropdown: DGE mode and either patient 
                 #or timepoint as the group by variable ####
                 # observeEvent(
                 #   c(input$dge_group_1, input$dge_group_2),
                 #   ignoreInit = TRUE,
                 #   label = "Reactive Patient Dropdown (DGE Groups)",
                 #   {
                 #     #Only update patients when response or treatment is the
                 #     #group_by variable
                 #     #If patient ID is the group by variable, update response 
                 #     #and treatment menus
                 #     #See else if at end of if statement below
                 #     if(input$dge_group_by %in% c("response","treatment")){
                 #       #Show a spinner while the valid patient ID's are calculated
                 #       waiter_show(
                 #         id = "dge_sidebar",
                 #         html = spin_loaders(id = 2, color = "#555588"),
                 #         color = "#B1B1B188",
                 #         hide_on_render = FALSE #Gives manual control of showing/hiding spinner
                 #       )
                 #       
                 #       ###### DGE Metadata Subset (groups mode) ####
                 #       #Filter object using the two group selections for either response or
                 #       #treatment, and the picker input selections for the other variable
                 #       if (input$dge_group_by=="treatment"){
                 #         valid_patients <- sobj@meta.data |> 
                 #           filter(
                 #             #Use two treatment selections and picker input for response
                 #             (.data[["response"]] %in% input$dge_response_selection)&
                 #               (.data[["treatment"]] %in% c(input$dge_group_1,input$dge_group_2))
                 #           ) |> 
                 #           #Select patients metadata column
                 #           select(.data[["htb"]]) |> 
                 #           #Return unique values
                 #           unique() |>
                 #           #Convert to a character vector
                 #           unlist()
                 #       } else if (input$dge_group_by=="response"){
                 #         valid_patients <- sobj@meta.data |> 
                 #           filter(
                 #             #Use two response selections and picker input for treatment
                 #             (.data[["response"]] %in% c(input$dge_group_1,input$dge_group_2))&
                 #               (.data[["treatment"]] %in% input$dge_treatment_selection)
                 #           ) |> 
                 #           #Select patients metadata column
                 #           select(.data[["htb"]]) |> 
                 #           #Return unique values
                 #           unique() |>
                 #           #Convert to a character vector
                 #           unlist() 
                 #       }
                 #       
                 #       #Form categorized list of valid patients for display in dropdown menu
                 #       valid_patients_categories <- build_patient_list(valid_patients)
                 #       #Sort patients categorized list so they appear in order
                 #       valid_patients_categories <- sort_patient_list(valid_patients_categories)
                 #       
                 #       #Update picker input with valid patient IDs
                 #       updatePickerInput(
                 #         session,
                 #         inputId = "dge_htb_selection",
                 #         label = "Restrict by Patient",
                 #         choices = valid_patients_categories,
                 #         selected = valid_patients,
                 #         options = list(
                 #           "selected-text-format" = "count > 3",
                 #           "actions-box" = TRUE
                 #         )
                 #       )
                 #       
                 #       #Hide waiter
                 #       waiter_hide(id = "dge_sidebar")
                 #       
                 #     } else if (input$dge_group_by=="htb") {
                 #       #In thus case, update *treatment* and *response* selections to reflect
                 #       #selected patients
                 #       #Show a spinner while the valid patient ID's are calculated
                 #       waiter_show(
                 #         id = "dge_sidebar",
                 #         html = spin_loaders(id = 2, color = "#555588"),
                 #         color = "#B1B1B188",
                 #         hide_on_render = FALSE #Gives manual control of showing/hiding spinner
                 #       )
                 #       
                 #       valid_choices <- sobj@meta.data |> 
                 #         filter(
                 #           #Use two treatment selections and picker input for response
                 #           (.data[["htb"]] %in% c(input$dge_group_1,input$dge_group_2))
                 #         ) |> 
                 #         #Select treatment and response columns
                 #         select(.data[["treatment"]],.data[["response"]]) #|> 
                 #       
                 #       #From the filtered metadata table above, fetch unique
                 #       #treatment and response values and process to correct input
                 #       valid_response <- valid_choices$response |> 
                 #         #Return unique values
                 #         unique() |> 
                 #         #Convert to a character vector
                 #         unlist() |> 
                 #         #Sort values (use function from stringr 
                 #         #package for natural sorting)
                 #         str_sort(numeric=TRUE)
                 #       
                 #       valid_treatment <- valid_choices$treatment |> 
                 #         #Return unique values
                 #         unique() |> 
                 #         #Convert to a character vector
                 #         unlist() |> 
                 #         #Sort values
                 #         str_sort(numeric=TRUE)
                 #       
                 #       #Update response input with valid choices
                 #       updatePickerInput(session,
                 #                         inputId = "dge_response_selection",
                 #                         label = "Restrict by Response",
                 #                         choices = valid_response,
                 #                         selected = valid_response)
                 #       
                 #       updatePickerInput(session,
                 #                         inputId = "dge_treatment_selection",
                 #                         label = "Restrict by Timepoint (approximate)",
                 #                         choices = valid_treatment,
                 #                         selected = valid_treatment)
                 #       
                 #       #Hide waiter
                 #       waiter_hide(id = "dge_sidebar")
                 #     }
                 #     
                 #   }
                 # )
                 
                 #### 2.2.1.3. Update group 2 menu after DGE group 1 selection ####
                 # observeEvent(
                 #   c(input$dge_group_1),
                 #   ignoreInit = TRUE,
                 #   label = "Reactive Patient Dropdown (DGE Groups)",
                 #   {
                 #     #Define valid choices for group 2 (excludes the choice currently
                 #     #selected for group 1)
                 #     new_choices<- 
                 #       rv$dge_group_choices[rv$dge_group_choices!=input$dge_group_1]  |>
                 #       #Sort choices
                 #       str_sort(numeric=TRUE)
                 #     
                 #     updateSelectInput(
                 #       session,
                 #       inputId = "dge_group_2",
                 #       label = "Group 2",
                 #       #Update choices to exclude the current group 1 choice
                 #       choices = new_choices,
                 #       #Preserve current selection in group 2, unless it is invalid
                 #       selected= 
                 #         if(input$dge_group_2!=input$dge_group_1) {input$dge_group_2} 
                 #       else {new_choices[1]}
                 #     )
                 #   })
                 
                 
                 ### 2.2.2. DGE table for selected metadata and restriction criteria ####
                 #Table updates only when the "Update" button is clicked
                 #### 2.2.2.1. Store table content as reactive value ####
                 #The tibble generated here is converted to a DT in 2.2.2.2. for viewing, and
                 #Is passed to the download handler when the "download table" button is clicked.
                 dge_table_content <- eventReactive(
                   input$submit,
                   label = "DGE Table Content",
                   ignoreNULL = FALSE,
                   {
                     #Show loading screen above main panel while table is computed (takes about a minute)
                     waiter_show(
                       id = "dge_main_panel",
                       html = spin_loaders(id = 2, color = "#555588"),
                       color = "#FFFFFF",
                       hide_on_render = FALSE #Gives manual control of showing/hiding spinner
                     )
                     
                     #Error handling: errors are frequent in this script, often 
                     #due to memory limitations, and they will result in the 
                     #spinner not disappearing from the main window since 
                     #waiter_hide() exists at the end this code block. Therefore, 
                     #the code in this block must be handled with tryCatch() to 
                     #capture errors. This error handling code has been duplicated
                     #verbatim from section 2.3.2.1 - likely isn't necessary here;
                     #should be removed if warranted by testing.
                     dge_table <- tryCatch(
                       #If an error is caught: attempt to determine type of 
                       #error by inspecting message text with grepl (not recommended, 
                       #but I currently don't know any other way to catch this error type)
                       error = function(cnd) {
                         #Use error_handler function to display notifications to the 
                         #user based on the error message
                         error_handler(session,
                                       cnd_message=cnd$message,
                                       #The error handling function uses a list 
                                       #of subset-specific errors 
                                       error_list = error_list,
                                       #Id prefix for notification elements
                                       id_prefix = "dge")
                         
                         #Return nothing if an error occurs
                         dge_table <-
                           NULL 
                         return(dge_table)
                       },
                       #End error function
                       #Begin tryCatch code
                       {
                         #Form subset based on chosen criteria
                         #Store in reactive variable so it can be accessed by 
                         #UMAP eventReactive() function
                         
                         ## Pick the appropriate input variables bases on the group_by selection
                         ## Not clear why the listing and unlisting is required for a character vector
                         dge_clusters_int = unlist(ifelse(
                           input$dge_group_by == "clusters",
                           list(input$dge_clusters_selection_group),
                           list(input$dge_clusters_selection)
                         ))
                         
                         dge_response_int = unlist(ifelse(
                           input$dge_group_by == "response",
                           list(input$dge_response_selection_group),
                           list(input$dge_response_selection)
                         ))
                         
                         dge_htb_int = unlist(ifelse(
                           input$dge_group_by == "htb",
                           list(input$dge_htb_selection_group),
                           list(input$dge_htb_selection)
                         ))
                         
                         dge_treatment_int = unlist(ifelse(
                           input$dge_group_by == "treatment",
                           list(input$dge_treatment_selection_group),
                           list(input$dge_treatment_selection)
                         ))
                         
                         #Subset method depends on whether marker identification or
                         #dge is selected
                         if (input$dge_mode=="mode_marker"){
                           #Marker identification: create subset based on the subset 
                           #dropdown menus, and include the group_by metadata in the 
                           #subset. The group_by dropdown is named reactively according
                           #to user choice, such that its id matches one of the four below. 
                           rv$dge_s_sub <- subset(
                             sobj,
                             subset =
                               (clusters %in% dge_clusters_int) &
                               (response %in% dge_response_int) &
                               (htb %in% dge_htb_int) &
                               (treatment %in% dge_treatment_int)
                           )#End subset
                         } else {
                           #For DGE, define subset conditionally based on the group by
                           #variable selection (VERY messy, but works for now)
                           if (input$dge_group_by=="clusters"){
                             #Subset will use the subset criteria chosen, as well as the two
                             #classes chosen for the group by variable 
                             
                             #Create a vector with the two classes chosen for DGE
                             clusters_selected_vector <- c(input$dge_group_1, input$dge_group_2)
                             
                             #Subset as usual for all variables except for the group by variable
                             rv$dge_s_sub <- subset(
                               sobj,
                               subset =
                                 #Use the vector above for the group_by_variable
                                 (clusters %in% clusters_selected_vector) &
                                 (response %in% dge_response_int) &
                                 (htb %in% dge_htb_int) &
                                 (treatment %in% dge_treatment_int)
                             )#End subset
                             
                           } else if (input$dge_group_by=="response"){
                             #Create a vector with the two response classes chosen for DGE
                             response_selected_vector <- c(input$dge_group_1, input$dge_group_2)
                             
                             #Subset as usual for all variables except for response
                             rv$dge_s_sub <- subset(
                               sobj,
                               subset =
                                 #Use the vector above for the group_by_variable
                                 (clusters %in% dge_clusters_int) &
                                 (response %in% response_selected_vector) &
                                 (htb %in% dge_htb_int) &
                                 (treatment %in% dge_treatment_int)
                             )#End subset
                             
                           } else if (input$dge_group_by=="htb"){
                             #Create a vector with the two htb classes chosen for DGE
                             htb_selected_vector <- c(input$dge_group_1, input$dge_group_2)
                             
                             #Subset as usual for all variables except for htb
                             rv$dge_s_sub <- subset(
                               sobj,
                               subset =
                                 #Use the vector above for the group_by_variable
                                 (clusters %in% dge_clusters_int) &
                                 (response %in% dge_response_int) &
                                 (htb %in% htb_selected_vector) &
                                 (treatment %in% dge_treatment_int)
                             )#End subset
                             
                           } else if (input$dge_group_by=="treatment"){
                             #Create a vector with the two treatment (timepoint) classes chosen for DGE
                             treatment_selected_vector <- c(input$dge_group_1, input$dge_group_2)
                             
                             #Subset as usual for all variables except for treatment
                             rv$dge_s_sub <- subset(
                               sobj,
                               subset =
                                 #Use the vector above for the group_by_variable
                                 (clusters %in% dge_clusters_int) &
                                 (response %in% dge_response_int) &
                                 (htb %in% dge_htb_int) &
                                 (treatment %in% treatment_selected_vector)
                             )#End subset
                           }
                           
                         }
                         
                         ###Subset Stats
                         #Cells in subset
                         rv$dge_n_cells <-
                           length(Cells(rv$dge_s_sub))
                         
                         ## DGE-specific Stats
                         #Number of classes in selection
                         rv$dge_n_classes <- length(unique(rv$dge_s_sub@meta.data[,input$dge_group_by]))
                         #Mode selection
                         rv$dge_mode <- ifelse(
                           rv$dge_n_classes == 2,
                           paste0(
                             "Differential Expression (",
                             unique(rv$dge_s_sub@meta.data[, input$dge_group_by])[1],
                             " vs. ",
                             unique(rv$dge_s_sub@meta.data[, input$dge_group_by])[2],
                             ")"
                           ),
                           paste0("Marker Identification (", rv$dge_n_classes, " classes)")
                         )
                         
                         print("n_by_class")
                         #Cells per class
                         #Calculate number of cells per class using metadata table of subset
                         n_by_class <- rv$dge_s_sub@meta.data |>
                           #Group by the specified metadata variable (.data and double braces 
                           #used so group_by can properly interpret the character vector input)
                           group_by(.data[[input$dge_group_by]]) |> 
                           summarise(n=n()) #Calculate number of cells per group 
                         
                         #Print class names and cell counts per class
                         #Convert class names and cell counts from tibble to character vector 
                         #Class names in first column of tibble
                         class_names <- as.character(n_by_class[[1]]) 
                         #Cell counts are in second column of tibble
                         n_cells <- n_by_class[[2]]
                         
                         #Build list of classes and the number of cells in each
                         n_list=list()
                         for (i in 1:nrow(n_by_class)){
                           n_list[[i]]<-glue("{class_names[i]}: {n_cells[i]}")
                         }
                         #Collapse list of class/count pairs into a string, 
                         #and store in reactive variable
                         rv$dge_n_by_class<-paste(n_list,collapse = "\n")
                         #\n is the separator (will be read by verbatimTextOutput())
                         
                         #Run Presto
                         dge_table <-
                           #Run presto on the subsetted object and indicated metadata slot
                           wilcoxauc(rv$dge_s_sub, group_by = input$dge_group_by) %>% 
                           #Explicitly coerce to tibble
                           as_tibble() %>%  
                           #remove stat and auc from the output table
                           select(-c(statistic, auc)) %>%  
                           #Using magrittr pipes here because the following statement 
                           #doesn't work with base R pipes
                           #remove negative logFCs if box is checked
                           {if (input$dge_pos) filter(., logFC > 0) else .} %>%
                           #Arrange in ascending order for padj, pval (lower values are more
                           #"significant"). Ascending order is used for the log fold-change
                           arrange(padj, pval, desc(abs(logFC))) 
                       }
                     )#End tryCatch
                     
                     #Hide loading screen
                     waiter_hide(id = "dge_main_panel")
                     waiter_hide(id = "dge_sidebar")
                     
                     #Return table for storage in dge_table_content()
                     dge_table
                   }
                 )
                 
                 #### 2.2.2.2. DGE table, as DT for viewing ####
                 dge_DT_content <- 
                   eventReactive(input$submit, 
                                 label = "DGE DT Generation",
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
                 
                 ### 2.2.3. DGE UI Components ####
                 #### 2.2.3.1. DGE Main UI (Stats and Table/UMAP Outputs) ####
                 #IgnoreNULL set to false to get UI to render at start up
                 dge_ui <- eventReactive(input$submit,
                                         label = "DGE Main UI (Define Content)",
                                         ignoreInit=TRUE,
                                         ignoreNULL = FALSE, 
                                         {
                                           print("DGE UI Function")
                                           waiter_show(
                                             id = "dge_main_panel",
                                             html = spin_loaders(
                                               id = 2, 
                                               color = "#555588"
                                               ),
                                             color = "#FFFFFF",
                                             #Gives manual control of 
                                             #showing/hiding spinner
                                             hide_on_render = FALSE 
                                           )
                                           
                                           #Also display spinner over the options menu to keep user from being able to click download buttons before content is ready
                                           waiter_show(
                                             id = "dge_sidebar",
                                             html = spin_loaders(id = 2, color = "#555588"),
                                             color = "#B1B1B188",
                                             hide_on_render = FALSE #Gives manual control of showing/hiding spinner
                                           )
                                           
                                           #UI to display
                                           div(
                                             tags$h2(
                                               glue(
                                                 "Differential Expression/Marker Genes by {input$dge_group_by} in Subset"
                                               ),
                                               class="center"
                                             ),
                                             #Table Metadata section
                                             tags$h3("Test Summary", class="center"),
                                             #Make each input criteria appear inline
                                             div(
                                               div(
                                                 tags$strong("Test selected", class="x-large inline-block"),
                                                 textOutput(outputId = "dge_print_mode", inline=FALSE)
                                               ),
                                               tags$strong("Subset Used for Test", class="x-large inline-block space-top"),
                                               div(
                                                 tags$strong("Clusters: "),
                                                 textOutput(outputId = "dge_selected_clusters", inline = TRUE)
                                               ),
                                               div(
                                                 tags$strong("Response criteria: "),
                                                 textOutput(outputId = "dge_selected_response", inline = TRUE)
                                               ),
                                               div(
                                                 tags$strong("Timepoints (approximate): "),
                                                 textOutput(outputId = "dge_selected_treatment", inline = TRUE)
                                               ),
                                               div(
                                                 tags$strong("Patients: "),
                                                 textOutput(outputId = "dge_selected_htb", inline = TRUE)
                                               ),
                                               div(
                                                 tags$strong("Number of cells in subset: ",
                                                             class="space-top inline-block"),
                                                 textOutput(outputId = "dge_print_n_cells", inline = TRUE)
                                               ),
                                               div(
                                                 tags$strong("Number of cells per class: "),
                                                 verbatimTextOutput(outputId = "dge_print_n_by_class") 
                                               )
                                               
                                             ),
                                             
                                             #Correlations table and plots
                                             tags$h3("DGE Table", class="center"),
                                             #Add table container
                                             div(#Use a DT data table
                                               DTOutput(outputId = "dge_table")),
                                             
                                             #UMAP plot
                                             #Label: depends on mode
                                             #The conditional below is passed as an argument 
                                             #To the div() function to generate the HTML for the
                                             #DGE UI. hasName() is used to avoid errors arising
                                             #From the rv$dge_n_classes not yet existing
                                             #This happens at startup when the UI is computed 
                                             #Before the table, where rv$dge_n_classes is undefined.
                                             if(hasName(rv,"dge_n_classes")){
                                               #Title for differential gene expression mode
                                               if (rv$dge_n_classes == 2){
                                                 tags$h3("UMAP of groups being compared")
                                               }else{
                                                 #Title for marker identification mode
                                                 tagList(
                                                   #Center text
                                                   tags$h3("UMAP by class", 
                                                           class="center"),
                                                   tags$p("(Markers are computed for each group shown)", 
                                                          class="center")
                                                 )
                                               }
                                             } else {
                                               #This conditional will be run at startup.
                                               #The default is to compute markers for each cluster,
                                               #so the text for marker identification will be shown.
                                               tagList(
                                                 #Center text
                                                 tags$h3("UMAP by class",
                                                         class="center"),
                                                 tags$p("(Markers are computed for each group shown)", 
                                                        class="center")
                                               )
                                             }, #This comma is very important (conditional is an argument)
                                             
                                             #UMAP container
                                             plotOutput(outputId = "dge_umap",
                                                        height = "600px")
                                             
                                           )#End UI div
                                         })
                 
                 #### 2.2.3.2. Conditional Dropdown Menus ####
                 #display based on whether DGE or marker identification is selected
                 #2.2.3.2.1. Main Dropdown interface
                 dge_conditional_menus <- 
                   eventReactive(
                     dge_mode(),
                     label = "DGE Conditional Menus",
                     ignoreNULL = FALSE,
                     {
                       print("Begin code to compute UI (dge_tab_module)")
                       ui <- 
                         tagList(
                           #Test selection module UI: display separate modules 
                           #depending on the chosen mode
                           if (dge_mode() == "mode_marker"){
                             dge_test_selection_ui(
                               id = ns("test_selection_marker"),
                               dge_mode = "mode_marker",
                               meta_choices = meta_choices
                               )
                             } else if (dge_mode() == "mode_dge"){
                               dge_test_selection_ui(
                                 id = ns("test_selection_dge"),
                                 dge_mode = "mode_dge",
                                 meta_choices = meta_choices
                                 )
                               },
                           #Subset selections module UI: same for both modes
                           subset_selections_ui(
                             id = ns("subset_selections"),
                             unique_metadata = unique_metadata,
                             metadata_config = metadata_config
                             )
                           )
                       
                       print("End code to compute UI (dge_tab_module)")
                       
                       return(ui)
                       
                       # if (input$dge_mode=="mode_marker"){
                       #   
                       #   
                       #   #Old implementation
                       #   # #Metadata to use for marker identification
                       #   # ui <- tagList(
                       #   #   selectInput(inputId = "dge_group_by",
                       #   #               label = "Choose metadata to use for marker identification:",
                       #   #               #Remove "none" and "best_response" from selectable options to group by
                       #   #               choices = meta_choices[!meta_choices %in% "none"],
                       #   #               #At startup, marker selection is ran with clusters as the
                       #   #               #group by variable.
                       #   #               selected="clusters"
                       #   #   ),#End selectInput
                       #   #   #Choice of classes to include in marker identification 
                       #   #   #Based on group_by selection above
                       #   #   uiOutput(outputId = "dge_marker_selection"),
                       #   #   #Subset choices: depend on what the user selects for the group by variable
                       #   #   uiOutput(outputId = "dge_subset_selection")
                       #   # )#end tagList 
                       #   
                       # } else {
                       #   #UI for differential gene expression
                       #   #Metadata to use for dge: text displayed to user is different
                       #   ui <- tagList(
                       #     selectInput(inputId = "dge_group_by",
                       #                 label = "Choose metadata to use for differential gene expression:",
                       #                 #Remove "none" and "best_response" from selectable options to group by
                       #                 choices = meta_choices[!meta_choices %in% "none"],
                       #                 #Clusters is selected by default
                       #                 selected = "clusters"
                       #     ),#End selectInput
                       #     #Choice of groups to compare: depends on what metadata type is selected
                       #     uiOutput(outputId = "dge_group_selection"),
                       #     #Subset choices: also depends on user selection
                       #     uiOutput(outputId = "dge_subset_selection")
                       #   )#End tagList
                       # }
                       
                       
                     })
                 
                 #2.2.3.2.2. UI to pick groups to compare for DGE
                 # dge_group_selection <- 
                 #   eventReactive(input$dge_group_by,
                 #                 label="DGE: groups for DGE test",
                 #                 ignoreNULL = FALSE,
                 #                 {
                 #                   print("Running code for DGE selections")
                 #                   #UI is only displayed when dge is selected
                 #                   if (input$dge_mode=="mode_dge"){
                 #                     #Use metadata type to determine choices 
                 #                     #for dropdown menu. Store in a reactive
                 #                     #variable so group 2 can be reactively
                 #                     #updated to exclude the selection in
                 #                     #group 1
                 #                     rv$dge_group_choices = 
                 #                       sobj@meta.data |>
                 #                       #Get unique values for the metadata 
                 #                       #type entered
                 #                       select(.data[[input$dge_group_by]]) |> 
                 #                       unique() |>
                 #                       #Convert to vector
                 #                       unlist() |> 
                 #                       #Remove names from vector 
                 #                       unname() |> 
                 #                       #Convert factor of choices to character vector
                 #                       as.character() |> 
                 #                       #Sort choices
                 #                       str_sort(numeric=TRUE)
                 #                     
                 #                     #Choose two groups for dge from the metadata
                 #                     #type specified by the user. 
                 #                     #tagList: combines the elements below to 
                 #                     #Output them together
                 #                     tagList(
                 #                       #Put choices beside one another in 
                 #                       #two-column format
                 #                       div(
                 #                         class="two_column float_left",
                 #                         selectInput(
                 #                           inputId = "dge_group_1",
                 #                           label = "Group 1",
                 #                           choices = rv$dge_group_choices,
                 #                           selected = rv$dge_group_choices[1]
                 #                           )
                 #                         ), #End div
                 #                       div(
                 #                         class="two_column float_right",
                 #                         selectInput(
                 #                           inputId = "dge_group_2",
                 #                           label = "Group 2",
                 #                           choices = rv$dge_group_choices,
                 #                           selected = rv$dge_group_choices[2]
                 #                           )
                 #                         )
                 #                       )#End tagList
                 #                     
                 #                     #Do not display UI if mode is not dge
                 #                     } else NULL
                 #                   })
                 
                 #2.2.3.2.3. UI to pick classes for marker identification
                 # dge_marker_selection <- 
                 #   eventReactive(input$dge_group_by,
                 #                 label="DGE: groups for marker identificaiton",
                 #                 ignoreNULL = FALSE,
                 #                 {
                 #                   #UI is only displayed when marker identification
                 #                   #is selected
                 #                   if (input$dge_mode=="mode_marker"){
                 #                     #Use metadata type to determine choices 
                 #                     #for picker menu
                 #                     marker_choices = 
                 #                       sobj@meta.data |>
                 #                       #Get unique values for the metadata type
                 #                       #entered
                 #                       select(.data[[input$dge_group_by]]) |> 
                 #                       unique() |>
                 #                       #Convert to vector
                 #                       unlist() |> 
                 #                       #Remove names from vector 
                 #                       unname() |> 
                 #                       #Convert factor of choices to character vector
                 #                       as.character() |> 
                 #                       #Sort choices
                 #                       str_sort(numeric=TRUE)
                 #                     
                 #                     #If the group by variable is patient
                 #                     #ID, form the categorized list of 
                 #                     #patients
                 #                     if (input$dge_group_by=="htb"){
                 #                       valid_patients_categorized <-
                 #                         build_patient_list(marker_choices)
                 #                       
                 #                       valid_patients_categorized <-
                 #                         sort_patient_list(
                 #                           valid_patients_categorized
                 #                         )
                 #                       }
                 #                     
                 #                     #Choose classes to include in marker search
                 #                     #Input id: use group by variable and syntax
                 #                     #Used for subsetting in other tabs to simplify
                 #                     #Subset computation when the table is computed
                 #                     pickerInput(
                 #                       inputId = 
                 #                         glue("dge_{input$dge_group_by}_selection_group"),
                 #                       label = "Choose classes to include in marker computation",
                 #                       #Choices: if patient ID
                 #                       #is the group by variable,
                 #                       #Use the categorized patient list
                 #                       choices = 
                 #                         if (input$dge_group_by!="htb") {
                 #                           marker_choices
                 #                           } else {
                 #                             valid_patients_categorized
                 #                             },
                 #                       #At startup, marker_choices 
                 #                       #is equal to all clusters in the
                 #                       #object. All are selected by 
                 #                       #default.
                 #                       selected = marker_choices,
                 #                       multiple = TRUE,
                 #                       options = list(
                 #                         "selected-text-format" = "count > 3",
                 #                         "size" = 10,
                 #                         #Define max options to show at 
                 #                         #a time to keep menu from being cut off
                 #                         "actions-box"=TRUE
                 #                         )
                 #                       )
                 #                     #Do not display UI if the mode is not marker identification
                 #                     } else NULL 
                 #                   })
                 
                 #2.2.3.2.4. UI to select subset, for both DGE and marker
                 #identification
                 # dge_subset_selection <- 
                 #   eventReactive(input$dge_group_by,
                 #                 label="DGE: Subset Menu UI",
                 #                 ignoreNULL = FALSE,
                 #                 {
                 #                   #Build subset menus for all metadata 
                 #                   #categories in meta_choices, except for 
                 #                   #the category selected in input$dge_group_by
                 #                   menu_categories <- 
                 #                     meta_choices[!meta_choices %in% 
                 #                                    c("none", input$dge_group_by)]
                 #                   
                 #                   #Use the subset_menus function to create the
                 #                   #subset dropdowns UI
                 #                   subset_menus(unique_metadata,
                 #                                metadata_config = config$metadata,
                 #                                #Creates menus for the 
                 #                                #catgories defined above
                 #                                menu_categories=menu_categories,
                 #                                input_prefix = "dge_")
                 #                   })
                 
                 #### 2.2.3.3. Download Buttons for Table and Plots ####
                 dge_downloads_ui <-
                   eventReactive(
                     c(input$submit, input$dge_table_rows_selected),
                     label = "DGE Download Buttons UI",
                     ignoreNULL = FALSE,
                     {
                       #Conditional level one, !hasName(): TRUE before table is created, FALSE after
                       if (!hasName(input, "dge_table_rows_selected")) {
                         #!hasName()==TRUE
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
                 test_selections <- 
                   eventReactive(dge_mode(),
                                 label = "test selections server",
                                 ignoreNULL = FALSE,
                                 {
                                   print("Code to Run dge_test_selection_server")
                                   if (dge_mode()=="mode_marker"){
                                     print("Run test selection server (marker)")
                                     selections <- 
                                       dge_test_selection_server(
                                         id = "test_selection_marker",
                                         sobj = sobj,
                                         unique_metadata = unique_metadata,
                                         metadata_config = metadata_config, 
                                         meta_choices = meta_choices,
                                         dge_mode = "mode_marker"
                                       )
                                     return(selections)
                                   } else if (dge_mode()=="mode_dge") {
                                     print("Run test selection server (dge)")
                                     selections <- 
                                       dge_test_selection_server(
                                         id = "test_selection_dge",
                                         sobj = sobj,
                                         unique_metadata = unique_metadata,
                                         metadata_config = metadata_config, 
                                         meta_choices = meta_choices,
                                         dge_mode = "mode_marker"
                                       )
                                     return(selections)
                                   }
                                   })
                 
                 #TEMP: display outputs of test selection server
                 output$test_selection_output <- renderPrint({
                   test_selections()
                 })
                 
                 #Processing of Subset Selection Options
                 subset_selections <- 
                   subset_selections_server(
                     id = "subset_selections",
                     sobj = sobj,
                     unique_metadata = unique_metadata,
                     metadata_config = metadata_config
                   )
                 
                 #### 2.2.4. UMAP of DE Selected Groups ####
                 dge_umap <- eventReactive(input$submit, 
                                           ignoreNULL = FALSE, 
                                           label="DGE UMAP", {
                                             print("DGE UMAP")
                                             #ncol_argument: number of columns
                                             #based on number of classes being
                                             #analyzed in the subset.
                                             #Use double-bracket means of
                                             #accessing the metadata variable
                                             #(supports entry of an arbitrary
                                             #variable)
                                             #This means of access returns a
                                             #dataframe. 
                                             #Slice for the first row (the 
                                             #unique values)
                                             n_panel <- unique(rv$dge_s_sub[[input$dge_group_by]])[,1] |>
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
                                             
                                             #Create UMAP of subsetted object,
                                             #split by metadata for object
                                             #calculation, colored by cluster
                                             DimPlot(rv$dge_s_sub,
                                                     split.by =
                                                       input$dge_group_by,
                                                     group.by = "clusters",
                                                     ncol=ncol)
                                           })
                 
                 #### 2.2.5. Render DGE UI, table, and statistics ####
                 #Main UI
                 output$main_ui <- renderUI({
                   dge_ui()
                 })
                 
                 #Sidebar conditional UI
                 #Main conditional UI
                 output$conditional_menus <- renderUI({
                   print("Rendering dge_conditional_menus")
                   dge_conditional_menus()
                 })
                 
                 # #Marker selection, if indicated
                 # output$dge_marker_selection <- renderUI({
                 #   dge_marker_selection()
                 # })
                 # 
                 # #Group selection, if indicated
                 # output$dge_group_selection <- renderUI({
                 #   dge_group_selection()
                 # })
                 # 
                 # #Subset selection (depends on entries for marker and 
                 # #group selection)
                 # output$dge_subset_selection <- renderUI({
                 #   dge_subset_selection()
                 # })
                 
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
                 
                 #### 2.2.6. Download Handler for DGE Table ####
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
  