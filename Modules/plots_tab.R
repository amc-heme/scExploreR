# Plots Tab Module

# Arguments
# id: ID to use for module elements.
# meta_choices: a named vector generated at app startup that contains 
# machine-readable and human-readable names for each metadata category 
# specified in the config file
# unique_metadata: a list of the unique metadata values for each of the metadata 
# categories listed in the config file. This is generated in the main server
# function at startup.

# TODO: replace metadata_config in the subset selections module with a more 
# specific variable
# metadata_config: the metadata section of the config file loaded at startup
plots_tab_ui <- function(id,
                         meta_choices,
                         unique_metadata,
                         metadata_config
                         ){
   # Namespace function: prevents conflicts with 
   # inputs/outputs defined in other modules 
   ns <- NS(id)
   
   #UI for plots tab
   fluidPage(
     # Sidebar layout: consists of a side panel and a main panel
     sidebarLayout(
       # 1. Sidebar panel for user input ---------------------------------------
       sidebarPanel(
         fluid=FALSE,
         ## 1.1 Checkboxes for choosing desired plot ####
         # Two-column checkboxes: put inside inline block elements 
         #that span half of the sidebar panel
         div(
           class="two_column",
           style="float: left;",
           #Specify if UMAP Plot is desired
           materialSwitch(
             inputId = ns("make_umap"),
             label = "UMAP plot", 
             value = TRUE,
             right = TRUE,
             status = "default"
             ),
           
           #Specify if feature plot is desired
           materialSwitch(
             inputId = ns("make_feature"),
             label = "Feature Plot", 
             value = FALSE,
             right = TRUE,
             status = "default"
             )
           ),#End div
         
         div(
           class="two_column",
           #Specify if violin plot is desired
           materialSwitch(
             inputId = ns("make_vln"),
             label = "Violin Plot", 
             value = FALSE,
             right = TRUE,
             status = "default"
             ),
           #Specify if dot plot is desired
           materialSwitch(
             inputId = ns("make_dot"),
             label = "Dot Plot", 
             value = FALSE,
             right = TRUE,
             status = "default"
             )
           ),#End div
         
         ## 1.2. Feature Text Entry. #### 
         # Applies to feature, violin, and dot plots unless the user specifies 
         # the use of different features for each plot (this is currently only 
         # possible for dot plots) 
         conditionalPanel(
           condition =
             glue("input['{ns('make_feature')}'] == true | 
                  input['{ns('make_vln')}'] == true |
                  input['{ns('make_dot')}'] == true"),
           # Content of conditionalPanel
           # Label
           tags$p(tags$strong("Enter features to display on plots:")),
           # Inline text entry and update button
           div(
             style="vertical-align: top; margin-bottom: 0px;",
             selectizeInput(
               inputId = ns("text_features"),
               multiple = TRUE, 
               label = NULL,
               choices = NULL,
               selected = NULL,
               # Add remove button to inputs
               options = list(
                 'plugins' = list('remove_button'),
                 # Do not allow user to input features not 
                 # in the list of options
                 'create'= FALSE
                 )
               ) 
             )
           ),# End 1.2.
         
         ## 1.3. Subsets for Plots ####
         # TODO: Put in New Module
         collapsible_panel(
           inputId = ns("subset_collapsible"),
           label = "Subset Options",
           active = FALSE,
           div(
             id = ns("subset_panel"),
             div(
               id = ns("subset_stats"),
               # Selected metadata outputs: still hard-coded
               tags$strong(
                 "Metadata in Displayed Subset",
                 id = ns("subset_header")
                 ),
               div(
                 tags$strong("Clusters: "),
                 textOutput(
                   outputId = ns("plots_selected_clusters"), 
                   inline = TRUE
                   )
                 ),
               div(tags$strong("Response criteria: "),
                   textOutput(
                     outputId = ns("plots_selected_response"), 
                     inline = TRUE
                     )
                   ),
               div(tags$strong("Timepoints: "),
                   textOutput(
                     outputId = ns("plots_selected_treatment"), 
                     inline = TRUE
                     )
                   ),
               div(tags$strong("Patients: "),
                   textOutput(
                     outputId = ns("plots_selected_htb"), 
                     inline = TRUE
                     )
                   )
               ),
             # Generate subset menus using the config file and unique_metadata
             # subset_selections module
             subset_selections_ui(
               id = ns("subset_selections"),
               unique_metadata = unique_metadata,
               metadata_config = metadata_config
               ),
             actionButton(
               inputId = ns("subset_submit"),
               label="Apply Subset"
               )
             )
           ),# End 1.3
         
         # TEMP: text output for subset selections
         verbatimTextOutput(
           outputId = ns("plots_subsets_return"),
           placeholder = TRUE
           ),
         
         ### Plot Specific Options ###
         ## 1.4. UMAP Options ####
         #Panel will display if "Make UMAP" switch is on
         conditionalPanel(
           condition = glue("input['ns(make_umap)']==true"),
           collapsible_panel(
             inputId = ns("umap_collapsible"),
             label = "UMAP Specific Options (Modular)",
             active = TRUE,
             plot_selections_ui(
               id = ns("umap"),
               ui_component = "options",
               meta_choices = meta_choices,
               plot_label = "UMAP",
               group_by =          TRUE,
               split_by =          TRUE,
               ncol_slider =       TRUE,
               label_checkbox =    TRUE,
               legend_checkbox =   TRUE,
               limits_checkbox =   TRUE,
               manual_dimensions = TRUE,
               download_button =   TRUE
             )
           )
         ),
         
         ## 1.5. Feature Plot Options ####
         conditionalPanel(
           condition = glue("input['ns(make_feature)']==true"),
           collapsible_panel(
             inputId = ns("feature_collapsible"),
             label = "Feature Plot Specific Options (Modular)",
             active = FALSE,
             plot_selections_ui(
               id = ns("feature"),
               ui_component = "options",
               meta_choices = meta_choices,
               plot_label = "Feature Plot",
               group_by =          FALSE,
               split_by =          TRUE,
               ncol_slider =       FALSE,
               label_checkbox =    FALSE,
               legend_checkbox =   TRUE,
               limits_checkbox =   TRUE,
               manual_dimensions = TRUE,
               download_button =   TRUE
             )
           )
         ),
         
         ## 1.6. Violin Plot Options ####
         conditionalPanel(
           condition = glue("input['ns(make_vln)']==true"),
           collapsible_panel(
             inputId = ns("vln_collapsible"),
             label = "Violin Plot Specific Options (Modular)",
             active = FALSE,
             plot_selections_ui(
               id = ns("violin"),
               ui_component = "options",
               meta_choices = meta_choices,
               plot_label = "Violin Plot",
               group_by =          TRUE,
               split_by =          TRUE,
               ncol_slider =       TRUE,
               label_checkbox =    FALSE,
               legend_checkbox =   TRUE,
               limits_checkbox =   FALSE,
               manual_dimensions = TRUE,
               download_button =   TRUE
             )
           )
         ), 
         
         ## 1.7. Dot Plot Options ####
         conditionalPanel(
           condition = glue("input['ns(make_dot)']==true"),
           collapsible_panel(
             inputId = ns("dot_collapsible"),
             label = "Dot Plot Specific Options (Modular)",
             active = FALSE,
             plot_selections_ui(
               id = ns("dot"),
               ui_component = "options",
               meta_choices = meta_choices,
               plot_label = "Dot Plot",
               group_by =          TRUE,
               split_by =          FALSE,
               ncol_slider =       FALSE,
               label_checkbox =    FALSE,
               legend_checkbox =   TRUE,
               limits_checkbox =   FALSE,
               manual_dimensions = TRUE,
               separate_features = TRUE,
               download_button =   TRUE
               )
             )
           ) #End 1.7
         ), #End 1.
       
       # 2. Main panel for displaying plot output ------------------------------
       mainPanel(
         # div added to contain Waiter spinner (forces the spinner to cover 
         # the full main panel)
         div(
           id = ns("main_panel"), 
           class = "spinner-container-main",
           # Panels for plots: display if checkboxes corresponding to 
           # each type are checked
           ## 2.1. UMAP plot panel
           plot_selections_ui(
             id = ns("umap"),
             ui_component = "plot"
           ),
           
           ## 2.2. Panel for feature plot 
           # Will be a message or a plot, depending on whether features have 
           # been entered
           plot_selections_ui(
             id = ns("feature"),
             ui_component = "plot"
           ),
           
           ## 2.3. Panel for violin plot
           # UI displayed will vary based on the entry into the feature text box
           plot_selections_ui(
             id = ns("violin"),
             ui_component = "plot"
           ),
           
           ## 2.4. Dot plot panel
           plot_selections_ui(
             id = ns("dot"),
             ui_component = "plot"
           )
         ) # End div
       ) # End 2.
     ) # End sidebarLayout() 
   ) # End fluidPage() 
   
}

# plots_tab_server
# Arguments
# id: Used to match server component of module to UI component
# sobj: The Seurat Object defined in the main server function
# assay_info: A list of assays defined in the config file and constructed in the
# main server function at startup.
# valid_features: a list giving the valid features that can be selected from 
# each assay. This is generated from the config file in the main server function
# error_list: a list of error messages to use in a tryCatch expression. This is 
# defined in the main server function at startup
# n_cells_original: Number of cells in full Seurat object. Calculated in main 
# server function.
# xlim_orig: x-limits of a UMAP plot of the full data. Applied when "use 
# original axes limits" is checked in the UMAP options after a subset is plotted
# ylim_orig: y-limits of a UMAP plot of the full data.

# TODO: replace metadata_config in the subset selections module with a more 
# specific variable
# metadata_config: the metadata section of the config file loaded at startup
plots_tab_server <- function(id,
                             sobj,
                             assay_info,
                             valid_features,
                             error_list,
                             n_cells_original,
                             xlim_orig,
                             ylim_orig,
                             metadata_config
                             ){
  moduleServer(id,
               function(input,output,session){
                 # Server namespace function: for dynamic UI
                 ns <- session$ns
                 
                 # 1. Plot Modules ---------------------------------------------
                 # A plot_selections module server is created for each plot
                 # UMAP Plot
                 plot_selections_server(
                   id = "umap",
                   object = plots_subset, # Reactive
                   # plot_switch: uses the input$make_umap switch
                   plot_switch = reactive({input$make_umap}),
                   plot_label = "UMAP", # Non-reactive
                   n_cells_original = n_cells_original, # Non-reactive
                   # Instructs server on which plot function to run
                   plot_type = "dimplot",
                   xlim_orig = xlim_orig,
                   ylim_orig = ylim_orig
                 )
                 
                 # Feature Plot
                 plot_selections_server(
                   id = "feature",
                   object = plots_subset, # Reactive
                   # plot_switch: uses the input$make_feature switch
                   plot_switch = reactive({input$make_feature}),
                   features_entered = reactive({input$text_features}),
                   plot_label = "Feature Plot", # Non-reactive
                   n_cells_original = n_cells_original, # Non-reactive
                   # Instructs server on which plot function to run 
                   plot_type = "feature",
                   assay_info = assay_info
                 )
                 
                 # Violin Plot
                 plot_selections_server(
                   id = "violin",
                   object = plots_subset, # Reactive
                   # plot_switch: uses the input$make_vln switch
                   plot_switch = reactive({input$make_vln}),
                   plot_label = "Violin Plot", # Non-reactive
                   features_entered = reactive({input$text_features}),
                   # Instructs server on which plot function to run 
                   plot_type = "violin",
                   assay_info = assay_info
                 )
                 
                 # Dot plot
                 plot_selections_server(
                   id = "dot",
                   object = plots_subset, # Reactive
                   # plot_switch: uses the input$make_dot switch
                   plot_switch = reactive({input$make_dot}),
                   features_entered = reactive({input$text_features}),
                   plot_label = "Dot Plot", # Non-reactive
                   # Instructs server on which plot function to run 
                   plot_type = "dot",
                   valid_features = valid_features,
                   separate_features_server = TRUE
                   )
                 
                 # 2. Process Subset -------------------------------------------
                 # 2.1 Module server to process user selections and report 
                 # to other modules
                 plots_subset_selections <- 
                   subset_selections_server(
                     id = "subset_selections",
                     sobj = sobj,
                     unique_metadata = unique_metadata,
                     metadata_config = metadata_config
                     )
                 
                 output$plots_subsets_return <- 
                   renderPrint({
                     plots_subset_selections()
                   })
                 
                 # 2.2. Update choices in subset selection menu based on user 
                 # selections
                 # Update patients menu based on entries in 'response' or 
                 # 'treatment' (timepoint)
                 observeEvent(
                   c(input$plots_response_selection,
                     input$plots_treatment_selection),
                   ignoreNULL = FALSE,
                   label = "Plots Update Patients",
                   {
                     # Display spinner during computation to keep user from 
                     # choosing outdated options
                     # Show a spinner while the valid patient ID's are calculated
                     waiter_show(
                       id = ns("subset_panel"),
                       html = spin_loaders(id = 2, color = "#555588"),
                       color = "#B1B1B188",
                       #Gives manual control of showing/hiding spinner
                       hide_on_render = FALSE 
                       )
                         
                     # Filter object for treatment and response selections
                     valid_patients <- 
                       sobj@meta.data |> 
                       filter(
                         (.data[["response"]] %in% input$plots_response_selection)&
                           (.data[["treatment"]] %in% input$plots_treatment_selection)
                         ) |> 
                       # Select patients metadata column
                       select(.data[["htb"]]) |> 
                       # Return unique values
                       unique() |>
                       # Convert to a character vector
                       unlist()
                     
                     # Form categorized list of valid patients for 
                     # display in dropdown menu
                     valid_patients_categories <- 
                       build_patient_list(valid_patients)
                     # Sort patients categorized list so they appear in order
                     valid_patients_categories <- 
                       sort_patient_list(valid_patients_categories)
                     
                     # Update picker input with valid patient IDs
                     updatePickerInput(
                       session,
                       inputId = ns("htb_selection"),
                       label = "Restrict by Patient",
                       choices = valid_patients_categories,
                       selected = valid_patients,
                       options = list(
                         "selected-text-format" = "count > 3",
                         "actions-box" = TRUE
                         )
                       ) # End updatePickerInput
                     
                     # Hide spinner
                     waiter_hide(ns("subset_panel"))
                     })
                 
                 # 2.3. Construct subset after "Apply Subset" button is clicked
                 plots_subset <- 
                   eventReactive(
                     input$subset_submit,
                     ignoreNULL=FALSE,
                     label = "Plots Subset", 
                     {
                       print("Executing subset code")
                       # Display spinner over main window while the 
                       # subset is being computed
                       waiter_show(
                         id = ns("main_panel"),
                         html = spin_loaders(id = 2, color = "#555588"),
                         color = "#FFFFFF",
                         # Gives manual control of showing/hiding spinner
                         hide_on_render = FALSE 
                         )
                       
                       # Also display a spinner over the text showing
                       # The metadata in the current subset
                       waiter_show(
                         id = ns("subset_stats"),
                         html = spin_loaders(id = 2, color = "#555588"),
                         color = "#B1B1B188",
                         # Gives manual control of showing/hiding spinner
                         hide_on_render = FALSE 
                         )
                       
                       plots_s_sub <- 
                         tryCatch(
                           error=function(cnd){
                             # Return errors to user using notifications
                             # If an error is caught: the function below
                             # determines the type of error by inspecting 
                             # message text with grepl (not recommended, 
                             # but I currently don't know any other way to 
                             # catch this error type)
                             error_handler(session,
                                           cnd_message = cnd$message,
                                           # Uses a list of 
                                           # subset-specific errors
                                           error_list = error_list,
                                           # Id prefix for the 
                                           # notification elements
                                           id_prefix = ns(""))
                             
                             # Return "NULL" for subset when an 
                             # error has occurred
                             plots_s_sub <- NULL
                             return(plots_s_sub)
                             }, # End tryCatch error function
                           # Begin tryCatch code
                           {
                             # Use subsetting function with the output of the 
                             # subset selections module as `criteria_list`.
                             plots_s_sub <-
                               make_subset(
                                sobj,
                                 criteria_list = plots_subset_selections
                                 )
                             }
                           )#End tryCatch
                       
                       # Hide the water
                       waiter_hide("plots_main_panel")
                       
                       # Return subset to the eventReactive variable
                       plots_s_sub
                       })
                 
                 # HARD CODED
                 # Rendering text for selected subsets
                 observeEvent(
                   input$subset_submit, 
                   ignoreNULL = FALSE,
                   label = "Plots: Render Subset Criteria",
                   {
                     # plots_subset() is NULL if errors are found during the 
                     # subsetting. Code will only proceed with identifying 
                     # metadata if no errors were encountered
                     if(!is.null(plots_subset())){
                       # Store the current metadata levels stored in the 
                       # selected subset
                       responses_found <- unique(plots_subset()$response)
                       treatments_found <- unique(plots_subset()$treatment)
                       patients_found <- unique(plots_subset()$htb)
                       clusters_found <- unique(plots_subset()$clusters)
                       
                       # Rendering Selections and Stats for report
                       output$plots_selected_clusters <- 
                         renderText({
                           #If all clusters are selected, print "All"
                           if(setequal(clusters_found,clusters)){
                             "All"
                             # Otherwise, print the selected clusters
                             } else { 
                               isolate(vector_to_text(clusters_found))
                               } # End Conditionals
                           }) # End renderText
                       
                       # Selected Response Criteria
                       output$plots_selected_response <- 
                         renderText({
                           # Print "All" if all response criteria are selected 
                           if(setequal(responses_found,responses)){
                             "All"
                             }else{
                               # Otherwise, print selected responses
                               isolate(vector_to_text(responses_found))
                               }# End conditionals
                           }) # End renderText
                       
                       # Selected Timepoints
                       output$plots_selected_treatment <- 
                         renderText({
                           # Print "All" if all treatment categories 
                           # (timepoints) are selected
                           if(setequal(treatments_found,treatments)){
                             "All"
                             }else{
                               # Otherwise, print selected treatment 
                               # categories (timepoints)
                               isolate(vector_to_text(treatments_found))
                               }# End conditionals
                           }) # End renderText
                       
                       # Selected Patients
                       output$plots_selected_htb <- 
                         renderText({
                           if(setequal(patients_found,patients)){
                             # Print "all" if all patient IDs are selected
                             "All"
                             }else{
                               # Otherwise, print selected patients
                               isolate(vector_to_text(patients_found))
                               } # End conditionals
                         }) # End renderText
                       }
                     
                     # When finished rendering current metadata, hide the 
                     # spinner over the panel
                     waiter_hide("subset_stats")
                     })
                 
                 
               })
  }
