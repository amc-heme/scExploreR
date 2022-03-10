# Plots Tab Module

# Arguments
# id: ID to use for module elements.
# meta_choices: a named vector generated at app startup that contains 
# machine-readable and human-readable names for each metadata category 
# specified in the config file
# unique_metadata: a list of the unique metadata values for each of the metadata 
# categories listed in the config file. This is generated in the main server
# function at startup.
# category_labels: list of labels for each metadata category, generated in main
# server at startup.
# reductions: a vector giving the reductions used in the Seurat object

# TODO: replace metadata_config in the subset selections module with a more 
# specific variable
# metadata_config: the metadata section of the config file loaded at startup
plots_tab_ui <- function(id,
                         meta_choices,
                         unique_metadata,
                         category_labels,
                         metadata_config,
                         reductions,
                         data_key
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
         # that span half of the sidebar panel
         # Division element that contains both columns (this element keeps one 
         # of the columns from protruding into the elements beneath of it 
         # becomes larger than the other column)
         div(
           class = "two-column-container",
           style = "height: 110px;",
           #Left column
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
           # Right column
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
         ),
         
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
             #Class below reduces margin beneath selectizeInput to 5px
             class="input-margin-5",
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
         collapsible_panel(
           inputId = ns("subset_collapsible"),
           label = "Subset Options",
           active = FALSE,
           # div for spinner that displays over the full subset options panel
           div(
             id = ns("subset_panel"),
             # 1.3.1 div for spinner that displays over the subset summary only
             div(
               id = ns("subset_stats"),
               # Header for subset summary
               tags$strong(
                 "Metadata in Displayed Subset",
                 id = ns("subset_header")
               ),
               # subset_summary module: prints the unique values for each
               # metadata category in the current subset/object
               subset_summary_ui(
                 id = ns("subset_summary"),
                 category_labels = category_labels
                 )
               ), # End subset_stats div

             # 1.3.2. Subset selection menus
             # With reactive objects, a new module must be created for each 
             # object to avoid collisions between subset menu ID's. 
             subset_selections_ui(
               id = glue(ns("{data_key()}_subset_selections")),
               unique_metadata = unique_metadata,
               metadata_config = metadata_config
               ),

             # 1.3.3. Submit button for subset
             actionButton(
               inputId = ns("subset_submit"),
               label="Apply Subset"
               )
             ) # End subset_panel div
         ), # End 1.3
         
         ### Plot Specific Options ###
         ## 1.4. UMAP Options ####
         # Panel will display if "Make UMAP" switch is on
         conditionalPanel(
           # Javascript expression for condition in which to show panel
           # Input is accesses using bracket notation
           # Must use {ns('id')} (with quotes) to get the namespaced id,
           # and that id must be within quotes 
           condition = glue("input['{ns('make_umap')}'] == true"),
           collapsible_panel(
             inputId = ns("umap_collapsible"),
             label = "UMAP Specific Options",
             active = TRUE,
             plot_module_ui(
               id = ns("umap"),
               ui_component = "options",
               meta_choices = meta_choices,
               plot_label = "UMAP",
               reductions = reductions,
               reductions_menu =   TRUE,
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
           condition = glue("input['{ns('make_feature')}'] == true"),
           collapsible_panel(
             inputId = ns("feature_collapsible"),
             label = "Feature Plot Specific Options",
             active = FALSE,
             plot_module_ui(
               id = ns("feature"),
               ui_component = "options",
               meta_choices = meta_choices,
               plot_label = "Feature Plot",
               reductions = reductions,
               reductions_menu =   TRUE,
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
           condition = glue("input['{ns('make_vln')}'] == true"),
           collapsible_panel(
             inputId = ns("vln_collapsible"),
             label = "Violin Plot Specific Options",
             active = FALSE,
             plot_module_ui(
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
           condition = glue("input['{ns('make_dot')}'] == true"),
           collapsible_panel(
             inputId = ns("dot_collapsible"),
             label = "Dot Plot Specific Options",
             active = FALSE,
             plot_module_ui(
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
           plot_module_ui(
             id = ns("umap"),
             ui_component = "plot"
           ),
           
           ## 2.2. Panel for feature plot 
           # Will be a message or a plot, depending on whether features have 
           # been entered
           plot_module_ui(
             id = ns("feature"),
             ui_component = "plot"
           ),
           
           ## 2.3. Panel for violin plot
           # UI displayed will vary based on the entry into the feature text box
           plot_module_ui(
             id = ns("violin"),
             ui_component = "plot"
           ),
           
           ## 2.4. Dot plot panel
           plot_module_ui(
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
# object: The Seurat Object defined in the main server function
# assay_config: the assays section of the config file loaded at app startup.
# category_labels: list of labels for each metadata category, generated in main
# server at startup.
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
                             object,
                             metadata_config,
                             assay_config,
                             meta_categories,
                             category_labels,
                             data_key,
                             unique_metadata,
                             valid_features,
                             error_list,
                             n_cells_original,
                             xlim_orig,
                             ylim_orig
                             ){
  moduleServer(id,
               function(input,output,session){
                 # Server namespace function: for dynamic UI
                 ns <- session$ns
                 
                 # Define spinners to show while subset and stats are computing
                 
                 # Spinner over subset criteria while options are updating
                 subset_options_spinner <-
                   Waiter$new(
                     id = ns("subset_panel"),
                     html = spin_loaders(id = 2, color = "#555588"),
                     color = "#B1B1B188",
                     #Gives manual control of showing/hiding spinner
                     hide_on_render = FALSE 
                   )
                 
                 # Spinner displaying over the unique values in the subset while
                 # a new subset is calculated
                 subset_meta_spinner <-
                   Waiter$new(
                     id = ns("subset_stats"),
                     html = spin_loaders(id = 2, color = "#555588"),
                     color = "#B1B1B188",
                     # Gives manual control of showing/hiding spinner
                     hide_on_render = FALSE 
                   )
                 
                 # Spinner for main panel
                 main_spinner <-
                   Waiter$new(
                     id = ns("main_panel"),
                     html = spin_loaders(id = 2, color = "#555588"),
                     color = "#FFFFFF",
                     # Gives manual control of showing/hiding spinner
                     hide_on_render = FALSE 
                   )
                 
                 # Feature choices for text entry 
                 observeEvent(
                   # Reactive - updates in response to change in dataset
                   # (valid_features computed downstream of object())
                   valid_features(),
                   label = "Render choices for feature selection",
                   {
                     updateSelectizeInput(
                       session,
                       # Do not namespace IDs in update* functions
                       inputId = "text_features", 
                       choices = valid_features(), 
                       server = TRUE
                       )
                   })
                 
                 # 2. Plot Modules ---------------------------------------------
                 # A server instance of the plot_module is created for each plot
                 # UMAP Plot
                 plot_module_server(
                   id = "umap",
                   object = plots_subset, # Reactive
                   # plot_switch: uses the input$make_umap switch
                   plot_switch = reactive({input$make_umap}),
                   plot_label = "UMAP", # Reactive
                   n_cells_original = n_cells_original, # Non-reactive
                   # Instructs server on which plot function to run
                   plot_type = "dimplot",
                   xlim_orig = xlim_orig,
                   ylim_orig = ylim_orig
                   )
                 
                 # Feature Plot
                 plot_module_server(
                   id = "feature",
                   object = plots_subset, # Reactive
                   # plot_switch: uses the input$make_feature switch
                   plot_switch = reactive({input$make_feature}),
                   features_entered = reactive({input$text_features}),
                   plot_label = "Feature Plot", # Non-reactive
                   n_cells_original = n_cells_original, # Reactive
                   # Instructs server on which plot function to run
                   plot_type = "feature",
                   assay_config = assay_config
                   )
                 
                 # Violin Plot
                 plot_module_server(
                   id = "violin",
                   object = plots_subset, # Reactive
                   # plot_switch: uses the input$make_vln switch
                   plot_switch = reactive({input$make_vln}),
                   plot_label = "Violin Plot", # Non-reactive
                   features_entered = reactive({input$text_features}),
                   # Instructs server on which plot function to run
                   plot_type = "violin",
                   assay_config = assay_config
                   )
                 
                 # Dot plot
                 plot_module_server(
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
                 
                 # 3. Process Subset -------------------------------------------
                 # 3.1 Module server to process user selections and report ####
                 # to other modules
                 # With reactive objects, a new module must be created for each 
                 # object to avoid collisions between subset menu ID's. 
                 plots_subset_selections <-
                   eventReactive(
                     meta_categories(),
                     label = "Plots Tab: Subset Selections Module",
                     {
                      selections <- 
                        subset_selections_server(
                          id = glue("{data_key()}_subset_selections"),
                          object = object,
                          unique_metadata = unique_metadata,
                          metadata_config = metadata_config,
                          meta_categories = meta_categories
                          )
                      
                      selections
                     })
                 
                 ## 3.2. Make Subset ####
                 # object_init: a reactive value set to TRUE when a new object 
                 # is loaded. When object_init is TRUE it signals the 
                 # plots_subset eventReactive to return the full object
                 # instead of a subset the first time a new dataset is loaded.
                 object_init <- reactiveVal(FALSE)
                 
                 # Reactive trigger function
                 # Creates an action button which is programmatically 
                 # triggered instead of triggered by the user
                 # Code adapted from thread by Joe Cheng, the Author of Shiny.
                 # https://community.rstudio.com/t/shiny-reactivetriggers-in-observeevent/42769
                 makeReactiveTrigger <- function(){
                   rv <- reactiveValues(a = 0)
                   list(
                     depend = function() {
                       rv$a
                       invisible()
                     },
                     trigger = function() {
                       rv$a <- isolate(rv$a + 1)
                     }
                   )
                 }
                 
                 # Create a reactive trigger 
                 object_trigger <- makeReactiveTrigger()
                 
                 # Set object_init to TRUE when an object is loaded, 
                 # and trigger the plots_subset eventReactive to run
                 observeEvent(
                   # Respond to downstream variable (results in less lag time 
                   # between removal of loading screen and rendering of UMAP)
                   metadata_config(),
                   label = "object_init(TRUE)",
                   {
                     print("object_init set to TRUE")
                     object_init(TRUE)
                     object_trigger$trigger()
                   })
                 
                 plots_subset <-
                   eventReactive(
                     # Reacts to the object also (plots_subset must produce a 
                     # new "subset" each time the object is loaded, even though
                     # the subset is the full object initially. All downstream
                     # operations in the plots tab respond to the subset instead
                     # of the main object.)
                     c(input$subset_submit, object_trigger$depend()),
                     ignoreNULL=FALSE,
                     label = "Plots Subset",
                     {
                       # Display spinner over main window while the
                       # subset is being computed
                       main_spinner$show()

                       # Also display a spinner over the text showing
                       # The metadata in the current subset
                       subset_options_spinner$show()

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
                             print("Begin make_subset")
                             if (object_init() == TRUE){
                               # if object_init is TRUE, return the full object
                               # instead of subsetting. Also set object_init
                               # back to FALSE. 
                               object_init(FALSE)
                               print("object_init set to FALSE")
                               plots_s_sub <- object()
                             } else {
                               # Use subsetting function with the output of the
                               # subset selections module as `criteria_list`.
                               plots_s_sub <-
                                 make_subset(
                                   object,
                                   # plots_subset_selections is a reactive 
                                   # expression inside another reactive 
                                   # expression (the eventReactive in 3.1). Must
                                   # extract from eventReactive before passing 
                                   # it to the function, which will extract the
                                   # reactive inside
                                   criteria_list = plots_subset_selections()
                                 )
                               }
                             }
                           )#End tryCatch

                       # Hide the spinners
                       main_spinner$hide()
                       subset_options_spinner$hide()

                       # Return subset to the eventReactive variable
                       plots_s_sub
                       })
                 
                 ## 3.3 Subset Summary Module ####
                 # Computes and exports the unique metadata values in the 
                 # current subset/object
                 subset_summary_server(
                   id = "subset_summary",
                   object = plots_subset,
                   category_labels = category_labels,
                   unique_metadata = unique_metadata
                   )
                 
               })
  }
