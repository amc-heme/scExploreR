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
# metadata_config: the metadata section of the config file loaded at startup
plots_tab_ui <- function(id,
                         meta_choices,
                         unique_metadata,
                         category_labels,
                         metadata_config,
                         reductions,
                         categorical_palettes,
                         continuous_palettes
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
           # Height must be defined for feature selection box beneath the 
           # element to display correctly
           style = "height: 165px;",
           #Left column
           div(
             class="two_column",
             style="float: left;",
             # Switch for Dimplot
             materialSwitch(
               inputId = ns("make_dimplot"),
               label = "DimPlot", 
               value = TRUE,
               right = TRUE,
               status = "default"
             ),
             
             # Switch for feature plot
             materialSwitch(
               inputId = ns("make_feature"),
               label = "Feature Plot", 
               value = FALSE,
               right = TRUE,
               status = "default"
             ),
             
             # Switch for scatterplot
             materialSwitch(
               inputId = ns("make_scatter"),
               label = "Scatterplot", 
               value = FALSE,
               right = TRUE,
               status = "default"
             )
           ),# End div
           # Right column
           div(
             class="two_column",
             # Switch for violin plot
             materialSwitch(
               inputId = ns("make_vln"),
               label = "Violin Plot", 
               value = FALSE,
               right = TRUE,
               status = "default"
             ),
             # Switch for dot plot
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
         
         
         ## 1.3. Palette pickers for plots ####
         collapsible_panel(
           inputId = ns("palettes"), 
           label = "Palettes",
           active = TRUE,
           ### 1.3.1 Categorical data ####
           pickerInput(
             inputId = ns("categorical_palette"),
             label = "Palette (Categorical Data)",
             # Use the names of the palettes for choices (names will be
             # server values of selections)
             choices = names(categorical_palettes),
             selected = "default",
             choicesOpt =
               list(
                 content =
                   # Define HTML to display for each choice
                   sapply(
                     categorical_palettes,
                     function(palette){
                       palette_html(palette, n = 8, output_html = TRUE)
                     }
                   )
               )
           ),
           
           ### 1.3.2. Continuous Data ####
           pickerInput(
             inputId = ns("continuous_palette"),
             label = "Palette (Continuous Data)",
             # Use the names of the palettes for choices (names will be
             # server values of selections)
             choices = names(continuous_palettes),
             selected = "default",
             choicesOpt =
               list(
                 content =
                   # Define HTML to display for each choice
                   sapply(
                     continuous_palettes,
                     function(palette){
                       palette_html(
                         palette,
                         type="continuous",
                         output_html = TRUE
                         )
                     }
                   )
               )
           )
         ),
         
         ## 1.4. Subsets for Plots ####
         collapsible_panel(
           inputId = ns("subset_collapsible"),
           label = "Subset Options",
           active = FALSE,
           # div for spinner that displays over the full subset options panel
           div(
             id = ns("subset_panel"),
             # 1.4.1 div for spinner that displays over the subset summary only
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

             # 1.4.2. Subset selection menus
             subset_selections_ui(
               id = ns("subset_selections"),
               unique_metadata = unique_metadata,
               metadata_config = metadata_config
               ),

             # 1.4.3. Submit button for subset
             actionButton(
               inputId = ns("subset_submit"),
               label="Apply Subset"
               )
             ) # End subset_panel div
         ), # End 1.4
         
         ### Plot Specific Options ###
         ## 1.5. DimPlot Options ####
         # Panel will display if "Make DimPlot" switch is on
         conditionalPanel(
           # Javascript expression for condition in which to show panel
           # Input is accesses using bracket notation
           # Must use {ns('id')} (with quotes) to get the namespaced id,
           # and that id must be within quotes 
           condition = glue("input['{ns('make_dimplot')}'] == true"),
           collapsible_panel(
             inputId = ns("dimplot_collapsible"),
             label = "DimPlot Specific Options",
             active = TRUE,
             plot_module_ui(
               id = ns("dimplot"),
               ui_component = "options",
               meta_choices = meta_choices,
               plot_label = "DimPlot",
               reductions = reductions,
               reductions_menu =   TRUE,
               title_menu =        TRUE,
               group_by =          TRUE,
               split_by =          TRUE,
               ncol_slider =       TRUE,
               order_checkbox =    FALSE,
               label_checkbox =    TRUE,
               legend_checkbox =   TRUE,
               limits_checkbox =   TRUE,
               custom_colors =     FALSE,
               manual_dimensions = TRUE,
               download_button =   TRUE
             )
           )
         ),
         
         ## 1.6. Feature Plot Options ####
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
               title_menu =        FALSE,
               group_by =          FALSE,
               split_by =          TRUE,
               ncol_slider =       TRUE,
               order_checkbox =    TRUE,
               label_checkbox =    FALSE,
               legend_checkbox =   TRUE,
               limits_checkbox =   TRUE,
               custom_colors =     TRUE,
               manual_dimensions = TRUE,
               download_button =   TRUE
             )
           )
         ),
         
         ## 1.7. Violin Plot Options ####
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
               title_menu =        FALSE,
               ncol_slider =       TRUE,
               order_checkbox =    FALSE,
               label_checkbox =    FALSE,
               legend_checkbox =   TRUE,
               limits_checkbox =   FALSE,
               custom_colors =     FALSE,
               manual_dimensions = TRUE,
               download_button =   TRUE
             )
           )
         ),
         
         ## 1.8. Dot Plot Options ####
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
               title_menu =        FALSE,
               ncol_slider =       FALSE,
               order_checkbox =    FALSE,
               label_checkbox =    FALSE,
               legend_checkbox =   TRUE,
               limits_checkbox =   FALSE,
               custom_colors =     FALSE,
               manual_dimensions = TRUE,
               separate_features = TRUE,
               download_button =   TRUE
               )
             )
           ), #End 1.8
         
         ## 1.9. Scatterplot Options ####
         conditionalPanel(
           condition = glue("input['{ns('make_scatter')}'] == true"),
           collapsible_panel(
             inputId = ns("scatter_collapsible"),
             label = "Scatterplot Specific Options",
             active = FALSE,
             plot_module_ui(
               id = ns("scatter"),
               ui_component = "options",
               meta_choices = meta_choices,
               plot_label = "Scatterplot",
               scatterplot_ui =    TRUE,
               group_by =          TRUE,
               split_by =          FALSE,
               ncol_slider =       FALSE,
               order_checkbox =    FALSE,
               label_checkbox =    FALSE,
               legend_checkbox =   TRUE,
               limits_checkbox =   FALSE,
               display_coeff =     TRUE,
               custom_colors =     FALSE,
               manual_dimensions = TRUE,
               separate_features = FALSE,
               download_button =   TRUE            
               )
           )
         ) # End 1.9.
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
           ## 2.1. DimPlot plot panel
           plot_module_ui(
             id = ns("dimplot"),
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
           ),
           
           ## 2.5. Scatterplot panel
           plot_module_ui(
             id = ns("scatter"),
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
# metadata_config: the metadata section of the config file corresponding 
# to the current object.
# assay_config: the assays section of the config file corresponding to the 
# current object.
# meta_categories: metadata categories retrieved from the config file
# category_labels: list of labels for each metadata category, generated in main
# server at startup.
# unique_metadata: a list of all the unique metadata values in the current 
# object for the categories defined in the config file.
# valid_features: a list giving the valid features that can be selected from 
# each assay. This is generated from the config file in the main server function
# error_list: a list of error messages to use in a tryCatch expression. This is 
# defined in the main server function at startup
# n_cells_original: Number of cells in full Seurat object. Calculated in main 
# server function.
# lim_orig: a list of original axes limits for each reduction currently enabled.

# TODO: replace metadata_config in the subset selections module with a more 
# specific variable
# metadata_config: the metadata section of the config file loaded at startup
plots_tab_server <- function(id,
                             object,
                             metadata_config,
                             assay_config,
                             meta_categories,
                             category_labels,
                             unique_metadata,
                             valid_features,
                             error_list,
                             n_cells_original,
                             lim_orig,
                             categorical_palettes,
                             continuous_palettes
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
                 
                 # 1. Palettes -------------------------------------------------
                 # Store selected palettes
                 ## 1.1. Categorical Palette ####
                 selected_categorical_palette <-
                   reactive(
                     label = "Plots: Store selected palette (categorical)",
                     {
                       # Require input$categorical_palette to be defined before
                       # proceeding
                       
                       #req(input$categorical_palette)
                       # print(glue("{ns('')}"))
                       # print("Server value of selected palette")
                       # print(input$categorical_palette)
                       # Stores the palette selected in the pickerInput
                       
                       if (isTruthy(input$categorical_palette)){
                         if (input$categorical_palette == "default"){
                           # Returning NULL will direct plotting functions to use
                           # the default palette
                           return(NULL)
                         } else {
                           # Return the palette corresponding to the selection
                           # (in this case, it is a character vector of 
                           # hex codes)
                           return(
                             categorical_palettes[[input$categorical_palette]]
                           )
                         }
                       } else {
                         # If input$categorical_palette is NULL, pass NULL to 
                         # this reactive expression. Default palettes will be 
                         # used when the output is NULL.
                         return(NULL)
                         }
                       
                       })
                 
                 ## 1.2. Continuous palette ####
                 selected_continuous_palette <-
                   reactive(
                     label = "Plots: Store selected palette (continuous)",
                     {
                       # Require input$continuous_palette to be defined before
                       # proceeding
                       if (isTruthy(input$continuous_palette)){
                         if (input$continuous_palette == "default"){
                           # Returning NULL will direct plotting functions to use
                           # the default palette
                           return(NULL)
                         } else {
                           # Return the palette corresponding to the selection,
                           # as a character vector of hex codes
                           return(
                             continuous_palettes[[input$continuous_palette]]
                           )
                         }
                       } else {
                         # If input$continuous_palette is NULL, pass NULL to 
                         # this reactive expression. Default palettes will be 
                         # used when the output is NULL.
                         return(NULL)
                       }
                       
                     })
                 
                 # 2. Plot Modules ---------------------------------------------
                 # A server instance of the plot_module is created for each plot
                 ## 2.1. Dimplot ####
                 plot_module_server(
                   id = "dimplot",
                   object = subset, # Reactive
                   # plot_switch: uses the input$make_dimplot switch
                   plot_switch = reactive({input$make_dimplot}),
                   plot_label = "DimPlot", # Reactive
                   n_cells_original = n_cells_original, # Non-reactive
                   # Instructs server on which plot function to run
                   plot_type = "dimplot",
                   lim_orig = lim_orig,
                   metadata_config = metadata_config,
                   # DimPlots use categorical palettes
                   # Pass categorical palette selected by user to the server
                   palette = selected_categorical_palette
                   )
                 
                 ## 2.2. Feature Plot ####
                 plot_module_server(
                   id = "feature",
                   object = subset, # Reactive
                   # plot_switch: uses the input$make_feature switch
                   plot_switch = reactive({input$make_feature}),
                   features_entered = reactive({input$text_features}),
                   plot_label = "Feature Plot", # Non-reactive
                   n_cells_original = n_cells_original, # Reactive
                   # Instructs server on which plot function to run
                   plot_type = "feature",
                   assay_config = assay_config,
                   lim_orig = lim_orig,
                   palette = selected_continuous_palette
                   )
                 
                 ## 2.3. Violin Plot ####
                 plot_module_server(
                   id = "violin",
                   object = subset, # Reactive
                   # plot_switch: uses the input$make_vln switch
                   plot_switch = reactive({input$make_vln}),
                   plot_label = "Violin Plot", # Non-reactive
                   features_entered = reactive({input$text_features}),
                   # Instructs server on which plot function to run
                   plot_type = "violin",
                   assay_config = assay_config,
                   # Use categorical palettes for violin plot
                   palette = selected_categorical_palette
                   )
                 
                 ## 2.4. Dot plot ####
                 plot_module_server(
                   id = "dot",
                   object = subset, # Reactive
                   # plot_switch: uses the input$make_dot switch
                   plot_switch = reactive({input$make_dot}),
                   features_entered = reactive({input$text_features}),
                   plot_label = "Dot Plot", # Non-reactive
                   # Instructs server on which plot function to run
                   plot_type = "dot",
                   valid_features = valid_features,
                   separate_features_server = TRUE,
                   # Use continuous palettes for dot plot
                   palette = selected_continuous_palette
                   )
                 
                 ## 2.5. Scatterplot ####
                 plot_module_server(
                   id = "scatter",
                   object = subset, # Reactive
                   # plot_switch: uses the input$make_scatter switch
                   plot_switch = reactive({input$make_scatter}),
                   plot_label = "Scatterplot", # Non-reactive
                   # Instructs server on which plot function to run
                   plot_type = "scatter",
                   # Valid features, for displaying choices for x- and y- axes
                   valid_features = valid_features,
                   # Use categorical palettes for scatterplot
                   palette = selected_categorical_palette
                   )
                 
                 # 3. Process Subset -------------------------------------------
                 # 3.1 Module server to process user selections and report ####
                 # to other modules
                 # With reactive objects, a new module must be created for each 
                 # object to avoid collisions between subset menu ID's. 
                 plots_subset_selections <-
                   subset_selections_server(
                     id = "subset_selections",
                     object = object,
                     unique_metadata = unique_metadata,
                     metadata_config = metadata_config,
                     meta_categories = meta_categories,
                     valid_features = valid_features
                     )
                 
                 ## 3.2. Make Subset ####
                 # object_init: a reactive value set to TRUE when a new object 
                 # is loaded. When object_init is TRUE it signals the 
                 # plots_subset eventReactive to return the full object
                 # instead of a subset the first time a new dataset is loaded.
                 object_init <- reactiveVal(FALSE)
                 
                 # Create a reactive trigger 
                 object_trigger <- makeReactiveTrigger()
                 
                 # Set object_init to TRUE when an object is loaded, 
                 # and trigger the plots_subset eventReactive to run
                 observeEvent(
                   # Respond to downstream variable (results in less lag time 
                   # between removal of loading screen and rendering of DimPlot)
                   metadata_config(),
                   label = "Plots: object_init(TRUE)",
                   {
                     object_init(TRUE)
                     object_trigger$trigger()
                   })
                 
                 subset <-
                   eventReactive(
                     # Also reacts to the object. All downstream functions in 
                     # the plots tab respond to the "subset" object, so the 
                     # subset must be created each time a new object is loaded 
                     # to avoid downstream errors. 
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
                             error_handler(
                               session,
                               cnd_message = cnd$message,
                               # Uses a list of
                               # subset-specific errors
                               error_list = error_list,
                               # Id prefix for the
                               # notification elements
                               id_prefix = ns("")
                               )

                             # Return "NULL" for subset when an
                             # error has occurred
                             plots_s_sub <- NULL
                             return(plots_s_sub)
                             }, # End tryCatch error function
                           # Begin tryCatch code
                           {
                             if (object_init() == TRUE){
                               # if object_init is TRUE, return the full object
                               # instead of subsetting. Also set object_init
                               # back to FALSE. 
                               object_init(FALSE)
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
                                   criteria_list = 
                                     plots_subset_selections$selections,
                                   user_string = 
                                     plots_subset_selections$user_string
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
                   object = subset,
                   category_labels = category_labels,
                   unique_metadata = unique_metadata
                   )
                 
                 observeEvent(
                   subset(),
                   ignoreInit = TRUE,
                   ignoreNULL = FALSE,
                   label = "Post-subset Memory Query",
                   {
                     log_session(session)
                     log_info(
                       glue("Memory used after creating subset in plots tab: {to_GB(mem_used())}")
                       )
                   })
                 
               })
  }
