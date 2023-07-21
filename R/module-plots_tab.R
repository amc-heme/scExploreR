#' Plots Tab Module (UI)
#'
#' @param id ID to use for module elements.
#' @param meta_choices Metadata available for selection from group_by and 
#' split_by dropdown menus. This is generated in the main server function upon 
#' app startup and when the object is changed. 
#' @param unique_metadata A list of the unique metadata values for the metadata
#' categories passed to `meta_choices`. This is generated in the main server
#' function upon app startup and when the object is changed. 
#' @param category_labels List of labels for each metadata category. This is 
#' loaded in the main server function when the object is changed.
#' @param assay_config The assays section of the config file. This is loaded in
#' the main server function at startup and when the object is changed.
#' @param metadata_config The metadata section of the config file loaded at 
#' startup. This is loaded in the main server function at startup and when the 
#' object is changed.
#' @param patient_colname the name of the metadata column to use for computing
#' patient- or sample-level metadata for plotting. This is defined in the config
#' file, and is loaded in the main server function at startup and when the 
#' object is changed.
#' @param reductions the reductions in the current object. This is loaded in the 
#' main server function at startup and when the object is changed.
#' @param categorical_palettes categorical palettes to show in the palette 
#' selection window.
#' @param continuous_palettes continuous palettes to show in the palette 
#' selection window.
#' @param auto_dictionary_path Path to the temporary file used for storing the
#' auto-generated object dictionary (created in the main app)
#' @param string_subsetting_href URL of the string subsetting vignette. 
#'
#' @noRd
plots_tab_ui <- function(id,
                         meta_choices,
                         unique_metadata,
                         category_labels,
                         assay_config,
                         metadata_config,
                         patient_colname,
                         reductions,
                         categorical_palettes,
                         continuous_palettes,
                         auto_dictionary_path,
                         string_subsetting_href
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
         fluid = FALSE,
         ## 1.1 Plot switches ####
         # Two-column format used
         # Division element containing both columns (this element keeps one 
         # of the columns from protruding into the elements beneath of it 
         # becomes larger than the other column)
         div(
           # Flexbox container: allows switches to resize when the size of the
           # window changes (flexbox behavior is created by adding 
           # 'display:"flex";' to the "two-column-container" class in the CSS
           # files)
           class = "two-column-container",
           # Left column
           div(
             class = "two_column",
             style = "float: left;",
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
             ),
             
             # Switch for cell type proportion bar plot
             materialSwitch(
               inputId = ns("make_proportion"),
               label = "Cell Proportion Plot", 
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
             ),
             # Switch for ridge plot
             materialSwitch(
               inputId = ns("make_ridge"),
               label = "Ridge Plot", 
               value = FALSE,
               right = TRUE,
               status = "default"
             ),
             
             # Switch for pie chart 
             # Only appears if a column for patient- or sample- level metadata
             # is defined
             if (!is.null(patient_colname())){
               materialSwitch(
                 inputId = ns("make_pie"),
                 label = "Metadata Pie Chart", 
                 value = FALSE,
                 right = TRUE,
                 status = "default"
                 )
               }
           ),#End div
         ),
         
         ## 1.2. Feature Text Entry. #### 
         # Applies to feature, violin, and dot plots unless the user specifies 
         # the use of different features for each plot (this is currently only 
         # possible for dot plots) 
         conditionalPanel(
           condition =
             glue("input['{ns('make_feature')}'] == true |
                  input['{ns('make_vln')}'] == true |
                  input['{ns('make_dot')}'] == true |
                  input['{ns('make_ridge')}'] == true"),
           # Content of conditionalPanel
           # Label
           tags$p(tags$strong("Enter features to display on plots:")),
           # Collapsible panel for types of features that may be entered
           collapsible_panel(
             inputId = ns("which_features"),
             label = "What Can I Enter Here?",
             active = FALSE,
             transparent = TRUE,
             size = "s",
             "The following types of features are available for this object:",
             tags$ul(
               lapply(
                 # Fetch assay labels from config file
                 assay_config(),
                 function (assay_entry) {
                   # Use label if defined, otherwise use `assay` field
                   if (!is.null(assay_entry$dropdown_title)){
                     tags$li(assay_entry$dropdown_title)
                   } else {
                     tags$li(assay_entry$assay)
                   }
                   
                   }
                 )
               )
             ),
           
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
                         type = "continuous",
                         output_html = TRUE
                         )
                     }
                   )
               )
           )
         ),
         
         ## 1.4. Feature summary statistics ####
         # Panel is hidden until at least one feature has been entered
         hidden(
           div(
             id = ns("feature_stats_showhide"),
             collapsible_panel(
               inputId = ns("feature_stats_collapsible"),
               label = "Feature Statistics",
               active = FALSE,
               feature_stats_ui(
                 id = ns("plots_feature_statistics")
                 )
               )
             )
           ), # End 1.4
         
         ## 1.5. Subsets for Plots ####
         collapsible_panel(
           inputId = ns("subset_collapsible"),
           label = "Subset Options",
           active = FALSE,
           # div for spinner that displays over the full subset options panel
           div(
             id = ns("subset_panel"),
             # 1.5.1 div for spinner that displays over the subset summary only
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

             # 1.5.2. Subset selection menus
             subset_selections_ui(
               id = ns("subset_selections"),
               unique_metadata = unique_metadata,
               metadata_config = metadata_config,
               auto_dictionary_path = auto_dictionary_path,
               string_subsetting_href = string_subsetting_href
               ),

             # 1.5.3. Submit button for subset
             actionButton(
               inputId = ns("subset_submit"),
               label="Apply Subset"
               )
             ) # End subset_panel div
         ), # End 1.5
         
         #----- Plot-specific options -----#####
         ## 1.6. DimPlot options ####
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
               reductions_menu =       TRUE,
               title_menu =            TRUE,
               group_by =              TRUE,
               split_by =              TRUE,
               ncol_slider =           TRUE,
               legend_options_menu =   TRUE,
               order_checkbox =        FALSE,
               label_checkbox =        TRUE,
               legend_checkbox =       TRUE,
               limits_checkbox =       TRUE,
               custom_colors =         FALSE,
               manual_dimensions =     TRUE,
               download_button =       TRUE
             )
           )
         ),
         
         ## 1.7. Feature plot options ####
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
               # Inputs included or excluded
               reductions_menu =            TRUE,
               title_menu =                 TRUE,
               legend_title_menu =          TRUE,
               # Group_by used for labeling groups
               group_by =                   TRUE,
               split_by =                   TRUE,
               ncol_slider =                TRUE,
               super_title_menu =           TRUE,
               share_scale_checkbox =       TRUE,
               color_by_feature_checkbox =  TRUE,
               blend_checkbox =             TRUE,
               order_checkbox =             TRUE,
               label_checkbox =             TRUE,
               legend_checkbox =            TRUE,
               limits_checkbox =            TRUE,
               custom_colors =              TRUE,
               manual_dimensions =          TRUE,
               download_button =            TRUE,
               # Default values for inputs
               label_default =              FALSE,
               # Modifications to group by menu
               # Label for group by menu: for feature plots, the group by
               # variable controls the metadata used for labeling groups
               group_by_label = "Metadata for Labeling Groups",
               group_by_include_none = FALSE
             )
           )
         ),
         
         ## 1.8. Violin plot options ####
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
               group_by =              TRUE,
               split_by =              TRUE,
               title_menu =            FALSE,
               sort_groups_menu =      TRUE,
               ncol_slider =           TRUE,
               legend_options =        TRUE,
               order_checkbox =        FALSE,
               label_checkbox =        FALSE,
               legend_checkbox =       TRUE,
               limits_checkbox =       FALSE,
               custom_colors =         FALSE,
               manual_dimensions =     TRUE,
               download_button =       TRUE
             )
           )
         ),
         
         ## 1.9. Dot plot options ####
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
               group_by =              TRUE,
               split_by =              FALSE,
               title_menu =            FALSE,
               sort_groups_menu =      TRUE,
               dot_x_labels_menu =     TRUE,
               ncol_slider =           FALSE,
               order_checkbox =        FALSE,
               label_checkbox =        FALSE,
               legend_checkbox =       TRUE,
               limits_checkbox =       FALSE,
               custom_colors =         FALSE,
               manual_dimensions =     TRUE,
               separate_features =     TRUE,
               download_button =       TRUE
               )
             )
           ), #End 1.9
         
         ## 1.10. Scatterplot options ####
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
               scatterplot_ui =        TRUE,
               group_by =              TRUE,
               split_by =              FALSE,
               ncol_slider =           FALSE,
               legend_options =        TRUE,
               order_checkbox =        FALSE,
               label_checkbox =        FALSE,
               legend_checkbox =       TRUE,
               limits_checkbox =       FALSE,
               display_coeff =         TRUE,
               custom_colors =         FALSE,
               manual_dimensions =     TRUE,
               separate_features =     FALSE,
               download_button =       TRUE            
               )
           )
         ), # End 1.10.
         
         ## 1.11. Ridge plot options ####
         conditionalPanel(
           condition = glue("input['{ns('make_ridge')}'] == true"),
           collapsible_panel(
             inputId = ns("ridge_collapsible"),
             label = "Ridge Plot Specific Options",
             active = FALSE,
             plot_module_ui(
               id = ns("ridge"),
               ui_component = "options",
               meta_choices = meta_choices,
               plot_label = "Ridge",
               group_by =              TRUE,
               split_by =              FALSE,
               title_menu =            TRUE,
               sort_groups_menu =      TRUE,
               ncol_slider =           FALSE,
               legend_options =        TRUE,
               order_checkbox =        FALSE,
               label_checkbox =        FALSE,
               legend_checkbox =       TRUE,
               limits_checkbox =       FALSE,
               display_coeff =         FALSE,
               custom_colors =         FALSE,
               custom_x_axis_ui =      TRUE,
               manual_dimensions =     TRUE,
               separate_features =     FALSE,
               download_button =       TRUE,
               # Add "none" to group by choices (ridge plots only)
               group_by_include_none =     TRUE
             )
           )
         ), # End 1.11
         
         ## 1.12. Proportion stacked bar plot options ####
         conditionalPanel(
           condition = glue("input['{ns('make_proportion')}'] == true"),
           collapsible_panel(
             inputId = ns("proportion_collapsible"),
             label = "Cell Proportion Plot Specific Options",
             active = FALSE,
             plot_module_ui(
               id = ns("proportion"),
               ui_component = "options",
               meta_choices = meta_choices,
               plot_label = "Cell Proportion",
               group_by =              TRUE,
               split_by =              TRUE,
               title_menu =            TRUE,
               sort_groups_menu =      TRUE,
               ncol_slider =           FALSE,
               legend_options =        TRUE,
               order_checkbox =        FALSE,
               label_checkbox =        FALSE,
               legend_checkbox =       TRUE,
               limits_checkbox =       FALSE,
               display_coeff =         FALSE,
               custom_colors =         FALSE,
               manual_dimensions =     TRUE,
               separate_features =     FALSE,
               download_button =       TRUE,
               # Modifications to group by/split by menus
               group_by_include_none = FALSE,
               split_by_include_none = FALSE,
               # Default selections for group by and split by
               group_by_default =
                 meta_choices()[[1]],
               split_by_default =      
                 if (isTruthy(patient_colname)){
                   patient_colname()
                   } else {
                     meta_choices()[[2]]
                   },
               group_by_label = 
                 "Choose Metadata for Proportions",
               split_by_label = 
                 "Choose Metadata for Proportion Comparison"
               )
             )
           ), # End 1.12.
         
         ## 1.13. Pie chart options ####
         # UI and module are only created if the metadata column used for 
         # patient/sample level metadata is defined.
         if (!is.null(patient_colname())){
           conditionalPanel(
             condition = glue("input['{ns('make_pie')}'] == true"),
             collapsible_panel(
               inputId = ns("pie_collapsible"),
               label = "Metadata Pie Chart Specific Options",
               active = FALSE,
               plot_module_ui(
                 id = ns("pie"),
                 ui_component = "options",
                 meta_choices = meta_choices,
                 plot_label = "Metadata Pie Chart",
                 patient_colname = patient_colname,
                 group_by =              TRUE,
                 split_by =              FALSE,
                 title_menu =            TRUE,
                 sort_groups_menu =      FALSE,
                 ncol_slider =           FALSE,
                 order_checkbox =        FALSE,
                 label_checkbox =        FALSE,
                 legend_checkbox =       TRUE,
                 limits_checkbox =       FALSE,
                 display_coeff =         FALSE,
                 custom_colors =         FALSE,
                 manual_dimensions =     TRUE,
                 separate_features =     FALSE,
                 download_button =       TRUE,
                 # Modifications to group by/split by menus
                 group_by_include_none = FALSE,
                 # Will later be set by config file
                 group_by_default =      "response",
                 group_by_label = 
                   "View Number of Patients by:",
                 # The patient/sample level column is removed from the group by
                 # choices (patient-level plots grouped by this column are not
                 # informative)
                 remove_patient_column = TRUE
                 )
               )
             ) # End 1.13.
           }
         ), # End 1.
       
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
             ),
           
           ## 2.6. Ridge plot panel
           plot_module_ui(
             id = ns("ridge"),
             ui_component = "plot"
             ),
           
           ## 2.7. Cell proportion stacked bar plot panel
           plot_module_ui(
             id = ns("proportion"),
             ui_component = "plot"
           ),
           
           ## 2.8. Metadata pie chart panel
           if (!is.null(patient_colname())){
             plot_module_ui(
               id = ns("pie"),
               ui_component = "plot"
             )
           }
         ) # End div
       ) # End 2.
     ) # End sidebarLayout() 
   ) # End fluidPage() 
   
}

#' Plots Tab Module (Server Instance)
#'
#' @param id ID to use for module elements.
#' @param object The Seurat object or subset. 
#' @param assay_config The assays section of the config file. This is loaded in
#' the main server function at startup and when the object is changed.
#' @param metadata_config The metadata section of the config file loaded at 
#' startup. This is loaded in the main server function at startup and when the 
#' object is changed.
#' @param meta_categories The metadata categories included in `metadata_config` 
#' (defines selectable metadata). This is generated in the main 
#' server function upon app startup and when the object is changed. 
#' @param category_labels List of labels for each metadata category. This is 
#' loaded in the main server function upon startup and when the object is 
#' changed.
#' @param unique_metadata A list of the unique metadata values for the metadata
#' categories included in `metadata_config`. This is generated in the main 
#' server function upon app startup and when the object is changed. 
#' @param valid_features  a list giving the valid features that can be selected 
#' from each assay. This is generated from the config file in the main server
#' function upon startup and when the object is changed.
#' @param error_list A list of error messages to use in a tryCatch expression. 
#' This is defined in the main server function at startup.
#' @param n_cells_original Number of cells in full Seurat object. Calculated in
#' the main server function.
#' @param lim_orig A list of original axes limits for each reduction 
#' enabled.
#' @param categorical_palettes Categorical palettes to show in the palette 
#' selection window.
#' @param continuous_palettes Continuous palettes to show in the palette 
#' selection window.
#' @param blend_palettes special palettes used for blended feature plots.
#' @param patient_colname The name of the metadata column to use for computing
#' patient- or sample-level metadata for plotting. This is defined in the config
#' file, and is loaded in the main server function at startup and when the 
#' object is changed.
#' 
#' @noRd
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
                             continuous_palettes,
                             blend_palettes,
                             patient_colname
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
                     html =
                       tagList(
                         spin_loaders(id = 2, color = "#555588"),
                         div(
                           class = "spinner_text",
                           "Preparing plots, please wait..."
                           )
                         ),
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
                   object = subset,
                   # plot_switch: uses the input$make_dimplot switch
                   plot_switch = reactive({input$make_dimplot}),
                   plot_label = "DimPlot",
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
                   object = subset, 
                   # plot_switch: uses the input$make_feature switch
                   plot_switch = reactive({input$make_feature}),
                   features_entered = reactive({input$text_features}),
                   plot_label = "Feature Plot",
                   n_cells_original = n_cells_original, 
                   # Instructs server on which plot function to run
                   plot_type = "feature",
                   assay_config = assay_config,
                   metadata_config = metadata_config,
                   lim_orig = lim_orig,
                   # Both palettes are passed to feature plot. Continuous
                   # palette is used unless "color_by_feature" is TRUE
                   palette = 
                     list(
                       "categorical_palette" = selected_categorical_palette,
                       "continuous_palette" = selected_continuous_palette
                       ),
                   blend_palettes = blend_palettes
                   )
                 
                 ## 2.3. Violin Plot ####
                 plot_module_server(
                   id = "violin",
                   object = subset, 
                   # plot_switch: uses the input$make_vln switch
                   plot_switch = reactive({input$make_vln}),
                   plot_label = "Violin Plot",
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
                   object = subset, 
                   # plot_switch: uses the input$make_dot switch
                   plot_switch = reactive({input$make_dot}),
                   features_entered = reactive({input$text_features}),
                   plot_label = "Dot Plot", 
                   # Instructs server on which plot function to run
                   plot_type = "dot",
                   assay_config = assay_config,
                   valid_features = valid_features,
                   separate_features_server = TRUE,
                   # Use continuous palettes for dot plot
                   palette = selected_continuous_palette
                   )
                 
                 ## 2.5. Scatterplot ####
                 plot_module_server(
                   id = "scatter",
                   object = subset, 
                   # plot_switch: uses the input$make_scatter switch
                   plot_switch = reactive({input$make_scatter}),
                   plot_label = "Scatterplot",
                   # Instructs server on which plot function to run
                   plot_type = "scatter",
                   # Valid features, for displaying choices for x- and y- axes
                   valid_features = valid_features,
                   # Use categorical palettes for scatterplot
                   palette = selected_categorical_palette,
                   assay_config = assay_config
                   )
                 
                 ## 2.6. Ridge Plot ####
                 plot_module_server(
                   id = "ridge",
                   object = subset, 
                   # plot_switch: uses the input$make_ridge switch
                   plot_switch = reactive({input$make_ridge}),
                   plot_label = "Ridge Plot", 
                   # Relies on feature text entry
                   features_entered = reactive({input$text_features}),
                   # Instructs server on which plot function to run
                   plot_type = "ridge",
                   assay_config = assay_config,
                   # Use categorical palettes for ridge plot
                   palette = selected_categorical_palette
                   )
                 
                 ## 2.7. Cell type proportion bar plot ####
                 plot_module_server(
                   id = "proportion",
                   object = subset,
                   # plot_switch: uses the input$make_proportion switch
                   plot_switch = reactive({input$make_proportion}),
                   plot_label = "Cell Proportion Plot", 
                   # Instructs server on which plot function to run
                   plot_type = "proportion",
                   # Use categorical palettes for cell type proportion plot
                   palette = selected_categorical_palette,
                   metadata_config = metadata_config,
                   assay_config = assay_config
                 )
                 
                 ## 2.8. Metadata pie chart ####
                 # Server instance is only created when patient_colname() is 
                 # defined
                 if (!is.null(patient_colname())){
                   plot_module_server(
                     id = "pie",
                     object = subset,
                     # plot_switch: uses the input$make_pie switch
                     plot_switch = reactive({input$make_pie}),
                     plot_label = "Metadata Pie Chart", 
                     # Instructs server on which plot function to run
                     plot_type = "pie",
                     # Use categorical palettes for cell type proportion plot
                     palette = selected_categorical_palette,
                     metadata_config = metadata_config,
                     assay_config = assay_config,
                     patient_colname = patient_colname
                     )
                   }
                 
                 # 3. Feature Summary Statistics -----
                 ## 3.1. Module server Instance ####
                 # Module server instance
                 feature_stats_server(
                   id = "plots_feature_statistics",
                   object = subset,
                   features_entered = reactive({input$text_features}),
                   assay_config = assay_config
                   )
                 
                 ## 3.2. Show and hide statistics window ####
                 # The feature statistics panel is shown only when features 
                 # have been entered.
                 observe({
                   target_id <- "feature_stats_showhide"
                 
                   if (isTruthy(input$text_features)){
                     showElement(
                       id = target_id,
                       anim = TRUE
                       )
                   } else {
                     hideElement(
                       id = target_id,
                       anim = TRUE
                       )
                   }
                 })
                 
                 # 4. Process Subset -------------------------------------------
                 ## 4.1 Module server to process user selections and report ####
                 # to other modules
                 # With reactive objects, a new module must be created for each 
                 # object to avoid collisions between subset menu ID's. 
                 subset_selections <-
                   subset_selections_server(
                     id = "subset_selections",
                     object = object,
                     unique_metadata = unique_metadata,
                     metadata_config = metadata_config,
                     assay_config = assay_config,
                     meta_categories = meta_categories,
                     valid_features = valid_features
                     )
                 
                 ## 4.2. Make Subset ####
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
                       # Display spinner over main window while the
                       # subset is being computed
                       main_spinner$show()

                       # Also display a spinner over the text showing
                       # The metadata in the current subset
                       subset_options_spinner$show()

                       plots_s_sub <-
                         tryCatch(
                           error = function(cnd){
                             # Return errors to user using notifications
                             # If an error is caught: the function below
                             # determines the type of error by inspecting
                             # message text with grepl (not recommended,
                             # but I currently don't know any other way to
                             # catch this error type)

                             # If the user has entered an advanced subsetting
                             # string, log what was entered
                             log_info("Error in plots tab subsetting.")
                             if (!is.null(subset_selections$user_string())){
                               log_info("Advanced subsetting: TRUE.")
                               log_info("String entered by user:")
                               log_info(subset_selections$user_string())
                             } else {
                               log_info("Advanced subsetting: FALSE.")
                             }

                             error_handler(
                               session,
                               cnd_message = cnd$message,
                               # Uses a list of
                               # subset-specific errors
                               error_list = error_list$subset_errors
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
                                   object(),
                                   criteria_list =
                                     subset_selections$selections(),
                                   user_string =
                                     subset_selections$user_string()
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
                 
                 ## 4.3 Check Subset ####
                 # Return notifications if conditions are not met.
                 observeEvent(
                   subset(),
                   ignoreNULL = FALSE,
                   ignoreInit = TRUE,
                   {
                     if (!is.null(subset())){
                       # Error A: Subset Only Contains one Cell
                       if (n_cells(subset()) == 1){
                         showNotification(
                           ui = 
                             icon_notification_ui(
                               icon = "exclamation-triangle",
                               # Change to feature when other 
                               # features are supported
                               "Only one cell is present in the current subset.
                               Plots may not draw correctly."
                             ),
                           #Show notification for 8 seconds
                           duration = 8,
                           session = session
                         )
                       }
                     }
                   })
                 
                 ## 4.4 Subset Summary Module ####
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
