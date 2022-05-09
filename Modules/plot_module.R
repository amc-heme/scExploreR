#' Plot Module
#' 
#' Displays selections menus for an individual plot, builds the plot, and 
#' displays it to the screen.
#' 
#' This module has two UI components, both of which must be called with the 
#' same ID for the module to display plots properly. The UI created with 
#' \code{plot_module_ui(ui_component = "options")} will give a list of menus used
#' for selecting options that apply to the plot created by the module, and the
#' UI created by \code{plot_module_ui(ui_component = "plot")} will display the 
#' plot itself.
#'
#' plot_module_ui
#'
#' The module UI has two components: "options", which builds the selection menus 
#' for plot options, and "plot", which contains the output for the plot itself. 
#' 
#' The UI component needs to be called twice, once in the desired location for 
#' the menus using ui_component="options", and again in the desired location for 
#' the plot output with ui_component="plot". For the plot component, the only
#' arguments that need to be set are id and the ui_component; all other
#' arguments are used for the options tab.
#'
#' @param id ID to use for module elements. IDs for the options and plot UI 
#' components, and the server component, must match.
#' @param ui_component Determines which UI component is to be plotted. Use 
#' "options" to create the input menus for plot options, and "plot" to create 
#' the output container for the plot.
#' @param meta_choices A named vector generated in the main server function 
#' giving the choices of metadata categories to split and group plots by.
#' @param plot_label A human readable name for the plot that will appear in the
#' menus. The name should make sense by itself (i.e. "Feature Plot" should be 
#' entered instead of "Feature").
#' @param reductions A vector giving all the reductions used in the Seurat
#' object. Must be supplied if reductions_menu == TRUE
#' @param reductions_menu if TRUE, display a menu with reduction options to 
#' choose for the plot
#' @param group_by If TRUE, display a menu with metadata categories to group by.
#' @param split_by If TRUE, display a menu with metadata categories to split by.
#' @param ncol_slider If TRUE, display a slider to specify the number of columns
#' to use for the plot. This only works for UMAP and violin plots. 
#' @param order_checkbox if TRUE, display a checkbox to plot cells in order of 
#' expression (for feature plots). This may be useful if a few cells with high 
#' expression are being obscured by neighboring cells with low expression, but 
#' it may over represent the percentage of cells expressing a feature.
#' @param label_checkbox If TRUE, display a checkbox for including labels on 
#' the plot.
#' @param legend_checkbox If TRUE, display a checkbox for including a legend.
#' @param limits_checkbox If TRUE, display a checkbox for the use of original 
#' axes. Limits when a subset is plotted. This only works for UMAP and 
#' Feature plots.
#' @param custom_colors If TRUE, display UI for adding custom colors to 
#' the current plot
#' @param manual_dimensions If TRUE, display an interface to specify manual 
#' height and width parameters for the plot.
#' @param separate_features If TRUE, display a checkbox to enter separate 
#' features for the plot. A text entry will be created if the checkbox 
#' is selected.
#' @param download_button If TRUE, display a download button that will save a
#' .png image of the plot to disk.
#'
plot_module_ui <- function(id,
                           # The plot_module UI consists of the options
                           # panels and the plot output, which exist in 
                           # different places in the app. This argument will
                           # allow the module server to update components in
                           # different locations
                           ui_component = c("options", "plot"),
                           meta_choices = NULL,
                           plot_label = "",
                           reductions = NULL,
                           # TEMP: conditionals vertically aligned
                           # for multi-cursor editing
                           reductions_menu =    FALSE,
                           scatterplot_ui =     FALSE,
                           group_by =           FALSE,
                           split_by =           FALSE,
                           title_menu =         FALSE,
                           ncol_slider =        FALSE,
                           order_checkbox =     FALSE,
                           label_checkbox =     FALSE,
                           legend_checkbox =    FALSE,
                           display_coeff =      FALSE,
                           limits_checkbox =    FALSE,
                           custom_colors =      FALSE,
                           manual_dimensions =  FALSE,
                           separate_features =  FALSE,
                           download_button =    FALSE
                           ){
  # Namespace function: prevents conflicts with IDs defined in other modules 
  ns <- NS(id)
  
  if (ui_component == "options"){
    # UI for plot options
    # Elements are added to tagList if specified when calling the module ui 
    # Attempted to use ifelse() for this; ifelse() did not print Shiny tags 
    # properly and was unable to process NULL
    tagList(
      # Add menus if their corresponding arguments are TRUE 
      # Two-feature entry menu specific to scatterplot
      if (scatterplot_ui == TRUE){
        tagList(
          # First feature
          selectizeInput(
            inputId = ns("scatter_1"),
            label = "Feature for X-axis",
            choices = NULL,
            selected = NULL,
            # Add remove button to inputs
            options = list(
              'plugins' = list('remove_button'),
              # Do not allow user to input features not
              # in the list of options
              'create'= FALSE
              )
            ),
          
          # Second feature
          selectizeInput(
            inputId = ns("scatter_2"),
            label = "Feature for Y-axis",
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
      } else NULL,
      
      # Group by menu
      if (group_by == TRUE){
        # Choices for group by selection: should exclude "none"
        group_by_choices <- meta_choices()[!meta_choices() %in% "none"]
        
        # If TRUE, add element
        selectInput(
          inputId = ns("group_by"), 
          label = "Metadata to Group By:",
          # Can select all options except "none"
          choices = group_by_choices, 
          # First option selected by default 
          selected = group_by_choices[1]
          )
        # Do not add element if FALSE
      } else NULL,
      
      # Split by menu
      if (split_by == TRUE){
        selectInput(
          inputId = ns("split_by"), 
          label = "Metadata to Split By:",
          # Use vector of included metadata category names from the config file
          choices = meta_choices(),  
          #"none" selected by default
          selected = "none"
         )
      } else NULL,
      
      # Reductions menu
      if (reductions_menu == TRUE){
        if (is.null(reductions)){
          # Error handling: return error if reductions is not specified when 
          # reductions_menu is TRUE
          stop("If reductions_menu is TRUE, `reductions` must be specified.")
        } else {
          if(!is.reactive(reductions)){
            # Also, return an error if `reductions` is not a reactive variable
            stop("`reductions` must be a reactive variable.")
          }
        }
        
        selectInput(
          inputId = ns("reduction"),
          label = "Choose Projection",
          choices = reductions()
          )
      } else NULL,
      
      # Title selection
      # User may select default title behavior, a custom title (if available)
      # or none.
      # Available options are updated server-side
      if (title_menu == TRUE){
        tagList(
          selectInput(
            inputId = ns("title_settings"),
            label = "Title options",
            # A third choice, "custom", is added server-side if it is 
            # valid based on the plot type
            choices = 
              c("Default" = "default", 
                "None" = "none"),
            selected = "default"
          ),
          # UI for entering a custom title: hidden with shinyjs and made visible
          # when "custom" is selected 
          hidden(
            div(
              id = ns("custom_title_div"),
              tags$b("Enter Custom Title"),
              tags$p(
                sytle = "font-size: 0.9em;",
                "(Press enter to update)"
              ),
              searchInput(
                inputId = ns("custom_title"),
                label = NULL,
                # Default value depends on current group-by category and is 
                # set server-side 
                value = "",
                # Search button: must be defined for updateSearchInput 
                # to trigger properly, but will be hidden 
                btnSearch = icon("sync"),
                # Icon for reset button
                btnReset = icon("redo-alt"),
                # resetValue: set to NULL to disable automatic resetting
                # resetting will occur manually via the server function
                resetValue = NULL
              )
            )
          )
          # UI for entering a custom title (shows if "custom" is selected)
          #uiOutput(ns("custom_title_ui"))
          )
      },
      
      # Slider to adjust number of columns
      if (ncol_slider == TRUE){
        #Dynamic UI (appears when split_by != "none")
        uiOutput(outputId = ns("ncol_slider"))
      } else NULL,
      
      # Checkbox to order cells by expression (feature plots only)
      if (order_checkbox == TRUE){
        checkboxInput(
          inputId = ns("order"),
          label = "Order Cells by Expression",
          # Order should be FALSE by default
          value = FALSE
          )
      } else NULL,
      
      # Checkbox to add/remove labels
      if (label_checkbox == TRUE){
        checkboxInput(
          inputId = ns("label"),
          label = "Label Groups",
          value = TRUE
          )
      } else NULL,
      
      # Checkbox to add or remove Legend
      if (legend_checkbox == TRUE){
        checkboxInput(
          inputId = ns("legend"),
          label = "Include Legend",
          value = TRUE
          )
      } else NULL,
      
      # Checkbox to remove title from plot
      if (display_coeff == TRUE){
        checkboxInput(
          inputId = ns("display_coeff"),
          label = "Show Pearson Coefficient",
          value = TRUE
          )
      } else NULL,
      
      # Checkbox to specify original axes limits
      if (limits_checkbox == TRUE){
        # Dynamic UI: displays when a subset is selected
        uiOutput(outputId = ns("limits_checkbox"))
      } else NULL,
      
      # Checkbox for custom colors on Feature plot
      if (custom_colors == TRUE){
        tagList(
          # Checkbox to select custom colors 
          checkboxInput(
            inputId = ns("custom_colors"),
            label = "Use Custom Colors",
            # FALSE by default
            value = FALSE
          ),
          conditionalPanel(
            condition = glue("input['{ns('custom_colors')}'] == true"), 
            tagList(
              div(
                class = "two-color-input-left",
                colourInput(
                  inputId = ns("min_color"),
                  label = "Low Expression",
                  # palette = "limited",
                  # allowedCols = color_choices,
                  value = "#E1E1E1"
                  )
                ),
              div(
                class = "two-color-input-right",
                colourInput(
                  inputId = ns("max_color"),
                  label = "High Expression",
                  # palette = "limited",
                  # allowedCols = color_choices,
                  value = "#000000"
                  )
                )
              )
            )
          )
        } else NULL,
      
      # UI for user control of plot dimensions
      if (manual_dimensions == TRUE){
        #If TRUE, call module for manual adjustment of plot dimensions
        manual_dimensions_ui(id = ns("manual_dim"))
      } else NULL,

      # UI to request use of separate features for plot
      if (separate_features == TRUE){
        # Store namespaced ID of use separate features check box
        # for facilitated entry in condionalPanel element
        sep_id <- ns("use_separate_features")
        
        tagList(
          # Checkbox to use separate features
          checkboxInput(
            inputId = sep_id,
            label = glue("Use separate features for {tolower(plot_label)}"), 
            value = FALSE
            ),
          
          # Use conditional panel (rendering UI will reset the text entry)
          conditionalPanel(
            condition = glue("input['{sep_id}'] == true"),
            # Text Entry Element
            tagList(
              # Label
              tags$p(
                HTML(
                  glue("<strong> Enter features <br> 
                       (specific to {tolower(plot_label)}): </strong>")
                  )
                ),
              
              # Selectize input for separate features
              div(
                style =
                  "vertical-align: top; margin-bottom: 0px;",
                selectizeInput(
                  inputId = ns("separate_features"),
                  multiple = TRUE,
                  label = NULL,
                  choices = NULL,
                  selected = NULL,
                  # Add remove button to inputs
                  options =
                    list(
                      'plugins' = list('remove_button'),
                      'create' = FALSE
                    )
                  )
                )
              )
            )
          )
      } else NULL,
      
      # UI for download button
      if (download_button == TRUE){
        div(
          class = "plot-download-button",
          dropdownButton(
            inputId = ns("download"),
            status = "info",
            up = TRUE,
            label = "",
            size = "sm",
            icon = icon("download"),
            # Dropdown content
            div(
              selectInput(
                inputId = ns("file_type"),
                label = "Select File Type",
                choices = c(".png" = "png", ".svg" = "svg"),
                selected = "png"
              ),
              downloadButton(
                outputId = ns("confirm_download"),
                label = "Download",
                class = "confirm-download",
                icon = NULL
              )
            )
          )
        )
      } else NULL
    )
    
  } else if (ui_component == "plot"){
    # UI for plot output 
    # Conditional UI: displays if the "make *" switch for the plot is turned on 
    # in the plots tab
    uiOutput(outputId = ns("plot_output_ui"))
  }
}

#' plot_module_server
#'
#' Server function to run alongside the corresponding UI function
#'
#' @param id The module ID used for namespacing. This should match the ID used
#' in the UI components for the plot module.
#' @param object The Seurat object to be used for plotting. It may be a subset 
#' or the full object.
#' @param plot_switch Switch in the plots tab specifying whether the user wishes 
#' to see the plot created in this server function 
#' @param plot_label The name for this plot type. Will display as entered 
#' in the app
#' @param n_cells_original The number of cells in the original object. This is
#' set in the main server function each time a new object is loaded.
#' @param features_entered 
#' @param manual_dimensions Creates a server instance for specifying manual 
#' dimensions if TRUE. This should be set to TRUE if manual_dimensions is also 
#' TRUE in the UI
#' @param plot_type The type of plot to create from the selected options.
#' @param valid_features 
#' @param xlim_orig The x limits of the dimplot of the full Seurat object 
#' (before a subset is created). This only applies to dimplots and feature plots.
#' @param ylim_orig The y axis limits of the dimplot of the full Seurat object.
#' @param palette A palette of colors currently selected by the user, to be used 
#' on the plot.
#' @param assay_config The assay section of the config file loaded in the main
#' server function
#' @param separate_features_server A boolean giving whether server code to 
#' process separate features (features specific to the plot created by this 
#' module) should be ran. This should be TRUE for all plots where the user can 
#' enter features that apply just to that plot.
#'
#' @return
#' @export
#'
#' @examples
plot_module_server <- function(id,
                               object, #Reactive
                               plot_switch, #Reactive
                               plot_label, #Non-reactive
                               n_cells_original, #Reactive
                               features_entered = NULL, #Reactive 
                               manual_dimensions = TRUE, #Non-reactive
                               plot_type = c("dimplot",
                                             "feature",
                                             "violin",
                                             "dot",
                                             "scatter"), #Non-reactive
                               valid_features = NULL, #Reactive
                               xlim_orig = NULL, #Reactive
                               ylim_orig = NULL, #Reactive
                               palette = NULL, # Reactive
                               metadata_config = NULL,
                               #Currently only needed for feature plots
                               assay_config = NULL,
                               separate_features_server = FALSE #Non-reactive
                               ){
  moduleServer(id,
               function(input,output,session){
                 # Server namespace function: for dynamic UI
                 ns <- session$ns
                 
                 # 1. Manual Dimensions Module Server --------------------------
                 if (manual_dimensions == TRUE){
                   manual_dim <- manual_dimensions_server(id = "manual_dim")
                   }
                 
                 # 2. Fill Feature Selection (Scatterplots Only) ---------------
                 if (plot_type == "scatter"){
                   if (!is.reactive(valid_features)){
                     warning("For scatterplots, valid_features must be specified as a reactive to plot_module_server.")
                   }
                   
                   # Upon module initialization, set up feature inputs 
                   # for the scatterplot 
                   # Use for loop to avoid duplication of code
                   for (input_id in c("scatter_1", "scatter_2")){
                     # Update selectize inputs for scatter_1 and scatter_2
                     updateSelectizeInput(
                       session,
                       # Do not namespace IDs in update* functions
                       inputId = input_id, 
                       # Use list of valid features generated at startup
                       choices = valid_features(), 
                       # Select none by default
                       selected = character(0),
                       server = TRUE
                       )
                     }
                   }
                 
                 # 3. Title Settings Menu --------------------------------------
                 ## 3.1. Update title settings menu ####
                 # If the menu is present, add the "custom" option if it applies
                 observe({
                   # Adds reactive dependency on input$title_settings
                   if (isTruthy(input$title_settings)){
                     print("Custom title conditional")
                     # Variable below is set to TRUE when custom titles 
                     # are possible
                     enable_custom = FALSE
                     
                     # Conditional tree to determine if 
                     # custom titles are possible
                     if (plot_type == "dimplot"){
                       # DimPlots: custom titles are always possible
                       enable_custom = TRUE
                     } else if (plot_type == "feature"){
                       # Feature plots: custom titles are currently possible 
                       # when only one feature is entered.
                       if (length(features_entered()) == 1){
                         enable_custom = TRUE
                       }
                     }
                     
                     # Update input based on conditionals above
                     updateSelectInput(
                       session,
                       inputId = "title_settings",
                       choices = 
                         if (enable_custom == TRUE){
                           c("Default" = "default", 
                             "Custom" = "custom", 
                             "None" = "none")
                         } else {
                           c("Default" = "default", 
                             "None" = "none")
                         },
                       selected =
                         # Preserve selection as long as it is still valid
                         # ("custom" is the only choice that is potentially 
                         # invalid)
                         if (
                           (enable_custom == TRUE) | 
                           (enable_custom == FALSE & 
                            input$title_settings != "custom")
                         )
                           input$title_settings
                     )
                   }
                   })
                 
                 ## 3.2. Show/hide custom title input ####
                 # Depends on state of input$title_settings and the current
                 # group_by category (for DimPlots)
                 observe({
                   if (isTruthy(input$title_settings)){
                     if (input$title_settings == "custom"){
                       showElement(id = "custom_title_div", anim = TRUE)
                     } else {
                       hideElement(id = "custom_title_div", anim = TRUE)
                     }
                   }
                 })
                 
                 ## 3.3. Set custom title input based on metadata ####
                 observe(
                   label = glue("{id}: 3.3 Set custom title menu"),
                   {
                     if (isTruthy(input$title_settings)){
                       if (input$title_settings == "custom"){
                         #Label to use as default value for searchInput
                         if (!is.null(plot_selections$group_by())){
                           config_label <-
                             metadata_config()[[
                               plot_selections$group_by()]]$label
                           
                           # Define initial value of text entry
                           initial_value <-
                             # Use label if it exists
                             if (!is.null(config_label)){
                               config_label
                             } else {
                               # If a label is not defined, use the name
                               # of the group by category
                               plot_selections$group_by()
                             }
                         } else {
                           initial_value <- ""
                         }
                         
                         # Update input with initial value
                         updateSearchInput(
                           session,
                           inputId = "custom_title",
                           value = initial_value,
                           trigger = TRUE
                         )
                       }
                     }
                   })
                 
                 # if (isTruthy(input$title_settings)){
                 #   observeEvent(
                 #     c(input$title_settings,
                 #       plot_selections$group_by()),
                 #     ignoreNULL = FALSE,
                 #     ignoreInit = TRUE,
                 #     label = glue("{id}: 3.3 Reset Custom title menu"),
                 #     {
                 #       print("Title settings observer")
                 #       if (input$title_settings == "custom"){
                 #         #Label to use as default value for searchInput
                 #         print("!is.null(plot_selections$group_by())")
                 #         print(!is.null(plot_selections$group_by()))
                 #         if (!is.null(plot_selections$group_by())){
                 #           
                 #           config_label <-
                 #             metadata_config()[[plot_selections$group_by()]]$label
                 # 
                 #           # Define initial value of text entry
                 #           initial_value <-
                 #             # Use label if it exists
                 #             if (!is.null(config_label)){
                 #               config_label
                 #             } else {
                 #               # If a label is not defined, use the name
                 #               # of the group by category
                 #               plot_selections$group_by()
                 #             }
                 #         } else {
                 #           initial_value <- ""
                 #         }
                 # 
                 #         print("Update search input")
                 #         print("Value to update with")
                 #         print(initial_value)
                 #         print("Value of input$custom title before updating")
                 #         print(isolate(input$custom_title))
                 #         # Update input with initial value
                 #         updateSearchInput(
                 #           session,
                 #           inputId = "custom_title",
                 #           value = initial_value,
                 #           trigger = TRUE
                 #         )
                 #         print("Value of input$custom title after updating")
                 #         print(isolate(input$custom_title))
                 #       }
                 #     })
                 # }
                 
                 ## 3.4. Manual reset of custom title input ####
                 observeEvent(
                   input$custom_title_reset,
                   label = glue("{id}: 3.4. Custom Title Reset"),
                   {
                     # Obtain initial value for metadata category,
                     # then update the input with the value.
                     #Label to use as default value for searchInput
                     if (!is.null(plot_selections$group_by())){
                       config_label <-
                         metadata_config()[[
                           plot_selections$group_by()]]$label
                       
                       # Define initial value of text entry
                       initial_value <-
                         # Use label if it exists
                         if (!is.null(config_label)){
                           config_label
                         } else {
                           # If a label is not defined, use the name
                           # of the group by category
                           plot_selections$group_by()
                         }
                     } else {
                       initial_value <- ""
                     }
                     
                     # Update input
                     updateSearchInput(
                       session,
                       inputId = "custom_title",
                       value = initial_value,
                       trigger = TRUE
                     )
                   })
                 
                 # 4. Record plot_selections -----------------------------------
                 #list of reactives for storing selected inputs
                 plot_selections <- 
                   list(
                     # Group_by
                     `group_by` = reactive({
                       if (!is.null(input$group_by)){
                         input$group_by
                         } else NULL
                       }),
                     
                     # Split_by
                     `split_by` = reactive({
                       if (!is.null(input$split_by)){
                         input$split_by
                         } else NULL
                     }),
                     
                     # Reduction
                     `reduction` = reactive({
                       if (!is.null(input$reduction)){
                         input$reduction
                       } else NULL
                     }),
                     
                     # Number of columns in multi-panel plot
                     # Special conditional used (in some cases ncol will still 
                     # have a value in situations where it shouldn't, and this
                     # can affect the plots)
                     `ncol` = 
                       # Conditions under which ncol should be defined vary 
                       # based on plot type. Separate reactive expressions are
                       # created based on the plot type used for the module.
                       if (plot_type == "dimplot"){
                         # Condition to record ncol for UMAP
                         # Equal to conditions where there are multiple panels
                         reactive({
                           if (input$split_by != "none"){
                             input$ncol
                             } else NULL
                           })
                         } else if (plot_type == "violin"){
                           # Condition to record ncol for violin plot
                           # Equal to conditions where there are multiple panels
                           reactive({
                             if (length(features_entered()) > 1){
                               input$ncol
                               } else NULL
                             })
                           },
                     
                     # Order cells by expression
                     `order` = reactive({
                       if ("order" %in% isolate(names(input))){
                         input$order
                       } else NULL
                     }),
                     
                     # Colors for min and max values
                     `min_color` = reactive({
                       # Pass color values if the custom colors checkbox 
                       # exists and is selected
                       if (isTruthy(input$custom_colors)){
                         input$min_color
                       } else NULL
                     }),
                     
                     `max_color` = reactive({
                       if (isTruthy(input$custom_colors)){
                         input$max_color
                       } else NULL
                     }),
                     
                     # Include legend
                     `legend` = reactive({
                       if ("legend" %in% isolate(names(input))){
                         input$legend
                       } else NULL
                     }),
                     
                     # Label groups
                     `label` = reactive({
                       if ("label" %in% isolate(names(input))){
                         input$label
                       } else NULL
                     }),
                     
                     # Original axes limits
                     # isolate(names(input)) does not work for testing the
                     # existence of the original axes limits checkbox. 
                     # input$original_limits will be either TRUE or FALSE if
                     # the checkbox exists, or NULL if it does not. A suitable
                     # existence test is !is.null(input$original_limits)
                     `limits` = reactive({
                       if (!is.null(input$original_limits)){
                         input$original_limits
                       } else NULL
                     }),
                     
                     # X-axis feature for scatterplots
                     `scatter_1` = reactive({
                       if (plot_type == "scatter"){
                         input$scatter_1
                       } else NULL
                     }),
                     
                     # Y-axis feature for scatterplots
                     `scatter_2` = reactive({
                       if (plot_type == "scatter"){
                         input$scatter_2
                       } else NULL
                     }),
                     
                     # Remove Title 
                     `display_coeff` = reactive({
                       if (!is.null(input$display_coeff)){
                         input$display_coeff
                       } else NULL
                     })
                   )
                 
                 # 5. Determine if a subset has been used  ----------------------
                 # This variable will be a boolean used in downstream 
                 # computations
                 is_subset <- eventReactive(
                   label = glue("{plot_label}: Test if Object is a Subset"),
                   object(),
                   ignoreNULL = FALSE,
                   {
                     # Throw an error if the subset does not exist or is NULL
                     validate(
                       need(
                         object(),
                         message = "error: subset is NULL"
                         )
                       )

                     # Compute number of cells in subset
                     n_cells_subset <-
                       object() |>
                       Cells() |>
                       length()

                     # Test if the number of cells in the subset differs from
                     # the number of cells in the original object. If this
                     # conditional is TRUE, then the object read is a subset
                     n_cells_original() != n_cells_subset
                 })
                 
                 # 6. Conditional UI -------------------------------------------
                 ## 6.1. ncol slider ####
                 # Conditions under which ncol slider appear differ based on 
                 # plot type
                 if (plot_type == "dimplot"){
                   # UMAP plots: appears when split_by != "none"
                   ncol_slider <-
                     eventReactive(
                       c(plot_selections$split_by(),
                         object()
                       ),
                       label = glue("{plot_label}: Make ncol Slider"),
                       ignoreNULL = TRUE,
                       {
                         #Do not render when split_by is "none"
                         if (plot_selections$split_by() == "none"){
                           NULL
                         } else {
                           # Number of panels: used to set bounds of ncol slider
                           # Number of panels is equal to the number of unique
                           # values for the chosen metadata category
                           n_panel <-
                             object()@meta.data[[plot_selections$split_by()]] |>
                             unique() |>
                             length()
                           
                           # Determine default value for ncol
                           # For less than four panels, this is equal to the
                           # number of panels.
                           if (n_panel < 4){
                             default_col <- n_panel
                             # For 4 or more panels, the default value is 2
                           } else {
                             default_col <- 2
                           }
                           
                           # Create slider input
                           sliderInput(
                             inputId = ns("ncol"),
                             label = "Number of Columns: ",
                             min = 1,
                             # Max value: equal to the number of levels
                             # in the given variable
                             max = n_panel,
                             # Only allow integer values
                             step = 1,
                             ticks = FALSE,
                             value = default_col
                           )
                         } # End else
                       })
                   
                 } else if (plot_type == "violin"){
                   # Violin plots: appears when multiple features are entered
                   ncol_slider <-
                     eventReactive(
                       features_entered(),
                       label = glue("{plot_label}: Make ncol Slider"),
                       ignoreNULL = TRUE,
                       {
                         # Number of panels equals number of features for violin 
                         # plots. Slider is needed only when more than one 
                         # feature is entered
                         if(length(features_entered()) > 1){
                           # Default number of columns: equal to the number of
                           # panels if there are less than four, otherwise equal 
                           # to two
                           if (length(features_entered()) < 4){
                             default_col <- length(features_entered())
                           } else {
                             default_col <- 2
                           }
                           
                           # Create/update slider input
                           sliderInput(
                             inputId = ns("ncol"),
                             label = "Number of columns: ",
                             min = 1,
                             #Max value: equal to the number of features entered
                             max = length(features_entered()),
                             #Only allow integer values
                             step = 1, 
                             ticks = FALSE,
                             value = default_col
                             )
                         } else NULL
                       })
                 }
    
                 ## 6.2. Checkbox to Specify Original Axis Limits ####
                 limits_checkbox <-
                   reactive(
                     label = glue("{plot_label}: Limits UI"),
                     {
                     # Checkbox will only appear when a subset is selected.
                     # The presence of a subset will be tested by observing
                     # the number of cells in the subset
                     if (is_subset()) {
                       checkboxInput(
                         inputId = ns("original_limits"),
                         label = "Use Original Axes Limits",
                         value = FALSE
                         )
                     } else {
                       # Display nothing when the number of cells are equal
                       # between the subset and the full dataset
                       NULL
                       }

                       })
                 
                 ## 6.3. Dynamic UI for plot output ####
                 # UI display depends on the plot type and whether the plot 
                 # has a separate features option
                 plot_output_ui <-
                   reactive(
                     label = glue("{plot_label}: Plot Output UI"),
                     {
                       # Only render UI when the switch corresponding to the 
                       # current plot is enabled
                       req(plot_switch())
                       
                       # input_error: set to TRUE or FALSE based on conditionals
                       input_error = FALSE
                       # message: message to display to user when 
                       # inputs are incorrect
                       message = NULL
                      
                       ### 5.3.1. Test for correct inputs #### 
                       # UI displayed depends on if inputs have been entered
                       # correctly. "Correct" inputs depend on plot type
                       # input_error is set to TRUE or FALSE based on input 
                       # conditions. 
                       if (plot_type == "dimplot"){
                         # All input scenarios are "correct" for dimplots
                         input_error = FALSE
                         
                       } else if (plot_type == "scatter") {
                         # Scatterplots: both features must be entered for a
                         # "correct" input
                         if (
                           !isTruthy(input$scatter_1) | 
                           !isTruthy(input$scatter_2)
                         ){
                           # Scatterplot and neither features defined
                           input_error = TRUE
                           message = 
                             glue(
                               'Please enter two features under 
                               "{tolower(plot_label)} Options" to 
                                view {tolower(plot_label)}.'
                               )
                             
                         # } else if (
                         #   isTruthy(input$scatter_1) & 
                         #   !isTruthy(input$scatter_2)
                         # ){
                         #   # Scatterplot and only the y-axis feature is defined
                         #   input_error = TRUE
                         #   message = 
                         #     glue(
                         #       'Please enter a feature for the y-axis
                         #       to view {tolower(plot_label)}.'
                         #       )
                         # } else if (
                         #   !isTruthy(input$scatter_1) & 
                         #   isTruthy(input$scatter_2)
                         # ){
                         #   # Scatterplot and only the x-axis feature is defined
                         #   input_error = TRUE
                         #   message = 
                         #     glue(
                         #      'Please enter a feature for the x-axis
                         #      to view {tolower(plot_label)}.'
                         #      )
                         } else if (
                           isTruthy(input$scatter_1) & 
                           isTruthy(input$scatter_2)
                         ){
                           # Scatterplot and both features are defined
                           input_error = FALSE
                         }
                         # End scatterplot conditionals
                         
                       } else if (plot_type %in% c("feature", "violin")) {
                         # Feature and violin plots: "Correct" input depends 
                         # on state of features_entered()
                         if (isTruthy(features_entered())){
                           # When features are entered, inputs are "correct"
                           input_error = FALSE
                         } else {
                           # When features are not entered, display message
                           # directing user to enter input
                           input_error = TRUE
                           message = 
                             glue(
                               "Please enter a feature to view 
                               {tolower(plot_label)}."
                               )
                           }
                         } else if (plot_type == "dot"){
                           # Dot plots: "correct" input depends on state of 
                           # features_entered() and input$use_separate_features
                           if (
                             input$use_separate_features == FALSE &
                             length(features_entered()) == 0
                           ){
                             input_error = TRUE
                             message = 
                               glue(
                                 "Please enter a feature to view 
                                 {tolower(plot_label)}."
                                 )
                           } else if (
                             input$use_separate_features == TRUE &
                             length(input$separate_features) == 0
                           ){
                             input_error = TRUE
                             message = 
                               glue(
                                 'Please specify at least one 
                               {tolower(plot_label)} specific feature to view 
                               plot. To use the same features as for other 
                               plots, please uncheck "use separate features".'
                               )
                           } else {
                             input_error = FALSE
                             }
                         }
                       
                       ### 6.3.2. Display message or plot based on inputs ####
                       if (input_error == TRUE){
                         # Message: use message defined in conditonal structure
                         ui <- 
                           tags$h3(
                             message, 
                             style="margin-bottom: 10em;"
                             )
                       } else if (input_error == FALSE){
                         # Plot: UI depends on whether manual 
                         # dimensions are specified
                         if (
                           (!is.null(manual_dim$width())) && 
                           (!is.null(manual_dim$height()))
                         ){
                           # If manual dimensions are specified, pass the 
                           # values specified by the user to plotOutput
                           ui <- 
                             plotOutput(
                               outputId = ns("plot"),
                               width = manual_dim$width(),
                               height = manual_dim$height()
                               )
                         } else {
                           # Otherwise, call plotOutput without defining 
                           # width and height
                           ui <-
                             plotOutput(
                               outputId = ns("plot")
                               )
                         }
                           
                       }
                       
                       # Return UI to plot_output_ui
                       ui
                     })
                 
                 ## 6.4. Render Dynamic UI ####
                 output$ncol_slider <- 
                   renderUI({
                     ncol_slider()
                     })

                 output$limits_checkbox <- 
                   renderUI({
                     limits_checkbox()
                     })
                 
                 # output$custom_colors_ui <-
                 #   renderUI({
                 #     custom_colors_ui()
                 #   })
                 
                 output$plot_output_ui <- 
                   renderUI({
                     plot_output_ui()
                     })

                 # 7. Separate Features Entry: Dynamic Update ------------------
                 # Observers for separate features only update for server 
                 # instances where features_entered
                 if (separate_features_server ==  TRUE){
                   ## 7.1 Update Separate Features in Background ####
                   # Before the checkbox to select separate features is checked, 
                   # update the text entry in the background so it is synced
                   # when it appears after the box is checked. 
                   # This process ensures the features are instantly available in
                   # the separate features text box when the checkbox is checked
                   observeEvent(
                     features_entered(),
                     label = 
                       glue("{plot_label}: Update Separate 
                            Features Text Entry"),
                     {
                       if (input$use_separate_features == FALSE){
                         updateSelectizeInput(
                           session,
                           inputId = "separate_features",
                           choices = valid_features(),
                           selected = features_entered(),
                           server = TRUE
                           )
                         }
                     })
                   
                   ## 7.2 Reset Separate Features Upon Checkbox Toggle ####
                   # If the "use separate features" checkbox is toggled and the
                   # features entered in the separate features text entry differ
                   # from the general features selected, update the separate
                   # features text entry to match the general features. 
                   # This is necessary to ensure that selections for general 
                   # features appear in the separate features text entry in the 
                   # event that the user checks the box, unchecks it, changes
                   # general features, and checks the box again.
                   observeEvent(
                     input$use_separate_features,
                     label = 
                       glue("{plot_label}: Set Separate Features Input"),
                     {
                       if (
                         # Check if general feature and separate feature text 
                         # entries are not in sync
                         !setequal(
                           features_entered(), 
                           input$separate_features
                           )
                         ){
                         updateSelectizeInput(
                           session,
                           inputId = "separate_features",
                           choices = valid_features(),
                           selected = features_entered(),
                           server = TRUE
                         )
                       }
                     })
                 }
                 
                 # 8. Plot -----------------------------------------------------
                 ## 8.1 Define Features to use ####
                 # For all plots except dimplot, scatterplot 
                 # Uses either the general feature entry (features_entered()),
                 # or the separate features text entry depending on whether
                 # separate features are used in the module and whether the 
                 # checkbox to use them is selected.
                 if (!plot_type %in% c("dimplot", "scatter")){
                   features <-
                     reactive(
                       label = glue("{plot_label}: Features for Plot"),            
                       {
                         # Test for separate_features_server first
                         # input$use_separate_features does not exist if 
                         # separate_features_server == FALSE
                         if (separate_features_server == TRUE){
                           # If separate features are used in this module,
                           # input them if the user checks the box to use
                           # them 
                           if(input$use_separate_features == TRUE){
                             #Use separate features
                             input$separate_features
                           } else if (input$use_separate_features == FALSE){
                             #Use general features
                             features_entered()
                           }
                         } else if (separate_features_server == FALSE){
                           # Otherwise, pass features_entered() to 
                           # shiny_dot() (general features)
                           features_entered()
                         }
                       })
                 }
                 
                 ## 8.2 Construct Plot ####
                 # Plot created based on the type specified when this server 
                 # function is called
                 if (plot_type == "dimplot"){
                   plot <- 
                     reactive(
                       label = glue("{plot_label}: Create Plot"),
                       {
                       # Create a UMAP plot using shiny_umap()
                       shiny_umap(
                         object = object,
                         group_by = plot_selections$group_by,
                         split_by = plot_selections$split_by,
                         show_label = plot_selections$label,
                         show_legend = plot_selections$legend,
                         ncol = plot_selections$ncol,
                         is_subset = is_subset,
                         original_limits = plot_selections$limits,
                         xlim_orig = xlim_orig,
                         ylim_orig = ylim_orig,
                         show_title =
                           # show_title controls how NULL values for plot_title
                           # are interpereted (NULL will remove the label by 
                           # default, but plot_title will be NULL if a label is
                           # not set in the config file (want the default title
                           # to be used in this case))
                           if (input$title_settings == "none"){
                             FALSE
                           } else TRUE,
                         plot_title =
                           # Human-readable plot title
                           # Depends on title_settings
                           if (isTruthy(input$title_settings)){
                             if (input$title_settings == "default"){
                               # Default behavior is to use the `label` property
                               # for the category in the config file
                               metadata_config()[[plot_selections$group_by()]]$label
                             } else if (input$title_settings == "custom"){
                               # Use the custom title entered by the user
                               # in this case
                               input$custom_title
                             } else if (input$title_settings == "none"){
                               # NULL is passed to the title argument,
                               # removing it
                               NULL
                             }
                           } else{
                             # Use default behavior if selection menu
                             # does not exist
                             metadata_config()[[plot_selections$group_by()]]$label
                           },
                         reduction = plot_selections$reduction,
                         palette = palette
                         )
                     })
                   
                 } else if (plot_type == "feature") {
                   plot <- 
                     reactive(
                       label = glue("{plot_label}: Create Plot"),
                       {
                         # Reactives to pass to 
                         print("split_by")
                         print(plot_selections$split_by())
                         print("order")
                         print(plot_selections$order())
                         print("show_label")
                         print(plot_selections$label())
                         print("show_legend")
                         print(plot_selections$legend())
                         print("is_subset")
                         print(is_subset())
                         print("original_limits")
                         print(plot_selections$limits())
                         print("xlim_orig")
                         print(xlim_orig())
                         print("ylim_orig")
                         print(ylim_orig())
                         print("palette")
                         if (
                           isTruthy(plot_selections$min_color()) & 
                           isTruthy(plot_selections$max_color())
                         ){
                           c(plot_selections$min_color(), 
                             plot_selections$max_color()) |> print()
                         } else if (isTruthy(palette())) {
                           palette() |> print()
                         } else print("NULL")
                         print("reduction")
                         print(plot_selections$reduction())
                         
                         # Feature plot using arguments relevant to 
                         # shiny_feature()
                         shiny_feature(
                           object = object(),
                           features_entered = features_entered(), 
                           assay_config = assay_config(),
                           split_by = plot_selections$split_by(),
                           order = plot_selections$order(),
                           #show_label = plot_selections$label(),
                           show_legend = plot_selections$legend(),
                           is_subset = is_subset(),
                           original_limits = plot_selections$limits(),
                           xlim_orig = xlim_orig(),
                           ylim_orig = ylim_orig(),
                           palette = 
                             # Use custom colors if defined; if not, use the 
                             # palette if defined; if not, pass NULL to use 
                             # Seurat defaults.
                             if (
                               isTruthy(plot_selections$min_color()) & 
                               isTruthy(plot_selections$max_color())
                               ){
                               c(plot_selections$min_color(), 
                                 plot_selections$max_color())
                             } else if (isTruthy(palette())) {
                               palette()
                             } else NULL,
                           reduction = plot_selections$reduction()
                           )
                         })
                   
                 } else if (plot_type == "violin") {
                   plot <- 
                     reactive(
                       label = glue("{plot_label}: Create Plot"),
                       {
                         # Violin plot using arguments relevant to shiny_vln()
                         shiny_vln(
                           object = object,
                           features_entered = features_entered, 
                           group_by = plot_selections$group_by,
                           split_by = plot_selections$split_by,
                           show_legend = plot_selections$legend,
                           ncol = plot_selections$ncol,
                           assay_config = assay_config,
                           palette = palette
                           )
                       })
                   
                 } else if (plot_type == "dot") {
                   # Dot plot using arguments relevant to shiny_dot()
                   plot <- 
                     reactive(
                       label = glue("{plot_label}: Create Plot"),
                       {
                         shiny_dot(
                           object = object,
                           # Features argument: uses value returned by reactive
                           features = features,
                           # use_separate_features = 
                           #   reactive({input$use_separate_features}),
                           # separate_features = 
                           #   reactive({input$separate_features}),
                           group_by = plot_selections$group_by,
                           show_legend = plot_selections$legend,
                           palette = palette
                           )
                         })
                 } else if (plot_type == "scatter"){
                   # Scatterplot using relevant inputs
                   plot <- 
                     reactive(
                       label = glue("{plot_label}: Create Plot"),
                       {
                         shiny_scatter(
                           object = object,
                           feature_1 = plot_selections$scatter_1,
                           feature_2 = plot_selections$scatter_2,
                           group_by = plot_selections$group_by,
                           show_legend = plot_selections$legend,
                           display_coeff = plot_selections$display_coeff,
                           palette = palette
                           )
                       })
                   }
                 
                 ## 8.3. Render plot ####
                 # Height and width arguments are left undefined
                 # If undefined, they will use the values from plotOutput, which
                 # respond to the manual dimensions inputs.
                 output$plot <- renderPlot({
                   if (plot_type %in% c("dimplot", "violin")){
                     validate(
                       need(
                         input$group_by != input$split_by, 
                         message = 
                           glue(
                             'Invalid selections for {plot_label}: "Group By" and "Split By" must be different.'
                             )
                         )
                     )
                   }
                   
                   plot()
                 })
                 
                 # 9. Download Handler -----------------------------------------
                 output$confirm_download <- 
                   downloadHandler(
                     # Filename: takes the label and replaces 
                     # spaces with underscores
                     filename = function(){
                       if (input$file_type == "png"){
                         glue("{sub(' ','_',plot_label)}.png")
                       } else if (input$file_type == "svg"){
                         glue("{sub(' ','_',plot_label)}.svg")
                         }
                       },
                     content = function(file){
                       # Conditional: manual dimensions are specified
                       if (
                         (!is.null(manual_dim$width())) && 
                         (!is.null(manual_dim$height()))
                       ){
                         # If manual dimensions are specified, apply them to 
                         # height and width arguments
                         ggsave(
                           file,
                           plot = plot(),
                           # Either 'png' or 'svg'
                           device = input$file_type,
                           width = manual_dim$width(),
                           height = manual_dim$height(),
                           # Set dpi to 72 so proportions of downloaded plot 
                           # match the plot in the app
                           dpi = 72,
                           units = "px",
                           # Set background color to white (background is 
                           # transparent on some plots)
                           bg="#FFFFFF"
                         )
                       } else {
                         ggsave(
                           file,
                           plot = plot(),
                           # Either 'png' or 'svg'
                           device = input$file_type,
                           # Set background color to white (background is 
                           # transparent on some plots)
                           bg ="#FFFFFF"
                         )
                       }
                     },#End content function
                     contentType = 
                       # contentType: uses MIME types
                       if (input$file_type == "png"){
                         "image/png"
                         } else if (input$file_type == "svg"){
                           "image/svg+xml"
                           }
                   ) #End downloadHandler function
                 
                 })
  }