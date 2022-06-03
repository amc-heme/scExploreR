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
                           # Plot UI component functions called here
                           #...
                           ui_component = c("options", "plot"),
                           meta_choices = NULL,
                           plot_label = "",
                           reductions = NULL,
                           # TEMP: conditionals vertically aligned
                           # for multi-cursor editing
                           reductions_menu =            FALSE,
                           scatterplot_ui =             FALSE,
                           group_by =                   FALSE,
                           split_by =                   FALSE,
                           title_menu =                 FALSE,
                           legend_title_menu =          FALSE,
                           ncol_slider =                FALSE,
                           share_scale_checkbox =       FALSE,
                           color_by_feature_checkbox =  FALSE,
                           super_title_menu =           FALSE,
                           group_by_label =             FALSE,
                           sort_groups_menu =           FALSE,
                           order_checkbox =             FALSE,
                           label_checkbox =             FALSE,
                           legend_checkbox =            FALSE,
                           display_coeff =              FALSE,
                           limits_checkbox =            FALSE,
                           custom_colors =              FALSE,
                           manual_dimensions =          FALSE,
                           separate_features =          FALSE,
                           download_button =            FALSE,
                           # Default values for inputs
                           label_default =              TRUE#,
                           ){
  # Namespace function: prevents conflicts with IDs defined in other modules 
  ns <- NS(id)
  
  if (ui_component == "options"){
    # UI for plot options ####
    # Elements are added to tagList if specified when calling the module ui 
    # Attempted to use ifelse() for this; ifelse() did not print Shiny tags 
    # properly and was unable to process NULL
    tagList(
      # Add menus if their corresponding arguments are TRUE 
      # Two-feature entry menu specific to scatterplot
      ## Scatterplot Features #### 
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
      
      ## Group by menu ####
      if (group_by == TRUE){
        # Choices for group by selection: should exclude "none"
        group_by_choices <- meta_choices()[!meta_choices() %in% "none"]
        
        # If TRUE, add element
        selectInput(
          inputId = ns("group_by"), 
          label = "Metadata to Group By",
          # Can select all options except "none"
          choices = group_by_choices, 
          # First option selected by default 
          selected = group_by_choices[1]
          )
        # Do not add element if FALSE
      } else NULL,
      
      ## Split by menu ####
      if (split_by == TRUE){
        selectInput(
          inputId = ns("split_by"), 
          label = "Metadata to Split By",
          # Use vector of included metadata category names from the config file
          choices = meta_choices(),  
          # "none" selected by default
          selected = "none"
         )
      } else NULL,
      
      ## Reductions menu ####
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
      
      ## Title selection and custom title ####
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
          # Input for a single custom title
          # UI is hidden with shinyjs and made visible when "custom" is selected 
          hidden(
            div(
              id = ns("custom_title_div"),
              class = "compact-options-container",
              tags$b("Enter Custom Title"),
              # tags$p(
              #   sytle = "font-size: 0.9em;",
              #   "(Press enter to update)"
              #   ),
              # searchInput(
              #   inputId = ns("custom_title_searchinput"),
              #   label = NULL,
              #   # Default value depends on current group-by category and is 
              #   # set server-side 
              #   value = "",
              #   # Search button: must be defined for updateSearchInput 
              #   # to trigger properly, but will be hidden 
              #   btnSearch = icon("sync"),
              #   # Icon for reset button
              #   btnReset = icon("redo-alt"),
              #   # resetValue: set to NULL to disable automatic resetting
              #   # resetting will occur manually via the server function
              #   resetValue = NULL
              # ),
              textInput(
                inputId = ns("custom_title"),
                label = NULL,
                value = ""
                ),
              div(
                id = ns("buttons"),
                actionButton(
                  inputId = ns("custom_title_reset"),
                  label = "Reset",
                  icon = icon("redo-alt"),
                  class = "button-ghost compact-button"
                ),
                actionButton(
                  inputId = ns("custom_title_update"),
                  label = "Update",
                  icon = icon("sync"),
                  class = "button-primary compact-button"
                )
              )
            )
          ),
          # UI for input of multiple custom titles (applies when a feature 
          # plot is created with multiple split by groups)
          hidden(
            div(
              id = ns("custom_title_multi_div"),
              multi_text_input_ui(
                id = ns("custom_title_multi"),
                label = "Enter Custom Titles for Each Panel:"
                )
              )
            )
          )
      },
      
      ## Legend Title (feature plots) ####
      if (legend_title_menu == TRUE){
        div(
          style = "margin-top: 10px;",
          id = ns("legend_title_div"),
          selectInput(
            inputId = ns("legend_title"),
            label = "Display Options for Legend Title",
            choices = 
              c("Feature Name" = "feature", 
                "Expression" = "assay_score",
                "No Title" = "none"),
            selected = "default"
            )
          )
        } else NULL,
      
      ## Group by for Feature Plot Labels ####
      # Feature plot only. Can't be used with group_by_menu.
      if (group_by_label == TRUE){
        if (group_by == TRUE){
          # Throw error if group_by menu is created (menu uses same ID)
          stop("`group_by_label` can't be TRUE when `group_by` is TRUE.")
        } else {
          # Choices for group by selection: should exclude "none"
          group_by_choices <- meta_choices()[!meta_choices() %in% "none"]

          hidden(
            selectInput(
              inputId = ns("group_by"),
              label = "Metadata for Labeling Groups",
              # Can select all options except "none"
              choices = group_by_choices,
              # First option selected by default
              selected = group_by_choices[1]
            )
          )
          }
      } else NULL,
      
      ## Order of groups (dot plots) ####
      if (sort_groups_menu == TRUE){
        selectInput(
          inputId = ns("sort_groups"),
          label = "Order of Groups on plot",
          # Can select all options except "none"
          choices = 
            c("Ascending" = "ascending",
              "Descending" = "descending")
          )
        } else NULL,
      
      ## Number of columns ####
      if (ncol_slider == TRUE){
        #Dynamic UI (appears when split_by != "none")
        uiOutput(outputId = ns("ncol_slider"))
      } else NULL,
      
      ## Display a title above multi-panel plots ####
      # (feature plots only)
      if (super_title_menu == TRUE){
        hidden(
          checkboxInput(
            inputId = ns("super_title"),
            label = "Display Feature Name Above Panels",
            value = TRUE
            )
          )
        } else NULL,
      
      ## Share Scales Checkbox ####
      # (feature plots with multiple features and no split.by groups)
      if (share_scale_checkbox == TRUE){
        hidden(
          checkboxInput(
            inputId = ns("share_scale"),
            label = "Share Scale Between Features",
            # Value is FALSE by default
            value = FALSE
            )
        )
      } else NULL,
      
      ## Color by Feature Checkbox ####
      if (color_by_feature_checkbox == TRUE){
        hidden(
          checkboxInput(
            inputId = ns("color_by_feature"),
            label = "Use Separate Colors for Features",
            # Value is FALSE by default
            value = FALSE
          )
        )
      } else NULL,
      
      ## Oder cells by expression (feature plots only) ####
      if (order_checkbox == TRUE){
        checkboxInput(
          inputId = ns("order"),
          label = "Order Cells by Expression",
          # Order should be FALSE by default
          value = FALSE
          )
      } else NULL,
      
      ## Add/remove labels ####
      if (label_checkbox == TRUE){
        checkboxInput(
          inputId = ns("label"),
          label = "Label Groups",
          # Default value is TRUE
          value = label_default
          )
      } else NULL,
      
      ## Add or remove Legend ####
      if (legend_checkbox == TRUE){
        checkboxInput(
          inputId = ns("legend"),
          label = "Include Legend",
          value = TRUE
          )
      } else NULL,
      
      ## Checkbox to remove title from plot ####
      if (display_coeff == TRUE){
        checkboxInput(
          inputId = ns("display_coeff"),
          label = "Show Pearson Coefficient",
          value = TRUE
          )
      } else NULL,
      
      ## Checkbox to specify original axes limits ####
      if (limits_checkbox == TRUE){
        # Dynamic UI: displays when a subset is selected
        uiOutput(outputId = ns("limits_checkbox"))
      } else NULL,
      
      ## Checkbox for custom colors on Feature plot ####
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
      
      ## UI for user control of plot dimensions ####
      if (manual_dimensions == TRUE){
        #If TRUE, call module for manual adjustment of plot dimensions
        manual_dimensions_ui(id = ns("manual_dim"))
      } else NULL,

      ## Separate Features UI #### 
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
      
      ## UI for download button ####
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
      } else NULL,
      
      # Add additional UI components requested by the user
      #...
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
#' @param lim_orig A list with the original x- and y- axes limits for each 
#' reuction enabled for the current object. The list is generated at app 
#' startup.
#' @param palette A palette of colors currently selected by the user, to be used
#' on the plot.
#' @param assay_config The assay section of the config file loaded in the main
#' server function
#' @param separate_features_server A boolean giving whether server code to 
#' process separate features (features specific to the plot created by this 
#' module) should be ran. This should be TRUE for all plots where the user can 
#' enter features that apply just to that plot.
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
                               lim_orig = lim_orig, #Reactive
                               palette = NULL, #Reactive
                               metadata_config = NULL,
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
                 # Title settings and custom titles are currently only enabled 
                 # for DimPlots and Feature Plots
                 if (plot_type %in% c("dimplot", "feature")){
                   # Reactive trigger for updating single custom title input
                   update_title_single <- makeReactiveTrigger()
                   
                   ## 3.1. Update title settings menu ####
                   # If the menu is present, add the "custom" option
                   # if it applies
                   # menu_type: stores whether a single- or multiple- entry 
                   # menu may be used
                   menu_type = reactiveVal(NULL)
                   
                   observe({
                     # Adds reactive dependency on input$title_settings
                     if (isTruthy(input$title_settings)){
                       print("Custom title conditional")
                       # Variable below is set to TRUE when custom titles 
                       # are possible
                       enable_custom = FALSE
                       menu_type("none")
                       
                       # Conditional tree to determine if 
                       # custom titles are possible
                       if (plot_type == "dimplot"){
                         # DimPlots: custom titles are always possible
                         enable_custom = TRUE
                         # Plot uses a single 
                         menu_type("single")
                       } else if (plot_type == "feature"){
                         # Feature plots: custom titles can be used when one 
                         # feature is entered, or when multiple features have
                         # been entered without a split_by selection
                         if (length(features_entered()) == 1){
                           enable_custom = TRUE
                           # Single- or multiple- entry menu: depends 
                           # on selected split_by setting
                           if (plot_selections$split_by() == "none"){
                             menu_type("single")
                           } else {
                             # Multiple title entry menu is used when a split_by
                             # setting is specified
                             menu_type("multiple")
                           }
                         } else if (length(features_entered()) > 1){
                           if (plot_selections$split_by() == "none"){
                             enable_custom = TRUE
                             menu_type("multiple")
                           }
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
                       if (input$title_settings == "custom" & 
                           menu_type() == "single"){
                         showElement(
                           id = "custom_title_div", 
                           anim = TRUE
                         )
                       } else {
                         hideElement(
                           id = "custom_title_div", 
                           anim = TRUE
                         )
                       }
                     }
                   })
                   
                   observe({
                     if (isTruthy(input$title_settings)){
                       if (input$title_settings == "custom" & 
                           menu_type() == "multiple"){
                         showElement(
                           id = "custom_title_multi_div", 
                           anim = TRUE
                         )
                       } else {
                         hideElement(
                           id = "custom_title_multi_div", 
                           anim = TRUE
                         )
                       }
                     } 
                   })
                   
                   ## 3.3. Set default custom title input ####
                   # Triggers when the custom title UI is shown, and when 
                   # selections that influence the default title change
                   observeEvent(
                     # EventExpr depends on plot type
                     if (plot_type == "dimplot"){
                       c(input$title_settings,
                         object(),
                         plot_selections$group_by()
                         )
                     } else if (plot_type == "feature"){
                       c(input$title_settings,
                         object(),
                         features_entered()
                       )
                     },
                     label = glue("{id}: 3.3 Set custom title menu"),
                     {
                       if (isTruthy(input$title_settings)){
                         if (input$title_settings == "custom"){
                           # Compute initial value for custom title input
                           initial_value <- 
                             initial_title(
                               plot_type = plot_type,
                               group_by = plot_selections$group_by(),
                               metadata_config = metadata_config(),
                               features_entered = 
                                 if (plot_type == "feature"){
                                   features_entered()
                                 } else NULL
                             )
                           
                           print(glue("ID: {id}"))
                           print("Updating input in response to startup")
                           print("Default value:")
                           print(initial_value)
                           
                           # Update input with initial value
                           updateTextInput(
                             session,
                             inputId = "custom_title",
                             value = initial_value
                           )
                           
                           # Keeps downstream reactives from processing until
                           # updateTextInput has completed (update occurrs 
                           # after all reactives have executed)
                           freezeReactiveValue(input, "custom_title")
                           
                           # Trigger processing of custom title input (3.5.)
                           #update_title_single$trigger()
                           # Programmatically click update button 
                           # to process value
                           shinyjs::click("custom_title_update")
                           
                           # updateSearchInput(
                           #   session,
                           #   inputId = "custom_title",
                           #   value = initial_value,
                           #   trigger = TRUE
                           # )
                         }
                       }
                     })
                   
                   # observe({
                   #   print("input$custom_title has changed. New value:")
                   #   print(input$custom_title)
                   # })
                   
                   ## 3.4. Manual reset of custom title input ####
                   observeEvent(
                     input$custom_title_reset,
                     label = glue("{id}: 3.4. Custom Title Reset"),
                     {
                       # Determine default value to reset to 
                       initial_value <- 
                         initial_title(
                           plot_type = plot_type,
                           group_by = plot_selections$group_by(),
                           metadata_config = metadata_config(),
                           features_entered = 
                             if (plot_type == "feature"){
                               features_entered()
                               } else NULL
                         )
                       
                       print("Updating input in response to reset button")
                       
                       # Update input
                       updateTextInput(
                         session,
                         inputId = "custom_title",
                         value = initial_value
                       )
                       
                       # Programmatically click update button to process value
                       shinyjs::click("custom_title_update")
                       
                       # updateSearchInput(
                       #   session,
                       #   inputId = "custom_title",
                       #   value = initial_value,
                       #   trigger = TRUE
                       # )
                     })
                   
                   ## 3.5 Process Input of Single Custom Title ####
                   # Input is updated when the "update" button is pressed.
                   custom_title_single <-
                     eventReactive(
                       # Respond to programmatic trigger
                       c(input$custom_title_update,
                         update_title_single$depend()),
                         
                         # Make title available before the update button 
                         # is pressed, and update with the default value 
                         # in response to changes in the group_by category 
                         # or the object)
                         #object(),
                         #plot_selections$group_by()),
                       ignoreNULL = FALSE,
                       ignoreInit = TRUE,
                       {
                         print("3.5. Update custom_title_single eventReactive")
                         print("Value of input$custom_title")
                         print(input$custom_title)
                         
                         # Pass value of custom_title if defined, otherwise pass
                         # the default value
                         default_title <-
                            initial_title(
                              plot_type = plot_type,
                              group_by = plot_selections$group_by(),
                              metadata_config = metadata_config(),
                              features_entered = features_entered()
                              )

                         if (isTruthy(input$custom_title)){
                           return(input$custom_title)
                         } else {
                           return(default_title)
                         }
                         
                         # ALTERNATE
                         # Require input$custom_title to be defined
                         #req(input$custom_title)
                         #input$custom_title
                       })
                   
                   # observe({
                   #   print("Value of custom_title_single:")
                   #   print(custom_title_single())
                   # })
                   
                   ## 3.6. Server for multiple custom title input ####
                   # Currently enabled for feature plots only 
                   if (plot_type == "feature"){
                     # Define default values for inputs
                     default_titles <-
                       reactive({
                         if (!is.null(features_entered())){
                           if (
                             length(features_entered()) == 1 &
                             plot_selections$split_by() != "none"
                             ){
                             # Single-feature plots with a split_by selection:
                             # use split_by groups
                             req(plot_selections$split_by())
                             
                             default_titles <-
                               object()@meta.data[[
                                 plot_selections$split_by()]] |> 
                               unique() |> 
                               str_sort(numeric = TRUE)
                             
                             return(default_titles)
                           } else if (
                             length(features_entered()) > 1 &
                             plot_selections$split_by() == "none"
                             ){
                             # Multi-feature plots with no split_by selection:
                             # use features entered for defaults
                             default_titles <- features_entered()
                             return(default_titles)
                           }
                         }
                       })
                     
                     # Pass default titles to multi-text input module
                     custom_vector <-
                       multi_text_input_server(
                         id = "custom_title_multi",
                         default_vector = default_titles
                         )
                     
                     # custom_vector <-
                     #   custom_title_multi_server(
                     #     id = "custom_title_multi",
                     #     object = object,
                     #     split_by = plot_selections$split_by,
                     #     label_header = "Original Value",
                     #     input_header = tagList("New Value")
                     #     )
                   }
                   
                   ## 3.7. Custom title input to feature plots ####
                   # Must pass either single or multiple custom title output 
                   # depending on the current number of panels on the feature 
                   # plot, or NULL 
                   if (plot_type == "feature"){
                     feature_plot_custom_title <-
                       reactive({
                         # Only continue if menu_type is not NULL (when custom
                         # titles is selected)
                         req(menu_type())
                         
                         if (input$title_settings == "custom"){
                           # Determine current menu structure
                           if (menu_type() == "single"){
                             # For single_panel plots (menu_type() == "single"), 
                             # use the single text entry
                             custom_title_single()
                           } else if (menu_type() == "multiple"){
                             req(custom_vector())
                             # For multi-panel plots, use the vector of custom
                             # title inputs from the custom_title_multi module
                             custom_vector()
                           }
                         } else {
                           # Pass NULL if custom titles are not specified
                           NULL
                         }
                       })
                   }
                 }
                 
                 # 4. Show/Hide Feature Plot-Specific Menus --------------------
                 ## 4.1. Super Title Menu ####
                 if (plot_type == "feature"){
                   observe({
                     # Show is set to TRUE when conditionals below are satisfied
                     show <- FALSE
                     
                     # Super-titles (titles above all panels) currently only 
                     # possible for multi-panel feature plots
                     if (!is.null(features_entered()) & 
                         !is.null(plot_selections$split_by())){
                       if (length(features_entered()) == 1 &
                           plot_selections$split_by() != "none"){
                         show <- TRUE
                       }
                     }
                     
                     if (show == TRUE){
                       showElement(
                         id = "super_title"
                         )
                     } else {
                       hideElement(
                         id = "super_title"
                       )
                     }
                   })
                 }
                 
                 ## 4.2. Legend Title Options ####
                 if (plot_type == "feature"){
                   observe({
                     # show is set to TRUE when conditions are met
                     show <- FALSE
                     # ID of legend title container
                     elem_id <- "legend_title_div"
                     # Default value of input: changed based on conditionals
                     default_value = "feature"
                     
                     # Appears on single-feature plots, and multi-feature plots
                     # with no split by category
                     if (!is.null(features_entered()) &
                         !is.null(plot_selections$split_by())){
                       if (length(features_entered()) == 1){
                         show <- TRUE
                       } else if (length(features_entered()) > 1 &
                                  plot_selections$split_by() == "none"){
                         show <- TRUE
                         # In this case, set default value of input to "none"
                         default_value = "none"
                       }
                     }
                     
                     if (show == TRUE){
                       showElement(
                         id = elem_id
                       )
                     } else {
                       hideElement(
                         id = elem_id
                       )
                     }
                     
                     # Update input with default value
                     updateSelectInput(
                       session,
                       inputId = "legend_title",
                       selected = default_value
                     )
                   })
                 }
                 
                 ## 4.3 Share Scale Between Features ####
                 # Show when conditions below are met
                 # Errors will result unless plot_type is restricted to feature 
                 # (DimPlots don't process features_entered())
                 if (plot_type == "feature"){
                   observe({
                     # show is set to TRUE when conditions below are met
                     show <- FALSE
                     elem_id <- "share_scale"
                     
                     if (!is.null(features_entered()) &
                         !is.null(plot_selections$split_by())){
                       # Currently visible when multiple features are chosen, 
                       # and no split by category is selected
                       if (length(features_entered()) > 1){
                         if (plot_selections$split_by() == "none"){
                           show <- TRUE
                         }
                       }
                     }
                     
                     # Show/hide based on outcome above
                     if (show == TRUE){
                       showElement(
                         id = elem_id
                       )
                     } else {
                       hideElement(
                         id = elem_id
                       )
                     }
                   })
                 }
                 
                 ### 4.3.1. Update legend_title based on share_scale ####
                 # If share_scale == TRUE, "feature" cannot be used for
                 # share_scale. This option must be removed in this case.
                 if (plot_type == "feature"){
                   observe({
                     if (!is.null(plot_selections$share_scale())){
                       updateSelectInput(
                         session,
                         inputId = "legend_title",
                         # Choices depend on state of share_scale
                         choices = 
                           if (plot_selections$share_scale() == TRUE){
                             c("Expression" = "assay_score",
                               "No Title" = "none")
                           } else {
                             c("Feature Name" = "feature", 
                               "Expression" = "assay_score",
                               "No Title" = "none")
                           },
                         selected = 
                           # Code preserves current selection 
                           # if it is still valid
                           if (
                             # All choices are valid when share_scale == FALSE
                             plot_selections$share_scale() == FALSE |
                             # When share_scale == TRUE, current value can be
                             # preserved if it is not "feature" 
                             (plot_selections$share_scale() == TRUE &
                              input$legend_title != "feature")
                             ){
                             input$legend_title
                             }
                         )
                       }
                   })
                 }
                 
                 ## 4.4. Color by Feature ####
                 # Available for feature plots when multiple 
                 # features are entered and split_by is "none"
                 if (plot_type == "feature"){
                   observe({
                     # show is set to TRUE when conditions below are met
                     show <- FALSE
                     elem_id <- "color_by_feature"
                     
                     if (!is.null(features_entered()) &
                         !is.null(plot_selections$split_by())){
                       if (length(features_entered()) > 1 &
                           plot_selections$split_by() == "none"){
                         # Color by feature cannot be used when
                         # share_scale is TRUE
                         if (plot_selections$share_scale() == TRUE){
                           show <- FALSE
                         } else {
                           show <- TRUE
                           }
                         }
                     }
                     
                     # Show/hide based on outcome above
                     if (show == TRUE){
                       showElement(
                         id = elem_id
                       )
                     } else {
                       hideElement(
                         id = elem_id
                       )
                       
                       # Set value to FALSE when hiding to avoid interference
                       # with share_scale setting
                       updateCheckboxInput(
                         session,
                         inputId = elem_id,
                         value = FALSE
                       )
                     }
                   })
                 }
                 
                 ## 4.5. Group by menu for labels ####
                 if (plot_type == "feature"){
                   observe({
                     # Show menu when "label groups" is selected
                     show <- FALSE
                     elem_id <- "group_by"
                     
                     if (isTruthy(plot_selections$label())){
                       show <- TRUE
                     }
                     
                     # Show/hide based on outcome above
                     if (show == TRUE){
                       showElement(
                         id = elem_id
                       )
                     } else {
                       hideElement(
                         id = elem_id
                       )
                       }
                   })
                 }
                 
                 # 5. Record plot_selections -----------------------------------
                 # list of reactives for storing selected inputs
                 plot_selections <- 
                   list(
                     # Group_by
                     `group_by` = 
                       reactive({
                         if (!is.null(input$group_by)){
                           input$group_by
                           } else NULL
                         }),
                     
                     # Split_by
                     `split_by` = 
                       reactive({
                         if (!is.null(input$split_by)){
                           input$split_by
                           } else NULL
                         }),
                     
                     # Reduction
                     `reduction` =
                       reactive({
                         if (!is.null(input$reduction)){
                           input$reduction
                           } else NULL
                         }),
                     
                     # Re-order groups (dot plots)
                     `sort_groups` = 
                       reactive({
                         if (isTruthy(input$sort_groups)){
                           input$sort_groups
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
                         reactive({
                           if (!is.null(input$split_by)){
                             if (input$split_by != "none"){
                               input$ncol
                               } else NULL
                             } else NULL
                           })
                         } else if (plot_type == "feature"){
                           # ncol applies when multiple panels are created on 
                           # feature plot
                           reactive({
                             req(features_entered())

                             # Process ncol for single feature plots when
                             # split_by is set, or for multiple feature plots
                             # when a split_by category is not set
                             if (
                               (length(features_entered()) == 1 &
                                input$split_by != "none") |
                               (length(features_entered()) > 1 &
                                input$split_by == "none")
                             ){
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
                     
                     # Title above all panels (feature plots)
                     `super_title` = 
                       reactive({
                         if (!is.null(input$super_title)){
                           input$super_title
                         } else NULL
                       }),
                     
                     # Share scale between features
                     `share_scale` = 
                       reactive({
                         if (!is.null(input$share_scale)){
                           input$share_scale
                         } else NULL
                       }),
                     
                     # Use Different Colors for Features
                     `color_by_feature` = 
                       reactive({
                         if (!is.null(input$color_by_feature)){
                           input$color_by_feature
                         } else NULL
                       }),
                     
                     # Order cells by expression
                     `order` = 
                       reactive({
                         if (!is.null(input$order)){
                           input$order
                           } else NULL
                         }),
                     
                     # Options for legend title
                     `legend_title` = 
                       reactive({
                         if (isTruthy(input$legend_title)){
                           input$legend_title
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
                     
                     # Super title checkbox (feature plots with more than one
                     # split_by category)
                     
                     
                     # Include legend
                     `legend` = reactive({
                       if (!is.null(input$legend)){
                         input$legend
                       } else NULL
                     }),
                     
                     # Label groups
                     `label` = reactive({
                       if (!is.null(input$label)){
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
                 
                 # 6. Determine if a subset has been used  ---------------------
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
                 
                 # 7. Conditional UI -------------------------------------------
                 ## 7.1. ncol slider ####
                 # Conditions under which ncol slider appear differ based on 
                 # plot type
                 if (plot_type == "dimplot"){
                   # UMAP plots: appears when split_by != "none"
                   ncol_slider <-
                     eventReactive(
                       c(plot_selections$split_by(),
                         object()),
                       label = glue("{plot_label}: Make ncol Slider"),
                       ignoreNULL = TRUE,
                       {
                         #Do not render when split_by is "none"
                         if (plot_selections$split_by() == "none"){
                           NULL
                         } else {
                           # Define min, max, and default values for slider
                           ncol_settings <-
                             ncol_settings(
                               object = object(),
                               rule = "split_by",
                               split_by = plot_selections$split_by()
                             )
                           
                           # Create slider input
                           sliderInput(
                             inputId = ns("ncol"),
                             label = "Number of Columns: ",
                             min = ncol_settings[1],
                             # Max value: equal to the number of levels
                             # in the given variable
                             max = ncol_settings[2],
                             # Only allow integer values
                             step = 1,
                             ticks = FALSE,
                             # Default value (3rd element)
                             value = ncol_settings[3]
                           )
                         } # End else
                       })
                   
                 } else if (plot_type == "feature"){
                   # Feature plots: appears when only one feature is entered
                   # and split by is defined
                   ncol_slider <-
                     eventReactive(
                       c(plot_selections$split_by(),
                         features_entered(),
                         object()),
                       label = glue("{plot_label}: Make ncol slider"),
                       ignoreNULL = TRUE,
                       {
                         if (!is.null(features_entered())){
                           # The ncol slider may be used for single feature 
                           # plots when a split by selection is made
                           condition_single <- 
                             length(features_entered()) == 1 & 
                             plot_selections$split_by() != "none"
                           
                           # For multi-feature plots, ncol is only available 
                           # when there *is not* a split by selection
                           condition_multiple <- 
                             length(features_entered()) > 1 & 
                             plot_selections$split_by() == "none"
                          
                           if (condition_single | condition_multiple){
                             # Define min, max, and default values for slider
                             if (condition_single){
                               # For single-feature plots, ncol limits are 
                               # based on the number of split_by groups
                               ncol_settings <-
                                 ncol_settings(
                                   object = object(),
                                   rule = "split_by",
                                   split_by = plot_selections$split_by()
                                 )
                             } else if (condition_multiple){
                               # For multi-feature plots, ncol limits are based
                               # on the number of features entered
                               ncol_settings <-
                                 ncol_settings(
                                   object = object(),
                                   rule = "features",
                                   features_entered = features_entered()
                                 )
                             }
                             
                             # Create slider input
                             sliderInput(
                               inputId = ns("ncol"),
                               label = "Number of Columns: ",
                               min = ncol_settings[1],
                               # Max value: equal to the number of levels
                               # in the given variable
                               max = ncol_settings[2],
                               # Only allow integer values
                               step = 1,
                               ticks = FALSE,
                               # Default value (3rd element)
                               value = ncol_settings[3]
                               )
                             } else NULL # No slider when conditions not 
                           }
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
                           # Define min, max, and default values for slider
                           # (depends on number of features entered)
                           ncol_settings <-
                             ncol_settings(
                               object = object(),
                               rule = "features",
                               features_entered = features_entered()
                             )
                           
                           # Create/update slider input
                           sliderInput(
                             inputId = ns("ncol"),
                             label = "Number of Columns: ",
                             min = ncol_settings[1],
                             # Max value: equal to the number of levels
                             # in the given variable
                             max = ncol_settings[2],
                             # Only allow integer values
                             step = 1,
                             ticks = FALSE,
                             # Default value (3rd element)
                             value = ncol_settings[3]
                             )
                         } else NULL
                       })
                 }
    
                 ## 7.2. Checkbox to Specify Original Axis Limits ####
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
                 
                 ## 7.3. Dynamic UI for plot output ####
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
                      
                       ### 6.3.1. Test for correct inputs #### 
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
                       
                       ### 7.3.2. Display message or plot based on inputs ####
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
                 
                 ## 7.4. Render Dynamic UI ####
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

                 # 8. Separate Features Entry: Dynamic Update ------------------
                 # Observers for separate features only update for server 
                 # instances where features_entered
                 if (separate_features_server ==  TRUE){
                   ## 8.1. Update Separate Features in Background ####
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
                   
                   ## 8.2. Reset Separate Features Upon Checkbox Toggle ####
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
                 
                 # 9. Plot -----------------------------------------------------
                 ## 9.1 Define Features to use ####
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
                 
                 ## 9.2. Construct Plot ####
                 # Plot created based on the type specified when this server 
                 # function is called
                 if (plot_type == "dimplot"){
                   ### 9.2.1. DimPlot ####
                   plot <- 
                     reactive(
                       label = glue("{plot_label}: Create Plot"),
                       {
                         # Create a UMAP plot using shiny_umap()
                         shiny_umap(
                           object = object(),
                           group_by = plot_selections$group_by(),
                           split_by = plot_selections$split_by(),
                           show_label = plot_selections$label(),
                           show_legend = plot_selections$legend(),
                           ncol = plot_selections$ncol(),
                           is_subset = is_subset(),
                           original_limits = plot_selections$limits(),
                           # Original x- and y- axis limits: use the values 
                           # for the currently selected reduction
                           xlim_orig = 
                             lim_orig()[[
                               plot_selections$reduction()]]$xlim_orig,
                           ylim_orig = 
                             lim_orig()[[
                               plot_selections$reduction()]]$ylim_orig,
                           show_title =
                             # show_title controls how NULL values for 
                             # plot_title are interpreted (NULL will remove the 
                             # label by default, but plot_title will be NULL if 
                             # a label is not set in the config file (want the 
                             # default title to be used in this case))
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
                                 metadata_config()[[
                                   plot_selections$group_by()]]$label
                               } else if (input$title_settings == "custom"){
                                 # Use the custom title entered by the user
                                 # in this case
                                 custom_title_single()
                               } else if (input$title_settings == "none"){
                                 # NULL is passed to the title argument,
                                 # removing it
                                 NULL
                               }
                             } else{
                               # Use default behavior if selection menu
                               # does not exist
                               metadata_config()[[
                                 plot_selections$group_by()]]$label
                             },
                           reduction = plot_selections$reduction(),
                           palette = palette()
                           )
                     })
                   
                 } else if (plot_type == "feature") {
                   ### 9.2.2. Feature Plot ####
                   plot <- 
                     reactive(
                       label = glue("{plot_label}: Create Plot"),
                       {
                         # Feature plot using arguments relevant to 
                         # shiny_feature()
                         shiny_feature(
                           object = object(),
                           features_entered = features(), 
                           assay_config = assay_config(),
                           # Group by: influences label placement
                           group_by = plot_selections$group_by(),
                           split_by = plot_selections$split_by(),
                           order = plot_selections$order(),
                           show_label = plot_selections$label(),
                           show_legend = plot_selections$legend(),
                           ncol = plot_selections$ncol(),
                           super_title = plot_selections$super_title(),
                           share_scale = plot_selections$share_scale(),
                           color_by_feature = 
                             plot_selections$color_by_feature(),
                           is_subset = is_subset(),
                           legend_title = plot_selections$legend_title(),
                           original_limits = plot_selections$limits(),
                           # Original x- and y- axis limits: use the values 
                           # for the currently selected reduction
                           xlim_orig = 
                             lim_orig()[[plot_selections$reduction()]]$xlim_orig,
                           ylim_orig = 
                             lim_orig()[[plot_selections$reduction()]]$ylim_orig,
                           palette = 
                             if (plot_selections$color_by_feature() == FALSE){
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
                                 palette$continuous_palette()
                               } else NULL
                             } else {
                               palette$categorical_palette()
                             },
                           reduction = plot_selections$reduction(),
                           show_title = 
                             if (input$title_settings == "none") FALSE else TRUE,
                           # From 3.5.
                           custom_titles = feature_plot_custom_title()
                           )
                         })
                   
                 } else if (plot_type == "violin") {
                   ### 9.2.3 Violin Plot ####
                   plot <- 
                     reactive(
                       label = glue("{plot_label}: Create Plot"),
                       {
                         # Violin plot using arguments relevant to shiny_vln()
                         shiny_vln(
                           object = object(),
                           features_entered = features(), 
                           group_by = plot_selections$group_by(),
                           split_by = plot_selections$split_by(),
                           show_legend = plot_selections$legend(),
                           ncol = plot_selections$ncol(),
                           assay_config = assay_config(),
                           palette = palette(),
                           sort_groups = plot_selections$sort_groups()
                           )
                       })
                   
                 } else if (plot_type == "dot") {
                   ### 9.2.4. Dot Plot ####
                   # Dot plot using arguments relevant to shiny_dot()
                   plot <- 
                     reactive(
                       label = glue("{plot_label}: Create Plot"),
                       {
                         shiny_dot(
                           object = object(),
                           # Features argument: uses value returned by reactive
                           features = features(),
                           # use_separate_features = 
                           #   reactive({input$use_separate_features}),
                           # separate_features = 
                           #   reactive({input$separate_features}),
                           group_by = plot_selections$group_by(),
                           show_legend = plot_selections$legend(),
                           palette = palette(),
                           sort_groups = plot_selections$sort_groups()
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
                 
                 ## 9.3. Render plot ####
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
                 
                 # 10. Download Handler -----------------------------------------
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