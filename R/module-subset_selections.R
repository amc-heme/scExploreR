#' Subset Selections Module UI
#'
#' The subset selections module displays menus for selecting the current subset.
#' It is currently used in all three tabs of the app.
#'
#' @param id id to use for the module.id to use for module namespacing. 
#' UI-server module pairs must use the same id. 
#' @param unique_metadata A list of the unique metadata values for each of the 
#' metadata categories listed in the config file. This is generated in the 
#' main server function.
#' @param metadata_config The metadata section of the config file. This does not 
#' need to be specified if the config list is stored as "config" in the global 
#' environment.
#' @param auto_dictionary_path Path to the temporary file used for storing the
#' auto-generated object dictionary (created in the main app)
#' @param string_subsetting_href URL of the string subsetting vignette.
#'
#' @return UI code for the subset selections module.
#' 
#' @noRd
subset_selections_ui <- 
  function(
    id,
    unique_metadata,
    metadata_config,
    auto_dictionary_path,
    string_subsetting_href
    ){
    # Namespace function: prevents conflicts with IDs defined in other modules 
    ns <- NS(id)
    
    tagList(
      tags$b(
        "Filters Chosen for Subset:"
      ),
      # New menu UI
      uiOutput(
        outputId = ns("filters_applied")
      ),
      actionButton(
        inputId = ns("add_filter"),
        label = "Add Filter",
        icon = icon("plus"),
        # show-on-idle: button will be hidden while a filter is 
        # being created/edited
        class = "button-primary compact-button show-on-idle"#,
        #style = "width: 25%;"
      ),
      # Reset all filters button: shown when at least one filter is present
      hidden(
        actionButton(
          inputId = ns("reset_all_filters"),
          label = "Remove All Filters",
          icon = icon("times-circle"),
          class = "button-ghost compact-button show-on-idle"
        )
      ),
      hidden(
        # UI for adding/editing a subsetting filter
        div(
          class = "show-on-add show-on-edit compact-options-container",
          selectInput(
            inputId = ns("filter_type"),
            label = "Choose Filter Type:",
            choices = 
              c("Select Type" = "",
                "Categorical Metadata" = "categorical",
                "Feature Expression" = "numeric"
              )
          ),
          # Categorical metadata filter menu
          hidden(
            div(
              id = ns("categorical_filter_ui"),
              tags$b(
                "Categorical Filter", 
                class = "center large"
              ),
              # Select menu to choose metadata for filtering (add mode)
              div(
                class = "show-on-add",
                selectInput(
                  inputId = ns("categorical_var"),
                  label = "Choose categorical metadata:",
                  # Choices are updated in server
                  choices = NULL
                )
              ),
              # Edit mode: display metadata being edited
              div(
                class = "show-on-edit",
                tags$b("Metadata variable:"),
                textOutput(
                  outputId = ns("edit_categorical_var")
                )
              ),
              # Picker input to choose values of metadata variable to include
              pickerInput(
                inputId = ns("categorical_values"),
                label = "Choose Values to Include:",
                # Choices are variable-dependent and are computed 
                # in the server function
                choices = NULL,
                # selected: nothing selected by default. A placeholder will
                # appear instead
                selected = character(0),
                multiple = TRUE,
                # Options for pickerInput
                options = 
                  list(
                    # Display number of items selected instead of their names
                    # when more than 5 values are selected
                    "selected-text-format" = "count > 5",
                    # Define max options to show at a time to keep menu
                    # from being cut off
                    "size" = 7,
                    # Add "select all" and "deselect all" buttons
                    "actions-box" = TRUE,
                    # Label for "deselect all" button
                    "deselectAllText" = "Remove filter",
                    # Define placeholder
                    "none-selected-text" = "Select values"
                  )
              )
            ),
            # UI for choosing a numeric filter
            div(
              id = ns("numeric_filter_ui"),
              tags$b(
                "Numeric Filter", 
                class = "center large"
              ),
              # Feature for numeric filere
              selectizeInput(
                inputId = ns("numeric_feature"),
                label = "Enter a feature to apply filter to:",
                choices = NULL,
                selected = character(0),
                options = 
                  list(
                    "placeholder" = "Enter feature",
                    "maxItems" = 1,
                    "plugins" = list("remove_button"),
                    "create" = FALSE
                    )
                ),
              # "Mode" of numeric filter (<, >, range)
              # Hidden until a feature is entered
              hidden(
                shinyWidgets::radioGroupButtons(
                  inputId = ns("numeric_mode"),
                  label = "Filter mode:",
                  choices = 
                    c(">" = "greater_than",
                      "<" = "less_than",
                      "Range" = "range"
                    ),
                  justified = TRUE,
                  status = "radio-primary"
                )
              ),
              # UI for choosing threshold for filtering
              # Interface is automatically hidden until a feature is selected
              threshold_picker_ui(
                id = ns("filter_threshold"),
                plot_height = "150px",
                instruction_panel = TRUE
                )
              )
            ),
          actionButton(
            inputId = ns("filter_cancel"),
            label = "Cancel",
            #icon = icon("redo-alt"),
            class = "button-ghost compact-button"
            ),
          actionButton(
            inputId = ns("filter_confirm"),
            label = "Confirm Filter",
            class = "button-primary compact-button"
            )
          )
        ),
      # Checkbox to enable advanced subsetting
      checkboxInput(
        inputId = ns("string_subsetting"), 
        label = "String subsetting (advanced mode)"
        ),
      # Advanced string subsetting
      # Hidden initially, then shown using shinyjs when the checkbox above is 
      # selected (ensures inputs are avilable upon initiation of module, so
      # updateSelectizeInput will work)
      shinyjs::hidden(
        div(
          id = ns("string_subsetting_ui"),
          # Feature Search Dropdown
          dropdownButton(
            inputId = ns("feature_search_dropup"),
            label = "",
            size = "sm",
            status = "feature_search_btn",
            icon = icon("search"), 
            up = TRUE,
            # Dropdown content
            div(
              class = "feature_statistics_container",
              onclick="event.stopPropagation()",
              selectizeInput(
                inputId = ns("search_feature"),
                label = "Feature Search:",
                choices = NULL,
                selected = character(0),
                options = 
                  list(
                    # Add remove button to inputs
                    'plugins' = list('remove_button'),
                    # Do not allow user to input features not
                    # in the list of options
                    'create' = FALSE,
                    'placeholder' = "enter feature"
                  )
              ),
              # Feature statistics: summary stats based on feature entered
              uiOutput(ns("feature_statistics")),
              tags$a(
                "View Object Metadata",
                href = auto_dictionary_path,
                target = "_blank",
                rel = "noopener noreferrer",
                class = "blue_hover underline underline-hover left",
                # Decrease padding around link
                style = "padding: 3px 6px; margin-top: 15px;"
              ),
              tags$a(
                "String Subsetting Help",
                href = string_subsetting_href,
                target = "_blank",
                rel = "noopener noreferrer",
                class = "blue_hover underline underline-hover left",
                # Decrease padding around link; do not add margin above
                style = "padding: 3px 6px;"
              )
            )
          ),
          
          # textAreaInput for entering string subset
          textAreaInput(
            inputId = ns("adv_subset"),
            label = NULL,
            width = "100%",
            rows = 4,
            resize = "vertical"
            )
          )
        )
      )
    }

#' Subset Selections Module 
#'
#' @param id id to use for module namespacing. UI-server module pairs must 
#' use the same id.
#' @param object The Seurat object for which subset criteria are to be defined.
#' @param unique_metadata A list of the unique metadata values for each of the 
#' metadata categories listed in the config file. This is generated in the main 
#' server function upon startup and object change. 
#' @param metadata_config The metadata section of the config file, loaded in the
#' main server function.
#' @param assay_config The assays section of the config file, loaded in the
#' main server function.
#' @param meta_categories A vector of the metadata categories (variables) 
#' included in the config file for the current object. This is computed in the 
#' main server function.
#' @param valid_features A list of the available features in the current object,
#' used to populate the choices of the feature selection menu when groups based
#' on feature thresholds are requested.
#' @param hide_menu (optional) a string or character vector giving 
#' the name(s) of metadata categories for which to hide menus in the
#' subset selection interface.
#' 
#' @return Server code for the subset selections module.
#' 
#' @noRd
#'
subset_selections_server <- function(id,
                                     object,
                                     unique_metadata,
                                     metadata_config,
                                     assay_config,
                                     meta_categories,
                                     valid_features,
                                     hide_menu = NULL
                                     ){
  # Initialize module 
  moduleServer(
    id,
    # Server function for module: uses a separate set of input/output variables 
    function(input,output,session){
      # Server namespace function: used for UI elements rendered in server
      ns <- session$ns
      
      # module_data: reactiveValues object used to store the ridgeplot 
      # interactively generated in 6.3.
      module_data <- reactiveValues()
      
      # Initial states for filter menus
      # State of the entire filter interface (idle by default, and can be 
      # set to "add" or "edit" via button presses)
      module_data$filter_menu_state <- "idle"
      # Type of filter being added/edited (categorical or numeric)
      module_data$filter_type <- "none"
      # Empty list for storing subset filters as they're created
      module_data$filters <- list()
      
      # editing data: reactiveValues list storing data related to a filter 
      # being edited
      # Data must be stored in a reactiveValues object while editing to prevent 
      # information loss. 
      editing_data <- reactiveValues()
      
      # Numeric feature mode
      editing_data$mode <- NULL
      # Categorical metadata variable, or feature, being edited 
      editing_data$var <- NULL
      # Value(s) of var 
      editing_data$value <- NULL
      # Special variable for numeric filters (threshold picker module responds 
      # to this value)
      editing_data$numeric_filter_value <- NULL
      # Index of filter being edited in module_data$filters
      editing_data$target_row <- NULL
      # Label to display to user
      editing_data$label <- NULL
      
      # Confirm button for adding filters should start in a disabled state
      shinyjs::disable(
        id = "filter_confirm"
      )
      
      # 1. Filter menu UI ------------------------------------------------------
      ## 1.1. Respond to "add filter" button ####
      observe({
        req(input$add_filter)
        input$add_filter
        
        # Set state to "add"
        module_data$filter_menu_state <- "add"
      })
      
      ## 1.2. Show/hide menus based on add/edit/idle states ####
      observe({
        # jQuery selectors for classes that show elements based on state
        add_selector <- "[class *= 'show-on-add']"
        edit_selector <- "[class *= 'show-on-edit']"
        idle_selector <- "[class *= 'show-on-idle']"
        
        # showElement statements are put last on purpose
        # This allows elements with show-on-add OR show-on-edit classes to be 
        # visible (if the hideElement statements come after the showElement 
        # statement, the container with both classes will be shown, then hidden)
        if (module_data$filter_menu_state == "add"){
          hideElement(
            selector = edit_selector
          )
          
          hideElement(
            selector = idle_selector
          )
          
          showElement(
            selector = add_selector
          )
        } else if (module_data$filter_menu_state == "edit"){
          hideElement(
            selector = add_selector
          )
          
          hideElement(
            selector = idle_selector
          )
          
          showElement(
            selector = edit_selector
          )
        } else if (module_data$filter_menu_state == "idle") {
          showElement(
            selector = idle_selector
          )
          
          hideElement(
            selector = add_selector
          )
          hideElement(
            selector = edit_selector
          )
        }
      })
      
      ## 1.3. Record state from filter type selection menu ####
      observe({
        if (isTruthy(input$filter_type)){
          if (input$filter_type == "categorical"){
            module_data$filter_type <- "categorical"
          } else if (input$filter_type == "numeric"){
            module_data$filter_type <- "numeric"
          }
          
          # Also reset the filter type input back to the blank state with
          # placeholder
          updateSelectInput(
            session = session,
            inputId = "filter_type",
            selected = ""
            )
        } 
      })
      
      ## 1.4. Show/hide UI based on type of filter being edited ####
      observe({
        type_selection_ui_id <- "filter_type"
        categorical_ui_id <- "categorical_filter_ui"
        numeric_ui_id <- "numeric_filter_ui"
        
        if (module_data$filter_type == "categorical"){
          print("show categorical filter interface")
          # Show categorical filter UI
          showElement(
            id = categorical_ui_id
          )
          
          # Hide the menu to select filter type
          hideElement(
            id = type_selection_ui_id
          )
        } else if (module_data$filter_type == "numeric"){
          # Show numeric filter UI
          showElement(
            id = numeric_ui_id
          )
          
          # Hide the menu to select filter type
          hideElement(
            id = type_selection_ui_id
          )
        } else if (module_data$filter_type == "none"){
          # Show type selection menu, hide categorical and numeric interfaces
          showElement(
            id = type_selection_ui_id
          )
          
          hideElement(
            id = categorical_ui_id
          )
          
          hideElement(
            id = numeric_ui_id
          )
        }
      })
      
      ## 1.5. Categorical filters ####
      ### 1.5.1. Update choices for categorical metadata variables ####
      observeEvent(
        module_data$filter_type,
        label = 
          glue("{id}: filter menus: update choices for categorical metadata"),
        {
          # Runs as long as the type is "categorical"
          req(module_data$filter_type == "categorical")
          # Do not run in editing mode
          req(module_data$filter_menu_state == "add")
            
          # Construct vector of metadata choices for filtering
          # Values: machine-readable variable names
          categorical_choices <-
            sapply(metadata_config(), function(var) var$meta_colname) |> 
            unname()
          
          # Names: variable names displayed to user
          names(categorical_choices) <-
            sapply(metadata_config(), function(var) var$label) |> 
            unname()
          
          print("length(module_data$filters) > 0")
          print(length(module_data$filters) > 0)
          if (length(module_data$filters) > 0){
            # Subset choices to exclude categorical variables that have already 
            # been entered as filters
            # Identify categorical filters
            existing_categorical_filters <- 
              # Must use single brackets to select multiple elements from a 
              # list of list (double bracket notation with [[c(i,j)]] will fetch
              # the jth element of the ith sublist)
              module_data$filters[
                # sapply: creates boolean identifying categorical filters
                sapply(
                  module_data$filters,
                  function(filter_i){
                    filter_i$type == "categorical"
                  }
                )
              ]
            
            # Get var names of invalid choices
            invalid_choices <- 
              sapply(
                existing_categorical_filters,
                function(filter_i){
                  filter_i$var
                }
              )
            
            print("invalid_choices")
            print(invalid_choices)
            
            # Remove invalid choices
            categorical_choices <- 
              categorical_choices[!categorical_choices %in% invalid_choices]
          }
          
          # If no variables are left, notify the user using a placeholder
          if (length(categorical_choices) == 0){
            categorical_choices <- 
              c("No remaining variables" = "")
          }
          
          # First element: placeholder text with "falsy" value
          # categorical_choices <-
          #   c(
          #     "Select variable" = "",
          #     categorical_choices
          #     )
            
          updateSelectInput(
            session = session,
            inputId = "categorical_var",
            choices = categorical_choices
            )
          })
      
      ### 1.5.2. Choices for values within a given variable ####
      observeEvent(
        c(module_data$filter_type, 
          input$categorical_var
          ),
        label = 
          glue("{id}: filter menus: update choices for categorical metadata"),
        {
          req(input$categorical_var)
          # Do not run in editing mode
          req(module_data$filter_menu_state == "add")
          
          # Determine choices to display for the selected metadata variable
          
          # meta_df <-
          #   SCEPlots::fetch_metadata(
          #     object = object(),
          #     full_table = TRUE
          #   )
          
          choices <-
            SCEPlots::unique_values(
              object(), 
              var = input$categorical_var
              )
          
          # Determine which choices, if any, are invalid based on the current
          # subset
          
          # Update picker input with valid choices
          updatePickerInput(
            session,
            inputId = "categorical_values",
            choices = choices,
            # Reset selection to nothing being selected (no filters applied; 
            # use character(0) to do this)
            selected = character(0)#,
            # Use boolean vector to enable all choices
            # choicesOpt = 
            #   list(
            #     disabled = is_disabled,
            #     style = ifelse(
            #       is_disabled,
            #       yes = "color: rgba(119, 119, 119, 0.5);",
            #       no = ""
            #     )
            #   )
          )
        })
      
      ## 1.6. Numeric filters ####
      ### 1.6.1. Update choices for numeric metadata features ####
      # Only needs to run once at startup
      observeEvent(
        # Responds to loading of update and creation of UI (to ensure
        # feature updating is always performed after the input is 
        # created)
        valid_features(),
        label = glue("{id}: Render choices for feature selection"),
        {
          updateSelectizeInput(
            session,
            inputId = "numeric_feature",
            choices = valid_features(),
            selected = character(0),
            server = TRUE
            )
          })
      
      ### 1.6.2. Hide Buttons for choosing filter mode ####
      # Hide buttons until a feature is entered
      observe({
        target_id <- "numeric_mode"
        
        if (isTruthy(input$numeric_feature)){
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
      
      ### 1.6.3. Interface for choosing threshold ####
      numeric_filter_value <- 
        threshold_picker_server(
          # Do not namespace module server function IDs 
          id = "filter_threshold",
          object = object,
          feature = reactive({input$numeric_feature}),
          mode = reactive({input$numeric_mode}),
          # set_threshold will set the interface to a threshold/range when 
          # editing a filter
          set_threshold = 
            reactive({
              req(input$numeric_mode)
              if (input$numeric_mode == "range"){
                req(length(editing_data$numeric_filter_value) == 2)
              } else {
                req(length(editing_data$numeric_filter_value) == 1)
              }
               
              print("Record new set_thrseshold() value:")
              print(editing_data$numeric_filter_value)
              
              editing_data$numeric_filter_value
              }),
          showhide_animation = TRUE
        )
      
      observe({
        req(numeric_filter_value())
        print("Chosen numeric filter threshold")
        print(numeric_filter_value())
      })
      
      ## 1.7. Disable confirm button until proper selections are made ####
      observe(
        label = 
          glue("{id}: disable confirm button until a sensible selection is made "),
        {
          target_id <- "filter_confirm"
          
          if (module_data$filter_type == "categorical"){
            # Categorical filters: enable button when a metadata variable 
            # and values are chosen
            if (isTruthy(input$categorical_var) & 
                isTruthy(input$categorical_values)){
              shinyjs::enable(
                id = target_id
              )
            } else {
              shinyjs::disable(
                id = target_id
              )
            }
          } else if (module_data$filter_type == "numeric"){
            # Numeric filters: enable button when a feature and 
            # threshold/range are chosen
            if (isTruthy(input$numeric_feature) &
                isTruthy(numeric_filter_value())
                ){
              shinyjs::enable(
                id = target_id
              )
            } else {
              shinyjs::disable(
                id = target_id
              )
                }
          } else if (module_data$filter_type == "none"){
            # Confirm button is disabled when the filter type is equal to none
            # (but the button should never show in this case)
            shinyjs::disable(
              id = target_id
            )
          }
      })
      
      ## 1.8. Respond to confirm button ####
      # Save filter and reset states/menus
      observeEvent(
        input$filter_confirm,
        label = glue("{id}: respond to confirm button"),
        {
          # Record filter in list of filters
          if (module_data$filter_type == "categorical"){
            # Categorical filters
            filter_data <-
              list(
                `type` = "categorical",
                # Not used for categorical filters
                `mode` = NULL,
                `var` = input$categorical_var,
                # Add display name of variable for filter criteria display
                `label` = metadata_config()[[input$categorical_var]]$label,
                `value` = input$categorical_values
                )
            
            if (module_data$filter_menu_state == "add"){
              # When adding a filter:
              # Append to end of list ((length + 1)'th element)
              # c() will collapse elements into a list at the same level
              module_data$filters[[length(module_data$filters) + 1]] <-
                filter_data
            } else if (module_data$filter_menu_state == "edit"){
              # When editing a filter:
              # Save data to the index of the row being edited
              module_data$filters[[editing_data$target_row]]<-
                filter_data
              
              # Clear all editing data variables
            }
          } else if (module_data$filter_type == "numeric"){
            # Reset the selected feature
            updateSelectizeInput(
              inputId = "numeric_feature",
              selected = character(0)
            )
            
            # Numeric filters
            filter_data <-
              list(
                `type` = "numeric",
                `mode` = input$numeric_mode,
                `var` = input$numeric_feature,
                `label` = 
                  # Compute display name using config file
                  scExploreR:::hr_name(
                    machine_readable_name = input$numeric_feature,
                    assay_config = assay_config()
                    ),
                # Use return value of threhold_picker module
                # Either a single number, or a vector of upper and lower bounds
                # if a range is chosen
                `value` = numeric_filter_value()
              )
            
            if (module_data$filter_menu_state == "add"){
              # Adding a filter:
              # Append to end of list ((length + 1)'th element)
              module_data$filters[[length(module_data$filters) + 1]] <-
                filter_data
            } else if (module_data$filter_menu_state == "edit"){
              # Editing a filter:
              # Save data to the index of the row being edited
              module_data$filters[[editing_data$target_row]]<-
                filter_data
              }
          }
          
          # If a filter was being edited, also reset the editing variables
          if (module_data$filter_menu_state == "edit"){
            editing_data$mode <- NULL
            editing_data$var <- NULL
            editing_data$value <- NULL
            editing_data$numeric_filter_value <- NULL
            editing_data$target_row <- NULL
            editing_data$label <- NULL
          }
          
          # Set state to idle
          module_data$filter_menu_state <- "idle"
          
          # Also reset state of current filter type
          module_data$filter_type <- "none"
        })
      
      ## 1.9. Respond to cancel button ####
      observeEvent(
        input$filter_cancel,
        label = glue("{id}: respond to cancel button"),
        {
          # If the filter was a numeric filter, reset the feature choice menu
          if (module_data$filter_type == "numeric"){
            # Reset the selected feature
            updateSelectizeInput(
              inputId = "numeric_feature",
              selected = character(0)
              )
          }
          
          # If a filter was being edited, also reset the editing variables
          if (module_data$filter_menu_state == "edit"){
            editing_data$mode <- NULL
            editing_data$var <- NULL
            editing_data$value <- NULL
            editing_data$numeric_filter_value <- NULL
            editing_data$target_row <- NULL
            editing_data$label <- NULL
            }
          
          # Reset menu state, filter type
          module_data$filter_menu_state <- "idle"
          module_data$filter_type <- "none"
        })
      
      observe({
        req(module_data$filters)
        print("State of recorded filters")
        print(module_data$filters)
      })
      
      ## 1.10. UI for displaying filters ####
      ### 1.10.1. HTML ####
      filters_ui <-
        reactive(
          label = glue("{id}: Compute UI for filter menus"),
          {
            if (isTruthy(module_data$filters)){
              if (length(module_data$filters) > 0){
                div(
                  # Set a max height for the filter interface, and scroll 
                  # when the hight is exceeded
                  style = 
                    "max-height: 60vh; 
                     overflow-y: auto;",
                  # Create UI for each filter criteria in the list
                  lapply(
                    1:length(module_data$filters),
                    function(i){
                      # Extract type, display name of variable, values
                      type <- module_data$filters[[i]]$type
                      # Only applies to numeric filters
                      mode <- module_data$filters[[i]]$mode
                      label <- module_data$filters[[i]]$label
                      value <- module_data$filters[[i]]$value
                      
                      # Construct "card" summarizing the filter criteria applied
                      div(
                        class = "filter-info-card",
                        # Container for storing filter information
                        if (type == "categorical"){
                          # Container for categorical filters: follows format below
                          # <metadata_variable> in:
                          # <metadata_values>
                          div(
                            #style = "float: left;",
                            tags$b(
                              glue("{label}:"), 
                              class = "center half-space-bottom"
                            ),
                            tags$p(
                              scExploreR:::vector_to_text(
                                value
                              )
                            )
                          ) 
                        } else if (type == "numeric"){
                          div(
                            # Numeric filter: UI depends on filter mode (<, >, range) 
                            tags$b(
                              glue("{label}:"), 
                              class = "center half-space-bottom"
                            ),
                            tags$p(
                              if (mode == "less_than"){
                                paste0("< ", value)
                              } else if (mode == "greater_than"){
                                paste0("> ", value)
                              } else if (mode == "range"){
                                paste0("Range: ", value[1], "- ", value[2])
                              }
                            )
                          )
                        },
                        # Container for edit/delete buttons
                        div(
                          div(
                            style = "float: right;",
                            class = "btn-group",
                            role = "group",
                            `aria-label` = glue("Options for filter {i})"),
                            # Content: custom button element
                            # Code adapted from 
                            # https://github.com/AntoineGuillot2/ButtonsInDataTable
                            HTML(
                              # Current list index of filter is passed to button ID
                              glue(
                                '<button type="button" class="btn icon-button edit" 
                                id = edit_{i}> 
                                <i class = "fa fa-pencil" role = "presentation" 
                                  aria-label = "Edit" style = "font-size: 1.2em;">
                                  </i>
                                </button>
                                <button type="button" class="btn icon-button 
                                  delete" id = delete_{i}> 
                                  <i class = "fa fa-times-circle" 
                                    role = "presentation" aria-label = "Delete" 
                                    style = "font-size: 1.2em;">
                                    </i>
                                  </button>'
                              )
                            )
                          )
                        )
                      )
                    }
                  )
                )
              } else {
                # Display "no filters applied" when no filters are entered
                div(
                  class = "filter-info-card",
                  tags$b(
                    "- No filters applied -", 
                    class = "center"
                    )
                )
              }
            } else {
              # Display "no filters applied" when no filters are entered
              # div(
              #   class = "filter-info-card",
              #   tags$b("- No filters applied -", style = "float: center;")
              #   )
              } 
            })
      
      ### 1.10.2. JavaScript for buttons ####
      filter_button_js <-
        reactive(
          label = glue("{id}: edit/delete button JavaScript"),
          {
            req(module_data$filters)
            
            if (length(module_data$filters) > 0){
              tags$script(
                paste0(
                  "$(document).on(
                    'click',",
                    # Single quotes enclose # selectior, namespaced id, and the 
                    # descendent button element selector  
                    "'#", ns("filters_applied"), " button',",
                    "function () {
                      Shiny.onInputChange('", ns("lastClickId"), "', this.id);
                      Shiny.onInputChange('", ns("lastClick"), "', Math.random());
                    });"
                  )
                  # "
                  # $(document).on(
                  #   'click', 
                  #   '#{ns('filters_applied')} button', 
                  #   function () {
                  #     Shiny.onInputChange('lastClickId', this.id);
                  #     Shiny.onInputChange('lastClick', Math.random())
                  #   });
                  #   "
                )
              }
            })
      
      ### 1.10.3. Render UI ####
      output$filters_applied <-
        renderUI({
          # Render UI elements, scripts together
          tagList(
            filters_ui(),
            filter_button_js()
          )
        })
      
      ## 1.11. Respond to click on edit/delete buttons ####
      observeEvent(
        # Respond to lastClick, which always changes with each click
        # (lastClickId does not necessarily change)
        input$lastClick,
        label = glue("{id}: Register click on edit/delete buttons"),
        {
         # Test if ID of click contains "edit" or "delete"
         if (grepl("edit", input$lastClickId)){
           # Determine row being edited, and store in editing information
           row_selected <- 
             gsub("edit_", "", input$lastClickId) |> 
             as.numeric()
           
           editing_data$target_row <-
             row_selected
           
           # Fetch filter data from list
           filter_data <-
             module_data$filters[[row_selected]]
           
           # Determine type of filter being edited
           # filter_type <- filter_data$type
           
           # Set up editing interface 
           if (filter_data$type == "categorical"){
             # Set state variable for filter type to show appropriate menus
             module_data$filter_type <- "categorical"
             
             # Store metadata variable for filter being edited
             editing_data$var <- 
               filter_data$var
             
             # Store label of metadata variable
             editing_data$label <-
               filter_data$label
               
             # Store selected values of feature
             editing_data$value <- 
               filter_data$value
             
             # Update selection menu with the values chosen
             # Determine possible choices for the selected variable
             choices <-
               SCEPlots::unique_values(
                 object(), 
                 var = editing_data$var
                )
             
             # Update picker input with choices, selected values
             updatePickerInput(
               session,
               inputId = "categorical_values",
               choices = choices,
               # Reset selection to nothing being selected (no filters applied; 
               # use character(0) to do this)
               selected = editing_data$value#,
               # Use boolean vector to enable all choices
               # choicesOpt = 
               #   list(
               #     disabled = is_disabled,
               #     style = ifelse(
               #       is_disabled,
               #       yes = "color: rgba(119, 119, 119, 0.5);",
               #       no = ""
               #     )
               #   )
             )
           } else if (filter_data$type == "numeric"){
             # Set state variable for filter type to show appropriate menus
             module_data$filter_type <- "numeric"
             
             # Set mode, move radio button selection to indicated mode
             editing_data$mode <- 
               filter_data$mode
             
             shinyWidgets::updateRadioGroupButtons(
               session = session,
               inputId = "numeric_mode",
               selected = editing_data$mode
               )
             
             # Store feature being edited
             editing_data$var <- 
               filter_data$var
             
             # Store threshold/range
             editing_data$numeric_filter_value <- 
               filter_data$value
             
             # Update feature selection menu
             updateSelectizeInput(
               session = session,
               inputId = "numeric_feature",
               choices = valid_features(),
               selected = editing_data$var
               )
           }
           
           # Set state of menu to edit to reveal editing interface
           module_data$filter_menu_state <- "edit"
           
         } else if (grepl("delete", input$lastClickId)){
           # Determine row deleted
           row_selected <- 
             gsub("delete_", "", input$lastClickId) |> 
             as.numeric()
           
           # Prevents errors in the event the selected_row is undefined
           if (!is.null(row_selected) | is.na(row_selected)){
             # Delete the indicated filter from the list
             module_data$filters[row_selected] <- NULL
             
             # Reset filter menu state variables in the event a 
             # filter is deleted while editing it
             # module_data$filter_menu_state <- "idle"
             # module_data$filter_type <- "none"
             
           } else {
             warning(
               "Unable to determine the index of the row selected for deletion"
               )
           }
         }
        })
      
      ## 1.12. Editing UI ####
      ### 1.12.1. Categorical features: feature being edited ####
      output$edit_categorical_var <-
        renderText({
          req(editing_data$label)
          
          editing_data$label
          })
      
      # 2. UI for string subsetting --------------------------------------------
      # Use shinyjs to show and hide menus based on whether the adv. subsetting
      # checkbox is checked (this ensures inputs exist and can be updated 
      # properly, and improves performance)
      observeEvent(
        input$string_subsetting,
        # Must ignore NULL values for the conditional to run without errors
        ignoreNULL = TRUE,
        {
          # Show or hide string subsetting UI based on state of checkbox
          if (input$string_subsetting == TRUE){
            shinyjs::show("string_subsetting_ui")
          } else {
            shinyjs::hide("string_subsetting_ui")
          }
        })
      
      
      # 3. Reset all filters ------------------------------
      ## 3.1. Show/hide reset button ####
      observe({
        target_id <- "reset_all_filters"
      
        if (length(module_data$filters) > 0){
          # Show when at least one filter has been entered
          showElement(
            id = target_id
            )
        } else {
        hideElement(
          id = target_id
          )
        }
      })
      
      ## 3.2. Respond to reset button ####
      observeEvent(
        input$reset_all_filters,
        label = "Reset Filter Menus",
        {
          # Reset the filter data to an empty list
          module_data$filters <- list()
          })
      
      # 4. Feature statistics --------------------------------------------------
      # Used for string subsetting
      # Assists user by displaying feature name as it should be entered into the
      # subset function (using the assay key prefix), along with summary
      # statistics for the feature to aid in choosing bounds when subsetting.
      
      ## 4.1. Update feature search choices ####
      # Updates occur each time the object is changed
      observeEvent(
        valid_features(),
        {
          updateSelectizeInput(
            session,
            inputId = "search_feature",
            choices = valid_features(),
            selected = character(0),
            server = TRUE,
            options = 
              list(
                # Add remove button to inputs
                'plugins' = list('remove_button'),
                # Do not allow user to input features not
                # in the list of options
                'create' = FALSE,
                'placeholder' = "enter feature"
                )
            ) 
        })
      
      ## 4.2. Define UI for feature statistics ####
      feature_stats_ui <-
        reactive({
          req(input$search_feature)
          
          # Obtain summary stats for feature
          feature_summary <-
            # Uses Seurat::FetchData to pull data matrix for the feature
            # Data slot is pulled by default; this can be changed
            FetchData(
              object(),
              vars = input$search_feature
            )[,1] |> 
            # (summary is a base function)
            summary()
          
          # Print UI below
          tagList(
            # Feature ID
            tags$p(
              class = "bold-blue large",
              tags$b(
                "Feature ID:"
                ),
              input$search_feature
              ),
            tags$p("(This must be entered exactly as it displays above)"),
            # Summary Statistics
            tags$b("Summary Statistics", class = "bold-blue"),
            summary_tags(
              feature_summary,
              header_class = "bold-blue"
              ),
            # Ridge plot showing histogram of feature expression (in all cells)
            plotOutput(
              outputId = ns("feature_summary_ridge"),
              height = "150px",
              # Draw vertical line upon user click
              # Adding "plot_hover" will create `input$plot_hover`.
              # A vertical line will be drawn according to the x coordinate 
              # corresponding to the pointer location 
              hover = 
                hoverOpts(
                  id = ns("plot_hover"),
                  # Draw line when 
                  delay = 300,
                  delayType = "throttle"
                  ),
              click =
                clickOpts(
                  id = ns("plot_click")
                )
              ),
            # UI to display statistics based on selected threshold
            uiOutput(
              outputId = ns("threshold_stats_ui")
            )#,
            
            # TEMP: Show hover coordinates
            # tags$p("Hover Data"),
            # verbatimTextOutput(
            #   outputId = ns("print_hover_coords")
            #   ),
            # tags$p("Click Data"),
            # verbatimTextOutput(
            #   outputId = ns("print_click_info")
            #   )
            )
        })
      
      ## 4.3. Define ridge plot showing feature expression ####
      # The plot must be stored in a reactiveValues object (module_data) to
      # avoid reactivity issues when processing hover and click values. 
      # The initial plot is generated below.
      observeEvent(
        input$search_feature,
        label = "Subset Selections: Ridge Plot Histogram",
        ignoreNULL = FALSE,
        {
          # When drawing a new plot, clear plots and data associated with the
          # last plot if present
          module_data$initial_ridge_plot <- NULL
          module_data$ridge_plot_with_threshold <- NULL
          module_data$ridge_plot <- NULL
          module_data$threshold_x <- NULL
          module_data$thresh_stats <- NULL
          
          # Set module_data$ridge_plot to the plot if a feature is entered
          # in the search bar, otherwise set the plot to NULL
          if (isTruthy(input$search_feature)){
            data <- 
              FetchData(
                object = object(),
                vars = input$search_feature
                )
            
            # Get minimum and maximum values of data
            module_data$plot_max <- 
              data |> 
              max()
            
            module_data$plot_min <-
              data |> 
              min()
            
            ### TEMP ###
            # Also make a ggplot histogram
            # module_data$gg_histogram <-
            #   ggplot(data = data, aes(x = .data[[input$search_feature]])) +
            #   geom_histogram(color = "#000000", fill = "#000088") +
            #   theme_light()
            
            # The coordinates returned from hover and click events will
            # be multiplied by the range to allow the user to properly select
            # a region on the plot
            # This is a workaround, it would be ideal to find out why Shiny
            # interactive coordinates aren't working for Seurat plots
            module_data$plot_range <- 
              module_data$plot_max - module_data$plot_min
            
            module_data$initial_ridge_plot <-
              shiny_ridge(
                object = object(), 
                features_entered = input$search_feature, 
                group_by = "none", 
                show_legend = FALSE, 
                palette = c("#000088"),
                center_x_axis_title = TRUE
                ) 
            # Extract the object from patchwork format
            module_data$initial_ridge_plot <-
              module_data$initial_ridge_plot[[1]]
          } else {
            module_data$initial_ridge_plot <-
              NULL
            }
          })
      
      ## 4.4. Add vertical Line Upon Hovering ####
      # In the event the user hovers over or clicks the plot, add the 
      # corresponding vertical line at the x-coordinate of the click.
      observeEvent(
        input$plot_hover,
        # IgnoreNULL must be TRUE to avoid the plot computing when
        # input$plot_hover is NULL, which will erase the line from the plot
        # as soon as it is drown
        ignoreNULL = TRUE,
        ignoreInit = TRUE,
        {
          # The saved plot must be different from the initial plot, or a series
          # of vertical lines will be created each time a hover event is 
          # registered
          
          # Transforming X-coordinates for compatibility with hover/click
          # Coordinates are incorrectly being from zero to one, with one
          # being the max value on the plot.
          x_original <-
            interactive_transform(
              x_coord = input$plot_hover$x, 
              distribution_range = module_data$plot_range,
              distribution_minimum = module_data$plot_min, 
              plot_min_coord = 0.08, 
              plot_max_coord = 0.9
              )
          
          # Draw vertical line using transformed hover coordinate  
          # Hover line is drawn over either the initial plot, or the plot
          # with a defined threshold, depending on whether the user has
          # clicked the plot
          
          # module_data$click_info is used. module_data$click_info only 
          # changes upon a new click, and does not become NULL while the 
          # plot is re-drawn
          # if (!is.null(module_data$click_info)){
          #  base_plot <- module_data$initial_ridge_plot 
          # } else {
          #   # If module_data$click_info is defined, use the plot with 
          #   # the vertical line at the click location
          #   base_plot <- module_data$ridge_plot_with_threshold 
          # }
          
          base_plot <- 
            if (!is.null(module_data$ridge_plot_with_threshold)){
              module_data$ridge_plot_with_threshold
            } else{
              module_data$initial_ridge_plot
            }
          
          module_data$ridge_plot <-
            base_plot +
            geom_vline(
              xintercept = x_original,
              color = "#666666",
              size = 0.75
            )
            
            # TEMP: update hover coordinates for renderPrint output
            module_data$hover_coords <- 
              input$plot_hover
            })
      
      ## 4.5. Respond to Click Event ####
      ### 4.5.1 Add Threshold Line, Record Click Coordinates ####
      observeEvent(
        input$plot_click,
        # IgnoreNULL must be TRUE to avoid the plot computing when
        # input$plot_click is NULL, which will erase the line from the plot
        # as soon as it is drown
        ignoreNULL = TRUE,
        ignoreInit = TRUE,
        {
          # Draw a solid, and persistent, line on the x-axis
          # Transfrom x-coordinate of click to match distribution
          x_original <-
            interactive_transform(
              x_coord = input$plot_click$x, 
              distribution_range = module_data$plot_range,
              distribution_minimum = module_data$plot_min, 
              plot_min_coord = 0.08, 
              plot_max_coord = 0.9
              )
          
          # Record transformed x-coordinate 
          # Round threshold to two decimal places (such that the threshold 
          # displayed will be consistent with the threshold stats computed if
          # it is copied and pasted. To my knowledge, there is no need to use
          # thresholds with greater precision)
          module_data$threshold_x <- 
            x_original |> 
            round(digits = 2)
          
          # Draw vertical line using transformed hover coordinate  
          module_data$ridge_plot_with_threshold <-
            module_data$initial_ridge_plot +
            geom_vline(
              xintercept = module_data$threshold_x,
              color = "#000000",
              size = 0.75
              )
          
          # Record threshold statistics
          module_data$thresh_stats <- 
            threshold_stats(
              object = object(), 
              feature = input$search_feature, 
              threshold = module_data$threshold_x
              )
          
          # Record click coordinates
          module_data$click_info <- 
            input$plot_click
        })
      
      # ### 4.5.2 Compute Threshold Stats ###
      # # Reactive expression must have separate name from function inside
      # threshold_statistics <-
      #   eventReactive(
      #     input$plot_click,
      #     # Must ignore NULL events to avoid stats resetting while the plot
      #     # is drawing the threshold line
      #     ignoreNULL = TRUE,
      #     ignoreInit = TRUE,
      #     {
      #       print("Compute threshold stats")
      #       
      #       # Threshold stats function
      #       threshold_stats(
      #         object = object(), 
      #         feature = input$search_feature, 
      #         threshold = module_data$threshold_x
      #         )
      #       })
      
      ### 4.5.2. Display Threshold Stats ####
      threshold_stats_ui <- 
        eventReactive(
          module_data$thresh_stats,
          ignoreNULL = FALSE,
          {
            # Display UI only when a selection is made
            if (!is.null(module_data$threshold_x)){
              div(
                class = "compact-options-container",
                tags$b(
                  class = "center",
                  "Chosen threshold"
                ),
                tags$b(
                  class ="center half-space-bottom",
                  style = "background-color: #FFFFFF; border-radius: 10px;",
                  format(
                    module_data$threshold_x,
                    # Display at least three sig figs in percentage
                    digits = 3,
                    # Display at least two digits after decimal point
                    nsmall = 2,
                    scientific = FALSE
                  )
                ),
                tags$b(
                  "Number of cells above threshold:"
                ),
                glue(
                  "{module_data$thresh_stats$n_above}
                  ({module_data$thresh_stats$percent_above}%)"
                ),
                tags$br(),
                tags$b(
                  "Number of cells below threshold:"
                ),
                glue(
                  "{module_data$thresh_stats$n_below} 
                  ({module_data$thresh_stats$percent_below}%)"
                )
              )
            } 
            })
      
      # output$print_hover_coords <-
      #   renderPrint({
      #     req(module_data$hover_coords)
      #     
      #     module_data$hover_coords
      #   })
      # 
      # output$print_click_info <-
      #   renderPrint({
      #     req(module_data$click_info)
      #     
      #     module_data$click_info
      #   })
      
      ## 4.6. Render Feature Statistics Components ####
      output$feature_statistics <- 
        renderUI({
          feature_stats_ui()
        })
      
      output$feature_summary_ridge <-
        renderPlot({
          # Must use suppressMessages and print(plot) to suppress a
          # "Picking joint bandwidth of ___" message from ggridges, which
          # overwhelms the console when interactive hovering is used
          # SuppressMessages alone is not enough, print(plot) must also be
          # used. See https://github.com/tidyverse/ggplot2/issues/1101
          suppressMessages(
            print(module_data$ridge_plot)
          )
        })
      
      output$threshold_stats_ui <-
        renderUI({
          threshold_stats_ui()
        })
      
      # 5. Form Reactive List From Menu Selections -----------------------------
      selections <- 
        reactive(
          label = glue("{id}: return value for selections"),
          {
            # Store selections for each input in the UI (one menu is created 
            # for each metadata category in the config file)
            # selections_list <- 
            #   lapply(
            #     names(metadata_config()),
            #     function(category){
            #       # Define input ID for each category 
            #       # Formula: <category>_selection
            #       category_id <- glue("{category}_selection")
            #       
            #       # When menus are empty, no filters are applied.
            #       # Therefore, all unique values should be returned 
            #       # for the category in question
            #       if (is.null(input[[category_id]])){
            #         unique_metadata()[[category]]
            #       } else {
            #         input[[category_id]]
            #       }
            #     }
            #   )
            # 
            # # Add categories from metadata file to list names
            # names(selections_list) <- names(metadata_config())
            # 
            # # If hide_menu is provided and is a reactive, remove all 
            # # hidden menus from the selections output
            # # is.reactive() is used as a conditional to keep app from crashing
            # # when hide_menu is NULL or not a reactive variable
            # if (!is.null(hide_menu) && is.reactive(hide_menu)){
            #   # Remove any categories from the selections list 
            #   # that are also in hide_menu
            #   selections_list <-
            #     selections_list[!names(selections_list) %in% hide_menu()]
            # }
            # 
            # selections_list
            
            # Return filters selected in interface
            module_data$filters
          })
      
      # 6. Process Subset String -----------------------------------------------
      # If entered by the user, record the advanced subsetting string each time 
      # the 'apply subset' button is pressed
      user_string <- 
        reactive({
          # isTruthy: if the manual subset string entry box does not exist 
          # (which is the case before the checkbox is selected), return NULL 
          # for the string
          if(isTruthy(input$adv_subset)){
            # If the subset string entry is defined, test for newlines.
            # If newline characters exist in the string, remove them.
            if (grepl("\\n", input$adv_subset)){
              # gsub used to remove newline characters
              return_string <- gsub("\\n", "", input$adv_subset)
            } else {
              return_string <- input$adv_subset
            }
            
            # Return the result to user_string
            return(return_string)
          } else {
            return(NULL)
          }
          })
      
      # 7. Return Menu Selections and Manual String Entry ----------------------
      return(
        list(
          `selections` = selections,
          # Store advanced string input reactively, if it exists
          `user_string` = user_string
          )
        )
    }
  )
}



