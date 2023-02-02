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
subset_selections_ui <- function(id,
                                 unique_metadata,
                                 metadata_config,
                                 auto_dictionary_path,
                                 string_subsetting_href
                                 ){
  # Namespace function: prevents conflicts with IDs defined in other modules 
  ns <- NS(id)
  
  # Create a list for storing the Shiny tags from each menu 
  menus <- tagList()
  
  for (category in names(metadata_config())){
    # Fetch config information for current category
    config_i <- metadata_config()[[category]]
    
    # Create menu for the current category
    menu_tag <- pickerInput(
      # input_prefix: will become unnecessary once the subset 
      # menus are placed within a module
      inputId = ns(glue("{category}_selection")),
      #label: uses the label defined for the category 
      # in the config file
      label = glue("Filter by {config_i$label}"),
      # choices: filled using the unique_metadata list
      # If the metadata category has defined groups, sort choices
      # into a named list based on the groups. This will show choices
      # divided by group in the pickerInput menu.
      # If groups do not exist for the metadata category, 
      # unique_metadata()[[category]] will be returned as a vector.
      choices = 
        process_choices(
          metadata_config,
          category = category,
          choices = unique_metadata()[[category]]
          ),
      
      # selected: nothing selected by default. A placeholder will appear instead
      selected = character(0),
      multiple = TRUE,
      # Options for pickerInput
      options = list(
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
        "none-selected-text" = "No Filters Applied"
        )
    ) # End pickerInput
    
    # Append tag to list using tagList
    menus <- tagList(menus, menu_tag)
  }
  
  # Elements to display beneath menu tags (appended using tagList)
  menus <- 
    tagList(
      menus,
      # Reset button: appears when subset menus are currently being filtered
      uiOutput(outputId = ns("reset_filter_button")),
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
          
          # Option B: display information in a modal
          # dropdownButton(
          #   inputId = ns("adv_subset_dropdown"),
          #   label = "",
          #   size = "xs",
          #   icon = icon("bars"),
          #   style = "border-radius: 10px;",
          #   # Dropdown content
          #   tagList(
          #     # Link to the auto-generated object dictionary
              # tags$a(
              #   "Metadata guide and help",
              #   href = "Auto_Dictionary.html",
              #   target = "_blank",
              #   rel = "noopener noreferrer",
              #   class = "blue_hover"
              # ),
          #     # Opens modal for statistics by feature
          #     actionLink(
          #       inputId = "feature_dictionary_modal",
          #       label = "Feature Search",
          #       class = "blue_hover"
          #       )
          #   )
          # ),
          
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
      
      # If checkbox is selected, show string subsetting menu
      #uiOutput(outputId = ns("string_subsetting_menu"))
     )
  
  # Return list of menu tags
  menus
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
#' @param meta_categories A vector of the metadata categories included in the 
#' config file for the current object. This is computed in the main server
#' function
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
      # interactively generated in 5.3.
      module_data <- reactiveValues()
      
      # 1. UI for String Subsetting --------------------------------------------
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
      
      # 2. UI for Filtering Selection Menus ------------------------------------
      # Subset menus will be filtered for 
      ## 2.1. filters_applied: a boolean that is TRUE when a subset has been 
      # filtered (this may be changed as the filter code is developed)
      filters_applied <- 
        eventReactive(
          selections(),
          ignoreNULL = FALSE,
          {
            # Check if selections() is shorter than the original list of 
            # choices used to generate the menus (account for hidden menus if 
            # these are specified).
            if (!is.null(hide_menu) && is.reactive(hide_menu)){
              # If hide menu is specified, remove the hidden category from 
              # the list of original choices
              all_values <- 
                unique_metadata()[!names(unique_metadata()) %in% hide_menu()]
            } else {
              # If hide_menu is not specified, use the list of original choices
              all_values <- unique_metadata()
              }
            
            not_equal <- 
              !(setequal(
                unlist(all_values),
                unlist(selections())
                ))
            
            # If selections is shorter (not_equal==TRUE), 
            # return TRUE, and vice versa 
            if (not_equal == TRUE){
              return(TRUE)
              } else {
                return(FALSE)
                }
            })
      
      ## 2.2. Create UI for "Reset Filter button"
      # Button is needed after filtering is applied to reset selections 
      reset_filter_ui <- 
        eventReactive(
          filters_applied(),
          ignoreNULL = FALSE,
          {
            if (filters_applied() == TRUE){
              # Display reset button if filters have been applied 
              actionButton(
                inputId = ns("reset_filter"),
                label = "Remove All Filters",
                icon = icon("times-circle")
                )
              
              # Do not display anything otherwise
              } else NULL
            })
      
      # Render UI for reset button
      output$reset_filter_button <- renderUI({
        reset_filter_ui()
      })
      
      
      # 3. For later: filter menus in UI based on selections --------------------
      ## 3.1. Update valid choices in selection menus #### 
      observeEvent(
        selections(),
        ignoreNULL = FALSE,
        {
          # Extract metadata table
          meta_df <- object()@meta.data
          
          # a. Determine cells that match selections made for each category 
          # where filter criteria have been entered
          
          #print("a. Filter by category")
          
          filter_by_category <-
            lapply(
              X = isolate(meta_categories()),
              FUN = function(category){
                # Define input ID for each category 
                # Formula: <category>_selection
                category_id <- glue("{category}_selection")
                
                # "" %in% input[[category_id]]: this is TRUE when the input
                # menu for the chosen category has nothing selected
                if (!is.null(input[[category_id]])){
                  if (! "" %in% input[[category_id]]){
                    meta_df[[category]] %in% input[[category_id]]
                  } else {
                    # If the menu is blank, return a vector of all TRUE values 
                    # (so the data is not narrowed down based on this category)
                    rep_len(TRUE, length(meta_df[[category]]))
                  }
                } else {
                  # Do the same when the menu is not defined 
                  # at all to avoid errors
                  rep_len(TRUE, length(meta_df[[category]]))
                }
              }
            )
          
          # b. Identify cells that match all of the above selected criteria 
          # Return TRUE for rows that match each criteria 
          # entered (TRUE for all categories)
          
          #print("b. Cells in Common")
          
          in_common <- Reduce(f = `&`, x = filter_by_category)
          
          # c. Subset original table for all rows that are TRUE
          #print("c. Filter Seurat Metadata")
          
          filter_df <- meta_df[in_common, ]
          
          # d. For each category where filters are not selected, 
          # fetch the unique values
          
          #print("d. Unique values for valid filter categories")
          
          valid_entries <-
            lapply(
              X = isolate(meta_categories()),
              FUN = function(category){
                # Define input ID for each category 
                # Formula: <category>_selection
                category_id <- glue("{category}_selection")
                
                # Compute unique values for categories where the user has not 
                # entered filter criteria (these inputs will be NULL)
                if (is.null(input[[category_id]])){
                  filter_df |> 
                    # Fetch unique values for each category 
                    dplyr::select(.data[[category]]) |>
                    unique() |> 
                    # Convert unique values to character vector
                    unlist() |>
                    as.character()
                }
                # For categories where inputs are defined, NULL will be 
                # entered for that category in valid_entries. 
              }
            )
          # Add category names to valid_entries
          names(valid_entries) <- isolate(meta_categories())
          
          # e. Update selection menus with options in valid entries
          # Invalid entries will still display, but will be disabled
          
          #print("e. Update individual menus")
          
          for (category in isolate(meta_categories())){
            # Update only menus for which the values in valid_entries 
            # are not NULL
            if (!is.null(valid_entries[[category]])){
              # Define input ID for category 
              # Formula: <category>_selection
              category_id <- glue("{category}_selection")
              # Define choices available (this does not change, but it must be 
              # passed to updateSelectizeinput for the update to work)
              # Sort choices into a named list if group information is defined
              # in the config file; if group information is not defined, a 
              # vector will be returned.
              sorted_choices <-
                process_choices(
                  metadata_config,
                  category = category,
                  # Choices: all unique metadata values for current category
                  choices = unique_metadata()[[category]]
                )
              # Choices vector: vectorized format of sorted_choices, without 
              # names. Used to identify invalid choices to disable. Vector will
              # display choices in the order they appear in the named list,
              # and therefore the order they appear in the app
              choices_vector <-
                sorted_choices |> 
                unlist() |> 
                unname()
              # Define invalid choices to disable 
              invalid_choices <- 
                choices_vector[!(choices_vector %in% valid_entries[[category]])]
              # Boolean of invalid choices
              # `Disable` attribute in choicesOpt argument requires a logical
              # vector of the same length as the choices passed (choices with a 
              # TRUE entry at the index of the logical vector corresponding to 
              # their index will be disabled)
              disable_boolean <- 
                # Use choices_vector (vector format is required for %in%)
                # All choices in invalid_choices are disabled
                choices_vector %in% invalid_choices
              
              updatePickerInput(
                session,
                inputId = category_id,
                # Choices: these do not change but must be defined 
                # to avoid errors
                choices = sorted_choices,
                # Selected: all previously selected choices that are not in
                # the disabled values (though there should not be any selected
                # choices)
                selected = 
                  input[[category_id]][
                    !input[[category_id]] %in% invalid_choices],
                choicesOpt = 
                  list(
                    disabled = disable_boolean,
                    style = ifelse(
                      disable_boolean,
                      yes = "color: rgba(119, 119, 119, 0.5);",
                      no = ""
                      )
                    ),
                options =
                  list(
                    "selected-text-format" = "count > 5",
                    "actions-box" = TRUE,
                    # Placeholder
                    "none-selected-text" = "No Filters Applied",
                    # Label for "deselect all" button
                    "deselectAllText" = "Remove filter"
                    )
                )
      }
          }
        })
      
      ## 3.2. Reset button ####
      observeEvent(
        input$reset_filter,
        label = "Reset Filter Menus",
        {
          # When the reset button is pressed, update all of the menus
          for (category in meta_categories()){
            # Define original values using unique_metadata, sorting into groups
            # if they are defined in the config file
            initial_choices_sorted <- 
              process_choices(
                metadata_config,
                category = category,
                # Choices input: all unique values for the metadata category
                choices = unique_metadata()[[category]]
              )
            
            # Unpack into a vector, using the same order as the named list 
            # (if groups are defined) or vector (if not) produced above
            initial_choices_vector <-
              initial_choices_sorted |> 
              unlist() |> 
              unname()
            
            # Boolean for disabled setting of updatePickerInput
            # No choices should be disabled. Therefore, the vector of booleans
            # for disabling choices should be all FALSE.
            disable_boolean <- 
              # rep_len: return a vector of all FALSE elements, with a length
              # equal to the number of choices (initial_choices_vector)
              rep_len(FALSE, length(initial_choices_vector))
            
            # Update picker input
            updatePickerInput(
              session,
              inputId = glue("{category}_selection"),
              # Choices: use all sorted choices
              choices = initial_choices_sorted,
              # Reset selection to nothing being selected (no filters applied; 
              # use character(0) to do this)
              selected = character(0),
              # Use boolean vector to enable all choices
              choicesOpt = 
                list(
                  disabled = disable_boolean,
                  style = ifelse(
                    disable_boolean,
                    yes = "color: rgba(119, 119, 119, 0.5);",
                    no = ""
                  )
                )
              )
          }
        })
      
      # 4. Hide menus, if specified by the user. -------------------------------
      # hide_menu is an optional argument that is NULL in modules where it is 
      # not specified. Since hide_menu is intended to be reactive, observers 
      # that use it will crash the app when NULL values are passed to them.
      if (!is.null(hide_menu) && is.reactive(hide_menu)){
        observeEvent(
          hide_menu(),
          label = "Subset Selections: Hide Menu",
          # NULL values should be processed (all menus shown when NULL)
          ignoreNULL = FALSE,
          {
            # Hide menus if hide_menu is not equal to NULL
            if (!is.null(hide_menu())){
              # Hide all menus specified in hide_menus (may be a 
              # single menu or multiple menus)
              for (menu_category in hide_menu()){
                hideElement(
                  id = glue("{menu_category}_selection")
                )
              }
              # Show all menus in the module that are not in the 
              # hide_menu vector
              for (category in names(metadata_config())){
                if (!category %in% hide_menu()){
                  showElement(
                    id = glue("{category}_selection")
                  )
                }
              }
            } else {
              # If hide_menu is NULL, show all menus 
              for (category in names(metadata_config())){
                showElement(
                  id = glue("{category}_selection")
                  )
                }
              }
            })
      }
      
      # 5. Feature Statistics --------------------------------------------------
      # Used for string subsetting
      # Assists user by displaying feature name as it should be entered into the
      # subset function (using the assay key prefix), along with summary
      # statistics for the feature to aid in choosing bounds when subsetting.
      
      ## 5.1. Update feature search choices ####
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
      
      ## 5.2. Define UI for Feature Statistics ####
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
      
      ## 5.3. Define Ridge Plot Showing Feature Expression ####
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
      
      ## 5.4. Add vertical Line Upon Hovering ####
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
      
      ## 5.5. Respond to Click Event ####
      ### 5.5.1 Add Threshold Line, Record Click Coordinates ####
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
      
      # ### 5.5.2 Compute Threshold Stats ###
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
      
      ### 5.5.2. Display Threshold Stats ####
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
      
      ## 5.4. Render Feature Statistics Components ####
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
      
      # 6. Form Reactive List From Menu Selections -----------------------------
      selections <- 
        reactive(
          label = glue("{id}: selections_list"),
          {
            # Store selections for each input in the UI (one menu is created 
            # for each metadata category in the config file)
            selections_list <- 
              lapply(
                names(metadata_config()),
                function(category){
                  # Define input ID for each category 
                  # Formula: <category>_selection
                  category_id <- glue("{category}_selection")
                  
                  # When menus are empty, no filters are applied.
                  # Therefore, all unique values should be returned 
                  # for the category in question
                  if (is.null(input[[category_id]])){
                    unique_metadata()[[category]]
                  } else {
                    input[[category_id]]
                  }
                }
              )
            
            # Add categories from metadata file to list names
            names(selections_list) <- names(metadata_config())
            
            # If hide_menu is provided and is a reactive, remove all 
            # hidden menus from the selections output
            # is.reactive() is used as a conditional to keep app from crashing
            # when hide_menu is NULL or not a reactive variable
            if (!is.null(hide_menu) && is.reactive(hide_menu)){
              # Remove any categories from the selections list 
              # that are also in hide_menu
              selections_list <-
                selections_list[!names(selections_list) %in% hide_menu()]
            }
            
            selections_list
          })
      
      # 6. Process Subset String
      # If entered by the user, record the advanced subsetting string each time 
      # the 'apply subset' button is pressed
      user_string <- 
        reactive({
          # isTruthy: if the manual subset string entry box does not exist 
          # (which is the case before the checkbox is selected), return NULL 
          # for the string
          if(isTruthy(input$adv_subset)){
            # If the subset string entry is defined, test for newlines.
            print("string entered")
            print(input$adv_subset)
            
            # If newline characters exist in the string, remove them.
            if (grepl("\\n", input$adv_subset)){
              print("Newline detected")
              # gsub used to remove newline characters
              return_string <- gsub("\\n", "", input$adv_subset)
            } else {
              return_string <- input$adv_subset
            }
            
            print("corrected string")
            print(return_string)
            
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



