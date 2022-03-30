# Subset Selections Module

# subset_selections_ui
# arguments
# id: namespace to use for this module. Reccomended id is "<tab_name>_subset_menus".
# unique_metadata: a list of the unique metadata values for each of the metadata 
# categories listed in the config file. This is generated in the main server function 
# at startup.
# metadata_config: the metadata section of the config file. This does not need to
# be specified if the config list is stored as "config" in the global environment.
subset_selections_ui <- function(id,
                                 unique_metadata,
                                 metadata_config
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
    )# End pickerInput
    
    # Append tag to list using tagList
    menus <- tagList(menus, menu_tag)
  }
  
  # Last element: a reset button that will appear when subset menus 
  # are filtered  to remove criteria that are mutually exclusive 
  # with current selections
  menus <- 
    tagList(
      menus,
      uiOutput(outputId = ns("reset_filter_button"))
    )
  
  # Return list of menu tags
  menus
  }

# Server function
# Arguments
# id: the namespace to use for the module. UI-server function pairs should 
# use the same id.
# object: The Seurat Object defined in the main server function
# unique_metadata: a list of the unique metadata values for each of the metadata 
# categories listed in the config file. This is generated in the main server
# function at startup.
# metadata_config: the metadata section of the config file. This does not need 
# to be specified if the config list is stored as "config" in the global
# environment.
# hide_menu (optional): a string or character vector giving the name(s) of 
# metadata categories for which to hide menus in the subset selection interface.
subset_selections_server <- function(id,
                                     object,
                                     unique_metadata,
                                     metadata_config,
                                     meta_categories,
                                     hide_menu = NULL,
                                     set_menu = NULL
                                     ){
  # Initialize module 
  moduleServer(
    id,
    # Server function for module: uses a separate set of input/output variables 
    function(input,output,session){
      # Server namespace function: used for UI elements rendered in server
      ns <- session$ns
      
      # 1. Store all input values from the UI as a reactive list ---------------
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
      
      # observeEvent(
      #   metadata_config(),
      #   ignoreNULL = FALSE,
      #   {
      #     print("Input values") 
      #     for (category in names(metadata_config())){
      #       print(glue("input${category}_selection"))
      #       print(input[[glue("{category}_selection")]])
      #     }
      #   })
      
      # 1a. Special case: when a new object is loaded, set "selections" equal to
      # all unique values in the new object
      # selections <- 
      #   eventReactive(
      #     metadata_config(),
      #     label = glue("{id}: Update Selections on Object Change"),
      #     ignoreNULL = FALSE,
      #     {
      #       # Construct list using all unique values for each category
      #       all_selected <-
      #         lapply(
      #           meta_categories(),
      #           function(category){unique_metadata()[[category]]}
      #         )
      #       
      #       names(all_selected) <- names(meta_categories())
      #       
      #       print("Finished special selections reactive")
      #       all_selected
      #     })
      
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
                    select(.data[[category]]) |>
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
                  input[[category_id]][!input[[category_id]] %in% invalid_choices],
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
          # This observer must not be ran when hide_menu is equal to NULL. In 
          # this case, the value will not be reactive and the app will crash
          ignoreNULL = TRUE,
          {
            # Only hide menus if hide_menu is not equal to NULL
            if (!is.null(hide_menu()) && length(hide_menu()) > 0){
              # Hide all menus specified in hide_menus (may be a 
              # single menu or multiple menus)
              for (menu_category in hide_menu()){
                hideElement(
                  id = ns(glue("{menu_category}_selection")),
                  #Disables automatic namespacing (for consistency in code)
                  asis = TRUE
                  )
                }
              # Show all menus in the module that are not in the 
              # hide_menu vector
              for (category in names(metadata_config())){
                if (!category %in% hide_menu()){
                  showElement(
                    id = ns(glue("{category}_selection")),
                    asis = TRUE
                  )
                }
              }
              }
            })
      }
      
      # Return the reactive list of selections 
      return(selections)
    }
  )
}



