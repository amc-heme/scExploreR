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
      label = glue("Restrict by {config_i$label}"),
      # choices: filled using the unique_metadata list
      # If the metadata category has defined groups, sort choices
      # into a named list based on the groups. This will show choices
      # divided by group in the pickerInput menu.
      choices = if(!is.null(config_i$groups)){
        # Use group_metadata_choices() to generate list
        group_metadata_choices(
          group_info = config_i$groups,
          choices = unique_metadata()[[category]]
        )
      } else {
        # If groups are not defined, use the vector of 
        # choices from unique_metadata
        unique_metadata()[[category]]
      },
      # selected: all choices selected by default
      selected = unique_metadata()[[category]],
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
        "actions-box" = TRUE
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
                                     set_menu = NULL){
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
                function(category){input[[glue("{category}_selection")]]}
                )
            
            # Add categories from metadata file to list names
            names(selections_list) <-  names(metadata_config())
            
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
            
            print("Finished standard selections reactive")
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
      # 2.1. filters_applied: a boolean that is TRUE when a subset has been 
      # filtered (this may be changed as the filter code is developed)
      filters_applied <- 
        eventReactive(
          selections(),
          ignoreNULL = FALSE,
          {
            # Check if selections() is shorter than the original list of choices
            # used to generate the menus (account for hidden menus if these are 
            # specified).
            if (!is.null(hide_menu) && is.reactive(hide_menu)){
              # If hide menu is specified, remove the hidden category from the 
              # list of original choices
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
            if (not_equal==TRUE){
              return(TRUE)
              } else {
                return(FALSE)
                }
            })
      
      # 2.2. Create UI for "Reset Filter button"
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
                label = "Reset Filters",
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
      ## 3.1. Filter menus 
      # Observers are created for each filter menu. Since the menus now change
      # when different objects are loaded, the observers must be defined 
      # reactively. This is not ideal as this means a new set of observers will
      # be created each time a new dataset is loaded, but this shouldn't be 
      # fatal for performance at this time.
      observeEvent(
        unique_metadata(),
        label = glue("{id}: Create filter menu observers"),
        {
          # available_choices: a reactive list of the choices available in 
          # each menu that updates when menu options are filtered. At startup, 
          # the list contains all of the unique values for each category. It 
          # will be updated with the current valid choices based on filter 
          # criteria selected
          available_choices <- reactiveValues()
          
          # Initialize available_choices with unique metadata values
          # for each category 
          for (category in isolate(meta_categories())){
            available_choices[[category]] <- 
              isolate(unique_metadata()[[category]])
          }
          
          # Use lapply to build observers for each metadata category
          lapply(
            X = isolate(meta_categories()),
            FUN = function(current_category){
              # Define input ID for current category in current object
              # Formula: <current_category>_selection
              current_input_id <- 
                glue("{current_category}_selection")
              
              observeEvent(
                # Each observer responds to the change in the current category 
                # iterated through using lapply
                input[[current_input_id]],
                ignoreNULL = FALSE,
                # Observer should not run at startup (filtering is 
                # unnecessary in this case)
                ignoreInit = TRUE,
                {
                  # When the observer is triggered, change all other menus
                  # based on the selections chosen
                  
                  # Define values of other categories
                  other_categories <-
                    meta_categories()[!meta_categories() %in% current_category]
                  
                  # For each other category, update the picker inputs based on 
                  # the values selected in the current menu (current category)
                  for (other_category in other_categories){
                    # Define the input id for the "other_category"
                    other_input_id <- 
                      glue("{other_category}_selection")
                    
                    # Generate list of valid choices for the category
                    valid_choices <-
                      object()@meta.data |> 
                      # Filter object for selections made in current category
                      filter(
                        .data[[current_category]] %in%
                          input[[current_input_id]]
                      ) |>
                      # Select the column for the *other* category
                      select(.data[[other_category]]) |>
                      # Fetch unique values
                      unique() |>
                      # Convert to character vector (must use both unlist()
                      # and as.character() to get correct values for
                      # categories that are factors)
                      unlist() |>
                      as.character()
                    
                    # process_choices tests if the category has groups defined 
                    # in the config file. If so, valid choices are sorted into 
                    # those groups, and if not, the choices are returned 
                    # unchanged.
                    choices_processed <- 
                      process_choices(
                        metadata_config,
                        category = other_category,
                        choices = valid_choices
                        )
                    
                    # Next, update each picker input with valid choices
                    # *but only*
                    # 1. if the options have not been narrowed down yet by the 
                    #    user (prevents an infinite loop)
                    # 2. if the current category has been narrowed down 
                    #    (prevents looping upon startup), and
                    
                    #    (maybe)
                    # 3. if at least one value is selected for the current 
                    #    category (prevents options from disappearing when the 
                    #    user clicks 'deselect all' in the picker menus)
                    
                    # Define initial choices and fetch selected choices for 
                    # current and other categories
                    #initial_choices_other <- 
                    #  unique_metadata[[other_category]]
                    selected_choices_other <- 
                      input[[other_input_id]]
                    
                    #available_choices[[current_category]] <-
                    #  unique_metadata[[current_category]]
                    selected_choices_current <- 
                      input[[current_input_id]]
                    
                    # Print statements to test specific menus
                    if (current_category == "tissue" & other_category == "htb"){
                      print(glue("Id: {id}"))
                      print(glue("Current category: {current_category}"))
                      print(glue("Other category: {other_category}"))
                      print(
                        glue("Number of selected choices (current): 
                         {length(selected_choices_current)}")
                      )
                      print(
                        glue("Number of available choices (current): 
                         {length(available_choices[[current_category]])}")
                      )
                      print("Available choices (current)")
                      print(available_choices[[current_category]])
                      print("Selected choices (current)")
                      print(selected_choices_current)
                      print(
                        glue("setequal test (current): 
                         {setequal(selected_choices_current, 
                         available_choices[[current_category]])}")
                      )
                      print(
                        glue("Number of selected choices (other): 
                         {length(selected_choices_other)}")
                      )
                      print(
                        glue("Number of available choices (other): 
                         {length(available_choices[[other_category]])}")
                      )
                      print(
                        glue("setequal test (other): 
                         {setequal(selected_choices_other, 
                         available_choices[[other_category]])}")
                      )
                    }
                    
                    # Use setequal to test if all possible choices are selected
                    if (
                      setequal(selected_choices_other, 
                               available_choices[[other_category]])# &
                      #!setequal(selected_choices_current, 
                      #          available_choices[[current_category]]) 
                    ){
                      # Update input
                      updatePickerInput(
                        session,
                        inputId = other_input_id,
                        # Choices: use the list of group choices 
                        # if it is defined
                        choices = choices_processed,
                        selected = valid_choices,
                        options =
                          list(
                            "selected-text-format" = "count > 5",
                            "actions-box"=TRUE
                          )
                      )
                      
                      # Store the new choices in the available_choices
                      # reactive list
                      available_choices[[other_category]] <- valid_choices
                    }
                  }
                }) #End observeEvent
            }) #End lapply
        })
      
      ## 3.2. Reset button
      observeEvent(
        input$reset_filter,
        label = "Reset Filter Menus",
        {
          # When the reset button is pressed, update all of the menus 
          for (category in meta_categories()){
            # Define original values using unique_metadata
            initial_values <- unique_metadata()[[category]]
            
            # If the metadata category has a groups property, build a list of
            # grouped metadata values
            initial_values_processed <- 
              process_choices(
                metadata_config,
                category = category,
                choices = initial_values
                )
            
            # Update picker input
            updatePickerInput(
              session,
              inputId = glue("{category}_selection"),
              # Choices: use the list of group choices if it is defined
              choices = initial_values_processed,
              selected = initial_values,
              options =
                list(
                  "selected-text-format" = "count > 5",
                  "actions-box"=TRUE
                )
            )
            
            # Also reset the available_choices list
            available_choices[[category]] <- initial_values
          }
        })
      
      # 4. Hide menus, if specified by the user. -------------------------------
      # hide_menu is an optional argument that is NULL in modules where it is 
      # not specified. Since hide_menu is intended to be reactive, observers 
      # that use it will crash the app when NULL values are passed to them.
      if (!is.null(hide_menu) && is.reactive(hide_menu)){
        observeEvent(hide_menu(),
                     label = "Subset Selections: Hide Menu",
                     # This observer must not be ran when hide_menu is equal to 
                     # NULL. In this case, the value will not be reactive and the 
                     # app will crash
                     ignoreNULL = TRUE,
                     {
                       # Only hide menus if hide_menu is not equal to NULL
                       if (!is.null(hide_menu()) && length(hide_menu()) > 0){
                         # Hide all menus specified in hide_menus (may be a 
                         # single menu or multiple menus)
                         for (menu_category in hide_menu()){
                           hideElement(id = ns(glue("{menu_category}_selection")), 
                                       #Disables automatic namespacing (for 
                                       #consistency in code)
                                       asis = TRUE)
                         }
                         # Show all menus in the module that are not in the 
                         # hide_menu vector
                         for (category in names(metadata_config())){
                           if (!category %in% hide_menu()){
                             showElement(id = ns(glue("{category}_selection")),
                                         asis = TRUE)
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



