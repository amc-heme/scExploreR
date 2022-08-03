#' DGE Test Selections UI
#'
#' @param id id for module. UI-server module pairs must use the same id.
#' @param meta_choices a vector giving the metadata categories included in the 
#' config file. This is computed in the main server function upon object change.
#' 
dge_test_selections_ui <- function(id,
                                  meta_choices
                                  ){
  # Namespace function: prevents conflicts with IDs defined in other modules 
  ns <- NS(id)
  
  # UI components: 
  # 1. Test to perform (DGE or Marker identification) --------------------------
  mode_menu <- 
    selectInput(
      inputId = ns("mode"),
      label = "Choose test to perform",
      # User chooses either dge or marker identification.
      # Human- and computer- readable names given for both options
      choices = 
        c("Marker Identification" = "mode_marker",
          "Differential Expression" = "mode_dge"),
      #Marker selection on clusters is displayed at startup
      selected = "mode_marker"
      )
  
  # 2. Choice of group by variable ---------------------------------------------
  # Default group_by choice: equal to first metadata category defined in 
  # the config file
  
  # Initial choices: metadata in config file, without "none" 
  initial_choices <- meta_choices()[!meta_choices() %in% "none"]
  
  group_by_menu <- 
    selectInput(
      # Input IDs for two modes must be named differently 
      # to avoid namespace collisions
      inputId = ns("group_by"),
      # Label: changes when mode is switched to DGE
      label = "Choose metadata to use for marker identification:",
      # Remove "none" from selectable options to group by
      choices = initial_choices,
      # Avoids bugs in the event the user passes a list of choices that only
      # has "none" in it (which would have a length of zero after it is removed)
      selected = 
        if (length(initial_choices) > 0) meta_choices()[1] else character(0)
      )
  
  #uiOutput(outputId = ns("group_by_menu"))
    
    # Both menus are created (to use a separate ID for DGE group by and marker 
    # identification group by menus), and the relevant menu is shown based
    # on the selected mode
    # Container for both menus created so they can be both shown/hidden 
    # based on whether thresholding is selected.
    # div(
    #   id = "group_by_ui",
    #   selectInput(
    #     # Input IDs for two modes must be named differently 
    #     # to avoid namespace collisions
    #     inputId = ns("group_by_marker"),
    #     label = 
    #       "Choose metadata to use for marker identification:",
    #     # Remove "none" and "best_response" from selectable
    #     # options to group by
    #     choices =
    #       meta_choices()[!meta_choices() %in% "none"],
    #     # At startup, marker selection is ran with clusters 
    #     # as the group by variable.
    #     selected = "clusters"
    #   ),
    #   # DGE group by menu is hidden at the start (marker selection is 
    #   # the default mode)
    #   hidden(
    #     selectInput(
    #       # Input IDs for two modes must be named differently 
    #       # to avoid namespace collisions
    #       inputId = ns("group_by_dge"),
    #       label = 
    #         "Choose metadata to use for differential gene expression:",
    #       # Remove "none" and "best_response" from selectable 
    #       # options to group 
    #       choices = meta_choices()[!meta_choices() %in% "none"],
    #       # Clusters is selected by default
    #       selected = "clusters"
    #       )
    #     )
    #   )
    

  # 3. Classes/Groups Menu -----------------------------------------------------
  #Selection of marker classes for group by variable, or groups if mode==dge.
  # UI for the following possibilities is created here, and conditionally shown: 
  # A: Standard DGE groups
  # B: Groups based on simple expression threshold
  # C: Marker classes
  classes_menu <- 
    tagList(
      hidden(
        ## 3.A. Standard DGE Groups UI ####
        # Display menus to select two classes from the metadata 
        # category chosen as the group_by variable. Multiple 
        # selections are possible for each group.
        div(
          id = ns("standard_groups_ui"),
          # Put choices beside one another in two-column format
          pickerInput(
            inputId = ns("group_1"),
            label = "Group 1",
            # Choices are populated in the server
            choices = NULL, 
              #group_choices(),
            # Nothing selected by default
            selected = character(0),
            multiple = TRUE,
            options =
              list(
                "selected-text-format" = "count > 5",
                "size" = 10,
                "actions-box" = TRUE,
                # Label for "deselect all" button
                "deselectAllText" = "Clear selections"
              )
          ),
          pickerInput(
            inputId = ns("group_2"),
            label = "Group 2",
            choices = NULL, 
              #group_choices(),
            selected = character(0),
            multiple = TRUE,
            options =
              list(
                "selected-text-format" = "count > 5",
                "size" = 10,
                "actions-box" = TRUE,
                # Label for "deselect all" button
                "deselectAllText" = "Clear selections"
              )
          )
        ),
        ## 3.B. Feature Threshold DGE UI ####
        # Eventually, will be divided into multiple separate 
        # interfaces for different types of thresholding
        div(
          id = ns("threshold_groups_ui"),
          # Simple threshold menu
          # One feature is chosen, and cells with expression above 
          # the threshold are compared to cells below threshold 
          selectizeInput(
            inputId = ns("simple_threshold_feature"),
            label = NULL,
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
          
          # Threshold picker UI (module hides plot
          # until feature is chosen above)
          threshold_picker_ui(
            id = ns("simple_threshold"),
            plot_height = "150px"
          )
        ),
        ## 3.C. Marker Identification UI ####
        # Display menu to choose values from the 
        # group by metadata category to include as classes in 
        # marker identification. 
        div(
          id = ns("marker_classes_ui"),
          pickerInput(
            inputId = ns("marker_class_selection"),
            label = "Choose classes to include in marker computation",
            # Choices: if there are groups defined in the 
            # config file for the metadata category, build a list 
            # with the defined groups for the menu
            choices = NULL,
              # if (
              #   !is.null(metadata_config()[[group_by_initial]]$groups)){
              #   # Use group_metadata_choices() to generate list
              #   group_metadata_choices(
              #     group_info =
              #       metadata_config()[[group_by_initial]]$groups,
              #     choices =
              #       unique_metadata()[[group_by_initial]]
              #   )
              # } else {
              #   # Otherwise, use the group_choices() vector
              #   group_choices()
              # },
            
            # Select all unique values at startup
            selected = NULL, 
              #group_choices(),
            multiple = TRUE,
            options = list(
              "selected-text-format" = "count > 3",
              "size" = 10,
              # Define max options to show at
              # a time to keep menu from being cut off
              "actions-box"=TRUE
              )
          ) # End pickerInput
        )
        
      )
      
      
      
    )
    
    #uiOutput(outputId = ns("classes_menu"))
  
  # 4. Feature Expression Checkbox ---------------------------------------------
  # Option to use feature expression instead of categorical
  # metadata to define groups
  feature_expression_checkbox <-
    # This option is only visible when differential expression is the mode
    hidden(
      checkboxInput(
        inputId = ns("use_feature_expression"),
        label = "Use Feature Expression to Define Groups"
        )
      )
  
  # Combine elements above into tagList and return for display in app
  ui <- 
    tagList(
      mode_menu,
      group_by_menu,
      classes_menu,
      feature_expression_checkbox
      )
  }

#' Test Selections in DGE Tab
#'
#' @param id ID to use for module namespacing. Must match the ID passed to 
#' dge_test_selection_ui.
#' @param object The Seurat Object defined in the main server function.
#' @param unique_metadata 
#' @param metadata_config the metadata section of the config file. This does not
#' need to be specified if the config list is stored as "config" in the global
#' environment.
#' @param meta_choices 
#' @param valid_features 
#'
#' @return A reactive list with information on the selected test:
#'         ``
dge_test_selections_server <- 
  function(id,
           object,
           unique_metadata,
           metadata_config,
           meta_choices,
           valid_features
           ){
    moduleServer(
      id,
      function(input,output,session){
        # Namespace function: for dynamic UI
        ns <- session$ns
        
        # 0. Populate choices for marker classes menu (marker classes is 
        # the default menu at startup)
        initial_group_by <- meta_choices()[!meta_choices() %in% "none"][1]
        
        # In the event the initial choices list only contains "none", NA will
        # be returned above. This must be tested for to avoid errors when
        # computing initial choices
        # NULL could also result if the user has not included any metadata in
        # the config file
        initial_marker_choices <-
          if (!is.na(initial_group_by) & !is.null(initial_group_by)){
            object()@meta.data |>
              # Get unique values for the group by category
              # First category is selected at startup
              select(.data[[initial_group_by]]) |>
              unique() |>
              # Convert to vector
              unlist() |>
              # Remove names from vector
              unname() |>
              # Convert factor of choices to character vector
              as.character() |>
              # Sort choices
              str_sort(numeric = TRUE)
          } else {
            NULL
          }
        
        updatePickerInput(
          session = session,
          inputId = "marker_class_selection",
          choices =
            # If groups are defined for the current group by category
            # in the config file, display the list of grouped choices
            if (
              !is.null(metadata_config()[[initial_group_by]]$groups)
            ){
              group_metadata_choices(
                group_info =
                  metadata_config()[[initial_group_by]]$groups,
                choices =
                  unique_metadata()[[initial_group_by]]
              )
            } else {
              # Otherwise, use the initial_marker_choices vector
              initial_marker_choices
            },
          selected = initial_marker_choices
        )
        
        # 1. Process test mode -------------------------------------------------
        dge_mode <- 
          reactive({
            input$mode
            })
        
        # 2. Modify Group by Selection Menu Based on Selections ----------------
        ## 2.1 Update group by selection menu ####
        observeEvent(
          dge_mode(),
          label = "Test Selections: Update Group by Menu",
          ignoreInit = TRUE,
          {
            # Label changes based on mode, but choices remain the same
            if (dge_mode() == "mode_dge"){
              updateSelectInput(
                session = session,
                inputId = "group_by",
                label = 
                  "Choose metadata to use for differential gene expression:"
                )
            } else if (dge_mode() == "mode_marker"){
              updateSelectInput(
                session = session,
                inputId = "group_by",
                label = 
                  "Choose metadata to use for marker identification:"
                )
            }
          })
        
        ## 2.2. Show/hide Group by menu ####
        observeEvent(
          input$use_feature_expression,
          label = "Test Selections: Update Group by Menu",
          ignoreNULL = FALSE,
          ignoreInit = TRUE,
          {
            target_id = "group_by"
            
            # Hide menu when the user requests groups 
            # based on a feature expression threshold
            if (isTruthy(input$use_feature_expression)){
              hideElement(
                id = target_id
              )
            } else{
              showElement(
                id = target_id
              )
            }
          })
        
        # group_by_menu <- 
        #   eventReactive(
        #     # Reacts to dge_mode() defined in 1.
        #     dge_mode(),
        #     #ignoreNULL = FALSE,
        #     label="Test Selection: Group by Selection Menu",
        #     {
        #       if (dge_mode() == "mode_dge"){
        #         # Group by menu for DGE mode
        #         selectInput(
        #           # Input IDs for two modes must be named differently 
        #           # to avoid namespace collisions
        #           inputId = ns("group_by_dge"),
        #           label = 
        #             "Choose metadata to use for differential gene expression:",
        #           # Remove "none" and "best_response" from selectable 
        #           # options to group 
        #           choices = meta_choices()[!meta_choices() %in% "none"],
        #           # Clusters is selected by default
        #           selected = "clusters"
        #           )
        #         } else if (dge_mode() == "mode_marker"){
        #           # Group by menu for marker identification mode 
        #           selectInput(
        #             # Input IDs for two modes must be named differently 
        #             # to avoid namespace collisions
        #             inputId = ns("group_by_marker"),
        #             label = 
        #               "Choose metadata to use for marker identification:",
        #             # Remove "none" and "best_response" from selectable
        #             # options to group by
        #             choices =
        #               meta_choices()[!meta_choices() %in% "none"],
        #             # At startup, marker selection is ran with clusters 
        #             # as the group by variable.
        #             selected = "clusters"
        #             )
        #           }
        #       })
        # 
        # # Render UI
        # output$group_by_menu <- 
        #   renderUI({
        #     group_by_menu()
        #     })
        
        # 3. Classes/Groups Menu -----------------------------------------------
        ## 3.1. Show/hide possible classes/groups interfaces ####
        observe({
          standard_groups_id <- "standard_groups_ui"
          threshold_groups_id <- "threshold_groups_ui"
          marker_classes_id <- "marker_classes_ui"
          
          # Hide all initially, then show based on the test requested
          for (id in 
               c(standard_groups_id, threshold_groups_id, marker_classes_id)){
            hideElement(
              id = id
            )
          }
          
          # Menu to show: depends on DGE mode 
          if (!is.null(dge_mode())){
            if (dge_mode() == "mode_dge"){
              # For DGE: if user checks "define groups based on 
              # feature expression", show the simple threshold id
              if (isTruthy(input$use_feature_expression)){
                # Show simple threshold id when box is checked
                showElement(
                  id = threshold_groups_id
                )
              } else {
                # Otherwise, show groups menu
                showElement(
                  id = standard_groups_id
                )
              }
            } else if (dge_mode() == "mode_marker"){
              # For marker selection: display UI for defining markers
              showElement(
                id = marker_classes_id
              )
            }
          }
          
          # if (!isTruthy(input$use_feature_expression)){
          #   # When use feature expression checkbox is de-selected, show 
          #   # standard DGE UI and hide the threshold DGE UI
          #   showElement(
          #     id = standard_groups_id
          #   )
          #   hideElement(
          #     id = threshold_groups_id
          #   )
          # } else {
          #   # When the checkbox is selected, show the threshold DGE UI, and 
          #   # hide the standard UI
          #   showElement(
          #     id = threshold_groups_id
          #   )
          #   hideElement(
          #     id = standard_groups_id
          #   )
          # }
        })
        
        ## 3.2. Process group_by choice ####
        # Reactive below avoids namespace collision issue while
        # pointing group by selections to one variable
        group_by_category <- 
          reactive(
            label = "Test Selection: Process Group by Choices",
            {
              # Record input unless the user requests to define groups based 
              # on a feature expression threshold
              if (!isTruthy(isolate({input$use_feature_expression}))){
                input$group_by
              } else {
                NULL
              }
              
              # # dge_mode() must be defined to avoid errors
              # if (!is.null(dge_mode())){
              #   # Input to use depends on the selected DGE mode
              #   if (dge_mode() == "mode_dge"){
              #     return(input$group_by_dge)
              #     } else if (dge_mode() == "mode_marker"){
              #       return(input$group_by_marker)
              #       }
              #   } else NULL
              })
        
        ## 3.3. Determine unique values for the group by category ####
        # Will be options for marker classes or groups in DGE). This process 
        # will be the same regardless of whether marker selection or 
        # differential gene expression is the chosen test
        group_choices <-
          eventReactive(
            # Reacts to group_by_category() defined in 3.
            group_by_category(),
            label = "Test Selection: Compute Valid Group by Choices",
            # Errors will arise in this function and downstream if
            # group_by_category is NULL
            ignoreNULL = TRUE,
            {
              choices <- 
                object()@meta.data |>
                # Get unique values for the group by category
                select(.data[[group_by_category()]]) |>
                unique() |>
                # Convert to vector
                unlist() |>
                # Remove names from vector
                unname() |>
                # Convert factor of choices to character vector
                as.character() |>
                # Sort choices
                str_sort(numeric = TRUE)
              
              choices
              })
        
        ## 3.4. Group Marker/Group Choices Based on Config File ####
        # If the group by metadata category has groups defined in the config 
        # file, construct a list using the information so the choices are 
        # grouped in the menu according to the defined groups. This is used for
        # the marker choices menu but not for the DGE menus, as the choicesOpt
        # argument used for updating the DGE menus does not work with lists of
        # grouped choices
        grouped_group_choices <-
          reactive(
            label = "Test Selections: Group Valid Marker/Group Choices",
            {
              if (
                !is.null(metadata_config()[[group_by_category()]]$groups)
              ){
                group_metadata_choices(
                  group_info =
                    metadata_config()[[group_by_category()]]$groups,
                  choices =
                    unique_metadata()[[group_by_category()]]
                  )
              } else {
                # Otherwise, use the group_choices() vector
                group_choices()
              }
            })
        
        ## 3.5. Update groups/classes menus ####
        ### 3.5.1. Update groups menu ####
        observe(
          label = "Test Selection: Update groups Menu",
          {
            req(dge_mode())
            
            # Update proceeds when simple DGE is selected
            if (dge_mode() == "mode_dge"){
              # Isolate input$use_feature_expression (prevents choices from
              # being erased if the user switches back and forth between 
              # enabling and disabling the thresholding checkbox)
              if (!isTruthy(isolate({input$use_feature_expression}))){
                # Update both group menus
                updatePickerInput(
                  session = session,
                  inputId = "group_1",
                  # Use group_choices value computed in 3.3
                  choices = group_choices(),
                  # No groups selected by default
                  selected = character(0)
                  )
                
                updatePickerInput(
                  session = session,
                  inputId = "group_2",
                  choices = group_choices(),
                  # No groups selected by default
                  selected = character(0)
                  )
                }
              }
          })
        
        ### 3.5.2 Update classes menu ####
        observe(
          label = "Test Selection: Update classes Menu",
          {
            req(dge_mode())
            
            # Update proceeds when simple DGE is selected
            if (dge_mode() == "mode_marker"){
              updatePickerInput(
                session = session,
                inputId = "marker_class_selection",
                choices = grouped_group_choices(),
                selected = group_choices()
                )
              }
          })
        
        ## 3.6. Show/hide menu for groups based on feature expression ####
        observe(
          label = "Test Selection: Show/Hide Groups by Expression Checkbox",
          {
            # Only evaluate when dge_mode() is defined
            req(dge_mode())
            
            target_id <- "use_feature_expression"

            if (dge_mode() == "mode_dge"){
              showElement(
                id = target_id
                )
              } else {
                hideElement(
                  id = target_id
                  )
                }
            })
        
        ## UI for Marker classes/group selection 
        # Define_UI
        # classes_menu <- 
        #   eventReactive(
        #     # Reacts to group choices, defined in 3.2.
        #     group_choices(),
        #     label = "Test Selection: Classes Selection Menu",
        #     #ignoreNULL = FALSE,
        #     {
        #       if (dge_mode() == "mode_dge") {
        #         # DGE mode: UI depends on whether user selects checkbox for
        #         # groups by feature expression
        #         # Both interfaces are hidden by default and are shown based on
        #         # the condition of the checkbox
        #         tagList(
        #           # Standard DGE UI
        #           # Display menus to select two classes from the metadata 
        #           # category chosen as the group_by variable. Multiple 
        #           # selections are possible for each group.
        #           div(
        #             id = ns("standard_groups_ui"),
        #             # Put choices beside one another in two-column format
        #             pickerInput(
        #               inputId = ns("group_1"),
        #               label = "Group 1",
        #               choices = group_choices(),
        #               # Nothing selected by default
        #               selected = character(0),
        #               multiple = TRUE,
        #               options =
        #                 list(
        #                   "selected-text-format" = "count > 5",
        #                   "actions-box" = TRUE,
        #                   # Label for "deselect all" button
        #                   "deselectAllText" = "Clear selections"
        #                   )
        #               ),
        #             pickerInput(
        #               inputId = ns("group_2"),
        #               label = "Group 2",
        #               choices = group_choices(),
        #               selected = character(0),
        #               multiple = TRUE,
        #               options =
        #                 list(
        #                   "selected-text-format" = "count > 5",
        #                   "actions-box" = TRUE,
        #                   # Label for "deselect all" button
        #                   "deselectAllText" = "Clear selections"
        #                 )
        #             )
        #           ),
        #           # Feature Threshold DGE UI
        #           # Eventually, will be divided into multiple separate 
        #           # interfaces for different types of thresholding
        #           hidden(
        #             div(
        #               id = ns("threshold_groups_ui"),
        #               # Simple threshold menu
        #               # One feature is chosen, and cells with expression above 
        #               # the threshold are compared to cells below threshold 
        #               selectizeInput(
        #                 inputId = ns("simple_threshold_feature"),
        #                 label = NULL,
        #                 choices = NULL,
        #                 selected = character(0),
        #                 options = 
        #                   list(
        #                     "placeholder" = "Enter feature",
        #                     "maxItems" = 1,
        #                     "plugins" = list("remove_button"),
        #                     "create" = FALSE
        #                     )
        #                 ),
        #               
        #               # Threshold picker UI (module hides plot
        #               # until feature is chosen above)
        #               threshold_picker_ui(
        #                 id = ns("simple_threshold"),
        #                 plot_height = "150px"
        #               )
        #             )
        #           )
        #         )
        #         } else if (dge_mode() == "mode_marker") {
        #           # Marker mode: display menu to choose values from the 
        #           # group by metadata category to include as classes in 
        #           # marker identification. 
        #           pickerInput(
        #             inputId = ns("marker_class_selection"),
        #             label = "Choose classes to include in marker computation",
        #             # Choices: if there are groups defined in the 
        #             # config file for the metadata category, build a list 
        #             # with the defined groups for the menu
        #             choices = 
        #               if (
        #                 !is.null(metadata_config()[[
        #                   group_by_category()]]$groups)){
        #                 # Use group_metadata_choices() to generate list
        #                 group_metadata_choices(
        #                   group_info=
        #                     metadata_config()[[group_by_category()]]$groups,
        #                   choices = 
        #                     unique_metadata()[[group_by_category()]]
        #                   )
        #                 } else {
        #                   # Otherwise, use the group_choices() vector 
        #                   group_choices()
        #                   },
        #             
        #             # Select all unique values at startup
        #             selected = group_choices(),
        #             multiple = TRUE,
        #             options = list(
        #               "selected-text-format" = "count > 3",
        #               "size" = 10,
        #               # Define max options to show at
        #               # a time to keep menu from being cut off
        #               "actions-box"=TRUE
        #               )
        #             ) # End pickerInput
        #           }
        #       })
        # 
        # # Render UI
        # output$classes_menu <-
        #   renderUI({
        #      classes_menu()
        #     })
        
        ## 3.7. Update Standard DGE Group Menus ####
        ### 3.7.1. Update Group 2 based on Group 1 Selection ####
        # Removes the value(s) in group 1 from the group 2 menu to keep 
        # user from selecting the same groups for DE comparison
        observeEvent(
          input$group_1,
          # Menu update not necessary at startup
          ignoreInit = TRUE,
          # NULL values cause errors in this function
          ignoreNULL = FALSE,
          {
            # Define valid choices for group 2 (excludes the choice 
            # currently selected for group 1)
            new_choices <- 
              if (!is.null(input$group_1)){
                group_choices()[!group_choices() %in% input$group_1] |> 
                  # Sort choices
                  str_sort(numeric = TRUE)
              } else {
                group_choices() |> 
                  # Sort choices
                  str_sort(numeric = TRUE)
              }
              
            # Create a boolean vector from choices that are now invalid
            disable_boolean <-
              if (!is.null(input$group_1)){
                group_choices() %in% input$group_1
              } else {
                # If no choices are selected for group 1, disable none 
                # A boolean vector of all FALSE elements is created 
                rep_len(FALSE, length(group_choices()))
              }              
            
            # Define values from group 2 to leave selected
            # If currently selected choices in group 2 are in group 1, define
            # which choices are in the group. These will be de-selected. 
            if (any(input$group_2 %in% input$group_1)){
              # Can't use input$group2[!input$group_2 %in% input$group_1]
              # Not sure why
              select_boolean <- !input$group_2 %in% input$group_1
              select_values <- input$group_2[select_boolean]
            } else {
              # If there is no overlap, preserve original selection for group 2
              select_values <- input$group_2
            }
            
            # Update group 2 input with new valid choices
            updatePickerInput(
              session,
              inputId = "group_2",
              label = "Group 2",
              # Choices never change, but they must be specified 
              # for choicesOpt updates to apply
              choices = group_choices(),
              # If any selected choices from group 2 are now selected for 
              # group 1, de-select those choices. 
              selected = select_values,
              # Disable choices that are selected for group 1
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
            })
        
        ### 3.7.2. Update Group 1 based on Group 2 Selection ####
        observeEvent(
          input$group_2,
          # Menu update not necessary at startup
          ignoreInit = TRUE,
          # NULL values cause errors in this function
          ignoreNULL = FALSE,
          {
            # Define valid choices for group 2 (excludes the choice
            # currently selected for group 1)
            new_choices <-
              if (!is.null(input$group_2)){
                group_choices()[!group_choices() %in% input$group_2] |>
                  # Sort choices
                  str_sort(numeric = TRUE)
              } else {
                group_choices() |> 
                  # Sort choices
                  str_sort(numeric = TRUE)
                }

            # Create a boolean vector from choices that are now invalid
            disable_boolean <-
              if (!is.null(input$group_2)){
                group_choices() %in% input$group_2
              } else {
                # If no choices are selected for group 2, disable none 
                # A boolean vector of all FALSE elements is created 
                rep_len(FALSE, length(group_choices()))
              }
              
            # Define values from group 1 to leave selected
            # If currently selected choices in group 1 are in group 2, define
            # which choices are in the group. These will be de-selected. 
            if (any(input$group_1 %in% input$group_2)){
              # Can't use input$group1[!input$group_1 %in% input$group_2]
              # Not sure why
              select_boolean <- !input$group_1 %in% input$group_2
              select_values <- input$group_1[select_boolean]
            } else {
              # If there is no overlap, preserve original selection for group 1
              select_values <- input$group_1
            }

            # Update group 1 input with new valid choices
            updatePickerInput(
              session,
              inputId = "group_1",
              label = "Group 1",
              # Choices never change, but they must be specified
              # for choicesOpt updates to apply
              choices = group_choices(),
              # De-select and disable choices that are selected for group 2
              selected = select_values,
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
          })
        
        ## 3.8. Simple Feature Threshold Processing ####
        ### 3.8.1. Populate Feature Choices for Simple Thresholding ####
        # Populate menu with valid features when simple thresholding is selected
        observeEvent(
          label = "Test Selection: Fill Available Features",
          input$use_feature_expression,
          {
            if (!isTruthy(input$simple_threshold_feature)){
              # Update unless the user has entered a feature (updating in this 
              # case is not necessary, and it would erase the user's input)
              updateSelectizeInput(
                session,
                # Do not namespace IDs in update* functions
                inputId = "simple_threshold_feature", 
                choices = valid_features(), 
                selected = character(0),
                server = TRUE
                )
            }
            
          })
        
        ### 3.8.2. Server for Interactive Ridge Plot ####
        simple_threshold <- 
          threshold_picker_server(
            # Do not namespace module server function IDs 
            id = "simple_threshold",
            object = object,
            feature = reactive({input$simple_threshold_feature}),
            showhide_animation = TRUE
            )
        
        # 4. Process Test Selections -------------------------------------------
        # Group 1: process input if DGE is the mode selected
        group_1 <- 
          reactive({
            # dge_mode() must be defined to avoid errors
            if (!is.null(dge_mode())){
              if (dge_mode() == "mode_dge") input$group_1 else NULL
              }
            })
        
        # Group 1: process input if DGE is the mode selected
        group_2 <-
          reactive({
            # dge_mode() must be defined to avoid errors
            if (!is.null(dge_mode())){
              if (dge_mode() == "mode_dge") input$group_2 else NULL
              }
            })
        
        # Selected marker classes: process input if marker 
        # identification is the mode selected
        classes_selected <- 
          reactive({
            # dge_mode() must be defined to avoid errors
            if (!is.null(dge_mode())){
              if (dge_mode() == "mode_marker"){
                input$marker_class_selection
                } else NULL
              }
            })
        
        # 5. Return information ------------------------------------------------
        # Return a reactive list with inputs, depending on the selected DGE mode
        selections <- 
          reactive(
            label = "Test Selection: Return Values",
            {
              # dge_mode() must be defined to avoid errors
              if (!is.null(dge_mode())){
                if (dge_mode() == "mode_dge"){
                  # DGE Mode: multiple possible return formats
                  if (isTruthy(input$use_feature_expression)){
                    # A. Groups defined using simple thresholding 
                    # Return feature name and threshold value
                    return(
                      list(
                        `dge_mode` = dge_mode(),
                        # Indicate simple threshold being used for groups
                        `group_mode` = "simple_threshold",
                        `threshold_feature` = 
                          input$simple_threshold_feature,
                        `threshold_value` = simple_threshold()
                        )
                      )
                    } else {
                      # B. Standard DGE
                      # Return selected groups
                      return(
                        list(
                          `dge_mode` = dge_mode(),
                          # Indicate that standard DGE is selected
                          `group_mode` = "standard",
                          `group_by` = group_by_category(),
                          `group_1` = group_1(),
                          `group_2` = group_2()
                          )
                        )
                      }
                  } else if (dge_mode() == "mode_marker"){
                    # Include classes when marker identification 
                    # is the selected mode
                    return(
                      list(
                        `dge_mode` = dge_mode(),
                        # Leave group_mode defined for compatability with 
                        # conditional statements in DGE tab, but set to "none"
                        `group_mode` = "none",
                        `group_by` = group_by_category(),
                        `classes_selected` = classes_selected()
                        )
                      )
                  }
                }
              }) # End reactive
        
        return(selections)
        })
}
