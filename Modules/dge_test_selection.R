# DGE Interface Module

# Processes Inputs for subsetting and specifying groups for marker analysis or 
# differential gene expression.

dge_test_selection_ui <- function(id,
                                  meta_choices
                                  ){
  # Namespace function: prevents conflicts with IDs defined in other modules 
  ns <- NS(id)
  
  # UI components: 
  # 1. Test to perform (DGE or Marker identification)
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
  
  # 2. choice of group by variable 
  group_by_menu <- uiOutput(outputId = ns("group_by_menu"))

  # 3. Selection of marker classes for group by variable, or groups if mode==dge
  # (dynamically updated based on group_by choice)
  classes_menu <- uiOutput(outputId = ns("classes_menu"))
  
  # Combine elements above into tagList and return for display in app
  ui <- 
    tagList(
      mode_menu,
      group_by_menu,
      classes_menu
      )
  }

# Server function
# Arguments
# id: the namespace to use for the module. UI-server function pairs should use the same id.
# object: The Seurat Object defined in the main server function
# metadata_config: the metadata section of the config file. This does not need to
# be specified if the config list is stored as "config" in the global environment.
dge_test_selection_server <- 
  function(id,
           object,
           unique_metadata,
           metadata_config,
           meta_choices
           ){
    moduleServer(
      id,
      function(input,output,session){
        # Namespace function: for dynamic UI
        ns <- session$ns
        
        # 1. Process test mode -------------------------------------------------
        dge_mode <- 
          reactive({
            input$mode
            })
        
        # 2. Dynamic UI for Group by Selection----------------------------------
        # Define UI
        group_by_menu <- 
          eventReactive(
            # Reacts to dge_mode() defined in 1.
            dge_mode(),
            #ignoreNULL = FALSE,
            label="Test Selection: Group by Selection Menu",
            {
              if (dge_mode() == "mode_dge"){
                # Group by menu for DGE mode
                selectInput(
                  # Input IDs for two modes must be named differently 
                  # to avoid namespace collisions
                  inputId = ns("group_by_dge"),
                  label = 
                    "Choose metadata to use for differential gene expression:",
                  # Remove "none" and "best_response" from selectable 
                  # options to group 
                  choices = meta_choices()[!meta_choices() %in% "none"],
                  # Clusters is selected by default
                  selected = "clusters"
                  )
                } else if (dge_mode() == "mode_marker"){
                  # Group by menu for marker identification mode 
                  selectInput(
                    # Input IDs for two modes must be named differently 
                    # to avoid namespace collisions
                    inputId = ns("group_by_marker"),
                    label = 
                      "Choose metadata to use formarker identification:",
                    # Remove "none" and "best_response" from selectable
                    # options to group by
                    choices =
                      meta_choices()[!meta_choices() %in% "none"],
                    # At startup, marker selection is ran with clusters 
                    # as the group by variable.
                    selected="clusters"
                    )
                  }
              })
        
        # Render UI
        output$group_by_menu <- 
          renderUI({
            group_by_menu()
            })
        
        # 3. Process group_by choice -------------------------------------------
        # Reactive below avoids namespace collision issue while
        # pointing group by selections to one variable
        group_by_category <- 
          reactive(
            label = "Test Selection: Process Group by Choices",
            {
              # dge_mode() must be defined to avoid errors
              if (!is.null(dge_mode())){
                # Input to use depends on the selected DGE mode
                if (dge_mode() == "mode_dge"){
                  return(input$group_by_dge)
                  } else if (dge_mode() == "mode_marker"){
                    return(input$group_by_marker)
                    }
                } else NULL
              })
        
        # 4. Determine unique values for the group by category -----------------
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
                str_sort(numeric=TRUE)
              })
        
        # 5. Dynamic UI for Marker classes/group selection ---------------------
        # Define_UI
        classes_menu <- 
          eventReactive(
            # Reacts to group choices, defined in 4.
            group_choices(),
            label = "Test Selection: Classes Selection Menu",
            #ignoreNULL = FALSE,
            {
              if (dge_mode() == "mode_dge") {
                # DGE mode: display menus to select two classes from the 
                # metadata category chosen as the group_by variable. This can be
                # expanded to include groups based on multiple metadata
                # selections
                tagList(
                  # Put choices beside one another in two-column format
                  pickerInput(
                    inputId = ns("group_1"),
                    label = "Group 1",
                    choices = group_choices(),
                    selected = group_choices()[1],
                    multiple = TRUE,
                    options =
                      list(
                        "selected-text-format" = "count > 5",
                        "actions-box" = TRUE,
                        # Label for "deselect all" button
                        "deselectAllText" = "Clear selections"
                      )
                  ),
                  pickerInput(
                    inputId = ns("group_2"),
                    label = "Group 2",
                    choices = group_choices(),
                    selected = group_choices()[2],
                    multiple = TRUE,
                    options =
                      list(
                        "selected-text-format" = "count > 5",
                        "actions-box" = TRUE,
                        # Label for "deselect all" button
                        "deselectAllText" = "Clear selections"
                      )
                    )
                  
                  ) # End tagList
                } else if (dge_mode() == "mode_marker") {
                  # Marker mode: display menu to choose values from the 
                  # group by metadata category to include as classes in 
                  # marker identification. 
                  pickerInput(
                    inputId = ns("marker_class_selection"),
                    label = "Choose classes to include in marker computation",
                    # Choices: if there are groups defined in the 
                    # config file for the metadata category, build a list 
                    # with the defined groups for the menu
                    choices = 
                      if (
                        !is.null(metadata_config()[[
                          group_by_category()]]$groups)){
                        # Use group_metadata_choices() to generate list
                        group_metadata_choices(
                          group_info=
                            metadata_config()[[group_by_category()]]$groups,
                          choices = 
                            unique_metadata()[[group_by_category()]]
                          )
                        } else {
                          # Otherwise, use the group_choices() vector 
                          group_choices()
                          },
                    
                    # Select all unique values at startup
                    selected = group_choices(),
                    multiple = TRUE,
                    options = list(
                      "selected-text-format" = "count > 3",
                      "size" = 10,
                      # Define max options to show at
                      # a time to keep menu from being cut off
                      "actions-box"=TRUE
                      )
                    ) # End pickerInput
                  }
              })
        
        # Render UI
        output$classes_menu <-
          renderUI({
             classes_menu()
            })
        
        # 6. Update Group 2 Menu Based on Group 1 Selection --------------------
        # Removes the value in group 1 from the group 2 menu to keep 
        # user from selecting the same groups for DE comparison
        
        ## 6.1. Update Group 2 based on Group 1 Selection ####
        observeEvent(
          input$group_1,
          # Menu update not necessary at startup
          ignoreInit=TRUE,
          # NULL values cause errors in this function
          ignoreNULL = TRUE,
          {
            # Define valid choices for group 2 (excludes the choice 
            # currently selected for group 1)
            new_choices <- 
              group_choices()[!group_choices() %in% input$group_1] |> 
              # Sort choices
              str_sort(numeric = TRUE)
            
            # Create a boolean vector from choices that are now invalid
            disable_boolean <- 
              group_choices() %in% input$group_1
            
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
              # for choicesOpt updates apply
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
        
        ## 6.2. Update Group 1 based on Group 2 Selection ####
        observeEvent(
          input$group_2,
          # Menu update not necessary at startup
          ignoreInit=TRUE,
          # NULL values cause errors in this function
          ignoreNULL = TRUE,
          {
            # Define valid choices for group 2 (excludes the choice
            # currently selected for group 1)
            new_choices <-
              group_choices()[!group_choices() %in% input$group_2] |>
              # Sort choices
              str_sort(numeric = TRUE)

            # Create a boolean vector from choices that are now invalid
            disable_boolean <-
              group_choices() %in% input$group_2

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
              # for choicesOpt updates apply
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
        
        # 7. Process Test Selections -------------------------------------------
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
        
        # 8. Return information ------------------------------------------------
        # Return a reactive list with inputs, depending on the selected DGE mode
        selections <- 
          reactive(
            label="Test Selection: Return Values",
            {
              # dge_mode() must be defined to avoid errors
              if (!is.null(dge_mode())){
                if (dge_mode() == "mode_dge"){
                  # Include groups when DGE is the selected mode
                  return(
                    list(
                      `dge_mode` = dge_mode(),
                      `group_by` = group_by_category(),
                      `group_1` = group_1(),
                      `group_2` = group_2()
                      )
                    )
                  } else if (dge_mode() == "mode_marker"){
                    # Include classes when marker identification 
                    # is the selected mode
                    return(
                      list(
                        `dge_mode` = dge_mode(),
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
