#' Multiple Custom Title Module
#'
#' @param id ID to use for module. All inputs and outputs created will be
#' namespaced using this ID.
#'
#' @return Module UI
#' 
#' @noRd
custom_title_multi_ui <- 
  function(
    id
    ){
    # Namespace function: prevents conflicts with IDs defined in other modules 
    ns <- NS(id)
    
    div(
      class = "compact-options-container",
      tags$b("Enter Custom Titles for Each Panel:"),
      # Conditional ui: a table with labels and text inputs
      uiOutput(outputId = ns("multi_ui")),
      # Update button to return vector from choices made in the table of inputs
      actionButton(
        inputId = ns("reset_all"),
        label = "Reset All",
        icon = icon("redo-alt"),
        class = "button-ghost multi-reset-all"
      ),
      actionButton(
        inputId = ns("update"),
        label = "Update",
        icon = icon("sync"),
        class = "button-primary multi-update"
        )
      )
  }

#' Multiple Custom Title Module
#'
#' @param id ID to use for module. All inputs and outputs created will be
#' namespaced using this ID.
#' @param object A Seurat object or subset.
#' @param split_by The split by variable chosen by the user in the plot module. 
#' @param label_header Unused
#' @param input_header Unused
#'
#' @return Module Server instance 
#' 
#' @noRd
custom_title_multi_server <- 
  function(
    id,
    object,
    split_by,
    label_header = "",
    input_header = ""
    ){
    moduleServer(
      id,
      function(input, output, session){
        ns <- session$ns
        
        # Create list for storing observers
        rv <- reactiveValues(observers = list())
        
        # Vector of split by Groups ####
        # Generate vector of groups (must match method used in the feature plot 
        # wrapper function)
        groups_vector <- 
          reactive({
            req(split_by())
            
            object()@meta.data[[split_by()]] |> 
              unique() |> 
              str_sort(numeric = TRUE)
            })
        
        # Conditional UI for Current Plot ####
        ui <-
          reactive({
            if (!split_by() == "none"){
              # UI: for each split by category, a text box with a reset button
              # beside it
              tagList(
                lapply(
                  1:length(groups_vector()),
                  function(i, groups_vector){
                    tagList(
                      div(
                        class = "custom-multi-individual-input",
                        div(
                          class = "custom-multi-text-container",
                          textInput(
                            inputId = ns(paste0("entry", as.character(i))),
                            # Default value is the default name for the split by 
                            # category (as defined in the Seurat object)
                            value = groups_vector[i],
                            label = NULL
                            )
                          ),
                        actionButton(
                          # Id contains multi-entry-reset for easy 
                          # access by CSS selectors 
                          inputId = 
                            ns(paste0("multi_entry_reset", as.character(i))),
                          label = "",
                          icon = icon("redo-alt")
                          )
                        )
                      )
                    },
                  groups_vector()
                )
              )
            }
          })
        
        output$multi_ui <-
          renderUI({
            ui()
            })
        
        # Create Observers for individual reset buttons
        observeEvent(
          groups_vector(),
          {
            # Clear any observers that currently exist, if they exist
            # For diagnostic purposes
            # n_destroy <- 0 
            # print("Observer query")
            for (observer in rv$observers){
              # print(class(observer))
              if ("Observer" %in% class(observer)){
                # n_destroy <- n_destroy + 1
                # print("Observer detected and marked for destruction.")
                observer$destroy()
              }
            }
           # print(glue("Total Number of observers destroyed: {n_destroy}"))
            
            # if (!is.null(observers)){
            #   if (length(observers) > 1){
            #     for (i in 1:length(observers)){
            #       print(glue("Destroying observer {i}"))
            #       print("Content")
            #       print(observers[[as.character(i)]])
            #       print("Class")
            #       print(class(observers[[as.character(i)]]))
            #       #observers[[as.character(i)]]$destroy()
            #     }
            #   }
            # }
            
            if (!is.null(groups_vector())){
              
              # Create an observer for each text input currently displayed
              rv$observers <-
                lapply(
                  1:length(groups_vector()),
                  function(i, groups_vector){
                    observeEvent(
                      # Observer responds to the individual 
                      # reset button of text field i
                      input[[paste0("multi_entry_reset", as.character(i))]],
                      label = 
                        glue("multi_entry Reset Button: Field {as.character(i)}"),
                      ignoreNULL = FALSE,
                      ignoreInit = TRUE,
                      {
                        # print("Reset observer triggered")
                        # print(glue("ID: {id}"))
                        # print(glue("Observer {as.character(i)}"))
                        updateTextInput(
                          session = session,
                          inputId = paste0("entry", as.character(i)),
                          value = groups_vector[i]
                          )
                      })
                  },
                  groups_vector()
                )
              
              # for (i in 1:length(groups_vector())){
              #   print("Observer creation")
              #   print(glue("i = {i}"))
              #   print(
              #     glue(
              #       'eventExpr = input[[{paste0("multi_entry_reset", as.character(i))}]]'
              #     )
              #   )
              #   
              #   rv$observers[[as.character(i)]] <-
              #     observeEvent(
              #       # Observer responds to the individual reset button 
              #       # of text field i
              #       input[[paste0("multi_entry_reset", as.character(i))]],
              #       label = 
              #         glue("multi_entry Reset Button: Field {as.character(i)}"),
              #       ignoreNULL = FALSE,
              #       ignoreInit = TRUE,
              #       {
              #         print("Reset observer triggered")
              #         print(glue("ID: {id}"))
              #         print(glue("Observer {as.character(i)}"))
              #         updateTextInput(
              #           session = session,
              #           inputId = paste0("entry", as.character(i)),
              #           value = groups_vector()[i]
              #         )
              #       })
              #   
              # }
            }
          })
        
        # Observer for Reset All Button ####
        observeEvent(
          input$reset_all,
          ignoreNULL = FALSE,
          ignoreInit = TRUE,
          {
            # Reset each input to the default value for the panel (as defined in
            # the split.by category)
            for (i in 1:length(groups_vector())){
             # print(glue('Update input {paste0("entry", as.character(i))} with value {groups_vector()[i]}'))
              
              updateTextInput(
                session = session,
                inputId = paste0("entry", as.character(i)),
                value = groups_vector()[i]
              )
            }
          })
        
        # Return Vector of Input Values ####
        vector <-
          eventReactive(
            c(input$update, groups_vector()),
            ignoreNULL = FALSE,
            {
              if (isTruthy(groups_vector())){
                sapply(
                  1:length(groups_vector()),
                  function(i){
                    # Input id of current iteration
                    id <- paste0("entry", as.character(i))
                    
                    # If input i is defined, return the input
                    if (!is.null(input[[id]])){
                      input[[id]]
                    } else {
                      # Otherwise, return the default entry for position i,
                      # as defined in groups_vector()
                      groups_vector()[i]
                    }
                    
                  })
              } else {
                NULL
              }
            })
        
        # Return contents of input vector
        vector
      })
  }