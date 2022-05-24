multi_text_input_ui <- function(id,
                                label = NULL
                                ){
  # Namespace function: prevents conflicts with IDs defined in other modules 
  ns <- NS(id)
  
  div(
    class = "compact-options-container",
    # Add label if defined
    if (!is.null(label)){
      tags$b(label)
    } else NULL,
    # Conditional UI: a table with labels and text inputs
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

multi_text_input_server <- function(id,
                                    default_vector
                                    ){
  moduleServer(
    id,
    function(input, output, session){
      ns <- session$ns
      
      # Create list for storing observers
      rv <- reactiveValues(observers = list())
      
      # Conditional UI for Current Plot ####
      ui <-
        reactive({
          # UI: for each split by category, a text box 
          # with a reset button beside it
          tagList(
            lapply(
              1:length(default_vector()),
              function(i, default_vector){
                tagList(
                  div(
                    class = "custom-multi-individual-input",
                    div(
                      class = "custom-multi-text-container",
                      textInput(
                        inputId = ns(paste0("entry", as.character(i))),
                        # Use default vector for value, if defined
                        value = 
                          if (!is.null(default_vector)){
                            default_vector[i]
                            } else "",
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
              default_vector()
            )
          )
          
        })
      
      output$multi_ui <-
        renderUI({
          ui()
        })
      
      # Create Observers for individual reset buttons
      observeEvent(
        default_vector(),
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
          
          
          # Create an observer for each text input currently displayed
          rv$observers <-
            lapply(
              1:length(default_vector()),
              function(i, default_vector){
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
                      value = 
                        if (!is.null(default_vector)){
                          default_vector[i]
                          } else ""
                      )
                  })
              },
              default_vector()
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
        })
      
      # Observer for Reset All Button ####
      observeEvent(
        input$reset_all,
        ignoreNULL = FALSE,
        ignoreInit = TRUE,
        {
          # Reset each input to the default value for the panel (as defined in
          # the split.by category)
          for (i in 1:length(default_vector())){
            # print(glue('Update input {paste0("entry", as.character(i))} with value {groups_vector()[i]}'))
            
            updateTextInput(
              session = session,
              inputId = paste0("entry", as.character(i)),
              value = 
                if (!is.null(default_vector())){
                  default_vector()[i]
                  } else ""
            )
          }
        })
      
      # Return Vector of Input Values ####
      vector <-
        eventReactive(
          c(input$update, default_vector()),
          ignoreNULL = FALSE,
          {
            if (isTruthy(default_vector())){
              sapply(
                1:length(default_vector()),
                function(i){
                  # Input id of current iteration
                  id <- paste0("entry", as.character(i))
                  
                  # If input i is defined, return the input
                  if (!is.null(input[[id]])){
                    input[[id]]
                  } else {
                    # Otherwise, return the default entry for position i,
                    # as defined in groups_vector()
                    default_vector()[i]
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