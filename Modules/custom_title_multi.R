custom_title_multi_ui <- function(id
                                  ){
  # Namespace function: prevents conflicts with IDs defined in other modules 
  ns <- NS(id)
  
  div(
    class = "multi-options-container",
    tags$b("Enter Custom Titles for Each Panel:"),
    # Conditional ui: a table with labels and text inputs
    uiOutput(outputId = ns("multi_ui")),
    # Update button to return vector from choices made in the table of inputs
    actionButton(
      inputId = ns("update"),
      label = "Update",
      icon = icon("sync"),
      class = "button-primary multi-update"
      ),
     actionButton(
       inputId = ns("reset_all"),
       label = "Reset All",
       icon = icon("redo-alt"),
       class = "button-ghost multi-reset-all"
       )
    )
}

custom_title_multi_server <- function(id,
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
      observers <- list()
      
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
                          ns(paste0("multi-entry-reset", as.character(i))),
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
        ignoreNULL = FALSE,
        {
          
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
            print(glue('Update input {paste0("entry", as.character(i))} with value {groups_vector()[i]}'))
            
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
          input$update,
          ignoreNULL = FALSE,
          ignoreInit = TRUE,
          {
            if (isTruthy(groups_vector())){
              sapply(
                1:length(groups_vector()),
                function(i){
                  input[[paste0("entry", as.character(i))]]
                })
            } else {
              NULL
            }
          })
      
      # Return contents of input vector
      vector
    })
}