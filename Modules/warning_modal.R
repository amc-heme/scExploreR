#' warning_modal_ui 
#'
#' @param id ID to use for module elements. This is called from within the 
#' server function, and thus will always be equal to the 
#'
warning_modal_ui <- 
  function(
    id
  ){
    # Namespace function: prevents conflicts with IDs defined in other modules
    ns <- NS(id)
    
    modalDialog(
      title = "Warning",
      footer = 
        tagList(
          actionButton(
            inputId = ns("continue"),
            class = "button-primary float-right",
            style = "margin-left: 10px;",
            label = "Continue"
          ),
          # A special button is required to close the modal (actionButtons
          # will behave strangely when tied to removeModal()). The modalButton
          # from Shiny uses HTML to close the modal: for details on the code
          # see the Shiny source code: https://rdrr.io/cran/shiny/src/R/modal.R
          modalButton(
            label = "Cancel"
          )
          # actionButton(
          #   inputId = ns("cancel"),
          #   class = "button-ghost float-right",
          #   label = "Cancel"
          # )
        ),
      # Modal content
      tags$p("Data for the threshold table will be erased if the designated
             ADT assay is changed. Continue?")
    )
  }

#' warning_server
#'
#' @param id ID to use for module server instance.
#' @param reactive_trigger A reactive expression giving the input value of an
#' action button, action link, or a reactive trigger (see makeReactiveTrigger 
#' in "../R"). If the trigger used is an action button or link, the input value 
#' of the button must be wrapped in a `reactive({})` call before passing to the
#' server (for example, pass `reactive({input$button})` instead of 
#' `input$button`). If using a reactive trigger, pass the trigger name and 
#' `$depend`, without the parenthesis. 
#'
warning_modal_server <- 
  function(
    id,
    reactive_trigger
  ){
    moduleServer(
      id,
      function(input, output, session){
        # 1. Show modal dialog ####
        observeEvent(
          # Responds to the trigger specified when this server is called
          reactive_trigger(),
          label = glue("{id}: Show modal"),
          ignoreInit = TRUE,
          ignoreNULL = FALSE,
          {
            print("Show modal")
            
            showModal(
              # Calls UI instance of this module
              warning_modal_ui(
                id = id
              ),
              session = session
            )
            
            print("add class function")
            # After showing the modal, add the button-ghost class to the
            # modalButton
            addClass(
              # Modal buttons have a data-bs-dismiss attribute set to "modal"
              selector = "button[data-dismiss = 'modal']",
              asis = TRUE
            )
          })
        
        
        # 2. Respond to Modal Buttons ####
        ## 2.1. Cancel button ####
        # Closes modal without proceeding
        # observeEvent(
        #   input$cancel,
        #   label = glue("{id}: Remove modal"),
        #   ignoreInit = TRUE,
        #   ignoreNULL = FALSE,
        #   {
        #     print("Remove modal")
        #     removeModal()
        #   })
        
        ## 2.2. Accept button ####
        # Closes modal and proceeds with the action that prompted the modal
        observeEvent(
          # Event expression: when setting the expresion to react when 
          # input$continue changes (standard behavior, the modal will either 
          # close immediately after it is opened when ignoreNULL is TRUE, or
          # not react to the button until it is clicked twice when ignoreNULL
          # is FALSE). This is because the actionButton is being re-created 
          # each time the modal is opened, resetting the value of the button
          # to zero. When ignoreNULL is FALSE, the expression will not execute,
          # and when ignoreNULL is TRUE, the expression will execute immediately
          # after the modal is opened. Since a removeModal() expression is ran,
          # the modal will close immediately after opening. When using isTruthy,
          # the modal will close whenever the button is set to a value greater 
          # than zero, which will only ocurr after the button is pressed, and
          # not when it is initialized with the modal.
          isTruthy(input$continue),
          {
            print("Continue")
            removeModal()
          })
        
        # The value of the continue action button is passed from the server.
        # Whenever the continue button is pressed, an action is triggered via
        # observers in the parent module that respond to the value returned by
        # the action button.
        return(
          reactive({input$accept})
          )
      })
  }
