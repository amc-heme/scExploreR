#' xlim_modal_ui 
#'
#' @param id ID to use for module elements. This is called from within the 
#' server function, and thus will always be equal to the 
#'
xlim_modal_ui <- 
  function(
    id,
    current_xlim
    ){
    # Namespace function: prevents conflicts with IDs defined in other modules
    ns <- NS(id)
    
    modalDialog(
      title = "Change X-axis limits",
      footer = 
        tagList(
          actionButton(
            inputId = ns("confirm"),
            class = "button-primary float-right",
            style = "margin-left: 10px;",
            label = "Confirm"
            ),
          actionButton(
            inputId = ns("cancel"),
            class = "button-ghost float-right",
            label = "Cancel"
            )
          ),
      # Modal content
      div(
        class = "inline-containers",
        textInput(
          inputId = ns("lower_lim"),
          label = "Lower Bound:",
          value = current_xlim[1]
          ),
        textInput(
          inputId = ns("upper_lim"),
          label = "Upper Bound:",
          value = current_xlim[2]
          )
        ),
      actionButton(
        inputId = ns("restore_lim"),
        label = "Restore original Limits",
        class = "button-ghost"
        )
      )
    }

#' xlim_modal_server
#'
#' @param id ID to use for module server instance.
#' @param reactive_trigger A reactive expression giving the input value of an
#' action button, action link, or a reactive trigger (see makeReactiveTrigger 
#' in "../R"). If the trigger used is an action button or link, the input value 
#' of the button must be wrapped in a `reactive({})` call before passing to the
#' server (for example, pass `reactive({input$button})` instead of 
#' `input$button`).
#'
xlim_modal_server <- 
  function(
    id,
    reactive_trigger,
    current_xlim,
    xlim_orig
    ){
    moduleServer(
       id,
       function(input, output, session){
         # 1. Show modal dialog ####
         observeEvent(
           # Responds to the trigger specified when this server is called
           reactive_trigger(),
           ignoreInit = TRUE,
           ignoreNULL = FALSE,
           {
             print("Reactive trigger activated")
             
             showModal(
               # Calls UI instance of this module
               xlim_modal_ui(
                 id = id,
                 current_xlim = current_xlim()
                 )
               )
           })
         
         # 2. Respond to Modal Buttons ####
         ## 2.1. Cancel button ####
         # Closes modal without applying changes
         observeEvent(
           input$cancel,
           ignoreInit = TRUE,
           ignoreNULL = FALSE,
           {
             removeModal()
           })
         
         })
}
