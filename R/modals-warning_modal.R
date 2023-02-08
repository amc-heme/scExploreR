#' Warning Modal
#'
#' Creates the UI for a warning modal, with "cancel" and "continue" buttons. This function should be called within a showModal function to display the modal, and reactive expressions that depend on the buttons pressed should be placed in the same module where showModal is called from. 
#'
#' @param confirmId The ID for the continue button, to be referenced by 
#' observers/reactive expressions that execute in response to the button. This 
#' mmust be unique within the app or module to avoid namespace collisions.
#' @param cancelId The ID for the cancel button, to be referenced by 
#' observers/reactive expressions that execute in response to the button. This 
#' mmust be unique within the app or module to avoid namespace collisions.
#' @param text Text to display for the warning message.
#'
warning_modal <-
  function(
    confirmId,
    cancelId,
    text
    ){
    modalDialog(
      title = "Warning",
      footer = 
        tagList(
          actionButton(
            inputId = cancelId,
            label = "Cancel",
            class = "button-ghost extra-space"
            ),
          actionButton(
            inputId = confirmId,
            label = "Continue",
            class = "button-primary extra-space"
            )
        ),
      size = "l",
      # Modal content
      div(
        tags$p(text)
        )
      )
  }