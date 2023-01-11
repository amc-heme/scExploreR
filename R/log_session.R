#' log_session
#' 
#' Prints a Linux style log of the session ID to the console, using log_info() from the rlog package. 
#' 
#' This function should be called within the server function of a Shiny app or module. It may be called either within or outside of a reactive context. 
#'
#' @param session Shiny session object
#'
#' 
#' @noRd
#' 
log_session <- function(session){
  rlog::log_info(
    glue("Session {session$token}")
    )
}