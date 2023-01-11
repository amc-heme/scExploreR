#' error_data
#'
#' error_data() will store information for an error message to be handled, and 
#' the desired Shiny notification to show for that error. 
#' 
#' @param message The error message as it appears in the console.
#' @param notification_ui a shiny.tag or shiny.tag.list object, and it is 
#' recommended to use the icon_notification_ui() function
#' @param notification_id id to apply to the notification container
#'
#' @noRd
error_data <- 
  function(
    message,
    notification_ui,
    notification_id
  ){
    # Create a list to store information on the error, including 
    # the message and the notification UI
    error <- list()
    error$err_message <- message
    error$notification <- notification_ui
    error$notification_id <- notification_id
    # Return the error info
    error
  }