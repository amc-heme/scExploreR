#' Error Handler Function
#' 
#' Compares an error condition to the errors in the provided error_list and 
#' shows the notification that matches the error. If no matches are found, the 
#' function will display a generic notification giving the error text and the 
#' link to the github issues page.  
#' 
#' @param session Shiny session object.
#' @param cnd_message the error message read by tryCatch.
#' @param source_reactive character vector to display source triggering error
#' @param error_list a list of list objects generated with the error_data() 
#' function from this package, connecting error messages to UI for shiny 
#' notifications.
#' @param issue_href The URL of the github issues page
#' @param duration the duration for which the notification displays. The default 
#' is NULL, which results in the notification remaining until dismissed.
#'
#' @noRd
error_handler <- 
  function(
    session,
    cnd_message,
    error_list = list(),
    issue_href = "https://github.com/amc-heme/DataExploreShiny/issues", 
    duration = NULL,
    source_reactive = "Unknown source" #default value
    ){
      #log full error to the console 
      rlog::log_error(glue("Error was triggered from the following reactive
                           expression: {source_reactive}: {cnd_message}"))
      #Define UI for generic error
      other_err_ui <- 
        icon_notification_ui(
          icon_name = "skull-crossbones",
          tagList(
          glue("An error occurred. Please"),
          github_link("report this issue"),
          " with a screenshot of the app window."
          )
        )
      
      # Display Notification
      showNotification(
        ui = other_err_ui,
        # Duration=NULL will make the message 
        # persist until dismissed (default)
        duration = duration,
        id = session$ns("other_error"),
        session = session
        )
}