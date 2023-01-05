#' Error Handler Function
#' 
#' Compares an error condition to the errors in the provided error_list and 
#' shows the notification that matches the error. If no matches are found, the 
#' function will display a generic notification giving the error text and the 
#' link to the github issues page.  
#' 
#' @param session Shiny session object.
#' @param cnd_message the error message read by tryCatch.
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
    duration = NULL
    ){
    # Error match: conditional that is set to TRUE when an error is found, 
    # signaling the function not to run the code for displaying a generic
    # error message
    error_match <- FALSE
    # Loop through all defined error types (error_list)
    for (error_type in error_list){
      # If the condition message (the error that is returned) matches the error 
      # message of a stored error type (error_type$err_message), show the 
      # notification associated with that error type
      print(glue("Testing error {error_type$err_message}"))
      if (grepl(pattern = error_type$err_message, x = cnd_message)){
        print("match.")
        print(error_type$err_message)
        print(cnd_message)
        
        # Display Notification
        showNotification(
          ui = error_type$notification, 
          # Duration = NULL will make the message 
          # persist until dismissed (default)
          duration = duration,
          id = session$ns(error_type$notification_id),
          session = session
        )
        
        # If error is found, set error_match to TRUE and break the loop 
        error_match <- TRUE
        break
      }
      
    }
    
    # If all error types are looped through and no match is found, 
    # display a generic error message
    if (error_match == FALSE){
      #Define UI for generic error
      other_err_ui <- 
        icon_notification_ui(
          icon_name = "skull-crossbones",
          glue("Error: {cnd_message}. Please "),
          github_link("report this issue"),
          " with a screenshot of the app window."
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
}