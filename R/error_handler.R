### Icon Notification Function ####
# (updated for error handling functions) 
# Defines the HTML to be printed within a notification box. The function takes 
# the name of a Font Awesome icon and a message as input, and will display the 
# icon and the message inline. The message is defined using
# any number of UI elements. 
icon_notification_ui_2 <- 
  function(
    icon_name,
    ...
    ){
    span(
      # Icon (inline and enlarged)
      icon(
        icon_name, 
        style="display: inline-block; font-size: 1.7em;"
        ),
      # Message (inline with icon, font slightly enlarged)
      span(
        tagList(...),
        style="font-size: 1.17em;"
        )
      )
    }
###

### Github_link ####
# creates an <a> tag with a link to the issues page of the github repository 
github_link <- 
  function(
    display_text,
    href="https://github.com/amc-heme/DataExploreShiny/issues"
    ){
    tags$a(
      display_text,
      href=href,
      # Opens link in new tab
      target="_blank", 
      # Cybersecurity measure
      rel="noopener noreferrer"
      )
    }

# error_data() ####
# error_data() will store information for an error message to 
# be handled, and the desired Shiny notification to show for that error.
# The argument notification_ui should be a shiny.tag or shiny.tag.list object, 
# and it is recommended to use the icon_notification_ui() function defined 
# in this source file. 
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

# Error Handler Function #### 
#Given an error condition message returned in a tryCatch() 
#statement and a list connecting error messages to UI for shiny notifications,
#error_handler() will compare the error condition to the errors in the list and 
#show the notification that matches the error. If no matches are found, the 
#function will display a generic notification giving the error code and the link
#to the github issues page. The URL of the issues page can be specified with the
#issue_href argument. The duration the message remains on the screen can be set 
#and is NULL by default; this will result in a persistent notification. 
error_handler <- 
  function(
    session,
    cnd_message,
    error_list,
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
        icon_notification_ui_2(
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