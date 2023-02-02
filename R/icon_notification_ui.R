#' Icon Notification Function
#' 
#' Defines the HTML to be printed within a notification box. The function takes 
#' the name of a Font Awesome icon and a message as input, and will display the 
#' icon and the message inline. The message is defined using any number of UI 
#' elements. 
#' 
#' @param icon_name 
#' @param ... 
#'
#' @noRd
icon_notification_ui <- 
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