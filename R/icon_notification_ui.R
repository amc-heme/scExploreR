#' Icon Notification Function
#' 
#' Defines the HTML to be printed within a notification box. The function takes 
#' the name of a Font Awesome icon and a message as input, and will display the 
#' icon and the message inline. The message is defined using any number of UI 
#' elements. 
#' 
#' @param icon_name The name of the icon to use (based on the fontawesome library)
#' @param icon_class Optional classes to apply to the icon. These may be user-defined, or they may be based on the fontawesome library (for example,"fas", "fal", and "far" will display alternate styles of supported icons.)
#' @param notification_style Style to apply to the notification body. 
#' @param ... Text or shiny.tag elements to be placed as message content.
#'
#' @noRd
icon_notification_ui <- 
  function(
    icon_name,
    icon_class = NULL,
    notification_style = NULL,
    ...
  ){
    div(
      style = notification_style,
      # Icon (inline and enlarged)
      icon(
        icon_name, 
        class = icon_class,
        style = "display: inline-block; font-size: 1.7em;"
      ),
      # Message (inline with icon, font slightly enlarged)
      span(
        tagList(...),
        style="font-size: 1.17em;"
      )
    )
  }