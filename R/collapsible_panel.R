#' Collapsible Panel
#' 
#' Creates a panel with a header that will toggle between hiding and showing 
#' its contents when the user clicks the header. The header of the panel behaves
#' as an action button; therefore, an inputId is required. 
#' 
#' Must include the files "collapsible_panel.css" and "collapsible_panel.js" 
#' in the UI function for this to work properly.
#' 
#' @param ... A list of tags to include in the body of the panel.
#' @param inputId Input ID used for panel. The panel behaves as an action 
#' button, and may be used as such in reactive expressions by referencing 
#' the provided input ID.
#' @param label Text to include in the header of the collapsible panel.
#' @param active If TRUE, panel appears in the open position upon app startup. 
#' If FALSE (the default) the panel starts in the closed position
#' @param transparent If TRUE, panel will appear transparent (default is FALSE).
#'
#' @noRd
                              
collapsible_panel <- 
  function(
    ...,
    inputId,
    label = NULL,
    active = FALSE,
    transparent = FALSE,
    size = "l"
    ){
    if (!size %in% c("s", "l")){
      stop("Invalid entry for `size`. Please enter `s` or `l`.")
    }
    
    # Value: a required component of action button and included in the 
    # actionButton source code. I'm not sure why. 
    value <- 
      restoreInput(
        id = inputId, 
        default = NULL
        )
    # Value is passed to `data-val` in the button tag (also don't know why)
    
    # CSS Class for header: "collapsible", plus other options as specified
    header_class <- 
      paste(
        c(
          "collapsible",
          if (active == TRUE) "active" else NULL,
          if (transparent == TRUE) "transparent" else NULL,
          if (size == "s") "collapsible-small"
        ),
        # CSS format: each class is separated by a space
        collapse = " "
        )
    
    content_class <-
      paste(
        c(
          "content",
          if (transparent == TRUE) "transparent" else NULL,
          if (size == "s") "collapsible-small"
        ),
        # CSS format: each class is separated by a space
        collapse = " "
      )
    
    # Use taglist to return button tag for header and div tag for content
    tagList( 
      # Header of panel: built with button tag. 
      # The label the user enters will be header text 
      button_html <- 
        tags$button(
          type = "button",
          #Set the id of the button to inputId
          id = inputId,
          # Required component for actionButton behavior
          `data-val` = value,
          # Use header class computed above
          class = header_class, 
          # Header text of panel: use user-defined label
          if (!is.null(label)) tags$span(label)
          ),
      
      # Pass all content to div tag
      # If active == TRUE, the style attribute display will be set to "block" to 
      # display the content upon loading
      if (active == TRUE){
        content_html <- 
          div(
            ...,
            class = content_class,
            style = "display: block;"
            )
        } else {
          # Otherwise, the default value of none will be used to 
          # hide content initially
          content_html <- 
            div(
              ...,
              class = content_class
              )
          }
      )#End taglist
    }
