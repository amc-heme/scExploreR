#' Config app main panel
#'
#' Creates the main panel of the config app (which uses a different width than the standard mainPanel, and responds to the width of the browser)
#'
#' @param ... Content to place in the main panel.
#' @param class CSS classes to apply to the main panel.
#'
#' @noRd
config_app_main_panel <- 
  function(
    ...,
    class=NULL
    ){
    # Use an empty string to define the class if it is not specified
    if (is.null(class)){
      class <- ""
      }
    
    # Use width = 0 to define column widths using Bootstrap classes
    mainPanel(
      width = 0,
      class = 
        paste0(
          "shinysc-main-panel col-sm-6 col-md-7 col-lg-8 ",
          class
        ),
      #Pass content to mainPanel 
      tagList(...)
    )
  }