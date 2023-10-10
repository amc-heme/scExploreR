#' Config Spp Sidebar Panel
#' 
#' Creates a sidebarPanel specific to the config app that responds to the size
#' of the screen. 
#'
#' @param ... Content to place in the sidebar panel. 
#' @param class CSS classes to apply to the sidebar panel.
#'
#' @noRd
config_app_sidebar_panel <- 
  function(
    ...,
    class = NULL
    ){
    # Use an empty string to define the class if it is not specified
    if (is.null(class)) {
      class <- ""
      }
    
    sidebarPanel(
      # The use of width=0 allows the width to be specified for different window 
      # sizes using the class argument (using the width argument will apply the 
      # style for all screens with at least a "medium" size viewport)
      width = 0,
      # Column width specifications vary based on viewport size and are given using 
      # Bootstrap classes (R Studio creates a "small" window by default on a MacBook
      # pro) https://getbootstrap.com/docs/3.3/css/#responsive-utilities
      class = paste0("shinysc-sidebar-panel col-sm-6 col-md-5 col-lg-4 ",class),
      # Pass content to sidebarPanel
      tagList(...)
    )
  }