# Config applet main panel
applet_main_panel <- function(...,
                              class=NULL){
  # Use an empty string to define the class if it is not specified
  if (is.null(class)){
    class <- ""
  }
  
  # Use width=0 to define column widths using Bootstrap classes
  mainPanel(
    width=0,
    class = 
      paste0(
        "shinysc-main-panel col-sm-6 col-md-7 col-lg-8 ",
        class
      ),
    #Pass content to mainPanel 
    tagList(...)
  )
}