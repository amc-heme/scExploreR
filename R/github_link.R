#' Github_link
#'
#' Creates an <a> tag with a link to the issues page of the github repository 
#'
#' @param display_text text to display on link
#' @param href Link to github issues page
#'
#' @noRd
github_link <- 
  function(
    display_text,
    href="https://github.com/amc-heme/DataExploreShiny/issues"
  ){
    tags$a(
      display_text,
      href = href,
      # Opens link in new tab
      target = "_blank", 
      # Cybersecurity measure
      rel = "noopener noreferrer"
    )
  }