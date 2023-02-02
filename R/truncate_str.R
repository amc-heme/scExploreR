#' Abbreviate Long Strings
#'
#' @param str an input string to condense
#' @param max_length maximum length of the input string. If the string is longer than max_length 
#' characters, it will be condensed to a length of max_length, with "..." added at the 
#' end.
#'
#' @noRd
truncate_str <- 
  function(
    str,
    max_length
  ){
    if (nchar(str) > max_length){
      # If the string is more than max_length characters long, 
      condensed <- substr(str, 1, max_length)
      condensed <- paste0(condensed, "...")
      
      return(condensed)
    } else {
      return(str)
    }
  }