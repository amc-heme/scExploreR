#' Converts an input in bytes to GB
#'
#' @param bytes Integer giving the size of an object in bytes
#'
#' @return A string. Gives the input value in GB (SI Units)
#'
#' @noRd
to_GB <- function(bytes){
  # Divide by 10^9 and round to two digits
  gb <- 
    round(
      bytes/(10^9),
      digits = 3
      )
  
  # Return value with 'GB' at end
  glue("{gb} GB")
}
