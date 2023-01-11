#' ReactiveVal Utilities
#' 
#' incrementReactiveVal takes a Shiny reactiveVal object and increases it 
#' by one.
#'
#' @param val A Shiny reactiveVal object
#'
#' @examples
#' 
#' # counter begins equal to 1
#' counter <- reactiveVal(1)
#' # counter will now be equal to 2
#' counter <- incrementReactiveVal(counter)
#' 
#' @noRd
incrementReactiveVal <- 
  function(val){
    # Take a reactiveVal object and increase by one
    # use `val()` to fetch value of object
    val_increment <- val() + 1
    # use `val(<new_value>)` to set value of object 
    # (object is modified in place, so <- is not needed. Errors result if it 
    # is added)
    val(val_increment)
    
    val
  }