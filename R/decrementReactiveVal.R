#' ReactiveVal Utilities
#' 
#' decrementReactiveVal takes a Shiny reactiveVal object and decreases it 
#' by one.
#'
#' @param val A Shiny reactiveVal object
#'
#' @examples
#' 
#' # counter begins equal to 1
#' counter <- reactiveVal(1)
#' # counter will now be equal to 0
#' counter <- decrementReactiveVal(counter)
#' 
#' @noRd
decrementReactiveVal <- 
  function(val){
    # Take a reactiveVal object and decrease by one
    # use `val()` to fetch value of object
    val_decrement <- val() - 1
    # use `val(<new_value>)` to set value of object
    # (object is modified in place, so <- is not needed. Errors result if it 
    # is added)
    val(val_decrement) 
    
    val
  }