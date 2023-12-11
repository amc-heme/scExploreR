#' Behavior of 
#' 
#' Directs the threshold_picker module as to whether the user is selecting a 
#' single threshold or a range, based on the state of the optional reactive 
#' variable passed to the module.
#'
#' @param mode mode reactive variable passed to threshold_picker module
#'
#' @return "range" or "threshold". The behavior is interpereted by downstream 
#' code in the module.
#' 
#' @noRd
threshold_picker_behavior <-
  function(mode){
    if (is.reactive(mode)){
      # If mode is a reactive variable, function will not 
      # run until mode is defined
      req(mode())
      if (mode() == "range"){
        # If a range is selected, clicks will record the lower and upper
        # bound of a range
        behavior <- "range"
      } else {
        # Draw a single solid line on the x-axis to denote a threshold
        behavior <- "threshold"
      }
    } else {
      # Default behavior: single line
      behavior <- "threshold"
    }
    
    # Return the behavior
    behavior
  }