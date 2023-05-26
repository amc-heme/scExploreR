#' Number of cells in object
#'
#' Returns the number of cells in a single cell object. Any object type 
#' supported by \code{\link[SCEPlots::unique_values]{SCEPlots::unique_values}} 
#' may be used.
#'
#' @param object A single cell object.
#'
#' @return Number of unique classes (integer).
#' 
#' @noRd
n_cells <-
  function(
    object
  ){
    SCEPlots::get_all_cells(object) |> 
      length()
  }