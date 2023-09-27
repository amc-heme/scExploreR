#' Number of cells in object
#'
#' Returns the number of cells in a single cell object. Any object type 
#' supported by \code{\link[SCUBA::unique_values]{SCUBA::unique_values}} 
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
    SCUBA::get_all_cells(object) |> 
      length()
  }
