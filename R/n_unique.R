#' Object metadata: number of unique values
#'
#' Returns the number of unique values in a single cell object, for the specified
#' metadata variable. Any object type supported by 
#' \code{\link[SCEPlots::unique_values]{SCEPlots::unique_values}} may be used.
#'
#' @param object A Seurat object.
#' @param meta_var Name of a metadata variable.
#'
#' @return Number of unique classes (integer).
#' 
#' @noRd
n_unique <-
  function(
    object,
    meta_var
  ){
    SCEPlots::unique_values(
      object = object, 
      var = meta_var
      ) |> 
      length()
  }