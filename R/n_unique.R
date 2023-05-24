#' Number of Classes in Seurat Object
#'
#' Returns the number of unique values in the Seurat object, for the specified
#' metadata column.
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