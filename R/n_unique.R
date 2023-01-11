#' Number of Classes in Seurat Object
#'
#' Returns the number of unique values in the Seurat object, for the specified
#' metadata column.
#'
#' @param object A Seurat object.
#' @param metadata_column Name of a column in the metadata table for the object.
#'
#' @return Number of unique classes (integer).
#' 
#' @noRd
n_unique <-
  function(
    object,
    metadata_column
  ){
    object@meta.data[[metadata_column]] |> 
      unique() |> 
      length()
  }