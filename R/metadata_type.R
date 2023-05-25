#' Metadata Type
#'
#' Takes a Seurat object and a metadata variable, and returns the class of the 
#' metadata variable.
#'
#' @param object a single cell object (currently, Seurat and SingleCellExperiment
#' are supported).
#' @param meta_var the name of a metadata variable to assess for data type.
#'
#' @noRd
metadata_type <- function(object, meta_var){
  # Get unique values of metadata field for display of summary statistics
  values <- SCEPlots::unique_values(object, var = meta_var)
  
  # Determine type of metadata (class of values)
  class <- class(values)
  
  # Simplify metadata type: "character" and "factor" classes are reported as 
  # "categorical", while "numeric" and "integer" classes are reported 
  # as "numeric"
  if (class == "character" || class == "factor"){
    type <- "Categorical"
  } else if (class == "numeric" || class == "integer"){
    type <- "Numeric"
  } else if (class == "logical"){
    type <- "Logical"
  } else {
    # Other metadata classes may exist: warn user for unexpected classes
    warning(
      glue("Unexpected class for metadata column {meta_var}: {class}.")
      )
    
    type <- class
  }
  
  type
}
