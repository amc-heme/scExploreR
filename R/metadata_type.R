#' Metadata Type
#'
#' Takes a Seurat object and a metadata field, and returns the class of the 
#' metadata field
#'
#' @param object a Seurat object.
#' @param metadata_field a column name of the object's `@meta.data` table
#'
#' @noRd
metadata_type <- function(object, metadata_field){
  # Get unique values of metadata field for display of summary statistics
  values <- unique(object@meta.data[[metadata_field]])
  
  # Determine type of metadata (class of values) for display in column
  # Must call @meta.data first with arbitrary metadata 
  # (object[[<metadata>]]) will return a dataframe
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
      glue("Unexpected class for metadata column {metadata_field}: {class}.")
      )
    
    type <- class
  }
  
  return(type)
}
