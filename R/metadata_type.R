#' Metadata Type
#'
#' Takes a single-cell object and a metadata variable, and returns the class of the 
#' metadata variable.
#'
#' @param object a single cell object (currently, Seurat and SingleCellExperiment
#' are supported).
#' @param meta_var the name of a metadata variable to assess for data type.
#'
#' @noRd
metadata_type <- function(object, meta_var){
  # Fetch metadata variable, determine data type/class
  values <- 
    SCUBA::fetch_metadata(
      object, 
      vars = meta_var, 
      return_class = "vector"
      )

  # Simplify metadata type: "character" and "factor" classes are reported as 
  # "Categorical", while "numeric" and "integer" classes are reported 
  # as "Numeric"
  if (inherits(values, c("character", "factor"))){
    type <- "Categorical"
  } else if (inherits(values, c("numeric", "integer"))){
    type <- "Numeric"
  } else if (inherits(values, c("logical"))){
    type <- "Logical"
  } else {
    # Other metadata classes may exist: warn user for unexpected classes
    warning(
      "Unexpected class for metadata column ",
      meta_var,
      ": ",
      paste(class(values), collapse = ', ')
      )
    
    # Return the class, or if multiple classes, the classes 
    # concatenated with commas
    type <- paste(class(values), collapse = ',')
  }
  
  type
}
