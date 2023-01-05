#' unique_values
#' 
#' Returns the unique values for a given metadata category in a Seurat object.
#'
#' @param object a Seurat object. This can be either the full object or a subset.
#' @param category the metadata variable (category) for which to compute unique values.
#' @return a character vector giving the unique values in the defined 
#' metadata variable. 
#'
#' @noRd
unique_values <- 
  function(
    object,
    category
    ){
    # Call object as either a reactive or non-reactive variable depending on how
    # it is entered
    if (is.reactive(object)){
      unique(object()@meta.data[[category]])
    } else {
      unique(object@meta.data[[category]])
    }
    
  }
