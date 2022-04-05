# unique_values 
# Returns the unique values for a given metadata category in a Seurat object.

# Arguments
# object: a Seurat object. This can be either the full object or a subset. This 
# is a reactive-agnostic parameter (can be either reactive or non-reactive).
# category: the metadata category for which to compute unique values.
unique_values <- function(object,
                          category){
  # Call object as either a reactive or non-reactive variable depending on how
  # it is entered
  if (is.reactive(object)){
    unique(object()@meta.data[[category]])
  } else {
    unique(object@meta.data[[category]])
  }
  
}
