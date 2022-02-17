# unique_values 
# Returns the unique values for a given metadata category in a Seurat object.

# Arguments
# object: a Seurat object. This can be either the full object or a subset. This
# is a reactive variable.
# category: the metadata category for which to compute unique values.
unique_values <- function(object,
                          category){
  # Display an error if 'object' is not a reactive variable
  stopifnot(`'object' must be a reactive variable.` = is.reactive(object))
  
  unique(object()@meta.data[[category]])
}
