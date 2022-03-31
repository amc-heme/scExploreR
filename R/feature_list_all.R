# feature_list_all: builds a list of features using the Seurat object and the 
# assay information defined in the config file. 

# Arguments
# object: the full Seurat object to be used in the app.
# assay_config: the assay section of the config file. 
# numeric_metadata: if TRUE, include numeric metadata columns as searchable 
# features
# numeric_metadata_title: the title to give numeric metadata features in the 
# dropdown menu that appears under the feature text entry.
feature_list_all <- function(object,
                             assay_config,
                             numeric_metadata = FALSE,
                             numeric_metadata_title = "Metadata Features"
                             ){
  # Features from each assay provided will be categorized by
  # assay type in a list.
  valid_features <- list()
  # Loop through each assay provided
  for (assay_entry in assay_config()){
    # Fetch the features included in the object under the current assay
    assay_features <- rownames(object()[[assay_entry$assay]])
    # Generate human-readable feature names
    if (assay_entry$suffix_human != ""){
      # If a human-readable suffix is defined, add it to the features 
      # with a space and parentheses
      human_readable <- 
        paste0(assay_features, " (", assay_entry$suffix_human, ")")
    } else {
      human_readable <- assay_features
    }
    
    # Generate machine-readable feature names 
    # (format "<key>_<feature_name>", where key is the key used to fetch 
    # features from an assay in the Seurat object)
    machine_readable <- paste0(assay_entry$key, assay_features)
    # Zip above into a list of key-value pairs 
    # (human-readable features as keys, machine-readable features as values)
    feature_pairs <- split(machine_readable, human_readable)
    # Add the list of key-value pairs to valid_features, using the 
    # "dropdown_title" element of the assay_entry list as the key for the assay
    valid_features[[assay_entry$dropdown_title]] <- feature_pairs
  }
  
  # After looping through all assays: if numeric metadata is requested, 
  # add that column.
  if (numeric_metadata==TRUE){
    # First, fetch all metadata columns in object.
    meta_cols <- names(object()@meta.data)
    # Next, select columns that have numeric or integer values
    numeric_cols <- 
      meta_cols[
        # sapply function will test each column for 
        # the conditional specified in FUN
        sapply(
          meta_cols, 
          FUN = function(x){
            (class(object()@meta.data[[x]])=="numeric") || 
              (class(object()@meta.data[[x]])=="integer")
            }
          )
        ]
    
    # Convert to list and include in valid_features, using the dropdown menu 
    # title set by the "numeric_metadata_title" argument
    valid_features[[numeric_metadata_title]] <- as.list(numeric_cols)
  }
  
  # Return the valid_features list
  return(valid_features)
}