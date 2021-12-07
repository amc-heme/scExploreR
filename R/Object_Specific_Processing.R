#Functions for Processing Object-Specific Data
#Functions used to create feature list
assay_entry <- function(assay, prefix_machine, suffix_human="", dropdown_title=assay){
  #Creates a list with assay information
  assay_info <- list()
  #"assay" argument: gives key of assay used to extract relevant information 
  #from the Seurat object. This will be automatically detected in the config app.
  assay_info$assay <- assay
  #prefix_machine argument: gives the key prefix used to reference a feature 
  #in the given assay when forming feature, violin, and dot plots. 
  #At this time, this must be set by the user. This is not very user-friendly 
  #though. Is there a different way we can do this?
  assay_info$prefix_machine <- prefix_machine
  #suffix_human argument (optional): user can specify a suffix to appear after 
  #features in the current assay to indicate which assay they belong to in the 
  #dropdown menu (for example, can put "(Surface Protein)" after the ADT entries). 
  assay_info$suffix_human <- suffix_human
  #Dropdown title: the title used to partition features by type in the dropdown 
  #menu. When using the feature search, this will appear above the divider separating
  #results by assay type. This will also be the key used to access the features 
  #associated with this assay in the list created by valid_features(). Because 
  #of this, it must be defined, and it must have a value unique to the assay. 
  #The config app will require the user to choose a unique name, but for now 
  #this will not be checked by the function.
  assay_info$dropdown_title <- dropdown_title
  
  return(assay_info)
}

#build_assay_list(): used to compile entries from the assay_entry() function 
#into a full list.
assay_list <- function(...){
  #Construct a list from the assay_info objects passed to the function
  assay_list <- list(...)
  #Return the list
  return(assay_list)
}

#feature_list_all: builds a list of features using the 
#included assays as defined in assay_list 
#The Seurat object must be passed to the function to determine 
#valid features for each assay
feature_list_all <- function(sobj,
                             assay_list,
                             numeric_metadata=FALSE,
                             numeric_metadata_title="Metadata Features") {
  #Features from the assays provided will be categorized by assay type in a list.
  valid_features <- list()
  #Loop through each assay provided
  for (assay_entry in assay_list){
    #Fetch the features included in the object under the current assay
    features <- rownames(sobj[[assay_entry$assay]])
    #Generate human-readable feature names
    human_readable <- paste0(features, assay_entry$suffix_human)
    #Generate machine-readable feature names (format "<prefix>_<feature_name>")
    machine_readable <- paste0(assay_entry$prefix_machine, features)
    #Zip above into a list of key-value pairs (human-readable features as keys, 
    #machine-readable features as values)
    feature_pairs <- split(machine_readable, human_readable)
    #Add the list of key-value pairs to valid_features, using the "dropdown_title" 
    #element of the assay_entry list as the key for the assay
    valid_features[[assay_entry$dropdown_title]] <- feature_pairs
  }
  
  #After looping through all assays: if numeric metadata is requested, add that column.
  if (numeric_metadata==TRUE){
    #First, fetch all metadata columns in object.
    meta_cols <- names(sobj@meta.data)
    #Next, select columns that have numeric or integer values
    numeric_cols <- meta_cols[sapply(meta_cols, FUN=function(x){
      (class(sobj@meta.data[[x]])=="numeric") || (class(sobj@meta.data[[x]])=="integer")
    })]
    #Convert to list and include in valid_features, using the dropdown menu 
    #title set by the "numeric_metadata_title" argument
    valid_features[[numeric_metadata_title]] <- as.list(numeric_cols)
  }
  
  #Return the valid_features list
  return(valid_features)
}


