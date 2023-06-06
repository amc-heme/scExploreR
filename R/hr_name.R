#' hr_name
#'
#' Given the machine-readable name of an input feature, return the human 
#' readable name. This is used for modifying the titles of plots, which use the
#' machine-readable name by default.
#'
#' @param machine_readable_name the name of the feature with the assay key as 
#' a prefix (for example, RNA features use the '_rna' key)
#' @param assay_config the assays section of the config file, loaded in the 
#' main app
#' @param adt_threshold_key
#' @param use_suffix if TRUE, add the human readable suffix defined in 
#' assay_config in parentheses (for example, if the label is "Surface Protein",
#' this will be added to the end of the title returned.)
#'
#' @noRd
hr_name <- 
  function(
    machine_readable_name, 
    assay_config,
    #adt_threshold_key,
    use_suffix = TRUE
    ){
    # Error if a multi-element vector is passed for the machine-readable name
    if (length(machine_readable_name) > 1){
      stop("Error: more than one machine-readable name entered.")
    }
    
    for (assay in assay_config){
      # Given the machine-readable name in question, test each assay to see if 
      # the assay key is in the name
      if (grepl(assay$key, machine_readable_name)){
        # When a match is found, remove the assay's machine-readable 
        # prefix with sub()
        HR <- sub(assay$key, "", machine_readable_name)
        
        if (use_suffix == TRUE){
          # If use_suffix == TRUE, add the assay's human-readable suffix to the 
          # title, if defined
          if (assay$suffix_human != ""){
            # If defined, add to the end of the name in parentheses.
            HR <- glue("{HR} ({assay$suffix_human})")
          } else {
            HR 
          }
        } else {
          # If use_suffix == FALSE, return HR without adding suffix
          HR
        }
        
        break
      }
    }
    
    # ADT threshold assay: not included in the config file
    # Must test separately
    # if (grepl(adt_threshold_key, machine_readable_name)){
    #   
    # }
    
    # Numeric metadata features: these are not defined in the config file. For 
    # these features, the loop above will not match any assay key, and will not 
    # generate the variable "HR". 
    # To allow plotting of numeric metadata variables, the code below will 
    # have hr_name return the machine readable name when no matches are found. 
    if (!exists("HR")){
      # If HR does not exist, return the machine readable name
      return(machine_readable_name)
    } else {
      # Return the human-readable name if it exists
      return(HR)
    }
  }