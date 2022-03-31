# hr_name: given the machine-readable name of an input feature, 
# return the human readable name. 
# This is used for modifying the titles of plots, which 
# use the machine-readable name by default.
# The machine readable name is the name of the feature with the assay key as 
# a prefix (for example, RNA features use the '_rna' key)
hr_name <- function(machine_readable_name, 
                    assay_config
                    ){
  for (assay in assay_config()){
    # Given the machine-readable name in question, test each assay to see if 
    # the assay key is in the name
    if (grepl(assay$key, machine_readable_name)){
      # When a match is found, remove the assay's machine-readable 
      # prefix with sub()
      HR <- sub(assay$key, "", machine_readable_name)
      # Add the assay's human-readable suffix to the title, if defined
      if (assay$suffix_human != ""){
        # If defined, add to the end of the name in parentheses.
        HR <- glue("{HR} ({assay$suffix_human})")
      } else {
        HR 
      }
      break
    }
  }
  
  # Numeric metadata features: these are not defined in assays. For these 
  # features, the loop above will not match any assay key, and will not generate
  # the variable "HR". 
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