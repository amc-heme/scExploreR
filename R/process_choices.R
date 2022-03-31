# process_choices

# Given metadata_config and a category, process_choices tests if group 
# information is defined for the given category. If it is defined, the choices
# given are grouped according to the information found for the category. If it 
# is not defined, return the choices vector, unchanged.

# Arguments
# metadata_config: the metadata section of the config file loaded at startup.
# category: a metadata category to search for in metadata_config.
# choices: a vector of choices for the metadata category.
process_choices <- function(metadata_config,
                            category,
                            choices
                            ){
  # Fetch group info from the metadata_config file
  group_info <- metadata_config()[[category]]$groups
  
  if(!is.null(group_info)){
    # If group info data exists, construct groups with group_metadata_choices
    choices_list <-
      group_metadata_choices(group_info, choices)
    # Return grouped choices
    choices_list
  } else {
    # If there is no group info for the category, sort the choices vector
    # alphanumerically and return it
    choices <- str_sort(choices, numeric = TRUE)
    choices
  }
}

