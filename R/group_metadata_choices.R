#group_metadata_choices
#Takes a list of choices for a metadata category and sorts the choices into 
#groups using the group selections defined in the config file.
#Arguments
#group_info: a list giving the information for each group. This should be accessed 
#from the config file using `config$metadata$<category_name>$groups`.
#choices: a vector of choices for the metadata category to be sorted into groups 
group_metadata_choices <- function(group_info, choices){
  #Create list for storing sorted choices
  grouped_choices <- list()
  
  #Sort each choice in the metadata category into each group
  for (choice in choices){
    #Look through each group to see if the choice is a member of that group
    for (group in group_info){
      #If the choice matches a group, store it in the list using the group name as the key
      if (choice %in% group$group_members){
        grouped_choices[[group$group_name]] <- 
          append(grouped_choices[[group$group_name]], choice)
      }
    }
  }
  
  #Sort the list created above so choices appear in order (natural sorting)
  for (group_name in names(grouped_choices)){
    grouped_choices[[group_name]] <- 
      str_sort(grouped_choices[[group_name]], numeric = TRUE)
  }
  
  #Return sorted list
  return(grouped_choices)
}
