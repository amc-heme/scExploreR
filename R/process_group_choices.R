#process_group_choices
#Takes the group_choices list defined in the config app and removes all outputs 
#representing fields that were removed using the delete button next to each field
process_group_choices <- function(group_choices){
  #Filter out group choices that have not been deleted
  #is_deleted: function used in sapply to identify fields that have been deleted
  #Returns TRUE if a list entry has been deleted, and FALSE if not
  is_deleted <- 
    function(entry){
      group_choices[[entry]]$deleted == TRUE
      }
  
  #Select the entries in the list that *have not* been deleted
  not_deleted <-
    sapply(
      names(group_choices),
      function(entry) !is_deleted(entry)
      )
  
  filtered_groups <- 
    group_choices[not_deleted]
  
  return(filtered_groups)
}

