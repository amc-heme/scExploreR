# print_reactive_list
# Takes a list of reactive values, and extracts the reactive content from each 
# element in the list
# Used to print the values in the reactive list to console or to a renderPrint
# statement

# The input must be a *list of reactive values* as opposed to a *reactive list 
# of values*
print_reactive_list <- function(reactive_list){
  #Extract each reactive value individually and add to list for printing
  print_list <- lapply(reactive_list, function(x){x()})
  #Set the names of the new list equal to those of the reactive list
  names(print_list) <- names(reactive_list)
  # Return new list
  return(print_list)
}