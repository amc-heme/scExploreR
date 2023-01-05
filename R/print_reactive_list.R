#' Print Reactive List
#' 
#' Takes a list of reactive values, and extracts the reactive content from each 
#' element in the list used to print the values in the reactive list to console 
#' or to a renderPrint statement.
#'
#' @param reactive_list The input must be a *list of reactive values* as opposed to a *reactive list of values* (the list of reactive values is the reccomended return format from modules).
#'
#' @return a non_reactive list of values that have been `unpacked` from their 
#' respective reactive expressions
#'
#' @noRd
print_reactive_list <- 
  function(
    reactive_list
    ){
    #Extract each reactive value individually and add to list for printing
    print_list <- lapply(reactive_list, function(x){x()})
    #Set the names of the new list equal to those of the reactive list
    names(print_list) <- names(reactive_list)
    # Return new list
    return(print_list)
  }