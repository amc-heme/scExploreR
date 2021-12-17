#Reactive_values_to_sorted_list
#Takes a Shiny reactiveValues object and returns a list with the 
#keys in natural sort order 
reactive_values_to_sorted_list <- function(rv_list){
  rv_list <- reactiveValuesToList(rv_list)
  rv_list <- rv_list[str_sort(names(rv_list), numeric = TRUE)]
  return(rv_list)
}
