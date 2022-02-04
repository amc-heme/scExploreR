#print_reactive_list
#Takes a list of reactive values, and extracts the reactive content from each 
#element in the list
#Used to print the values in the reactive list to console or to a renderPrint
#statement
print_reactive_list <- function(reactive_list){
  print_list <- list()
  
  #Loop through each key in the reactive list (names of each list item)
  for (item in names(reactive_list)){
    #Reconstruction of list: call the reactive content for each item and store 
    #under the item's key
    reactive_item <- reactive_list[[item]]()
    
    if (!is.null(reactive_item)){
      print_list[[item]] <- reactive_item
    } else {
      #If the reactive item is NULL, add the character "NULL" to the list
      #Adding literal NULL will remove the item from the list
      print_list[[item]] <- "NULL"
    }
    
  }
  
  #Return reconstructed list
 return(print_list)
}