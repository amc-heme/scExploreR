#' Vector to Text
#' 
#' Prints the contents of a vector as a string with commas separating each 
#' element. Used to create the formal report giving the subset criteria in the 
#' gene correlations tab.
#'
#' @param vector a character vector
#'
#' @return a single-element character vector with all levels of the character vector in grammatically correct format (comma-separated, with "and" before the last element)
#'
#' @noRd
vector_to_text <- function(vector){
  if (is.null(vector)){
    "NULL"
  }
  #For one entry, report the choice.
  else if (length(vector)==1){
    paste(vector)
  }
  #For two entries, use 'and' between choices
  else if (length(vector)==2){
    paste0(vector[1]," and ",vector[2])
  }
  #Multiple entries: iteratively integrate elements in a single string using paste0.
  else {
    #For more than 2 entries, must list with commas between each choice, and 'and' in front of the last choice
    for (i in (1:(length(vector)))){
      #First element: create string_return string and store the first element in the string. 
      #Add a comma at the end.
      if (i==1){
        string_return <- paste0(vector[i],",")
      }
      #For all entries except the last entry, add the ith entry to the text vector with a comma at the end.
      else if (i!=length(vector)){
        string_return <- paste0(string_return," ",vector[i],",",collapse = "")
      }
      #Last entry: add ', and', then the last entry, then a period. 
      else{
        string_return <- paste0(string_return," and ",vector[i],collapse="")
      }
    }
    #When finished iterating through all entries, print the result.
    string_return
  }
}
###