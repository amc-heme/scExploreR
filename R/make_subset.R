# Subset Creation Function

# Arguments
# criteria_list: a list of name-value pairs with the name of each category used 
# as a criterion, and the unique values within that category to include in the 
# subset. The name of the category must be entered exactly as it appears in the 
# metadata slot of the Seurat object, and the list object itself must be 
# reactive, as opposed to each individual item being reactive. The app code 
# should generate the list in the correct format automatically.  
make_subset <- function(object, 
                        criteria_list
                        ){
  # Define sub-function vector_code (generates code to be passed as a string 
  # to the subset function)
  # Given a vector, vector_code will export a string that will 
  # produce the vector when passed to eval(parse(text=<vector output>)).
  # Format: c("item_1","item_2")
  vector_code <- function(vector){
    # content: separate vector elements with "," 
    content <- paste(vector, collapse='\",\"')
    # string: add c(" and ") to the ends of the content
    string <- paste0('c("', content, '")')
    # Handling special cases
    # 1. NA
    # NA values appear as "NA" in the vector created above. 
    # "NA" (NA-as-a-string) will cause issues with data that uses the literal 
    # NA, so the string representation of the vector created here must output 
    # the literal NA
    string <- gsub('\"NA\"', 'NA', string)
    
    # 2. NaN
    # While less common, NaN should also be corrected from a string to a literal
    string <- gsub('\"NaN\"', 'NaN', string)
    
    # 3. NULL 
    # When NULL literals are passed to a vector, they do not appear in the 
    # vector at all. Thus, it is unlikely they will need to be corrected.
    # They will be substituted for literals here, but a NULL literal could 
    # cause issues downstream
    string <- gsub('\"NULL\"', 'NULL', string)
    
    return(string)
  }
  
  # A string with the defined criteria will be used for subsetting.
  # Begin with empty string
  subset_str <-""
  
  # If the criteria list passed to the function is a reactive expression, unpack 
  # it into a non-reactive variable
  if (is.reactive(criteria_list)){
    criteria_list <- criteria_list()
  }
  # Loop through criteria defined in criteria_list 
  # Construct subset syntax for each input (indexing is used to allow a 
  # different format to be used for the last criterion)
  for (i in 1:length(criteria_list)){
    # Get the metadata category associated with the current index (name of
    # criteria_list[[i]])
    category <- names(criteria_list)[i]
    
    # Each criterion is enclosed in parentheses and will evaluate to TRUE for 
    # the cells with metadata matching the criterion. When multiple criteria 
    # are used, cells with metadata matching all criteria are marked TRUE and 
    # included in the subset. The ampersand character is used to separate all
    # criteria, to search for cells where all criteria apply. 
    
    # Construct criterion for the current category 
    if (i < length(criteria_list)){
      # For all entries except for the last: add "&" after the criterion
      criterion <- 
        glue("({category} %in% {vector_code(criteria_list[[i]])}) & ")
      # Do not use "&" for last criterion, or if there is only one criterion
    } else if (i == length(criteria_list)){
      criterion <- glue("({category} %in% {vector_code(criteria_list[[i]])})")
    }
    
    # Add the criterion to the subset string
    subset_str <- paste0(subset_str, criterion)
  }
  
  # Subset using the subset string 
  subset <- eval(parse(text=paste0("subset(object(), subset = ", subset_str, ")")))
  
  # Return subset
  return(subset)
}