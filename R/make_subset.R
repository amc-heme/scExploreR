#' Subset Creation Function
#'
#' @param object a Seurat object.
#' @param criteria_list a list of name-value pairs with the name of each 
#' category used as a criterion, and the unique values within that category to 
#' include in the subset. The name of the category must be entered exactly as it
#' appears in the metadata slot of the Seurat object, and the list object itself
#' must be reactive, as opposed to each individual item being reactive. The app
#'  code should generate the list in the correct format automatically.  
#' @param user_string if the user enables the entry of a subset string, the 
#' value of the string should be passed to this variable. The string passed will
#' be added to the end of the string generated from criteria_list, with the '&'
#' operator separating the string.
#'
#' @return A Seurat object subsetted for the criteria entered.
make_subset <- function(object, 
                        criteria_list,
                        user_string = NULL
                        ){
  # vector_code sub-function
  # Converts a vector of subset criterion to a string representation of that 
  # vector, for passing to eval(parse())
  vector_code <- function(vector){
    # content: separate vector elements with "," 
    content <- paste(vector, collapse= '\",\"')
    # string: add c(" and ") to the ends of the content
    string <- paste0('c("', content, '")')
    
    # Special cases
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
  
  # Construct string for subsetting
  # Begin with empty string
  subset_str <-""
  
  # Construct a string representation for each criterion, and append to string
  for (i in 1:length(criteria_list)){
    # Get the metadata category associated with the current index (name of
    # criteria_list[[i]])
    category <- names(criteria_list)[i]
    
    # Construct criterion for the current category 
    if (i < length(criteria_list)){
      # For all entries except for the last: add AND (`&`) after the criterion
      # (Specified criteria are mutually exclusive, so they use the 
      # AND operator)
      criterion <- 
        glue("({category} %in% {vector_code(criteria_list[[i]])}) & ")
      # Do not use "&" for last criterion, or if there is only one criterion
    } else if (i == length(criteria_list)){
      criterion <- glue("({category} %in% {vector_code(criteria_list[[i]])})")
    }
    
    # Add the criterion to the subset string
    subset_str <- paste0(subset_str, criterion)
  }
  
  # String Subsetting
  # If string subsetting is enabled, user_string will be defined.
  if (!is.null(user_string)){
    # When defined, sdd user string in parentheses and link it to conditionals 
    # in subset string with "&" operator
    subset_str <- glue('{subset_str} & ({user_string})')
  }
  
  # Subset using the subset string 
  subset <- eval(parse(text=paste0("subset(object, subset = ", subset_str, ")")))
  
  # Re-level factors in subset: test every metadata category to see if it 
  # is a factor
  for (category in colnames(subset@meta.data)){
    if (class(subset@meta.data[[category]]) == "factor"){
      # If the metadata category is a factor, drop unused levels
      subset@meta.data[[category]] <- 
        droplevels(subset@meta.data[[category]])
    }
  }
  
  # Return subset
  return(subset)
}