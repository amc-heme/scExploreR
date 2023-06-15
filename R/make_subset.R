#' Subset Creation Function
#'
#' @param object a Seurat object.
#' @param criteria_list a list of name-value pairs with the name of each 
#' metadata variable used as a criterion, and the unique values within that variable to 
#' include in the subset. The name of the variable must be entered exactly as it
#' appears in the metadata slot of the Seurat object, and the list object itself
#' must be reactive, as opposed to each individual item being reactive. The app
#'  code should generate the list in the correct format automatically.  
#' @param user_string if the user enables the entry of a subset string, the 
#' value of the string should be passed to this variable. The string passed will
#' be added to the end of the string generated from criteria_list, with the '&'
#' operator separating the string.
#'
#' @return A Seurat object subsetted for the criteria entered.
#' 
#' @noRd
make_subset <- 
  function(
    object, 
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
    subset_str <- ""
    
    # Construct a string representation for each criterion, and append to string
    for (i in 1:length(criteria_list)){
      # Get the metadata variable associated with the current index (name of
      # criteria_list[[i]])
      meta_var <- names(criteria_list)[i]
      
      # Construct criterion for the current variable 
      if (i < length(criteria_list)){
        # For all entries except for the last: add AND (`&`) after the criterion
        # (Specified criteria are mutually exclusive, so they use the 
        # AND operator)
        criterion <- 
          glue("({meta_var} %in% {vector_code(criteria_list[[i]])}) & ")
        # Do not use "&" for last criterion, or if there is only one criterion
      } else if (i == length(criteria_list)){
        criterion <- glue("({meta_var} %in% {vector_code(criteria_list[[i]])})")
      }
      
      # Add the criterion to the subset string
      subset_str <- paste0(subset_str, criterion)
    }
    
    # String Subsetting
    # If string subsetting is enabled, user_string will be defined, but it may 
    # be equal to "". This will cause errors in the concatenation below due to 
    # the creation of an "&" operator before parentheses with no 
    if (isTruthy(user_string)){
      # Add the user-defined string in parentheses with an "&" 
      # operator if it is defined and not equal to "". 
      subset_str <- glue('{subset_str} & ({user_string})')
    }
    
    # Subset using the subset string 
    subset <- 
      eval(
        parse(
          text = 
            subset_call(
              object, 
              subset_str = subset_str
              )
          )
        )
    
    # Re-level factors in subset: test every metadata variable 
    # to see if it is a factor

    #must pull metadata table, edit, then save to object
    meta_table <- 
      SCEPlots::fetch_metadata(
        subset, 
        full_table = TRUE
        )
    
    for (meta_var in colnames(meta_table)){
      if (class(meta_table[[meta_var]]) == "factor"){
        # If the metadata variable is a factor, drop unused levels
        meta_table[[meta_var]] <- 
          droplevels(meta_table[[meta_var]])
      }
    }
    
    scExploreR:::update_object_metadata(
      subset,
      table = meta_table
    )
    
    # Return subset
    return(subset)
  }