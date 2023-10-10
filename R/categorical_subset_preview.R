#' Preview a subset based on categorical metadata
#' 
#' Applies subsetting to the metadata table of the object rather than the full
#' object, saving time. This function is used in the subsetting module to display
#' the valid values of a new metadata variable used in subsetting based on the 
#' subset filters entered so far.
#'
#' @param object a single cell object supported by SCUBA (formerly SCUBA).
#' @param filter_list a list of filters generated in the subset_selections module.
#'
#' @noRd
#'
categorical_subset_preview <-
  function(object, filter_list){
    # Pull full metadata table
    meta_table <-
      SCUBA::fetch_metadata(
        object = object,
        full_table = TRUE
        )
    
    for (i in 1:length(filter_list)){
      # Extract current filter entry
      entry <- filter_list[[i]]
      
      # Subset based on categorical filter criteria only
      if (entry$type == "categorical"){
        # For each category, subset the table for cells that match the values 
        # specified 
        # The table is progressively filtered, narrowing down the cells each time
        # (This works when all criteria are joined using AND logic, which is the 
        # case for now)
        meta_table <- 
          meta_table[meta_table[[entry$var]] %in% entry$value, ]
      }
    }
    
    # Return the subsetted metadata table
    meta_table
  }
