#' Log Chosen Subset
#'
#' Returns a message via rog::log_info with the currently selected subset.
#'
#' @param filter_list a list of filters to apply, created by the subset 
#' selections module.
#'
#' @noRd
#' 
log_subset <- function(filter_list){
  # If the filter list is empty, there is no subset.
  if (length(filter_list) == 0){
    rlog::log_info(
      paste0("Subset selected: none (full object)")
      )
  } else {
    # Compose text for each filter criterion
    log_subset <- 
      sapply(
        filter_list,
        function(filter_i){
          if (filter_i$type == "categorical"){
            paste0(
              filter_i$var,
              " in ", 
              paste(filter_i$value, collapse = ", "), 
              ";\n"
              )
          } else if (filter_i$type == "numeric"){
            if (filter_i$mode == "greater_than"){
              paste0(filter_i$var, " >= ", filter_i$value, ";\n")
            } else if (filter_i$mode == "less_than"){
              paste0(filter_i$var, " <= ", filter_i$value, ";\n")
            } else if (filter_i$mode == "range"){
              paste0(
                filter_i$var, " >= ", filter_i$value[1], "&", 
                filter_i$var, " <= ", filter_i$value[2], ";\n")
            } else {
              warning("Filter logging: numeric filter unrecognized")
            }
          } else if (filter_i$type == "advanced"){
            paste0(filter_i$value)
          } else {
            warning("Filter logging: filter type unrecognized")
          }
        }
      )
    
    rlog::log_info(
      paste0(
        "Subset selected:\n", 
        paste(log_subset, collapse = "")
        )
      )
  }
}
