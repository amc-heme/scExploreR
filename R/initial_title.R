#' Initial Value for Single Custom Title Input
#' 
#' Using config info for the currently selected object, generates the default 
#' title for the custom title input box. The default title for DimPlots is the
#' currently selected group_by category, and the default title for feature plots
#' is the current feature. 
#'
#' @param plot_type The type of plot for which custom titles are being generated. Either "dimplot" or "feature".
#' @param group_by The currently selected group_by category. Must be secified when plot_type == "dimplot", and is not used when plot type == "feature".
#' @param metadata_config The metadata section of the config file loaded at app startup. Must be secified when plot_type == "dimplot", and is not used when plot type == "feature".
#' @param features_entered The features currently selected (for feature plots)
#'
#' @noRd
initial_title <- function(
  plot_type,
  group_by = NULL,
  metadata_config = NULL, 
  features_entered = NULL
  ){
  if (!plot_type %in% c("dimplot", "feature", "proportion", "pie")){
    stop(
      'Agrument `plot_type` must be one of "dimplot", "feature", "proportion", or "pie"'
      )
  }
  
  if (plot_type %in% c("dimplot", "proportion", "pie")){
    # For DimPlots, cell proportion plots, and pie charts: use name of group_by category
    
    # Throw error if metadata_config or group_by are undefined
    if (is.null(metadata_config)){
      stop('When `plot_type` is equal to "dimplot" or "proportion", `metadata_config` must be defined.')
    }
    if (is.null(group_by)){
      stop('When `plot_type` is equal to "dimplot" or "proportion", `group_by` must be defined.')
    }
    
    if (!is.null(group_by)){
      config_label <-
        metadata_config[[group_by]]$label
      
      # Define initial value of text entry
      initial_value <-
        # Use label if it exists
        if (!is.null(config_label)){
          config_label
        } else {
          # If a label is not defined, use the name
          # of the group by category
          group_by
        }
    } else {
      initial_value <- ""
    }
  } else if (plot_type == "feature"){
    # For feature plots: name of feature
    if (!is.null(features_entered)){
      if (length(features_entered) == 1){
        initial_value <- features_entered
      } else {
        # For multi-feature plots, there currently is 
        # no framework for a single-title entry.
        # Set inital_value to "" regardless to avoid 
        # errors (may have to change this to improve
        # perfomrance)
        initial_value <- ""
      }
    } else {
      initial_value <- ""
    }
  }
  
  # Return value
  initial_value
}