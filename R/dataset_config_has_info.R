#' @inherit browser_config_has_info title description params
#'
#' @return a boolean describing if the general dataset info for all datasets is contained in the individual dataset config files. 
dataset_config_has_info <- 
  function(datasets){
    all(
      sapply(
        names(datasets),
        function(dataset, datasets) {
          dataset_config_i <- datasets[[dataset]]$config
          
          all(
            c("label", "description", "preview") %in% names(dataset_config_i)
            )
          },
        datasets
        )
      )
  }