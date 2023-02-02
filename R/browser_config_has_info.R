#' Test Location of General Config Info
#'
#' Older versions of the config file have general dataset information defined in the config file, while newer versions have the info defined in the config files for each dataset. If there are inconsistencies in where this info is stored, the modal for choosing datasets will throw an error. This function is a test to determine the source of the dataset information, and to ensure the source is consistent for all datasets.
#'
#' @param datasets the list of datasets generated in the main app.
#'
#' @return a boolean describing if the general dataset info for all datasets is contained in the browser config file. 
#' @noRd
browser_config_has_info <- 
  function(datasets){
    # Test if all of the datasets have information in the config file for either 
    # the label, description, or plot image.
    all(
      sapply(
        names(datasets),
        function(dataset, datasets){
          dataset_i <- datasets[[dataset]]
          
          all(
            c("label", "description", "plot") %in% names(dataset_i)
          )
        },
        datasets
      )
    )
}