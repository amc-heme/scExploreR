#' hr_title
#' 
#' For feature plots with no split.by argument and violin plots, converts the 
#' title from the default machine-readable name to a human-readable name
#' 
#' @param patchwork the feature plot (as a patchwork object)
#' @param n_patches the number of patches in the patchwork object (calculated with the n_patches function)
#' @param assay_config the assay section of the config file
#'
#' @noRd
hr_title <- 
  function(
    patchwork, 
    n_patches, 
    assay_config
    ){
    for (i in 1:n_patches){
      #Fetch title of plot
      title_machine <- patchwork[[i]]$labels$title
      #Convert title to human-readable format
      title_HR <- hr_name(title_machine, assay_config)
      #Set the plot title to the human readable 
      patchwork[[i]] <- patchwork[[i]] + ggtitle(title_HR)
    }
    #Return corrected patchwork object when complete
    return(patchwork)
  }