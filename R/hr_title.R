#hr_title: for feature plots with no split.by argument and violin plots, 
#converts the title from the default machine-readable name to a human-readable name.
hr_title <- function(patchwork, n_patches, assay_config){
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