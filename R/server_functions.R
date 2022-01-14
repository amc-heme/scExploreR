#hr_name: given the machine-readable name of an input feature, 
#return the human readable name. Used for modifying the titles of plots, which 
#use the machine-readable name by default.
 hr_name <- function(machine_readable_name,assay_info){
  #Check each assay to see which assay's machine-readable prefix corresponds to 
  #the server value of the input
  for (assay in assay_info){
    if (grepl(assay$prefix_machine, machine_readable_name)){
      #When a match is found, remove the assay's machine-readable prefix with sub()
      HR <- sub(assay$prefix_machine,"",machine_readable_name)
      #Add the assay's human-readable suffix to the title
      HR <- paste0(HR,assay$suffix_human)
      break
    }
  }
  
  #Return the human-readable name 
  return(HR)
}

#n_patches: get number of plots in a patchwork object
#Derived from patchwork source code and used to determine number of plots to 
#iterate through in a for loop
#https://github.com/thomasp85/patchwork/blob/master/R/plot_patchwork.R
n_patches <- function(patchwork){
  n_patches <- length(patchwork$patches$plots)
  #length(patchwork$patches$plots) is always one less than the number of plots,
  #except when there are no plots in the object.
  if (!is_empty(patchwork)) {
    n_patches <- n_patches + 1
    }
  return(n_patches)
}

#hr_title: for feature plots with no split.by argument and violin plots, 
#converts the title from the default machine-readable name to a human-readable name.
hr_title <- function(patchwork,n_patches,assay_info){
  for (i in 1:n_patches){
    #Fetch title of plot
    title_machine <- patchwork[[i]]$labels$title
    #Convert title to human-readable format
    title_HR <- hr_name(title_machine,assay_info)
    #Set the plot title to the human readable 
    patchwork[[i]] <- patchwork[[i]] + ggtitle(title_HR)
  }
  #Return corrected patchwork object when complete
  return(patchwork)
}

