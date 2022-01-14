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