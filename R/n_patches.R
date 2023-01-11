#' n_patches
#'
#' Get the number of plots in a patchwork object. This is derived from patchwork
#' source code and used to determine number of plots to iterate through in a for
#' loop (https://github.com/thomasp85/patchwork/blob/master/R/plot_patchwork.R)
#'
#' @param patchwork 
#'
#' @return an integer giving the number of plots in a patchwork object.
#' 
#' @noRd
n_patches <- function(patchwork){
  n_patches <- length(patchwork$patches$plots)
  #length(patchwork$patches$plots) is always one less than the number of plots,
  #except when there are no plots in the object.
  if (!is_empty(patchwork)) {
    n_patches <- n_patches + 1
  }
  return(n_patches)
}