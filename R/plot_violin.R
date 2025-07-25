#' Single cell violin plot
#'
#' Draws a violin plot of single cell data (gene expression, metrics, PC
#' scores, etc.)
#'
#' @inheritParams plot_ridge
#' @param pt_size Point size for geom_violin
#' @param split_by A variable to split the violin plots by,
#' @param split_plot  plot each group of the split violin plots by multiple or
#' single violin shapes.
#' @param adjust Adjust parameter for geom_violin
#' @param flip flip plot orientation (identities on x-axis)
#' @param add_noise determine if adding a small noise for plotting
#' @param raster Convert points to raster format. Requires 'ggrastr' to be installed.
# default is \code{NULL} which automatically rasterizes if ggrastr is installed and
# number of points exceed 100,000.
#'
#' @return A \code{\link[patchwork]{patchwork}ed} ggplot object if
#' \code{combine = TRUE}; otherwise, a list of ggplot objects
#'
#' @export
#' @concept visualization
#'
#' @keywords internal
#'
#' @seealso \code{\link{FetchData}}
#'
#' @examples
#' plot_violin(AML_Seurat, features = "PC_1", group_by = "condensed_cell_type")
#'
plot_violin <- function(
    object,
    features,
    group_by,
    cols = NULL,
    pt_size = NULL,
    idents = NULL,
    sort = FALSE,
    assay = NULL,
    split_by = NULL,
    adjust = 1,
    y_max = NULL,
    same_y_lims = FALSE,
    log = FALSE,
    ncol = NULL,
    slot = NULL,
    split_plot = FALSE,
    stack = FALSE,
    combine = TRUE,
    fill_by = 'feature',
    flip = FALSE,
    add_noise = TRUE,
    raster = NULL
){
  return(
    expr_plot(
      object = object,
      type = ifelse(test = split_plot, yes = 'splitViolin', no = 'violin'),
      features = features,
      idents = idents,
      ncol = ncol,
      sort = sort,
      y_max = y_max,
      same_y_lims = same_y_lims,
      adjust = adjust,
      pt_size = pt_size,
      cols = cols,
      group_by = group_by,
      split_by = split_by,
      log = log,
      slot = slot,
      stack = stack,
      combine = combine,
      fill_by = fill_by,
      flip = flip,
      add_noise = add_noise,
      raster = raster
    )
  )
}
