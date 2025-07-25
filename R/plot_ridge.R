#' Single cell ridge plot
#'
#' Draws a ridge plot of single cell data (gene expression, metrics, PC
#' scores, etc.)
#'
#' @param object A Seurat or SingleCellExperiment object
#' @param features Features to plot (gene expression, metrics, PC scores,
#' anything that can be retreived by fetch_data).
#' @param group_by Group (color) cells in different ways. Unlike
#' \code{Seurat::RidgePlot()} or \code{Seurat::VlnPlot()}, this must be defined
#' (SingleCellExperiment objects don't have \code{Idents()} functionality).
#' @param cols Colors to use for plotting
#' @param idents Which levels of the group by variable to include in the plot
#' (default is all)
#' @param sort Sort group by levels (on the x-axis) by the average
#' expression of the attribute being potted, can also pass 'increasing' or 'decreasing' to change sort direction
#' @param assay Name of assay to use, defaults to the active assay
#' @param y_max Maximum y axis value
#' @param same_y_lims Set all the y-axis limits to the same values
#' @param log plot the feature axis on log scale
#' @param ncol Number of columns if multiple plots are displayed
#' @param slot Slot (Seurat objects) or assay (SingleCellExperiment objects) to
#' pull expression data from (counts/data for Seurat objects, counts/logcounts
#' for SingleCellExperiment objects)
#' @param stack Horizontally stack plots for each feature
#' @param combine Combine plots into a single \code{\link[patchwork]{patchwork}ed}
#' ggplot object. If \code{FALSE}, return a list of ggplot
#' @param fill_by Color violins/ridges based on either 'feature' or 'ident'
#'
#' @return A \code{\link[patchwork]{patchwork}ed} ggplot object if
#' \code{combine = TRUE}; otherwise, a list of ggplot objects
#'
#' @export
#' @concept visualization
#' 
#' @keywords internal
#' 
#' @examples
#' plot_ridge(AML_Seurat, features = "PC_1", group_by = "condensed_cell_type")
#'
plot_ridge <- function(
    object,
    features,
    group_by,
    cols = NULL,
    idents = NULL,
    sort = FALSE,
    assay = NULL,
    y_max = NULL,
    same_y_lims = FALSE,
    log = FALSE,
    ncol = NULL,
    slot = NULL,
    stack = FALSE,
    combine = TRUE,
    fill_by = 'feature'
){
  return(
    expr_plot(
      object = object,
      type = 'ridge',
      features = features,
      idents = idents,
      ncol = ncol,
      sort = sort,
      y_max = y_max,
      same_y_lims = same_y_lims,
      cols = cols,
      group_by = group_by,
      log = log,
      slot = slot,
      stack = stack,
      combine = combine,
      fill_by = fill_by
    )
  )
}
