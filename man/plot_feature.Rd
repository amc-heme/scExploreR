% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/plot_feature.R
\name{plot_feature}
\alias{plot_feature}
\alias{FeatureHeatmap}
\title{Visualize 'features' on a dimensional reduction plot}
\usage{
plot_feature(
  object,
  features,
  label_by = NULL,
  dims = c(1, 2),
  cells = NULL,
  cols = if (blend) {
     c("lightgrey", "#ff0000", "#00ff00")
 } else {
    
    c("lightgrey", "blue")
 },
  pt_size = NULL,
  order = FALSE,
  min_cutoff = NA,
  max_cutoff = NA,
  reduction = NULL,
  split_by = NULL,
  keep_scale = "feature",
  shape_by = NULL,
  slot = NULL,
  blend = FALSE,
  blend_threshold = 0.5,
  label = FALSE,
  label_size = 4,
  label_color = "black",
  repel = FALSE,
  ncol = NULL,
  coord_fixed = FALSE,
  by_col = TRUE,
  combine = TRUE,
  raster = NULL,
  raster_dpi = c(512, 512)
)
}
\arguments{
\item{object}{a Seurat object or a SingleCellExperiment object}

\item{features}{Vector of features to plot. Features can come from:
- An \code{Assay} feature (e.g. a gene name - "MS4A1")
- A column name from meta.data (e.g. mitochondrial percentage - "percent.mito")
- A column name from a \code{DimReduc} object corresponding to the cell embedding values
(e.g. the PC 1 scores - "PC_1")}

\item{label_by}{A metadata column used for labeling groups on the featute plot,
if label is \code{TRUE}.}

\item{dims}{Dimensions to plot, must be a two-length numeric vector specifying x- and y-dimensions i.e. c(1,2) to plot the first and second dimensions from the
reduction results.}

\item{cells}{Vector of cells to plot (default is all cells)}

\item{cols}{The two colors to form the gradient over. Provide as string vector with
the first color corresponding to low values, the second to high. Also accepts a Brewer
color scale or vector of colors. Note: this will bin the data into number of colors provided.
When blend is \code{TRUE}, takes anywhere from 1-3 colors:
\itemize{
\item 1 color:
\itemize{
\item Treated as color for double-negatives, will use default colors 2 and 3 for per-feature expression
}
\item 2 colors:
\itemize{
\item Treated as colors for per-feature expression, will use default color 1 for double-negatives
}
\item 3+ colors:
\itemize{
\item First color used for double-negatives, colors 2 and 3 used for per-feature expression, all others ignored
}
}}

\item{pt_size}{Adjust point size for plotting}

\item{order}{Boolean determining whether to plot cells in order of expression. Can be useful if
cells expressing given feature are getting buried.}

\item{min_cutoff, max_cutoff}{Vector of minimum and maximum cutoff values for each feature,
may specify quantile in the form of 'q##' where '##' is the quantile (eg, 'q1', 'q10')}

\item{reduction}{Which dimensionality reduction to use. If not specified, first searches for umap, then tsne, then pca}

\item{split_by}{A metadata column to split the feature plot by. Unlike \code{Seurat::FeaturePlot},
"ident" may not be passed since the ident functionality is not supported by SingleCellExperiment
objects. A metadata column name must be passed, or \code{NULL} to disable split plots.}

\item{keep_scale}{How to handle the color scale across multiple plots. Options are:
\itemize{
\item \code{feature} (default; by row/feature scaling):
\itemize{
\item The plots for each individual feature are scaled to the maximum expression of the feature across the conditions provided to 'split_by'.
}
\item \code{all} (universal scaling):
\itemize{
\item The plots for all features and conditions are scaled to the maximum expression value for the feature with the highest overall expression.
}
\item \code{NULL} (no scaling):
\itemize{
\item Each individual plot is scaled to the maximum expression value of the feature in the condition provided to 'split_by'. Be aware setting NULL will result in color scales that are not comparable between plots.
}
}}

\item{shape_by}{If NULL, all points are circles (default). You can specify any
cell attribute (that can be pulled with fetch_data) allowing for both
different colors and different shapes on cells.  Only applicable if \code{raster = FALSE}.}

\item{slot}{Which slot to pull expression data from? If \code{NULL}, defaults to "data" for Seurat objects, and "logcounts" for SingleCellExperiment objects.}

\item{blend}{Scale and blend expression values to visualize co-expression of two features}

\item{blend_threshold}{The color cutoff from weak signal to strong signal; ranges from 0 to 1.}

\item{label}{Whether to label the clusters}

\item{label_size}{Sets size of labels}

\item{label_color}{Sets the color of the label text}

\item{repel}{Repel labels}

\item{ncol}{Number of columns to combine multiple feature plots to, ignored if \code{split_by} is not \code{NULL}}

\item{coord_fixed}{Plot cartesian coordinates with fixed aspect ratio}

\item{by_col}{If splitting by a factor, plot the splits per column with the features as rows; ignored if \code{blend = TRUE}}

\item{combine}{Combine plots into a single \code{\link[patchwork]{patchwork}ed}
ggplot object. If \code{FALSE}, return a list of ggplot objects}

\item{raster}{Convert points to raster format, default is \code{NULL} which
automatically rasterizes if plotting more than 100,000 cells}

\item{raster_dpi}{Pixel resolution for rasterized plots, passed to geom_scattermore().
Default is c(512, 512).}
}
\value{
A \code{\link[patchwork]{patchwork}ed} ggplot object if
\code{combine = TRUE}; otherwise, a list of ggplot objects
}
\description{
Colors single cells on a dimensional reduction plot according to a 'feature'
(i.e. gene expression, PC scores, number of genes detected, etc.)
}
\note{
For the old \code{do.hover} and \code{do.identify} functionality, please see
\code{HoverLocator} and \code{CellSelector}, respectively.
}
\examples{
plot_feature(AML_Seurat, features = "PC_1")

}
\seealso{
\code{\link{DimPlot}} \code{\link{HoverLocator}}
\code{\link{CellSelector}}
}
\concept{visualization}
\keyword{internal}
