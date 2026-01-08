# Dimensional reduction plot

Graphs the output of a dimensional reduction technique on a 2D scatter
plot where each point is a cell and it's positioned based on the cell
embeddings determined by the reduction technique. The function accepts
both Seurat and SingleCellExperiment objects. For Seurat objects, cells
are colored by their identity class by default, and for
SingleCellExperiment objects, cells are colored by the first metadata
column in colData(). The metadata variable used for coloring cells can
be changed with the group_by parameter).

## Usage

``` r
plot_reduction(
  object,
  dims = c(1, 2),
  cells = NULL,
  cols = NULL,
  pt_size = NULL,
  reduction = NULL,
  group_by = NULL,
  split_by = NULL,
  shape_by = NULL,
  order = NULL,
  shuffle = FALSE,
  seed = 1,
  label = FALSE,
  label_size = 4,
  label_color = "black",
  label_box = FALSE,
  repel = FALSE,
  cells_highlight = NULL,
  cols_highlight = "#DE2D26",
  sizes_highlight = 1,
  na_value = "grey50",
  ncol = NULL,
  combine = TRUE,
  raster = NULL,
  raster_dpi = c(512, 512)
)
```

## Arguments

- object:

  a Seurat object or a SingleCellExperiment object

- dims:

  Dimensions to plot, must be a two-length numeric vector specifying x-
  and y-dimensions i.e. c(1,2) to plot the first and second dimensions
  from the reduction results.

- cells:

  Vector of cells to plot (default is all cells)

- cols:

  Vector of colors, each color corresponds to an identity class. This
  may also be a single character or numeric value corresponding to a
  palette as specified by
  [`brewer.pal.info`](https://rdrr.io/pkg/RColorBrewer/man/ColorBrewer.html).
  By default, ggplot2 assigns colors. We also include a number of
  palettes from the pals package. See `DiscretePalette` for details.

- pt_size:

  Adjust point size for plotting

- reduction:

  Which dimensionality reduction to use. If not specified, first
  searches for umap, then tsne, then pca

- group_by:

  Name of one or more metadata columns to group (color) cells by (for
  example, orig.ident). Unlike
  [`Seurat::DimPlot`](https://satijalab.org/seurat/reference/DimPlot.html),
  "ident" may not be passed since the ident functionality is not
  supported by SingleCellExperiment objects. A metadata column name must
  be passed.

- split_by:

  Name of a metadata column to split plot by. Unlike
  [`Seurat::DimPlot`](https://satijalab.org/seurat/reference/DimPlot.html),
  "ident" may not be passed since the ident functionality is not
  supported by SingleCellExperiment objects. A metadata column name must
  be passed, or `NULL` to disable split plots.

- shape_by:

  If NULL, all points are circles (default). You can specify any cell
  attribute (that can be pulled with fetch_data) allowing for both
  different colors and different shapes on cells. Only applicable if
  `raster = FALSE`.

- order:

  Specify the order of plotting for the idents. This can be useful for
  crowded plots if points of interest are being buried. Provide either a
  full list of valid idents or a subset to be plotted last (on top)

- shuffle:

  Whether to randomly shuffle the order of points. This can be useful
  for crowded plots if points of interest are being buried. (default is
  FALSE)

- seed:

  Sets the seed if randomly shuffling the order of points.

- label:

  Whether to label the clusters

- label_size:

  Sets size of labels

- label_color:

  Sets the color of the label text

- label_box:

  Whether to put a box around the label text (geom_text vs geom_label)

- repel:

  Repel labels

- cells_highlight:

  A list of character or numeric vectors of cells to highlight. If only
  one group of cells desired, can simply pass a vector instead of a
  list. If set, colors selected cells to the color(s) in
  `cols_highlight` and other cells black (white if dark.theme = TRUE);
  will also resize to the size(s) passed to `sizes_highlight`

- cols_highlight:

  A vector of colors to highlight the cells as; will repeat to the
  length groups in cells_highlight

- sizes_highlight:

  Size of highlighted cells; will repeat to the length groups in
  cells_highlight

- na_value:

  Color value for NA points when using custom scale

- ncol:

  Number of columns for display when combining plots

- combine:

  Combine plots into a single
  [`patchwork`](https://patchwork.data-imaginist.com/reference/patchwork-package.html)`ed`
  ggplot object. If `FALSE`, return a list of ggplot objects

- raster:

  Convert points to raster format, default is `NULL` which automatically
  rasterizes if plotting more than 100,000 cells

- raster_dpi:

  Pixel resolution for rasterized plots, passed to geom_scattermore().
  Default is c(512, 512).

## Value

A
[`patchwork`](https://patchwork.data-imaginist.com/reference/patchwork-package.html)`ed`
ggplot object if `combine = TRUE`; otherwise, a list of ggplot objects

## Details

The code for this function was from the [Seurat
Package](https://github.com/satijalab/seurat/blob/master/R/visualization.R)
and adapted for use with
[SingleCellExperiment](https://www.bioconductor.org/packages/release/bioc/html/SingleCellExperiment.html)
objects.
