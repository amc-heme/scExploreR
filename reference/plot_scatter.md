# Scatter plot of single cell data

Creates a scatter plot of two features (typically feature expression),
across a set of single cells. Cells are colored by their identity class.
Pearson correlation between the two features is displayed above the
plot.

## Usage

``` r
plot_scatter(
  object,
  feature_1,
  feature_2,
  group_by,
  cells = NULL,
  shuffle = FALSE,
  seed = 1,
  cols = NULL,
  pt_size = 1,
  shape_by = NULL,
  span = NULL,
  smooth = FALSE,
  combine = TRUE,
  slot = NULL,
  plot_cor = TRUE,
  raster = NULL,
  raster_dpi = c(512, 512),
  jitter = FALSE
)
```

## Arguments

- object:

  a Seurat object or a SingleCellExperiment object

- feature_1:

  First feature to plot. Typically feature expression but can also be QC
  metrics, PC scores, etc. - anything that can be retrieved with
  FetchData

- feature_2:

  Second feature to plot.

- group_by:

  Name of one or more metadata variables to group (color) cells by.
  Unlike
  [`Seurat::FeatureScatter`](https://satijalab.org/seurat/reference/FeatureScatter.html),
  at least one group_by variable must be defined

- cells:

  Cells to include on the scatter plot.

- shuffle:

  Whether to randomly shuffle the order of points. This can be useful
  for crowded plots if points of interest are being buried. (default is
  FALSE)

- seed:

  Sets the seed if randomly shuffling the order of points.

- cols:

  Colors to use for identity class plotting.

- pt_size:

  Size of the points on the plot

- shape_by:

  Ignored for now

- span:

  Spline span in loess function call, if `NULL`, no spline added

- smooth:

  Smooth the graph (similar to smoothScatter)

- combine:

  Combine plots into a single
  [`patchwork`](https://patchwork.data-imaginist.com/reference/patchwork-package.html)`ed`

- slot:

  Slot to pull data from, should be one of 'counts', 'data', or
  'scale.data'

- plot_cor:

  Display correlation in plot title

- raster:

  Convert points to raster format, default is `NULL` which will
  automatically use raster if the number of points plotted is greater
  than 100,000

- raster_dpi:

  Pixel resolution for rasterized plots, passed to geom_scattermore().
  Default is c(512, 512).

- jitter:

  Jitter for easier visualization of crowded points (default is FALSE)

## Value

A ggplot object
