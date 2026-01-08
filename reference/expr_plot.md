# Plot feature expression by group

Common codebase for violin, ridge plots.

## Usage

``` r
expr_plot(
  object,
  features,
  group_by,
  type = "violin",
  idents = NULL,
  ncol = NULL,
  sort = FALSE,
  y_max = NULL,
  same_y_lims = FALSE,
  adjust = 1,
  cols = NULL,
  pt_size = 0,
  split_by = NULL,
  log = FALSE,
  slot = NULL,
  stack = FALSE,
  combine = TRUE,
  fill_by = NULL,
  flip = FALSE,
  add_noise = TRUE,
  raster = NULL
)
```

## Arguments

- object:

  Seurat object

- features:

  Features to plot (gene expression, metrics, PC scores, anything that
  can be retreived by fetch_data)

- group_by:

  Group (color) cells in different ways (for example, orig.ident)

- type:

  Plot type, choose from 'ridge', 'violin', or 'splitViolin'

- idents:

  Which classes to include in the plot (default is all)

- ncol:

  Number of columns if multiple plots are displayed

- sort:

  Sort identity classes (on the x-axis) by the average expression of the
  attribute being potted, or, if stack is True, sort both identity
  classes and features by hierarchical clustering

- y_max:

  Maximum y axis value

- same_y_lims:

  Set all the y-axis limits to the same values

- adjust:

  Adjust parameter for geom_violin

- cols:

  Colors to use for plotting

- pt_size:

  Point size for geom_violin

- split_by:

  A variable to split the plot by

- log:

  plot Y axis on log scale

- slot:

  Slot (Seurat objects) or assay (SingleCellExperiment objects) to pull
  expression data from (counts/data for Seurat objects, counts/logcounts
  for SingleCellExperiment objects)

- stack:

  Horizontally stack plots for multiple feature

- combine:

  Combine plots into a single
  [`patchwork`](https://patchwork.data-imaginist.com/reference/patchwork-package.html)`ed`
  ggplot object. If `FALSE`, return a list of ggplot objects

- fill_by:

  Color violins/ridges based on either 'feature' or 'ident'

- flip:

  flip plot orientation (identities on x-axis)

- add_noise:

  determine if adding a small noise for plotting

- raster:

  Convert points to raster format, default is `NULL` which automatically
  rasterizes if plotting more than 100,000 cells

## Value

A
[`patchwork`](https://patchwork.data-imaginist.com/reference/patchwork-package.html)`ed`
ggplot object if `combine = TRUE`; otherwise, a list of ggplot objects
