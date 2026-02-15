# Test script to debug hexbin_density plot_title issue
devtools::load_all()
library(shiny)  # For validate/need functions

# Load test data
obj <- readRDS('inst/extdata/test_dataset.rds')
config <- yaml::read_yaml('inst/extdata/test_dataset_config.yaml')

# Try calling shiny_hexbin_density directly
result <- tryCatch({
  shiny_hexbin_density(
    object = obj,
    nbins = 80,
    reduction = "umap",  # lowercase to match object
    split_by = NULL,
    scale_density = FALSE,
    ncol = NULL,
    scales = "fixed",
    show_title = TRUE,
    plot_title = "Test Title",
    show_legend = TRUE,
    is_subset = FALSE,
    original_limits = FALSE,
    xlim_orig = NULL,
    ylim_orig = NULL
  )
}, error = function(e) {
  cat("Error occurred:\n")
  print(e)
  return(NULL)
})

if (!is.null(result)) {
  cat("Success! Plot created.\n")
  print(result)
} else {
  cat("Failed to create plot.\n")
}
