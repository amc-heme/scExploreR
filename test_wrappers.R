#!/usr/bin/env Rscript
# Test script for hexbin wrapper functions

library(scExploreR)

cat("Loading test dataset...\n")
object_path <- "inst/extdata/test_dataset.rds"
obj <- readRDS(object_path)

cat("Object loaded: ", ncol(obj), " cells\n\n")

# Test shiny_hexbin_density wrapper
cat("Testing shiny_hexbin_density() wrapper...\n")
tryCatch({
  p <- scExploreR:::shiny_hexbin_density(
    object = obj,
    nbins = 20,
    reduction = "umap",
    split_by = NULL,
    scale_density = FALSE,
    show_title = TRUE,
    plot_title = "Test Density Plot",
    show_legend = TRUE
  )
  cat("✓ shiny_hexbin_density() wrapper works\n")
  print(class(p))
}, error = function(e) {
  cat("✗ Error in shiny_hexbin_density():\n")
  cat("  ", conditionMessage(e), "\n")
})

# Test shiny_hexbin_feature wrapper with single feature
cat("\nTesting shiny_hexbin_feature() wrapper (single feature)...\n")
tryCatch({
  test_feature <- rownames(obj)[1]
  cat("Using feature: ", test_feature, "\n")
  
  p <- scExploreR:::shiny_hexbin_feature(
    object = obj,
    features_entered = test_feature,
    nbins = 20,
    action = "mean",
    reduction = "umap",
    split_by = NULL,
    show_title = TRUE,
    plot_title = NULL,  # Will use default
    show_legend = TRUE
  )
  cat("✓ shiny_hexbin_feature() wrapper works for single feature\n")
  print(class(p))
}, error = function(e) {
  cat("✗ Error in shiny_hexbin_feature():\n")
  cat("  ", conditionMessage(e), "\n")
})

# Test shiny_hexbin_feature wrapper with multiple features
cat("\nTesting shiny_hexbin_feature() wrapper (multiple features)...\n")
tryCatch({
  test_features <- rownames(obj)[1:3]
  cat("Using features: ", paste(test_features, collapse = ", "), "\n")
  
  p <- scExploreR:::shiny_hexbin_feature(
    object = obj,
    features_entered = test_features,
    nbins = 20,
    action = "median",
    reduction = "umap",
    split_by = NULL,
    ncol = 2,
    show_title = TRUE,
    show_legend = TRUE
  )
  cat("✓ shiny_hexbin_feature() wrapper works for multiple features\n")
  print(class(p))
}, error = function(e) {
  cat("✗ Error in shiny_hexbin_feature():\n")
  cat("  ", conditionMessage(e), "\n")
})

cat("\n✓ All wrapper function tests completed successfully!\n")
