test_that("correlation tables, row plots and subsets agree with Pearson data", {
  app <- browser_app("correlation-regression", fixture = "correlations")
  browser_set(app, "object_corr-feature_selection", "rna_MPO")
  app$click("object_corr-submit")
  app$wait_for_value(output = "object_corr-corr_header_gene")
  app$wait_for_idle()
  expect_equal(app$get_value(output = "object_corr-corr_header_gene"), "MPO")

  object <- browser_object()
  features <- SCUBA::features_in_assay(object, assay = "RNA")
  expression <- SCUBA::fetch_data(object, vars = paste0("rna_", features))
  global <- read.csv(browser_download(app, "object_corr-download_table", ".csv"))
  expect_setequal(global$Feature, setdiff(features, "MPO"))
  expected <- browser_correlations(expression, "rna_MPO")
  expect_equal(
    global$Correlation_Global, unname(expected[paste0("rna_", global$Feature)]),
    tolerance = 1e-10
  )

  app$wait_for_js(
    "document.querySelectorAll('#object_corr-corr_table tbody tr').length > 1"
  )
  app$click(
    selector = "#object_corr-corr_table tbody tr:first-child td:first-child"
  )
  app$wait_for_value(input = "object_corr-corr_table_rows_selected")
  browser_plot(app, "object_corr-full_data_scatterplot")

  # Covers both the one-type and pooled-type subset recordings, without
  # replaying incidental waiter/hover events from the obsolete shinytest logs.
  metadata <- SCUBA::fetch_metadata(object, full_table = TRUE)
  selections <- list(
    "B Cells",
    c("NK Cells", "PBMC Monocytes", "Plasma cells",
      "Plasmacytoid dendritic cells", "Primitive")
  )
  for (selection in selections) {
    browser_filter(
      app, "object_corr-subset_selections", "condensed_cell_type", selection
    )
    app$click("object_corr-submit")
    app$wait_for_idle()
    subset_table <- read.csv(browser_download(
      app, "object_corr-download_table", ".csv"
    ))
    cells <- rownames(metadata)[metadata$condensed_cell_type %in% selection]
    subset_expression <- expression[cells, , drop = FALSE]
    subset_expected <- browser_correlations(subset_expression, "rna_MPO")
    expect_setequal(subset_table$Feature, global$Feature)
    expect_equal(
      subset_table$Correlation_Global,
      unname(expected[paste0("rna_", subset_table$Feature)]), tolerance = 1e-10
    )
    expect_equal(
      subset_table$Correlation_Subset,
      unname(subset_expected[paste0("rna_", subset_table$Feature)]),
      tolerance = 1e-10
    )
    expect_equal(
      as.integer(app$get_value(output = "object_corr-stats-n_cells")),
      length(cells)
    )
    app$wait_for_js(
      "document.querySelectorAll('#object_corr-corr_table tbody tr').length > 1"
    )
    app$click(
      selector = "#object_corr-corr_table tbody tr:first-child td:first-child"
    )
    browser_plot(app, "object_corr-full_data_scatterplot")
    browser_plot(app, "object_corr-subset_scatterplot")
    app$click("object_corr-subset_selections-reset_all_filters")
    app$wait_for_idle()
  }
})
