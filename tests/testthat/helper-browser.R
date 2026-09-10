#' Start an isolated browser fixture
#'
#' Launches the current package in shinytest2's child R process. Missing
#' dependencies, browser executables, and fixtures deliberately fail outside CRAN.
#'
#' @param name Unique test scenario name.
#' @param fixture App directory under `tests/testthat/apps`.
#' @param config_path Optional saved configuration to load.
#' @param cleanup_environment Environment whose exit stops the app.
#' @return A running `shinytest2::AppDriver`.
browser_app <- function(name, fixture = "cellDIVER", config_path = NULL,
                        cleanup_environment = parent.frame()) {
  testthat::skip_on_cran()
  app_options <- list()
  if (!is.null(config_path)) {
    app_options$cellDIVER.test_config <- normalizePath(config_path, mustWork = TRUE)
  }
  app <- tryCatch(
    shinytest2::AppDriver$new(
      app_dir = normalizePath(
        testthat::test_path("apps", fixture), mustWork = TRUE
      ),
      name = name,
      seed = 325,
      width = 1440,
      height = 1000,
      load_timeout = 120000,
      timeout = 60000,
      options = app_options
    ),
    error = function(condition) {
      # shinytest2 attaches a partially initialized driver to startup errors.
      if (!is.null(condition$app)) condition$app$stop()
      stop(condition)
    }
  )
  withr::defer(app$stop(), envir = cleanup_environment)
  app
}

#' Read the shipped single-cell browser fixture
#'
#' @return The unmodified Seurat object used by the browser.
browser_object <- function() {
  readRDS(system.file(
    "extdata", "test_dataset.rds", package = "cellDIVER", mustWork = TRUE
  ))
}

#' Set a dynamically named bound browser input
#'
#' @param app Running browser driver.
#' @param input Fully namespaced input ID.
#' @param value Value to send through the real input binding.
#' @return The driver, invisibly, after reactive updates settle.
browser_set <- function(app, input, value) {
  do.call(app$set_inputs, c(
    stats::setNames(list(value), input), list(wait_ = FALSE)
  ))
  app$wait_for_idle()
  invisible(app)
}

#' Add a categorical filter through the current filter editor
#'
#' @param app Running browser driver.
#' @param namespace Namespace of the subset selections module.
#' @param variable Metadata column to filter.
#' @param values Metadata values to retain.
#' @return The driver, invisibly, with the filter saved but not submitted.
browser_filter <- function(app, namespace, variable, values) {
  app$click(paste0(namespace, "-add_filter"))
  browser_set(app, paste0(namespace, "-filter_type"), "categorical")
  browser_set(app, paste0(namespace, "-categorical_var"), variable)
  browser_set(app, paste0(namespace, "-categorical_values"), values)
  app$click(paste0(namespace, "-filter_confirm"))
  app$wait_for_idle()
  for (value in values) {
    testthat::expect_match(
      app$get_text(paste0("#", namespace, "-filters_applied")),
      value, fixed = TRUE
    )
  }
  invisible(app)
}

#' Assert an actual plot image has rendered in the browser
#'
#' Checks the decoded image, not the presence of its UI placeholder.
#'
#' @param app Running browser driver.
#' @param output Fully namespaced plot output ID.
#' @return The plot output value, including its image and coordinate map.
browser_plot <- function(app, output) {
  selector <- jsonlite::toJSON(paste0("#", output, " img"), auto_unbox = TRUE)
  app$wait_for_js(sprintf(
    paste0(
      "(() => { const image = document.querySelector(%s);",
      " return image && image.complete && image.naturalWidth > 20 &&",
      " image.naturalHeight > 20; })()"
    ),
    selector
  ))
  value <- app$get_value(output = output)
  testthat::expect_match(value$src, "^data:image/png;base64,")
  testthat::expect_gt(value$width, 20)
  testthat::expect_gt(value$height, 20)
  value
}

#' Download a browser result into a test-local file
#'
#' @param app Running browser driver.
#' @param output Download output ID.
#' @param extension File suffix, including the dot.
#' @param cleanup_environment Environment whose exit removes the download.
#' @return Absolute path to the downloaded file.
browser_download <- function(app, output, extension,
                             cleanup_environment = parent.frame()) {
  path <- tempfile(
    pattern = "browser-download-", tmpdir = testthat::test_path(),
    fileext = extension
  )
  path <- file.path(normalizePath(dirname(path)), basename(path))
  withr::defer(unlink(path), envir = cleanup_environment)
  app$get_download(output, filename = path)
  testthat::expect_gt(file.info(path)$size, 0)
  path
}

#' Read a plot's semantic SVG export
#'
#' @param app Running browser driver.
#' @param namespace Namespace of a plot module.
#' @return SVG markup from the production download handler.
browser_svg <- function(app, namespace) {
  browser_set(app, paste0(namespace, "-file_type"), "svg")
  path <- browser_download(app, paste0(namespace, "-confirm_download"), ".svg")
  svg <- paste(readLines(path, warn = FALSE), collapse = "\n")
  testthat::expect_match(svg, "<svg")
  svg
}

#' Count data points in a legend-free SVG scatterplot
#'
#' @param svg SVG markup exported with legend and labels disabled.
#' @return Number of circle geometries.
browser_point_count <- function(svg) {
  length(regmatches(svg, gregexpr("<circle\\b", svg))[[1]])
}

#' Assert browser DGE results against an independent scDE computation
#'
#' Preserves the old numeric regression intent without relying on column
#' positions or frozen sums from a different scDE version. The caller supplies
#' an independently selected object, never the browser's internal subset.
#'
#' @param app Running browser driver after submitting a DGE analysis.
#' @param object Independently constructed expected subset.
#' @param group_by Metadata column defining the expected comparison.
#' @param previous Previous result that must not be returned again.
#' @return The verified result table.
browser_expect_dge <- function(app, object, group_by, previous = NULL) {
  actual <- app$wait_for_value(
    export = "object_dge-dge_table", ignore = list(NULL, previous)
  )
  expected <- scDE::run_dge(
    object, group_by = group_by, seurat_assay = "RNA",
    positive_only = TRUE, lfc_format = "log2", remove_raw_pval = FALSE
  )
  testthat::expect_s3_class(actual, "data.frame")
  testthat::expect_gt(nrow(actual), 0)
  columns <- c(
    "group", "feature", "avgExpr", "log2FC", "auc",
    "pval", "pval_adj", "pct_in", "pct_out"
  )
  testthat::expect_true(all(columns %in% names(actual)))
  actual_order <- order(actual$group, actual$feature)
  expected_order <- order(expected$group, expected$feature)
  testthat::expect_equal(
    as.data.frame(actual[actual_order, columns]),
    as.data.frame(expected[expected_order, columns]),
    tolerance = 1e-6, ignore_attr = TRUE
  )
  testthat::expect_true(all(actual$log2FC > 0))
  app$wait_for_value(output = "object_dge-subset_stats-n_cells")
  testthat::expect_equal(
    as.integer(app$get_value(output = "object_dge-subset_stats-n_cells")),
    length(SCUBA::get_all_cells(object))
  )
  browser_plot(app, "object_dge-umap")
  actual
}

#' Click inside a rendered plot using real browser pointer events
#'
#' @param app Running browser driver.
#' @param output Fully namespaced plot output ID.
#' @param fraction Horizontal fraction of the plot image at which to click.
#' @return The driver, invisibly, after the pointer event is processed.
browser_click_plot <- function(app, output, fraction = 0.4) {
  selector <- jsonlite::toJSON(paste0("#", output, " img"), auto_unbox = TRUE)
  position <- app$get_js(sprintf(
    paste0(
      "(() => { const image = document.querySelector(%s);",
      " image.scrollIntoView({block: 'center'});",
      " const bounds = image.getBoundingClientRect();",
      " return {x: bounds.left + bounds.width * %f,",
      " y: bounds.top + bounds.height / 2}; })()"
    ),
    selector, fraction
  ))
  session <- app$get_chromote_session()
  session$Input$dispatchMouseEvent(
    type = "mousePressed", x = position$x, y = position$y,
    button = "left", clickCount = 1
  )
  session$Input$dispatchMouseEvent(
    type = "mouseReleased", x = position$x, y = position$y,
    button = "left", clickCount = 1
  )
  app$wait_for_idle()
  invisible(app)
}
