#' Run Presto in scExploreR
#'
#' Runs presto using either a Seurat or SingleCellExperiment object. 
#'
#' @param object a single-cell object. Currently, Seurat and
#' SingleCellExperiment objects are supported.
#' @param designated_genes_assay an assay designated in the config file as being
#' the genes assay. At this time, only the genes assay can be ran in presto.
#' @param assay_config the assays section of the config file, defined in the 
#' main server function.
#' @param metaclusters_present set in the DGE tab server. If TRUE, perform DGE 
#' based on the metaclusters metadata variable set up in the DGE tab
#' @param thresholding_present set in the DGE tab server. If TRUE, perform DGE 
#' based on the simple expression threshold metadata variable set up in the 
#' DGE tab.
#' @param group_by_category set in the DGE tab server. The metadata variable 
#' chosen for performing DGE via presto. 
#'
#' @return presto output (same output as [presto::wilcoxauc])
#' 
#' @noRd
run_presto <-
  function(
    object,
    designated_genes_assay, 
    assay_config, 
    metaclusters_present, 
    thresholding_present, 
    group_by_category
  ){
    UseMethod("run_presto")
  }

#' Function to display an error message when an unsupported object
#' type is detected
#'
#' @export
#' @noRd
run_presto.default <-
  function(
    object,
    designated_genes_assay, 
    assay_config, 
    metaclusters_present, 
    thresholding_present, 
    group_by_category
  ){
    warning(
      paste0(
        "run_presto does not know how to handle object of class ",
        paste(class(object), collapse = ", "),
        ". Currently supported classes: Seurat and SingleCellExperiment."
      )
    )
  }

#' @describeIn run_presto Seurat objects
#' @export
#' @noRd
run_presto.Seurat <-
  function(
    object,
    designated_genes_assay, 
    assay_config, 
    metaclusters_present, 
    thresholding_present, 
    group_by_category
  ){
    wilcoxauc(
      object, 
      assay = "data",
      # Assay: fixed to the designated gene assay for now
      seurat_assay =
        if (isTruthy(designated_genes_assay)){
          designated_genes_assay
          # If designated assay is undefined, use the first
          # assay included in the config file.
        } else names(assay_config)[[1]],
      # If metaclusters are requested, use 
      # "metaclusters" for dge groups
      group_by = 
        if (metaclusters_present){
          "metacluster"
        } else if (thresholding_present){
          "simple_expr_threshold"
        } else {
          group_by_category
        }
    )
  }

#' @describeIn run_presto SingleCellExperiment objects
#' @export
#' @noRd
run_presto.SingleCellExperiment <-
  function(
    object,
    designated_genes_assay, 
    assay_config, 
    metaclusters_present, 
    thresholding_present, 
    group_by_category
  ){
    wilcoxauc(
      object, 
      assay = "logcounts",
      group_by = 
        if (metaclusters_present){
          "metacluster"
        } else if (thresholding_present){
          "simple_expr_threshold"
        } else {
          group_by_category
        }
    )
  }
