#' Get names of all features in an assay/experiment
#'
#' Returns the names of all features in an assay (Seurat objects) or experiment
#' (SingleCellExperiment objects)
#'
#' @param object a single-cell object. Currently, Seurat and
#' SingleCellExperiment objects are supported.
#' @param assay the name of an assay/experiment for which to view features
#' @param ... Currently unused.
#'
#' @rdname features_in_assay
#'
#' @export
features_in_assay <-
  function(
    object,
    assay,
    ...
  ){
    if (length(assay) > 1){
      stop("Only one assay/experiment name may be passed to `assay`.")
    }

    UseMethod("features_in_assay")
  }

#' Function to display an error message when an unsupported object
#' type is detected
#'
#' @noRd
#' @export
features_in_assay.default <-
  function(
    object
  ){
    warning(
      paste0(
        "features_in_assay does not know how to handle object of class ",
        paste(class(object), collapse = ", "),
        ". Currently supported classes: Seurat and SingleCellExperiment."
      )
    )
  }

#' @describeIn features_in_assay Seurat objects
#' @export
features_in_assay.Seurat <-
  function(
    object,
    assay
  ){
    if (!assay %in% assay_names(object)){
      stop("Assay", assay, "not found in the current object.")
    }

    # Return rownames of assay entered
    rownames(object@assays[[assay]])
  }

#' @describeIn features_in_assay SingleCellExperiment objects
#' @export
features_in_assay.SingleCellExperiment <-
  function(
    object,
    assay
  ){
    if (!assay %in% assay_names(object)){
      stop("Assay", assay, "not found in the current object.")
    }

    if (assay == mainExpName(object)){
      rownames(object)
    } else {
      rownames(altExps(object)[[assay]])
    }
  }
