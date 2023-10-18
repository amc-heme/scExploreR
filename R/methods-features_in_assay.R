#' Get names of all features in an assay/experiment
#'
#' Returns the names of all features in an assay/modality
#'
#' @param object a single-cell object. 
#' @param assay the name of an assay/modality for which to view features
#' @param ... Currently unused.
#'
#' @rdname features_in_assay
#' 
#' @keywords internal
#'
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

#' @describeIn features_in_assay Anndata objects
#' @export
features_in_assay.AnnDataR6 <-
  function(
    object,
    assay
  ){
    if (!assay %in% assay_names(object)){
      stop("Assay", assay, "not found in the current object.")
    }
    
    if (assay == "X"){
      # For the X matrix, feature names are in var_names
      object$var_names
    } else {
      # Otherwise, use the column names of the matrix stored in obsm
      colnames(object$obsm[[assay]])
    }
  }