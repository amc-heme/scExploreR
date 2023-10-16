#' Get names of reductions in object
#'
#' Returns the names of all reductions in a single-cell object.
#'
#' @param object a single-cell object. Currently, Seurat and
#' SingleCellExperiment objects are supported.
#' @param ... Currently unused.
#'
#' @rdname reduction_names
#'
#' @export
reduction_names <-
  function(
    object,
    ...
  ){
    UseMethod("reduction_names")
  }

#' Function to display an error message when an unsupported object
#' type is detected
#'
#' @noRd
#' @export
reduction_names.default <-
  function(
    object
  ){
    warning(
      paste0(
        "reduction_names does not know how to handle object of class ",
        paste(class(object), collapse = ", "),
        ". Currently supported classes: Seurat and SingleCellExperiment."
      )
    )
  }

#' @describeIn reduction_names Seurat objects
#' @export
reduction_names.Seurat <-
  function(
    object
  ){
    # Seurat objects: access reductions directly
    names(object@reductions)
  }

#' @describeIn reduction_names SingleCellExperiment objects
#' @export
reduction_names.SingleCellExperiment <-
  function(
    object
  ){
    # SingleCellExperiment objects: use reducedDimNames
    reducedDimNames(object)
  }

#' @describeIn reduction_names Anndata objects
#'
#' Anndata objects do not have an exclusive structure for reductions. Reductions
#' are stored in obsm, but matrices that are not reductions may exist in this
#' slot. To use reduction_names with Anndata objects, a vector of reductions
#' (python list) must be stored in `object.uns["SCUBA_reductions"]`. This method
#' will return an error if this data is not present in the object.
#'
#' @export
reduction_names.AnnDataR6 <-
  function(
    object
  ){
    if (!is.null(object$uns[["scExploreR_reductions"]])){
      # Add X matrix to the obsm keys matching modalities
      c(object$uns[["scExploreR_reductions"]])
    } else {
      stop("For anndata objects, the obsm_keys that correspond to reductions ",
           "must be defined in `uns$SCUBA_reductions`. If no reductions are ",
           "present in obsm, uns$SCUBA_reductions should be NULL/None.")
    }
  }





