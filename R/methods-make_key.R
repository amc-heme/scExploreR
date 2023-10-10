#' Return key for an assay
#'
#' Equivalent to \code{Key(object[[assay]])} for Seurat objects. Returns the 
#' key associated with an assay, which is used in [feature_list_all] to define
#' the machine-readable names for all features, so they can be accessed in the 
#' app.
#'
#' @param object a single-cell object. Currently, Seurat and
#' SingleCellExperiment objects are supported.
#' @param assay the assay (or experiment for SingleCellExperiment objects) to 
#' return the key for.
#'
#' @return the key for the assay, ending with an underscore. The key can be used
#' for accessing features not in the main assay.
#' 
#' @noRd
make_key <-
  function(
    object,
    assay
  ){
    UseMethod("make_key")
  }

#' Function to display an error message when an unsupported object
#' type is detected
#'
#' @export
#' @noRd
make_key.default <-
  function(
    object,
    assay
  ){
    warning(
      paste0(
        "make_key does not know how to handle object of class ",
        paste(class(object), collapse = ", "),
        ". Currently supported classes: Seurat and SingleCellExperiment."
      )
    )
  }

#' @describeIn make_key Seurat objects
#' @export
#' @noRd
make_key.Seurat <-
  function(
    object,
    assay
  ){
    Key(object[[assay]])
  }

#' @describeIn make_key SingleCellExperiment objects
#' @export
#' @noRd
make_key.SingleCellExperiment <-
  function(
    object,
    assay
  ){
    # SingleCellExperiment objects don't have a Key() method. Instead, a 
    # Seurat-style key will be generated using the assay name and an underscore.
    # This will be intelligible by the FetchData.SingleCellExperiment defined in
    # the SCUBA package.
    paste0(assay, "_")
  }
