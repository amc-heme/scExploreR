#' String subsetting: Construct Subset Call
#' 
#' Constructs a string that is passed to eval(parse()) to subset an object based
#' on the subset string passed to it (currently formed in make_subset).
#' 
#' @param object a single-cell object. 
#' @param subset_str a string specifying the criteria used to make a subset.
#' 
#' @noRd
subset_call <-
  function(
    object,
    subset_str
  ){
    UseMethod("subset_call")
  }

#' Function to display an warning when an unsupported object type is detected
#'
#' @export
#' @noRd
subset_call.default <-
  function(
    object,
    subset_str
  ){
    warning(
      paste0(
        "subset_call does not know how to handle object of class ",
        paste(class(object), collapse = ", "),
        ". Currently supported classes: Seurat and SingleCellExperiment."
        )
      )
    }

#' @describeIn subset_call Seurat objects
#' @export
#' @noRd
subset_call.Seurat <-
  function(
    object,
    subset_str
  ){
    paste0("subset(object, subset = ", subset_str, ")")
  }

#' @describeIn subset_call SingleCellExperiment objects
#' @export
#' @noRd
subset_call.SingleCellExperiment <-
  function(
    object,
    subset_str
  ){
    paste0("subset(object, select = ", subset_str, ")")
  }
