#' Object class warning
#'
#' Returns a warning if the object loaded is not a supported single-cell-object,
#' and does nothing if it is. Currently, Seurat and SingleCellExperiment objects 
#' are supported.
#'
#' @param object a single-cell object. 
#' @param path path of object loaded
#' @param return_error if TRUE, an error is returned if the dataset is of an 
#' unsupported class. If FALSE, a warning is returned.
#' 
#' @noRd
check_dataset <-
  function(
    object,
    path = NULL,
    return_error = FALSE
  ){
    UseMethod("check_dataset")
  }

#' Function to display an warning when an unsupported object type is detected
#'
#' @export
#' @noRd
check_dataset.default <-
  function(
    object,
    path = NULL,
    return_error = FALSE
  ){
    msg <-
      paste0(
        if (is.null(path)){
          paste0(
            "Unsupported object class: ",
            paste(class(object), collapse = ", "),
            ". ",
            )
        } else {
          paste0(
            "Object at path ",
            path,
            " is of an unsupported object class (",
            paste(class(object), collapse = ", "),
            "). "
          )
        },
        if (return_error == FALSE){
          "Unexpected behavior will likely result. "
        },
        "Currently supported classes: Seurat and SingleCellExperiment."
      )
    
    # Display error or warning based on return_error
    if (return_error){
      stop(msg)
      } else {
        warning(msg)
        }
    }

#' @describeIn check_dataset Seurat objects
#' @export
#' @noRd
check_dataset.Seurat <-
  function(
    object,
    path = NULL,
    return_error = FALSE
  ){
    # Do nothing for supported object classes
  }

#' @describeIn check_dataset SingleCellExperiment objects
#' @export
#' @noRd
check_dataset.SingleCellExperiment <-
  function(
    object,
    path = NULL,
    return_error = FALSE
  ){
    # Do nothing for supported object classes
  }
