#' Theshold Stats Function
#' 
#' This function is designed to work with the ridge plots in the subsetting
#'  window. Ridge plots with a distribution for one feature are displayed, 
#'  a threshold is chosen based on the distribution to determine +/- status
#'  (in the case of an ADT marker), and to set upper and lower limits for
#'  the scales on feature plots (so all cells below a threshold display
#'  with the color of the minimum value of the gradient, and all cells
#'  above display with the color of the maximum value).
#' 
#' Computes the total number of cells in the Seurat object entered, as well
#' as the number and percentage of cells above and below the threshold. 
#'
#' @param object A Seurat object or subset.
#' @param feature Feature for which the thresholding is being performed.
#' Only one feature may be entered.
#' @param threshold Threshold for separating cells. 
#'
#' @return A list of statistics. The elements of the list are named as
#' follows:
#' `n_total`: Total number of cells in object.
#' `n_above`: Number of cells with expression greater than or equal to the
#' threshold value.
#' `percent_above`: Percentage of cells with expression greater than or
#' equal to the threshold value.
#' `n_below`: Number of cells with expression below the threshold value.
#' `percent_below`: Percentage of cells with expression below the threshold
#' value.
#' 
#' @noRd
threshold_stats <- 
  function(
    object,
    feature,
    threshold
  ){
    if (length(feature) != 1){
      stop("Please enter only one feature.")
    }
    
    # Data: get expression distribution for the feature entered
    data <- 
      SCUBA::fetch_data(
        object = object,
        vars = feature
      ) 
    
    # Compute total number of cells in `object`
    n_total <-
      data |> 
      nrow()
    
    # Compute number of cells above threshold (or equal to threshold)
    n_above <-
      data |> 
      dplyr::filter(.data[[feature]] >= threshold) |> 
      nrow()
    
    # Percentage above threshold
    percent_above <-
      format(
        (n_above/n_total) *100,
        # Display at least three sig figs in percentage
        digits = 3,
        # Display at least two digits after decimal point
        nsmall = 2,
        scientific = FALSE
      )
    
    # Number of cells below threshold
    n_below <-
      data |> 
      dplyr::filter(.data[[feature]] < threshold) |> 
      nrow()
    
    # Percentage of cells below threshold
    percent_below <-
      format(
        (n_below/n_total) * 100,
        # Display at least three sig figs in percentage
        digits = 3,
        # Display at least two digits after decimal point
        nsmall = 2,
        scientific = FALSE
      )
    
    # Return list of stats
    list(
      `n_total` = n_total,
      `n_above` = n_above,
      `percent_above` = percent_above,
      `n_below` = n_below,
      `percent_below` = percent_below
      )
  }