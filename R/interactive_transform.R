#' Transformation for Interactive Plots
#' 
#' This function was created to resolve an issue with Shiny interactive plots.
#' When the user hovers over and clicks plots, the x-coordinate returned is not
#' equal to the actual x-value as shown on the plot; instead, the coordinate 
#' ranges from roughly 0 to 1, with 0 ocurring at the left edge of the plot, and
#' 1 ocurring at the right edge. This causes issues when placing the vertical 
#' line on ridge plots for thresholding, as the x-coordinate as shown on the 
#' plots is required.
#' 
#' The function below attempts to correct this by using a min-max transformation.
#' The function takes the range and minimum of the distribution being plotted, 
#' as well as the upper and lower bounds of the coordinates being returned in
#' hover and click events, as well as the current x-coordinate returned from 
#' hover/click. The output of the function is the x-value of the plot 
#' corresponding to the current hover/click x-corrdinate.
#' 
#' This function is a workaround; there is an underlying issue that needs to 
#' be corrected. Coordinates are supposed to match the ggplot objects without 
#' needing transformation.
#' 
#' Written Equation 
#' Let X = Original x-coord (x value on plot)
#' Let x = transformed x-coord (input$plot_hover$x, input$plot_click$x)
#' let N = Distribution of expression values
#' let M = Range of x-coordinates from hover/click in interactive plot 
#' X = ((x - min(M)) * range(N)) /(max(M) - min(M)) + min(N)
#'
#' @param x_coord The "x" item of the list returned from the input value for hover and click events, as defined by hoverOpts and clickOpts in plotOutput(for example, input$plot_hover$x)
#' @param distribution_range The range of the distribution being plotted.
#' This function is intended for use with ridge plots or histograms, but a
#' range of x values for any plot with cartesan corrdinates will work.
#' @param distribution_minimum The minimum value of the distribution being
#' plotted.
#' @param plot_min_coord The x-value returned from hover/click events when
#' the cursor is at the left edge of the plot. 
#' @param plot_max_coord The x-value returned from hover/click events when
#' the cursor is at the right edge of the plot. 
#'
#' @return The x-coordinate on the plot corresponding to the hover/click
#' event coordinates.
interactive_transform <- 
  function(
    x_coord,
    distribution_range,
    distribution_minimum,
    plot_min_coord,
    plot_max_coord
  ){
    # Uses a min-max transformation
    # Written Equation 
    # Let X = Original x-coord (x value on plot)
    # Let x = transformed x-coord (input$plot_hover$x, input$plot_click$x)
    # let N = Distribution of expression values
    # let M = Range of x-coordinates from hover/click in interactive plot 
    
    # X = ((x - min(M)) * range(N)) /(max(M) - min(M)) + min(N)
    intermediate <- x_coord - plot_min_coord
    intermediate <- intermediate * distribution_range
    intermediate <- intermediate / (plot_max_coord - plot_min_coord)
    x_original <- intermediate + distribution_minimum
    
    # Return X
    x_original
  }