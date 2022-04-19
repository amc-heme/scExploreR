#' shiny_scatter
#'
#' Creates a flow-like scatterplot using the Seurat FeatureScatter function and 
#' modifies the plot post-creation according to user selections.
#'
#' @param object (Reactive) a Seurat object. May be the full object or a subset.
#' @param feature_1 (Reactive) the feature to plot on the x-axis.
#' @param feature_2 (Reactive) the feature to plot on the y-axis.
#' @param group_by (Reactive) metadata category to be passed to the group.by 
#' argument (affecting how cells are colored on the plot).
#' @param display_coeff (Reactive) if TRUE, show the correlation coefficient 
#' as the title of the plot (default behavior).
#'
#' @examples
shiny_scatter <- function(object,
                          feature_1,
                          feature_2,
                          group_by,
                          show_legend,
                          display_coeff
                          ){
  # Plot: uses FeatureScatter
  plot <- 
    FeatureScatter(
      object(), 
      feature1 = feature_1(),
      feature2 = feature_2(),
      group.by = group_by()
      )
  
  # List of layers to be applied to plot after creation
  layers <- 
    # c() will combine lists of elements into a single list
    # (use of individual lists works best with conditional framework)
    c(
      # Element A 
      # Legend position: "right" if a legend is desired, 
      # and "none" if not
      list(
        theme(
          legend.position = 
            if (show_legend()==TRUE) {
              "right"
            } else "none"
          )
      ), # End list() (element A)
      
      # Element B: Remove title if requested
      if (display_coeff() == FALSE){
        list(
          labs(title = NULL)
          )
      }
    )
  
  # Add layers to plot
  plot <- 
    plot & 
    layers
  
  # Return finished plot
  plot
}