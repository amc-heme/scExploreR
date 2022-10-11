#' shiny_scatter
#'
#' Creates a flow-like scatterplot using the Seurat FeatureScatter function and 
#' modifies the plot post-creation according to user selections.
#'
#' @param object A Seurat object. May be the full object or a subset. This is 
#' a reactive-agnostic parameter (can be either reactive or non-reactive).
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
                          display_coeff,
                          palette,
                          legend_ncol = NULL,
                          legend_font_size = NULL,
                          legend_key_size = NULL
                          ){
  # Make function reactive-agnostic (will use either a non-reactive object, or
  # a reactive object unpacked to a non-reactive variable within this function) 
  if (is.reactive(object)){
    object <- object()
  } 
  
  # Palette: must determine number of colors to create from provided palette
  # The number of colors is equal to the number of unique values in 
  # the group.by category
  n_colors <- 
    object@meta.data[[group_by()]] |>
    unique() |> 
    length()
  
  # Plot: uses FeatureScatter
  plot <- 
    FeatureScatter(
      object, 
      feature1 = feature_1(),
      feature2 = feature_2(),
      group.by = group_by(),
      # Cols: use user defined palette, or the defaults if palette() == NULL 
      cols = 
        if (!is.null(palette())){
          # colorRampPalette() extends or contracts the given palette to 
          # produce exactly the required number of colors
          colorRampPalette(palette())(n_colors)
          # Use ggplot2 defaults if palette() is unspecified
        } else NULL, 
      )
  
  # List of layers to be applied to plot after creation
  layers <- 
    # c() will combine lists of elements into a single list
    # (use of individual lists works best with conditional framework)
    c(
      # Element A 
      # Legend position: "right" if a legend is desired, 
      # and "none" if not
      # list(
      #   theme(
      #     legend.position = 
      #       if (show_legend()==TRUE) {
      #         "right"
      #       } else "none"
      #     )
      # ), # End list() (element A)
      
      list(
        do.call(
          theme,
          # List of arguments to call with theme
          args = 
            c(
              list(
                # Element A: Legend position
                # "right" if a legend is desired, and "none" if not
                legend.position = 
                  if (show_legend()==TRUE) {
                    "right"
                  } else "none"
              ),
              
              # Element B: Legend font size 
              if (isTruthy(legend_font_size)){
                list(
                  legend.text = 
                    element_text(
                      size = legend_font_size
                    )
                )
              },
              
              # Element C: Legend key size (passed here as well as in
              # guides())
              if (isTruthy(legend_key_size)){
                list(
                  legend.key.size =
                    unit(legend_key_size, "points")
                )
              }
            )
          )
        ),
      
      # Elements specified with guides()
      list(
        guides(
          # Guide for scatterplot is "color"
          color = 
            do.call(
              guide_legend,
              # List of arguments to call
              args =
                c(
                  # Empty list: passes no arguments if none are specified
                  list(),
                  # Element D: Number of columns in legend
                  if (isTruthy(legend_ncol)){
                    list(
                      ncol = legend_ncol
                    )
                  },
                  # Legend key size (specified here and in theme())
                  if (isTruthy(legend_key_size)){
                    list(
                      override.aes =
                        list(
                          size = legend_key_size
                        )
                    )
                  }
                )
            )
        )
      ),
      
      # Element E: Remove title if requested
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