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
#' @param assay_config the assays section of the config file, loaded at app 
#' startup and upon changing datasets.
#'
#' @return a ggplot2 object with a scatterplot created from the Seurat object according to user specifications.
#' 
#' @noRd
shiny_scatter <- function(object,
                          feature_1,
                          feature_2,
                          group_by,
                          show_legend,
                          display_coeff,
                          palette,
                          assay_config = NULL
                          ){
  # Palette: must determine number of colors to create from provided palette
  # The number of colors is equal to the number of unique values in 
  # the group.by category
  n_colors <- 
    object@meta.data[[group_by]] |>
    unique() |> 
    length()
  
  # Plot: uses FeatureScatter
  plot <- 
    FeatureScatter(
      object, 
      feature1 = feature_1,
      feature2 = feature_2,
      group.by = group_by,
      # Cols: use user defined palette, or the defaults if palette() == NULL 
      cols = 
        if (!is.null(palette)){
          # colorRampPalette() extends or contracts the given palette to 
          # produce exactly the required number of colors
          colorRampPalette(palette(n_colors))
          # Use ggplot2 defaults if palette() is unspecified
        } else NULL, 
      )
  
  # Use human-readable names on axes (remove assay key from features)
  plot <- 
    plot +
    xlab(
      # Remove assay key from feature name
      hr_name(
        machine_readable_name = feature_1,
        assay_config = assay_config,
        use_suffix = TRUE
      )
    ) +
    ylab(
      # Remove assay key from feature name
      hr_name(
        machine_readable_name = feature_2,
        assay_config = assay_config,
        use_suffix = TRUE
      )
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
            if (show_legend == TRUE) {
              "right"
            } else "none"
          )
      ), # End list() (element A)
      
      # Element B: Remove title if requested
      if (display_coeff == FALSE){
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