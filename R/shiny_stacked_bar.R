shiny_stacked_bar <-
  function(
    object,
    group_by,
    split_by,
    x_axis_title = NULL,
    y_axis_title = NULL,
    plot_title = NULL,
    show_title = TRUE,
    show_legend = TRUE,
    palette = NULL,
    sort_groups = NULL,
    legend_ncol = NULL,
    legend_font_size = NULL,
    legend_key_size = NULL
  ){
    # validate will keep plot code from running if the subset 
    # is NULL (no cells in subset)
    validate(
      need(
        object,
        # No message displayed (a notification is already 
        # displayed) (*was displayed*)
        message = ""
      )
    )
    
    # Processing of palette
    # Determine number of colors needed, which is equal to the number of 
    # unique values in the group by category
    n_colors <- 
      object@meta.data[[group_by]] |>
      unique() |> 
      length()
    
    # Expand or contract palette so number of colors exactly matches the 
    # number needed
    colors <- 
      if (!is.null(palette)){
        # colorRampPalette() extends or contracts the given palette to 
        # produce exactly the required number of colors
        colorRampPalette(palette)(n_colors)
        # If palette() is unspecified, use ggplot2 defaults
      } else NULL
    
    # Ordering proportion split by groups on stacked bar chart
    # Default value of sort_groups: set to "ascending" if groups is NULL
    if (is.null(sort_groups)){
      sort_groups <- "ascending"
    }

    # Refactor split by groups to plot in ascending or 
    # descending order by group name
    object@meta.data[[split_by]] <-
      # factor() creates a factor if the metadata category is not a factor
      # already, and re-orders a factor if it already exists.
      factor(
        object@meta.data[[split_by]],
        levels = 
          object@meta.data[[split_by]] |> 
          unique() |> 
          str_sort(
            numeric = TRUE,
            # Cell type proportion plots will plot groups in an order
            # consistent with the value of the `decreasing` argument
            # (FALSE will plot in ascending order, from left to right)
            decreasing = 
              if (sort_groups == "ascending"){
                FALSE
              } else if (sort_groups == "descending"){
                TRUE
              }
            )
        )
    
    # Create cell type proportion stacked bar chart
    plot <-
      ggplot(
        data = object@meta.data, 
        mapping = aes(.data[[split_by]]) 
      ) +
      geom_bar(
        # Stacked bar plot created by default from group_by category 
        # specified in `fill` argument
        mapping = aes(fill = .data[[group_by]]), 
        # "fill" creates a proportion bar chart
        position = "fill",
        key_glyph = "rect"
      ) +
      theme_cowplot() +
      theme(
        axis.text.x = 
          element_text(
            angle = 45,
            vjust = 1,
            hjust = 1
            ),
        # Hide legend title (group_by category that would appear above legend
        # is used for plot title instead, as is done for Seurat::DimPlot)
        legend.title = element_blank(),
        # Center plot title 
        plot.title = 
          element_text(
            hjust = 0.5
          )
        )
    
    # Additional Layers
    layers <-
      c(
        # A. Application of categorical palette, if specified
        # If a palette is not defined, scale_color_manual is not called, 
        # and ggplot2 defaults will be used
        if (!is.null(colors)){
          scale_color_manual(
            values = colors,
            # Must explicitly specify the scale as working with the "fill"
            # property
            aesthetics = "fill",
            # Color to use for NA values: currently fixed as "grey50"
            na.value = "grey50"
            )
          },
        
        # B. X-axis title
        # Use value of x_axis_title if defined
        if (!is.null(x_axis_title)){
          list(
            xlab(x_axis_title)
            )
          } else {
            # If an x-axis title is not defined, use the 
            # name of the split_by category
            list(
              xlab(split_by)
              )
            },
        
        # C. Y-axis title
        # Use y-axis title if defined, otherwise "Proportion of Cells"
        if (!is.null(y_axis_title)){
          list(
            ylab(y_axis_title)
            )
        } else {
          list(
            ylab("Proportion of Cells")
            )
          },
        
        # D. Plot title
        list(
          labs(
            title = 
              if (!is.null(plot_title)){
                plot_title
              } else {
                group_by
              }
            ) 
        ),
        
        # E-H. Arguments passed to theme
        list(
          do.call(
            theme,
            # List of arguments to call with theme
            args = 
              # Arguments are included in list conditionally. If no elements 
              # are included, the list() call will return an empty list instead
              # of NULL (NULL will cause errors with do.call)
              c(
                list(),
                # E. Show/hide plot title
                if (show_title == FALSE){
                  list(
                    plot.title = element_blank()
                    )
                },
                # F. Show/hide legend title (shown by default)
                list(
                  legend.position = 
                    if (show_legend == TRUE) {
                      "right"
                    } else "none"
                ),
                # G. Legend font size 
                if (isTruthy(legend_font_size)){
                  list(
                    legend.text = 
                      element_text(
                        size = legend_font_size
                      )
                  )
                },
                
                #I_ALT. Legend key size
                if (isTruthy(legend_key_size)){
                  list(
                    legend.key.size =
                      unit(legend_key_size, "points")
                  )
                }
              )
          )
        ),
        
        # H-I. Number of columns in legend, size of legend keys
        list(
          guides(
            # Stacked bar plots use "fill" aesthetic (umaps, 
            # feature plots use "color" instead)
            fill =
              do.call(
                guide_legend,
                # List of arguments to call
                args =
                  c(
                    # Empty list: passes no arguments if none are specified
                    list(),
                    # list(
                    #   byrow = TRUE
                    # ),
                    # H. Number of columns in legend
                    if (isTruthy(legend_ncol)){
                      list(
                        ncol = legend_ncol
                      )
                    },
                    # I. Size of keys
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
          )
        )
        
    plot <-
      plot +
      layers
    
    plot
  }