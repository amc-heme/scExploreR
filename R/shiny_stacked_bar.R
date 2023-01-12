#' scExploreR stacked bar plot
#'
#' Accepts inputs from plots_selections module and outputs a stacked bar plot
#' giving cell proportions by metadata groups from the Seurat object passed to 
#' it. 
#'
#' @param object A Seurat object. This can be either the full object or a subset.
#' @param group_by metadata variable used for cell type proportions. The number of cells in each level of this variable will be plotted.
#' @param split_by metadata variable used for comparing cell type proportions. The number of cells in each level of this variable will be plotted.
#' @param x_axis_title the title to use for the x-axis.
#' @param y_axis_title the title to use for the y-axis.
#' @param plot_title the title of the plot. Defaults to NULL; in this case, the group_by variable name will be displayed.
#' @param show_title if TRUE, the title is displayed above the plot.
#' @param show_legend if TRUE, the legend is shown to the right side of the plot. The default is TRUE.
#' @param palette a color palette to use for the plot. The plot uses a categorical palette.
#' @param sort_groups the order with which to sort groups on the dot plot. This may be set to "ascending" or "descending". If ascending, groups will be sorted in increasing alphabetical order. If descending, they will be sorted in decreasing alphabetical order. 
#' @param custom_factor_levels A character vector giving the order of groups if `sort_groups` is set to "custom".
#'
#' @return  a ggplot2 object with a stacked bar plot created according to user specifications.
#' 
#' @noRd
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
    custom_factor_levels = NULL
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
    
    print("Cell proportion plot: value of custom factor levels")
    print(custom_factor_levels)
    
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
          # Levels based on ascending or descending order
          if (sort_groups %in% c("ascending", "descending")){
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
          } else {
            # If sort_groups is "custom", use the user-defined levels 
            
            # Error message when custom_factor_levels is not defined (error 
            # returned by factor() is too generic) 
            if (is.null(custom_factor_levels)){
              stop(
                'When `sort_groups` is equal to "custom", 
                  `custom_factor_levels` must be defined.'
              )
            }
            
            custom_factor_levels
          }
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
        position = "fill"
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
        
        # E. Show/hide plot title
        # Plot title is shown by default
        if (show_title == FALSE){
          list(
            theme(
              plot.title = element_blank()
              )
            )
          },
        
        # F. Show/hide legend title (shown by default)
        list(
          theme(
            legend.position = 
              if (show_legend==TRUE) {
                "right"
              } else "none"
            )
          )
        
        )
        
    plot <-
      plot +
      layers
    
    plot
  }