shiny_stacked_bar <-
  function(
    object,
    group_by,
    split_by,
    x_axis_title = NULL,
    y_axis_title = NULL,
    plot_title = NULL,
    show_title = TRUE,
    show_legend = TRUE
  ){
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
        # A. X-axis title
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
        
        # B. Y-axis title
        # Use y-axis title if defined, otherwise "Proportion of Cells"
        if (!is.null(y_axis_title)){
          list(
            ylab(y_axis_title)
          )
        } else {
          list(
            ylab("Proportion of cells")
          )
        },
        
        # C. Plot title
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
        
        # D. Show/hide plot title
        # Plot title is shown by default
        if (show_title == FALSE){
          list(
            theme(
              plot.title = element_blank()
              )
            )
          },
        
        # E. Show/hide legend title (shown by default)
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