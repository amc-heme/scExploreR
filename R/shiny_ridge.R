shiny_ridge <- 
  function(
   object,
   features_entered,
   group_by = "none",
   show_legend = TRUE,
   palette = NULL,
   center_x_axis_title = FALSE,
   xlim = NULL
  ){
    # validate will keep plot code from running if the subset 
    # is NULL (no cells in subset), or if the group by selection and features
    # are not yet defined.
    validate(
      need(
        isTruthy(object) & isTruthy(group_by) & isTruthy(features_entered),
        # No message displayed (a notification is already 
        # displayed) (*was displayed*)
        message = ""
      )
    )
    
    # If group_by is equal to none, add a dummy metadata column that labels
    # all cells with... "All Cells"
    if (group_by == "none"){
      object@meta.data$allcells <- "All Cells"
    }
    
    # n_colors (equal to number of groups, determines how palette is applied)
    if (group_by != "none"){
      n_colors <- 
        object@meta.data[[group_by]] |>
        unique() |> 
        length()
    } else {
      # Only one group exists when group_by is equal to none
      n_colors <- 1
    }
    
    # If there is at least one feature entered, create the ridge plot
    if (length(features_entered) > 0){
      plot <-
        RidgePlot(
          object,
          # cols: uses user-defined categorical palette, 
          # or default palette if not provided
          cols = 
            if (!is.null(palette)){
              # colorRampPalette() extends or contracts the given palette to 
              # produce exactly the required number of colors
              colorRampPalette(palette)(n_colors)
              # Use ggplot2 defaults if palette() is unspecified
              } else NULL, 
          features = features_entered, 
          group.by = if (group_by == "none") "allcells" else group_by, 
          # Add toggle for same.y.lims
          #same.y.lims = same_y_lims
        ) 
      
      # If group_by is "none", remove y-axis labels (which appear redundant
      # when there is only one group for all cells) 
      if (group_by == "none"){
        suppressMessages(
          plot <-
            # Use & in the event multiple plots are drawn 
            # (when there are two or more features)
            plot &
            theme(
              # Disables "All Cells" on y-axis
              axis.text.y = element_blank() 
            ) 
        )
      }
      
      # Additional layers
      layers <-
        c(
          # Element A: Toggle legend
          list(
            theme(
              legend.position =
                if (show_legend == TRUE) {
                  "right"
                } else "none"
              ) 
            ),
          # Element B: Disables "identity" label on Y-axis
          list(
            theme(
              axis.title.y = element_blank()
              )
            ),
          # Element C: Centers title on plot
          list(
            theme(
              plot.title = 
                element_text(
                  hjust = 0.5
                  )
              )
            ),
          # Element D: Center X-axis label
          if (center_x_axis_title == TRUE){
            list(
              theme(
                axis.title.x = 
                  element_text(
                    hjust = 0.5
                  )
              )
            )
          },
          # Element E: Manually defined x-axis limits
          if (!is.null(xlim)){
            # scale_x_continuous(
            #   limits = xlim
            #   )
            coord_cartesian(
              xlim = xlim
              )
          }
          )

      # Apply layers
      suppressMessages(
        plot <-
          plot &
          layers
      )
      
    } else {
      # Return nothing if no features are entered
      plot <- NULL
    }
    
    # Return plot
    plot
  }