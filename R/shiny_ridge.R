#' scExploreR ridge plots
#'
#' @param object a Seurat object or subset.
#' @param features_entered a character vector giving the features to use in the
#' plot.
#' @param group_by user specified group_by metadata variable
#' @param show_legend user choice as to whether a legend should be shown (default is TRUE)
#' @param palette the color palette to use for the plot (plot uses a continuous color palette)
#' @param center_x_axis_title if TRUE, the title on the x_axis is centered (default is FALSE).
#' @param xlim a 2-element character vector giving the lower and upper bounds 
#' of the x-axis.
#' @param sort_groups the order with which to sort groups on the dot plot. This may be set to "ascending" or "descending". If ascending, groups will be sorted in increasing alphabetical order. If descending, they will be sorted in decreasing alphabetical order. 
#' @param custom_factor_levels A character vector giving the order of groups if `sort_groups` is set to "custom".
#'
#' @return a ggplot2 object with a ridge plot created according to user specifications.
#'
#' @noRd
shiny_ridge <- 
  function(
   object,
   features_entered,
   group_by = "none",
   show_legend = TRUE,
   palette = NULL,
   center_x_axis_title = FALSE,
   xlim = NULL,
   sort_groups = NULL,
   custom_factor_levels = NULL,
   custom_titles = NULL,
   assay_config = NULL
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
    
    # Factor/refactor group by metadata, but only if sort_groups is defined,
    # and group by is not "none"
    if (isTruthy(sort_groups) && group_by != "none"){
      object@meta.data[[group_by]] <-
        # factor() creates a factor if the metadata category is not a factor
        # already, and re-orders a factor if it already exists.
        factor(
          object@meta.data[[group_by]],
          levels = 
            # Levels based on ascending or descending order
            if (sort_groups %in% c("ascending", "descending")){
              object@meta.data[[group_by]] |> 
                unique() |> 
                str_sort(
                  numeric = TRUE,
                  decreasing =
                    # Order of plotting for ridge plots is inverted from the 
                    # expected order (decreasing == TRUE will plot in ascending 
                    if (sort_groups == "ascending"){
                      TRUE
                    } else if (sort_groups == "descending"){
                      FALSE
                    }
                )
            } else if (sort_groups == "custom"){
              # If sort_groups is "custom", use the user-defined levels 
              
              # Error message when custom_factor_levels is not defined (error 
              # returned by factor() is too generic) 
              if (is.null(custom_factor_levels)){
                stop(
                  'When `sort_groups` is equal to "custom", 
                  `custom_factor_levels` must be defined.'
                )
              }
              
              # Must reverse order for ridge plots
              custom_factor_levels |> 
                rev()
            }
        )
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
      
      # Apply custom titles, if provided
      if (!is.null(custom_titles)){
        for (i in 1:length(custom_titles)){
          suppressMessages(
            plot[[i]] <-
              plot[[i]] +
              ggtitle(custom_titles[i])
              )
          }
      } else {
          # Default behavior for titles
        for (i in 1:length(features_entered)){
          if (!is.null(assay_config)){
            suppressMessages(
              plot[[i]] <-
                plot[[i]] +
                ggtitle(
                  hr_name(
                    features_entered[i], 
                    assay_config,
                    use_suffix = TRUE
                  )
                )
              )
          } else {
              warning("assay_config must be defined to create human-readable titles")
            }
          }
        }
      
    } else {
      # Return nothing if no features are entered
      plot <- NULL
    }
    
    # Return plot
    plot
  }