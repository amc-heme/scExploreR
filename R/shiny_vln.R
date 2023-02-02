
#' shiny_vln
#' 
#' Accepts inputs from plots_selections module and outputs a Seurat feature plot from the Seurat object passed to it. 
#'
#' @param object a Seurat object. This can be either the full object or a subset.
#' @param features_entered 
#' @param group_by user specified group_by metadata category
#' @param split_by user specified split_by metadata category
#' @param show_legend if TRUE, a legend is shown to the right of the plot. If FALSE, the legend is hidden (default is TRUE).
#' @param ncol number of columns, as specified by user
#' @param assay_config the assays section of the config file loaded at app startup.
#' @param palette The palette to use for coloring groups. If the palette passed
#' to this function is NULL, the default (hue_pal()) is used.
#' @param sort_groups the order with which to sort groups on the dot plot. This may be set to "ascending", "descending", or "custom". If ascending, groups will be sorted in increasing alphabetical order. If descending, they will be sorted in decreasing alphabetical order. If custom, groups will be sorted according to how they appear in `custom_factor_levels`.
#' @param custom_factor_levels A character vector giving the order of groups if `sort_groups` is set to "custom".
#'
#' @return  a ggplot2 object with a violin plot created according to user specifications.
#'
#' @noRd
shiny_vln <- 
  function(
    object, 
    features_entered, 
    group_by, 
    split_by, 
    show_legend, 
    ncol, 
    assay_config, 
    palette, 
    sort_groups = NULL,
    custom_factor_levels = NULL
  ){
    # Default value of sort_groups: set to "ascending" if groups is NULL
    if (is.null(sort_groups)){
      sort_groups <- "ascending"
    }
    
    # At least one feature must be entered to avoid errors when computing plot
    if (length(features_entered) > 0){
      
      # validate will keep plot code from running if the subset 
      # is NULL (no cells in subset)
      validate(
        need(
          object,
          # No message displayed (a notification is already displayed) 
          # (*was displayed*)
          message = ""
        )
      )
      
      # Sort group_by levels by default 
      # Plot groups in ascending or descending order by group name
      object@meta.data[[group_by]] <-
        # factor() creates a factor if the metadata category is not a factor
        # already, and re-orders a factor if it already exists.
        factor(
          object@meta.data[[group_by]],
          levels = 
            # If sort_groups is "ascending" or "descending", re-factor based on
            # an alphanumeric order
            if (sort_groups %in% c("ascending", "descending")){
              object@meta.data[[group_by]] |> 
                unique() |> 
                str_sort(
                  numeric = TRUE,
                  # For violin plots, groups plot from left to right in same order
                  # vector levels appear (therefore ascending should use
                  # deceasing = FALSE)
                  decreasing = 
                    if (sort_groups == "ascending"){
                      FALSE
                    } else if (sort_groups == "descending"){
                      TRUE
                    }
                )
            } else if (sort_groups == "custom"){
              # If sort_groups is custom but custom_factor_levels is not 
              # defined, throw an informative error message (error returned
              # by by factor() is too generic) 
              if (is.null(custom_factor_levels)){
                stop(
                  'When `sort_groups` is equal to "custom", 
                    `custom_factor_levels` must be defined.'
                )
              }
              
              # If sort_groups is "custom", use the user-defined levels 
              custom_factor_levels
            }
        )
      
      # Palette: must determine number of colors to create from provided palette
      # The number of colors is equal to the number of unique values in 
      # the group.by category
      n_colors <- 
        object@meta.data[[group_by]] |>
        unique() |> 
        length()
      
      vln_plot <- 
        VlnPlot(
          # Object or subset
          object,
          features = features_entered,
          group.by = group_by,
          # Split.by: NULL if user selects "none", otherwise equal 
          # to user selection
          split.by = if (split_by == "none") NULL else split_by,
          # Cols: use user defined palette, or the defaults if palette() == NULL 
          cols = 
            if (!is.null(palette)){
              # colorRampPalette() extends or contracts the given palette to 
              # produce exactly the required number of colors
              colorRampPalette(palette)(n_colors)
              # Use ggplot2 defaults if palette() is unspecified
            } else NULL, 
          # ncol: use value of ncol defined in plot_module when more than
          # one feature is entered
          ncol = if (length(features_entered)==1) NULL else ncol
          ) +
        # Legend position: "right" if a legend is desired, and "none" if not
        theme(legend.position = if (show_legend==TRUE) "right" else "none")
      
      
      # Correct titles: change machine-readable name to human-readable name
      # Determine number of plots created
      n_patches <- n_patches(vln_plot)
      # Iterate through each plot, correcting the title
      vln_plot <- hr_title(vln_plot, n_patches, assay_config)
      
      # Return the plot
      vln_plot
    }
  }
