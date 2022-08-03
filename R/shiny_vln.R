# shiny_vln

# Accepts inputs from plots_selections module and outputs a Seurat FeaturePlot
# from the Seurat object passed to it. 

# object: a Seurat object. This can be either the full object or a subset. This 
# is a reactive-agnostic parameter (can be either reactive or non-reactive).
# features_entered: a character vector giving the features entered by the user.
# group_by: user specified group_by metadata category
# split_by: user specified split_by metadata category
# show_legend: user choice as to whether a legend should be shown
# ncol: number of columns, as specified by user
# is_subset: reactive boolean value stating whether the object is a subset
# assay_config: the assays section of the config file loaded at app startup.
shiny_vln <- function(
  object, # Reactive
  features_entered, # Reactive
  group_by, # Reactive
  split_by, # Reactive
  show_legend, # Reactive
  ncol, # Reactive
  assay_config, # Reactive
  palette, #Reactive
  sort_groups = NULL
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
