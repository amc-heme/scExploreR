#' shiny_umap
#'
#'Accepts inputs from plots_selections module and outputs a Seurat DimPlot from 
#'the Seurat object passed to it. 
#'
#' @param object A Seurat object. This can be either the full object or a subset. 
#' This is a reactive-agnostic parameter (can be either reactive or non-reactive).
#' @param group_by User specified group_by metadata category.
#' @param split_by User specified split_by metadata category.
#' @param show_label User choice as to whether labels should be shown on the
#' plot.
#' @param show_legend User choice as to whether a legend should be shown.
#' @param ncol Number of columns, as specified by user
#' @param is_subset Reactive boolean value stating whether the object is a 
#' subset
#' @param original_limits User choice as to whether original axes limits 
#' should be used
#' @param xlim_orig The original x limits for the plot, computed from the full 
#' object at app startup
#' @param ylim_orig The original y limits for the umap, computed from full 
#' object at app startup
#' @param plot_title The plot title, using the human-readable format defined in
#' the config file. 
#' @param reduction The reduction (UMAP, t-SNE, etc.) to use for plotting
#' @param palette The palette to use for coloring groups. If the palette passed
#' to this function is NULL, the default (hue_pal()) is used.
shiny_umap <- function(object, 
                       group_by, 
                       split_by, 
                       show_label, 
                       show_legend, 
                       ncol, 
                       is_subset, 
                       original_limits, 
                       xlim_orig = NULL, 
                       ylim_orig = NULL, 
                       show_title = TRUE, 
                       plot_title = NULL, 
                       reduction = NULL, 
                       palette = NULL 
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

  # Palette: must determine number of colors to create from provided palette
  # The number of colors is equal to the number of unique values in 
  # the group.by category
  n_colors <- 
    object@meta.data[[group_by]] |>
    unique() |> 
    length()
    
  # Produce a single UMAP plot if no features to split by are specified
  if (split_by == "none"){
    umap_plot <- 
      DimPlot(
        # Object or subset (reactive-agnostic)
        object,
        group.by = group_by,
        # Cols: use user defined palette, or the defaults if palette() == NULL 
        cols = 
          if (!is.null(palette)){
            # colorRampPalette() extends or contracts the given palette to 
            # produce exactly the required number of colors
            colorRampPalette(palette)(n_colors)
            # Use ggplot2 defaults if palette() is unspecified
          } else NULL, 
        #TRUE if "label groups" is checked, FALSE otherwise
        label = show_label, 
        # Reduction: uses the input for reduction if it exists, otherwise
        # it is set to NULL and will use default settings.
        reduction = if(!is.null(reduction)) reduction else NULL
        ) 
  } else {
    #UMAP with split.by defined and no special subset
    umap_plot <- 
      DimPlot(
        # Object or subset (reactive-agnostic)
        object,
        group.by = group_by,
        split.by = split_by,
        cols = 
          if (!is.null(palette)){
            # colorRampPalette() extends or contracts the given palette to 
            # produce exactly the required number of colors
            colorRampPalette(palette)(n_colors)
            # Use ggplot2 defaults if palette() is unspecified
          } else NULL, 
        label = show_label,
        ncol = ncol,
        reduction = if(!is.null(reduction)) reduction else NULL
        ) 
  }
  
  # Modify plot after creation with ggplot layers according 
  # to user input
  # 'layers' is a list of layers that is applied to the plot
  layers <- 
    c(
      # Element A 
      # Legend position: "right" if a legend is desired, 
      # and "none" if not
      list(
        theme(
          legend.position = 
            if (show_legend==TRUE) {
              "right"
            } else "none")
        ),
      # B-C. Axis limits: use limits from full dataset if 
      # specified
      # First, simultaneously test if subset is present and if the corresponding
      # original_limits reactive is truthy (i.e. both present and checked).
      if (is_subset & isTruthy(original_limits)){
        # If so, add original limits to the list
        list(
          scale_x_continuous(limits = xlim_orig),
          scale_y_continuous(limits = ylim_orig)
          )
      },
      # D: Title: Use label from config file if it is defined
      # If label is undefined, plot_title will be NULL
      # this would remove the title if not properly handled
      # Must control whether to remove the title, or use the default based on
      # the circumstances in which NULL is specified
      
      # Conditional below passes NULL to labs() only when show_title == FALSE 
      if (!is.null(plot_title) | show_title == FALSE ){
        list(
          labs(title = plot_title)
        )
      } # Otherwise, labs() is not run and the Seurat default is used
    )

  # Modify the plot using the layers defined above
  umap_plot <- 
    umap_plot &
    layers
  
  # Return finished plot
  umap_plot
}
