# shiny_feature

# Accepts inputs from plots_selections module and outputs a Seurat FeaturePlot
# from the Seurat object passed to it. 

# object: a Seurat object. This can be either the full object or a subset. This 
# is a reactive-agnostic parameter (can be either reactive or non-reactive).
# features_entered: a character vector giving the features entered by the user.
# group_by: Influences labels
# split_by: user specified split_by metadata category.
# show_label: user choice as to whether labels should be shown on the plot
# show_legend: user choice as to whether a legend should be shown
# ncol: number of columns, as specified by user
# is_subset: reactive boolean value stating whether the object is a subset
# original_limits: user choice as to whether original axes limits should be used
# assay_config: the assays section of the config file loaded at app startup.
# xlim_orig: the original x limits for the plot, computed from the full object 
# at app startup
# ylim_orig: the original y limits for the umap, computed from full object at 
# app startup
shiny_feature <- function(object, 
                          features_entered, 
                          assay_config, 
                          split_by, 
                          group_by = NULL,
                          order = FALSE, 
                          show_label = FALSE, 
                          show_legend = TRUE, 
                          is_subset = FALSE, 
                          original_limits = FALSE, 
                          xlim_orig = NULL, 
                          ylim_orig = NULL,
                          show_title = TRUE, 
                          super_title = FALSE,
                          share_scale = FALSE,
                          color_by_feature = FALSE,
                          custom_titles = NULL,
                          legend_title = NULL,
                          ncol = NULL, 
                          palette = NULL,
                          blend = FALSE, 
                          reduction = NULL
){
  # validate will keep plot code from running if the subset
  # is NULL (no cells in subset)
  validate(
    need(
      if (is.reactive(object)) object() else object,
      # No message displayed (a notification is already
      # displayed) (*was displayed*)
      message = ""
    )
  )
  
  # Throw a silent exception if both share_scale and color_by_feature are TRUE
  # This combination produces an undesirable plot
  # The combination is automatically corrected in the plot_module server, but 
  # the plot produced by these settings will display briefly before this happens
  validate(
    need(
      !(share_scale == TRUE & color_by_feature == TRUE),
      # Display loading message until settings are corrected
      message = "Loading plot, please wait..."
    )
  )
  
  if (!is.null(group_by)){
    object <-
      SetIdent(
        object,
        value = group_by
      )
    }
  
  if (length(features_entered) == 1){
    # Use FeaturePlotSingle.R for single-feature plots
    FeaturePlotSingle(
      object,
      feature = features_entered,
      metadata_column = if (split_by != "none") split_by else NULL,
      # Colors: set to colors passed in the palette. 
      # If NULL, Seurat defaults are used
      colors = if (!is.null(palette)) palette,
      show_title = show_title,
      # Use custom titles if defined
      custom_titles = if (!is.null(custom_titles)) custom_titles else NULL,
      legend_title = legend_title,
      super_title = super_title,
      ncol = ncol,
      reduction = 
        if (!is.null(reduction)) reduction else NULL,
      xlim = 
        if (isTruthy(original_limits)){
          xlim_orig
        } else NULL,
      ylim = 
        if (isTruthy(original_limits)){
          ylim_orig
        } else NULL,
      show_legend = show_legend,
      # `...` arguments passed to FeaturePlot
      label = show_label,
      order = order
      )
  } else if (length(features_entered) > 1 & blend == FALSE) {
    # Multi-feature plot: method depends on split_by choice
    if (split_by == "none"){
      # Use feature plot wrapper for multiple feature plots with 
      # no split_by category 
      MultiFeatureSimple(
        object,
        features = features_entered,
        # Colors: set to colors passed in the palette. 
        # If NULL, Seurat defaults are used
        colors = if (!is.null(palette)) palette,
        color_by_feature = color_by_feature,
        show_title = show_title,
        # Use custom titles if defined
        custom_titles = if (!is.null(custom_titles)) custom_titles else NULL,
        share_scale = share_scale,
        legend_title = legend_title,
        ncol = ncol,
        reduction = 
          if (!is.null(reduction)) reduction else NULL,
        xlim = 
          if (isTruthy(original_limits)){
            xlim_orig
          } else NULL,
        ylim = 
          if (isTruthy(original_limits)){
            ylim_orig
          } else NULL,
        show_legend = show_legend,
        # `...` arguments passed to FeaturePlot
        label = show_label,
        order = order
      )
    } else {
      # If a split_by category is defined, use Seurat FeaturePlot function
      feature_plot <-
        FeaturePlot(
          # Object or subset (reactive-agnostic)
          object,
          features = features_entered,
          split.by = if (split_by != "none") split_by else NULL,
          # ncol: valid when split.by is not defined
          ncol = ncol,
          # Order: whether to plot cells in order by expression
          # Set to FALSE if undefined
          order = if (is.null(order)) FALSE else order,
          # Show/hide cluster labels
          label = show_label,
          # Reduction: uses the input for reduction if it exists, otherwise
          # it is set to NULL and will use default settings.
          reduction = if(!is.null(reduction)) reduction else NULL
        )
      
      #+
      # Clean up title: this changes the feature names on each plot
      # to a human-readable format
      # (used when split.by == NULL)
      # Determine number of plots created
      #n_patches <- n_patches(feature_plot)
      # Iterate through each plot, correcting the title
      # feature_plot <-
      #   hr_title(
      #     feature_plot,
      #     n_patches,
      #     assay_config
      #     )
      
      # Modify plot after creation with ggplot layers according
      # to user input
      # 'layers' is a list of layers that is applied to the plot
      layers <-
        c(list(
          # Element A
          # Legend position: "right" if a legend is desired,
          # and "none" if not
          theme(
            legend.position =
              if (show_legend == TRUE) {
                "right"
              } else "none")),
          
          # Elements B-C. Axis limits:
          # Use limits from full dataset if specified.
          # The conditional is tied to a reactive value
          # instead of the input to avoid an error that
          # occurs when this function is evaluated
          # before the input is #defined.
          # First, simultaneously test if subset is
          # present and if the corresponding
          # original_limits reactive is truthy
          # (i.e. both present and checked).
          if (is_subset & isTruthy(original_limits)){
            # If so, add original limits to the list
            list(
              scale_x_continuous(limits = xlim_orig),
              scale_y_continuous(limits = ylim_orig)
            )
          },
          
          # Element D: Custom colors
          # If a palette is entered, use the palette for the plot.
          # The palette may be a continuous palette selected in the plot tab,
          # or it may be a pair of user-defined custom colors.
          if (isTruthy(palette)){
            scale_color_gradientn(
              colors = c(palette)
            )
          }
        )
      
      # Modify the plot using the layers defined above
      suppressMessages(
        feature_plot <-
          feature_plot &
          layers
      )
      
      # Return finished plot for display
      feature_plot
    }
  } 
}
