# shiny_feature

# Accepts inputs from plots_selections module and outputs a Seurat FeaturePlot
# from the Seurat object passed to it. 

# object: a Seurat object. This can be either the full object or a subset.
# features_entered: a character vector giving the features entered by the user.
# split_by: user specified split_by metadata category
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
shiny_feature <- function(object, #Reactive
                          features_entered, #Reactive
                          split_by, #Reactive
                          show_label, #Reactive
                          show_legend, #Reactive
                          is_subset, #Reactive
                          original_limits, #Reactive
                          assay_config, #Reactive
                          xlim_orig, #Reactive
                          ylim_orig, #Reactive
                          reduction = NULL #Reactive
){
  # At least one feature must be entered to avoid errors when computing plot
  if (length(features_entered()) > 0){
    # validate will keep plot code from running if the subset 
    # is NULL (no cells in subset)
    validate(
      need(
        object(),
        # No message displayed (a notification is already 
        # displayed) (*was displayed*)
        message = ""
        )
      )
    
    # Creation of plot
    if (split_by() == "none"){
      # If no split.by category is specified, create a feature plot without 
      # the split.by argument
      feature_plot <- 
        FeaturePlot(
          object(),
          features = features_entered(),
          # Reduction: uses the input for reduction if it exists, otherwise
          # it is set to NULL and will use default settings.
          reduction = if(!is.null(reduction)) reduction() else NULL
          )
      # Clean up title: this changes the feature names on each plot 
      # to a human-readable format
      # Determine number of plots created
      n_patches <- n_patches(feature_plot)
      # Iterate through each plot, correcting the title
      feature_plot <- 
        hr_title(
          feature_plot,
          n_patches,
          assay_config
          )
    }
    
    else {
      # If a split by category is defined, use that category
      feature_plot <- 
        FeaturePlot(
          object(), 
          features = features_entered(),
          split.by = split_by(),
          # Reduction: uses the input for reduction if it exists, otherwise
          # it is set to NULL and will use default settings.
          reduction = if(!is.null(reduction)) reduction() else NULL
          )
    }
    
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
            if (show_legend()==TRUE) {
              "right"
            } else "none")),
        
        # Elements B-C. Axis limits: 
        # use limits from full dataset if specified
        # Simultaneously test if subset is present
        # and if the corresponding limits reactive
        # is truthy.
        if(is_subset() & isTruthy(original_limits())){
          # Add original limits to the list if the 
          # corresponding checkbox is checked
              list(scale_x_continuous(limits = xlim_orig()),
                   scale_y_continuous(limits = ylim_orig()))
        }
      )
    
    # Modify the plot using the layers defined above
    feature_plot <- 
      feature_plot &
      layers
    
    # Return finished plot
    feature_plot
  }
}
