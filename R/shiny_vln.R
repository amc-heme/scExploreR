# shiny_vln

# Accepts inputs from plots_selections module and outputs a Seurat FeaturePlot
# from the Seurat object passed to it. 

# object: a Seurat object. This can be either the full object or a subset.
# features_entered: a character vector giving the features entered by the user.
# group_by: user specified group_by metadata category
# split_by: user specified split_by metadata category
# show_legend: user choice as to whether a legend should be shown
# ncol: number of columns, as specified by user
# is_subset: reactive boolean value stating whether the object is a subset
# assay_info: contains information on the assays included. 
# TODO: update assay_info and dependent functions using config file
shiny_vln <- function(
  object, # Reactive
  features_entered, # Reactive
  group_by, # Reactive
  split_by, # Reactive
  show_legend, # Reactive
  ncol, # Reactive
  assay_info # Non-reactive
){
  # At least one feature must be entered to avoid errors when computing plot
  if (length(features_entered()) > 0){
    # validate will keep plot code from running if the subset 
    # is NULL (no cells in subset)
    validate(
      need(
        object(),
        # No message displayed (a notification is already displayed) 
        # (*was displayed*)
        message = ""
      )
    )
    
    print(glue("Value of ncol: {ncol()}"))
    
    vln_plot <- 
      VlnPlot(
        object(),
        features = features_entered(),
        group.by = group_by(),
        # Split.by: NULL if user selects "none", otherwise equal 
        # to user selection
        split.by = if (split_by() == "none") NULL else split_by(),
        # ncol: use value of ncol defined in plot_module when more than
        # one feature is entered
        ncol = if (length(features_entered())==1) NULL else ncol()
        ) +
      # Legend position: "right" if a legend is desired, and "none" if not
      theme(legend.position = if (show_legend()==TRUE) "right" else "none")
    
    
    # Correct titles: change machine-readable name to human-readable name
    # Determine number of plots created
    n_patches <- n_patches(vln_plot)
    # Iterate through each plot, correcting the title
    vln_plot <- hr_title(vln_plot,n_patches,assay_info)
    
    # Return the plot
    vln_plot
  }
}
