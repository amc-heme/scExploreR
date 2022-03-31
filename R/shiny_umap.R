#shiny_umap

#Accepts inputs from plots_selections module and outputs a Seurat DimPlot from 
#the Seurat object passed to it. 

# object: a Seurat object. This can be either the full object or a subset.
# group_by: user specified group_by metadata category
# split_by: user specified split_by metadata category
# show_label: user choice as to whether labels should be shown on the plot
# show_legend: user choice as to whether a legend should be shown
# ncol: number of columns, as specified by user
# is_subset: reactive boolean value stating whether the object is a subset
# original_limits: user choice as to whether original axes limits should be used
# xlim_orig: the original x limits for the plot, computed from the full object at 
# app startup
# ylim_orig: the original y limits for the umap, computed from full object at 
# app startup
shiny_umap <- function(object, #Reactive
                       group_by, #Reactive
                       split_by, #Reactive
                       show_label, #Reactive
                       show_legend, #Reactive
                       ncol, #Reactive
                       is_subset, #Reactive
                       original_limits, #Reactive
                       xlim_orig, #Reactive
                       ylim_orig, #Reactive
                       reduction = NULL #Reactive
                      ){
  
  # validate will keep plot code from running if the subset 
  # is NULL (no cells in subset)
  validate(
    need(
      object(),
      # No message displayed (a notification is already 
      # displayed) (*was displayed*)
      message = ""
      )
    )
  
  # Produce a single UMAP plot if no features to split by are specified
  if (split_by() == "none"){
    umap_plot <- 
      DimPlot(
        object(),
        group.by = group_by(),
        #TRUE if "label groups" is checked, FALSE otherwise
        label = show_label(), 
        # Reduction: uses the input for reduction if it exists, otherwise
        # it is set to NULL and will use default settings.
        reduction = if(!is.null(reduction)) reduction() else NULL
        ) 
  } else {
    #UMAP with split.by defined and no special subset
    umap_plot <- 
      DimPlot(
        object(),
        group.by = group_by(),
        split.by = split_by() ,
        label = show_label(),
        ncol = ncol(),
        reduction = if(!is.null(reduction)) reduction() else NULL
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
            if (show_legend()==TRUE) {
              "right"
            } else "none")
        ),
      # B-C. Axis limits: use limits from full dataset if 
      # specified
      # First, simultaneously test if subset is present and if the corresponding
      # original_limits reactive is truthy (i.e. both present and checked).
      if (is_subset() & isTruthy(original_limits())){
        # If so, add original limits to the list
        list(
          scale_x_continuous(limits = xlim_orig()),
          scale_y_continuous(limits = ylim_orig())
          )
      }
    )

  # Modify the plot using the layers defined above
  umap_plot <- 
    umap_plot &
    layers
  
  # Return finished plot
  umap_plot
}
