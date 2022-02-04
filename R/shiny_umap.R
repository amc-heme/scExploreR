shiny_umap <- function(subset, #Reactive
                       group_by, #Reactive
                       split_by, #Reactive
                       show_label, #Reactive
                       show_legend, #Reactive
                       ncol, #Reactive
                       is_subset, #Reactive
                       original_limits, #Reactive
                       xlim_orig, #Non-reactive
                       ylim_orig #Non-reactive
                      ){
  
  umap_plot_content <- 
    #validate will keep plot code from running if the subset 
    #is NULL (no cells in subset)
    validate(
      need(subset(),
           #No message displayed (a notification is already 
           #displayed) (*was displayed*)
           message = "")
    )
  
  #Produce a single UMAP plot if no features to split by are specified
  if (split_by() == "none"){
    #Use full object if is_subset is FALSE, and use the 
    #subset otherwise
    umap_plot <- 
      DimPlot(
        subset(),
        group.by = group_by(),
        #TRUE if "label groups" is checked, FALSE otherwise
        label = show_label(), 
        #TODO: reduction is hard coded as umap. This should be changed 
        reduction = "umap"
      ) 
  } else {
    #UMAP with split.by defined and no special subset
    umap_plot <- 
      DimPlot(
        subset(),
        group.by = group_by(),
        split.by = split_by() ,
        label = show_label(),
        ncol = ncol(),
        reduction = "umap"
      ) 
  }
  
  #Modify plot after creation with ggplot layers according 
  #to user input
  #'layers' is a list of layers that is applied to the plot
  layers <- 
    list(
      #Element A 
      #Legend position: "right" if a legend is desired, 
      #and "none" if not
      theme(
        legend.position = 
          if (show_legend()==TRUE) {
            "right"
          } else "none"),
      
      #B-C. Axis limits: use limits from full dataset if 
      #specified
      #Element B
      #Must first test to see if subset is present
      #Input container does not exist if there is no subset
      if(is_subset()){
        #Add original limits to the list if the 
        #corresponding checkbox is checked
        #The conditional is tied to a reactive value 
        #instead of the input to avoid
        #An error that occurs when this function is 
        #evaluated before the input is #defined. 
        if (original_limits() == TRUE) {
          scale_x_continuous(limits = xlim_orig)
        }
      },
      #Element C
      #Check for subset (input container in child conditional 
      #does not exist before a subset is created)
      if(is_subset()){
        #Add original limits to the list if the 
        #corresponding checkbox is checked
        if(original_limits() == TRUE) {
          scale_y_continuous(limits = ylim_orig)
        } 
      }
    )
  
  #Modify the plot using the layers defined above
  umap_plot <- 
    umap_plot &
    layers
  
  #Return finished plot
  umap_plot
}