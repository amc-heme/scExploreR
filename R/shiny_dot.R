# shiny_dot

# object: a Seurat object. This can be either the full object or a subset.
# features: a character vector giving the features to use in the plot.
# group_by: user specified group_by metadata category
# split_by: user specified split_by metadata category
# show_legend: user choice as to whether a legend should be shown
shiny_dot <- function(
  object, #Reactive
  features, #Reactive
  group_by, #Reactive
  show_legend #Reactive
){
  if(length(features()) > 0){
    #Create plot if at least one feature is passed to shiny_dot()
    DotPlot(
      object(), 
      features = features(),
      group.by = group_by()
      ) + 
      RotatedAxis() +
      #Legend position: "right" if a legend is desired, and "none" if not
      theme(legend.position = if (show_legend() == TRUE) "right" else "none")
  }
}