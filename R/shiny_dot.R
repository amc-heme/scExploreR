# shiny_dot

# object: a Seurat object. This can be either the full object or a subset.
# features_entered: a character vector giving the features entered by the user.
# use_separate_features: boolean giving whether to use separate features for 
# the dot plot.
# separate: reactive vector of separate features selected, if applicable.
# group_by: user specified group_by metadata category
# split_by: user specified split_by metadata category
# show_legend: user choice as to whether a legend should be shown
shiny_dot <- function(
  object, #Reactive
  features_entered, #Reactive
  use_separate_features, #Reactive
  separate_features, #Reactive
  group_by, #Reactive
  show_legend #Reactive
){
  if (use_separate_features() == TRUE){
    # If use_separate_features() == TRUE, make dot plot using those features
    if (length(separate_features()) > 0){
      DotPlot(
        object(),
        features = separate_features(),
        group.by = group_by()
        ) + 
        RotatedAxis() +
        # Legend position: "right" if a legend is desired, and "none" if not
        theme(legend.position = if (show_legend() == TRUE) "right" else "none")
    } 
  } else if (use_separate_features() == FALSE){
    # Otherwise, use the general features for the dot plot
    if(length(features_entered()) > 0){
      DotPlot(
        object(), 
        features = features_entered(),
        group.by = group_by()
        ) + 
        RotatedAxis() +
        #Legend position: "right" if a legend is desired, and "none" if not
        theme(legend.position = if (show_legend() == TRUE) "right" else "none")
    }
  }
}