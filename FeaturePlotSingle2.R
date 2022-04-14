FeaturePlotSingle2 <- 
  function(
    obj, 
    feature,
    metadata_column, 
    color_lower,
    color_upper, 
    custom_titles, 
    ...
    ){
    all_cells <- colnames(obj)
    groups <- levels(obj@meta.data[, metadata_column])
    # the minimal and maximal of the value to make the legend scale the same.
    minimal <- min(obj[['RNA']]@data[feature, ])
    maximal <- max(obj[['RNA']]@data[feature, ])
    # the minimal and maximal of the values to make the x and y scales the same.
    xmin <- min(obj@reductions[["umap"]]@cell.embeddings[,1])
    xmax <- max(obj@reductions[["umap"]]@cell.embeddings[,1])
    ymin <- min(obj@reductions[["umap"]]@cell.embeddings[,2])
    ymax <-max(obj@reductions[["umap"]]@cell.embeddings[,2])
    
    legend_title <- feature
    
    ps <- list()
    for (group in groups) {
      print(group)
      i <- which(groups == group)
      print(i)
      subset_indx<- obj@meta.data[, metadata_column] == group
      subset_cells<- all_cells[subset_indx]
      
      p <- 
        FeaturePlot(
          obj, 
          features = feature, 
          cells= subset_cells, 
          ...) +
        scale_color_gradientn(
          legend_title, 
          colors = c(color_lower, color_upper), 
          limits = c(minimal, maximal)) +
        
        coord_cartesian(xlim = c(xmin, xmax), ylim = c(ymin, ymax)) +
        ggtitle(custom_titles[i]) +
        theme(plot.title = element_text(size = 14, face = "bold"))
      
      ps[[group]] <- p
  }
  return(ps)
}

