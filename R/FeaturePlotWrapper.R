library(Seurat)
library(patchwork)
library(stringr)

#' Wrapper for FeaturePlot
#'
#' This function allows for additional customization of feature plots. Only one
#' feature is supported by this function.
#'
#' @param object A Seurat object (may be the full object or a subset).
#' @param feature A feature to plot: only one feature is supported
#' @param metadata_column Metadata to split plots by. Identical in function to 
#' split.by used for FeaturePlots.
#' @param colors A vector of colors to use for plotting expression values. Any
#' number of colors may be specified, and palettes can be passed if they are 
#' formatted so that they yield a list of color hex codes. If NULL, the default
#' Seurat palette will be used.
#' @param color_lower This is deprecated, please use `colors` instead.
#' @param color_upper This is deprecated, please use `colors` instead.
#' @param ncol The number of columns to use when plotting multiple panels 
#' (applies when metadata_column is defined)
#' @param custom_titles A vector of titles to use for each panel of the plot. 
#' If NULL, default titles will be used (defaults to name of each split by
#' category if metadata_column is defined, or the name of the feature if no 
#' split.by category is defined). If an empty string (""), no titles will be 
#' shown.
#' @param reduction The dimensionality reduction to use for plotting cell 
#' coordinates.
#' @param xlim If specified, custom limits to use for the x-axis of the feature 
#' plot. When xlim is NULL, x-axis limits are based on the min and max x 
#' coordinates in the selected reduction of the object specified. xlim should 
#' be a two-element numeric vector giving the min and max x values.
#' @param ylim If specified, custom limits to use for the y-axis of the
#' feature plot. ylim should be a two-element numeric vector giving the min and 
#' max y values.
#' @param show_legend If TRUE, show the legend of the feature plot (default is 
#' TRUE). 
#' @param ... A list of arguments to pass to FeaturePlot
#'
#' @examples 
#' FeaturePlotSingle(
#'      object = prim.mono, 
#'      feature = "XIST", 
#'      metadata_column = "htb", 
#'      pt.size = 0.05, 
#'      colors = viridisLite::rocket(42, direction = -1),
#'      custom_titles = c("Normal", "Ven Reistant", "Ven Sensitive"),
#'      order = TRUE
#'      )
#'
FeaturePlotSingle<- 
  function(
    obj, 
    feature, 
    metadata_column = NULL, 
    colors = NULL,
    # Deprecated: use colors vector instead
    color_lower = NULL,
    color_upper = NULL,
    ncol = NULL,
    custom_titles = NULL, 
    reduction = NULL,
    xlim = NULL,
    ylim = NULL,
    show_legend = TRUE,
    ...
    ){
    # Handling of NULL values
    # If reduction is NULL, set to the default for the object
    if (is.null(reduction)){
      reduction <- DefaultDimReduc(object)
    }
    # If colors is NULL, set to default Seurat gradient for continuous data
    if (is.null(colors)){
      colors <- c('lightgrey', 'blue')
    }
    
    
    # Handling color_lower and color_upper inputs: form colors 
    # vector and warn user 
    if (!is.null(color_lower) & !is.null(color_upper)){
      warning(
        "Use of color_lower and color_upper is deprecated. Please pass a vector of colors to the `colors` argument instead"
      )
      colors <- c(color_lower, color_upper)
    }
  all_cells <- colnames(obj)
  # If a metadata column is defined, define a vector of groups based on the
  # unique values of the column.
  if (!is.null(metadata_column)){
    groups <- 
      obj@meta.data[, metadata_column] |> 
      unique() |> 
      # Will plot groups in lexicographical order
      str_sort(numeric = TRUE)
  } else {
    # If a split.by column is not defined, groups is set to an empty character
    # vector of length 1
    groups <- ""
  }
  
  # Define number of columns
  # If ncol is not provided, default is number of groups
  if (is.null(ncol)){
    ncol <- length(groups)
  }
  
  # the minimal and maximal of the value to make the legend scale the same.
  feature_data <-
    FetchData(
      obj,
      vars = feature,
      slot = "data"
    )[,1]
  minimal <- min(feature_data)
  maximal <- max(feature_data)
  
  # the minimal and maximal of the values to make the x and y scales the same.
  # If limits are not defined (default behavior), use the min and max of cell
  # coordinates in the current object
  if (is.null(xlim)){
    xmin <- min(obj@reductions[[reduction]]@cell.embeddings[,1])
    xmax <- max(obj@reductions[[reduction]]@cell.embeddings[,1])
  } else {
    xmin <- xlim[1]
    xmax <- xlim[2]
  }
  
  if (is.null(ylim)){
    ymin <- min(obj@reductions[[reduction]]@cell.embeddings[,2])
    ymax <- max(obj@reductions[[reduction]]@cell.embeddings[,2]) 
  } else {
    ymin <- ylim[1]
    ymax <- ymax[2]
  }
  
  
  # Put feature name on top of colorbar legend
  legend_title <- feature
  
  # If a split.by (metadata) column is defined, create a plot for each group 
  ps <- list()
  
  if (length(groups) > 1){
    # Feature plot with multiple split.by groups
    for (group in groups) {
      i <- which(groups == group)
      
      subset_indx <- obj@meta.data[, metadata_column] == group
      subset_cells <- all_cells[subset_indx]
      
      p <- 
        FeaturePlot(
          obj, 
          features = feature, 
          cells = subset_cells, 
          reduction = reduction,
          ...
        ) +
        # List of layers to apply to plot: depends on settings specified
        coord_cartesian(
          xlim = c(xmin, xmax), 
          ylim = c(ymin, ymax)
        ) +
        theme(
          plot.title = 
            element_text(size = 14, face = "bold")
        ) 
      
      # Title: use custom titles if defined, otherwise use the group name
      if (is.null(custom_titles)){
        # custom_titles is NULL: use default behavior (group names)
        p <-
          p +
          ggtitle(group)
      } else if (custom_titles == ""){
        # custom_titles is an empty string: remove titles
        p <-
          p +
          ggtitle(NULL)
      } else {
        # All other cases: use custom titles (custom titles should be a 
        # character vector)
        p <-
          p +
          ggtitle(custom_titles[i])
      }
      
      ps[[group]] <- p
      
    }
    
    plot <- 
      wrap_plots(
        ps, 
        ncol = ncol, 
        guides = "collect"
        ) 
    
    plot <-
      suppressMessages(
        plot &
          scale_color_gradientn(
            legend_title, 
            colors = colors, 
            limits = c(minimal, maximal)
          ) &
          theme(
            legend.position = 
              # Place legend at right if show_legend == TRUE, 
              # remove legend otherwise
              if (show_legend == TRUE) "right" else "none"
            )
      )
    
    return(plot)
    
  } else {
    # Metadata column is not defined:
    # If only one group or if metadata_column is undefined, create one plot 
    # for the object provided.
    p <- 
      suppressMessages(
        FeaturePlot(
          obj, 
          features = feature, 
          ...
        ) +
          coord_cartesian(
            xlim = c(xmin, xmax), 
            ylim = c(ymin, ymax)
          ) +
          scale_color_gradientn(
            legend_title, 
            colors = colors, 
            limits = c(minimal, maximal)
          ) +
          theme(
            legend.position = 
              # Place legend at right if show_legend == TRUE, 
              # remove legend otherwise
              if (show_legend == TRUE) "right" else "none"
            )
      )
    
    # Title: depends on custom titles argument
    if (is.null(custom_titles)){
      # custom_titles is NULL: use default behavior
      # Default behavior for single-group plots: use feature title
      p <-
        p +
        ggtitle(feature)
    } else if (custom_titles == ""){
      # custom_titles is an empty string: remove titles
      p <-
        p +
        ggtitle(NULL)
    } else {
      # All other cases: use custom titles
      p <-
        p +
        # Custom titles should be a single-element character vector in this case
        ggtitle(custom_titles)
    }
    
    
    # Apply title theme  
    p <-
      p +
      theme(
        plot.title = 
          element_text(size = 14, face = "bold")
      )
    
    return(p)
      
  }
  
}