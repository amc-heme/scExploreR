#' Pie charts for summarizing metadata in object
#'
#' shiny_pie() summarizes the object by showing the number of patients 
#' represented by each level of a categorical metadata column (patient level, as 
#' opposed to number of cells represented by each class).
#'
#' @param object The full Seurat object or a subset.
#' @param patient_colname The metadata column corresponding to the patient ID. 
#' This is required to compute patient-level metadata
#' @param group_by The metadata column to get patient count information for. 
#' The pie chart will display the number of patients represented by each level 
#' of this category. 
#' @param palette The palette to use for coloring groups. If the palette passed
#' to this function is NULL, the default ggplot2 palette is used.
#' @param show_legend If TRUE, the legend is shown (default is TRUE) 
#' @param show_title If TRUE, display a title above the plot. When TRUE, the 
#' legend title is hidden, as the default legend title is equal to the default
#' plot title (the name of the group_by metadata column). The plot title can be
#' customized by supplying a string to `plot_title`.
#' @param plot_title If defined, the value entered will be displayed as the plot
#' title, if `show_title` is TRUE.
#'
#' @noRd
shiny_pie <- 
  function(
    object,
    patient_colname,
    group_by,
    palette = NULL,
    show_legend = TRUE,
    show_title = FALSE,
    plot_title = NULL
    ){
    # For patient_level metadata: select for the specified patient metadata
    # column and the chosen group by variable. 
    plot_data <-
      object@meta.data |> 
      dplyr::select(c(.data[[patient_colname]], .data[[group_by]])) |>
      dplyr::group_by(.data[[group_by]]) |>
      # Take unique values by each patient, and count number of unique values
      unique() |> 
      dplyr::summarize(count = n())
    
    # Determine palette for plot
    # Number of colors needed: equal to number of rows in the summary table
    n_colors <- nrow(plot_data)
    
    # Palette: use colorRampPalette to "stretch" the palette so the number of
    # colors in the palette exactly matches the number needed
    colors <- 
      if (!is.null(palette)){
        # colorRampPalette() extends or contracts the given palette to 
        # produce exactly the required number of colors
        colorRampPalette(palette)(n_colors)
        # If palette() is unspecified, use ggplot2 defaults
      } else NULL
    
    plot <-
      ggplot(plot_data) +
      # Use count column created above to give number of patients by 
      # response type
      aes(x = "", y = count, fill = .data[[group_by]]) +
      geom_col(width = 1, color = "#000000") +
      geom_text(
        aes(label = count),
        position = position_stack(vjust = 0.5)
      ) +
      coord_polar(theta = "y") +
      theme_cowplot() +
      #scale_fill_brewer(palette = "Paired") +
      theme_void()
    
    layers <- 
      c(
        # Element A: Apply palette
        if (!is.null(colors)){
          list(
            scale_color_manual(
              values = colors,
              # Must explicitly specify the scale as working with 
              # the "fill" property
              aesthetics = "fill",
              # Color to use for NA values: currently fixed as "grey50"
              na.value = "grey50"
            )
          )
        },
        
        # Element B: Show/hide legend
        list(
          theme(
            legend.position = 
              if (show_legend==TRUE) {
                "right"
              } else "none"
          )
        ),
        
        # Element C: Show title if desired
        if (show_title == TRUE){
          list(
            labs(
              title = 
                if (!is.null(plot_title)){
                  plot_title
                } else {
                  group_by
                }
            ),
            theme(
              plot.title = 
                element_text(
                  hjust = 0.5,
                  face = "bold"
                ),
              # When the title is shown, hide the legend title (same text is 
              # used)
              legend.title = element_blank(),
            )
          )
        }
      )
    
    # Apply list of layers and return plot 
    plot + layers
  }