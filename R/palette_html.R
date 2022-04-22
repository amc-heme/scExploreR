#' palette_html
#' 
#' Displays a color palette using HTML.
#'
#' @param palette The palette to convert to HTML. This should be a character 
#' vector with each color as an element. It is recommended to use hex codes for
#' colors, but RBG colors will work if they are entered using a format
#' interpretable by CSS.
#' @param palette_name Optional, a character vector giving the name of the
#' palette as you would like it to be displayed. The name will appear to the
#' right of the palette.
#' @param n The number of colors to show in the palette. The default is 6.
#' @param output_html If TRUE, output raw HTML as a character vector. This is
#' useful for inclusion as a `shinywidgets::pickerInput` option, or in
#' situations where the default shiny.tag format is not interpereted correctly.
#' The default is FALSE, which will return a shiny.tag with the HTML. When using
#' this function in an R script with other Shiny elements (such as within a UI
#' function such as `fluidPage()` or `tagList()`, output_html should be FALSE. 
#'
#' @return Either a shiny.tag, or a character vector with HTML code for the palette
palette_html <- function(palette, 
                         palette_name = NULL, 
                         n = 6, 
                         output_html = FALSE
){
  # Compress palette to n colors (6 recommended)
  palette_compressed <- colorRampPalette(palette)(n)
  
  # Create HTML tag for palette
  tags <- 
    div(
      class = "palette",
      tagList(
        lapply(
          palette_compressed,
          function(color){
            span(
              style = 
                glue(
                  "background-color: {color};
                height: 1em; 
                width: 1em; 
                display: inline-block;"
                )
            )
          }
        ),
        if (!is.null(palette_name)){
          span(palette_name)
        } else NULL
      )
    )
  
  # If requested, output raw HTML. This is required for passage to pickerInput 
  # options, and for anything else that requires HTML as a character vector.
  # The default output_hyml == FALSE should be used for printing the palette 
  # within a UI function in a Shiny app.
  if (output_html == TRUE){
    return(as.character(tags))
  } else {
    return(tags)
  }
  
}