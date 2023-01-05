#' summary_tags
#'
#' Generates a list of Shiny tags to print the output of base R's `summary()`.
#'
#'
#' @param summary_results The output of base R `summary()`, as a factor.
#' @param header_class Optional, CSS class to apply to headers ("Min:", "Q1:", 
#' "Median", "etc.)  
#' @param text_class Optional, CSS class to apply to the results. 
#' 
#' @return Output format:
#'           Min: <minimum value>
#'           Q1: <first quartile>
#'           Median: <median>
#'           Mean: <mean>
#'           Q3: <third quartile>
#'           Max: <max>
#' @noRd
summary_tags <- 
  function(summary_results,
           header_class = NULL,
           text_class = NULL
           ){
    # Text for each header
    headers <- c("Min:","Q1:","Median:","Mean:","Q3:","Max:")
    
    # List of tags to return 
    tagList(
      lapply(
        # Create one tag for each of the six summary statistics
        1:6,
        function(i){
          # Tag group for each statistic
          tags$p(
            # Header text ("Min:", "Q1:", "Median", etc.)
            tags$b(
              headers[i],
              # Apply header class, if the user provides one
              class = 
                if (!is.null(header_class)){
                  header_class
                }
              ),
            # Text class for summary result, if provided
            class =
              if (!is.null(text_class)){
                text_class
              },
            summary_results[i]
          )
        }
      )
    )
  }

