#' Subset_Summary
#'
#' This module displays the unique values for each metadata variable (category)
#' defined in the config file. The module is used in the subset menu of the 
#' plots tab to dynamically display the current subset to the user. 
#'
#' @param id ID to use for module. All inputs and outputs created will be
#' namespaced using this ID.
#' @param category_labels labels used for each metadata variable (category), as
#' defined in the config file.
#'
#' @return UI code for the subset summary module.
#' 
#' @noRd
subset_summary_ui <- function(id,
                              category_labels){
  # Namespace function: prevents conflicts with IDs defined in other modules
  ns <- NS(id)
  
  # UI: Generate output containers for each category to display unique values
  # Format: <category>: <unique values in category, or all if all are present>
  tagList(
    # lapply is used to produce an output container for each metadata category
    # in the config file (as defined in category_labels) 
    lapply(
      # names(category_labels) give the ID's of each category (as they appear in
      # the Seurat object), whereas the values of category_labels are the labels 
      # for each category
      names(category_labels()), 
      # 'category' is an iteration of names(category_labels) 
      function(category){
        # UI element: an inline container with the label and the output computed
        # in the server function
        div(
          tags$strong(glue("{category_labels()[[category]]}: ")),
          # OutputId: uses category name (not the label)
          textOutput(
            outputId = ns(glue("selected_{category}")), 
            inline = TRUE
            )
          )
      })
  )
}

#' Subset_Summary
#'
#' This module displays the unique values for each metadata variable (category)
#' defined in the config file. The module is used in the subset menu of the 
#' plots tab to dynamically display the current subset to the user.
#' 
#' @param id ID to use for module. All inputs and outputs created will be
#' namespaced using this ID.
#' @param object the Seurat object (either the full object or a subset).
#' @param category_labels labels used for each metadata variable (category), as
#' defined in the config file.
#' @param unique_metadata a list giving the unique values for each metadata 
#' variable (category), from the full object. 
#'
#' @return server code for the subset summary module.
#' 
#' @noRd
subset_summary_server <- function(id,
                                  object,
                                  category_labels,
                                  unique_metadata
                                  ){
  moduleServer(
    id,
    function(input,output,session){
      # The categories for which to create inputs now depend on the dataset 
      # loaded. Outputs must now be reactively created each time a new dataset
      # is loaded. This will result in duplicate outputs existing when a dataset 
      # is re-loaded after loading a different dataset, but this should be fine
      # for now. 
      observeEvent(
        # Observer reacts when unique_metadata() is changed, which should be 
        # when the dataset is changed
        unique_metadata(),
        {
          # Compute unique values in the object for each category
          lapply(
            # Loop through category IDs (names of `category_labels`)
            names(isolate(category_labels())),
            function(category, object){
              output[[glue("selected_{category}")]] <-
                renderText({
                  # If the object is undefined, return an error message
                  validate(
                    need(
                      object(),
                      message = "no cells in subset"
                    )
                  )
                  
                  # Store unique values for category in full object and subset
                  # Uses unique_values() in-house function 
                  subset_values <- unique_values(object, category)
                  original_values <- isolate(unique_metadata())[[category]]
                  
                  if (setequal(subset_values, original_values)){
                    # If the unique values for the subset match the unique
                    # values in the full object, print "all".
                    "All"
                  } else {
                    # If they do not match, print the values 
                    # in the current subset.
                    vector_to_text(subset_values)
                  }
                })
            },
            # Additional variables in the function above are passed to lappy here
            object
          )
        })
    })
}