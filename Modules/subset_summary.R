# Subset_Summary
# This module displays the unique values for each metadata category defined in
# the config file.

# subset_summary_ui
# Arguments
# id: ID to use for module elements.
# category_labels: a named list of each metadata category in the config file, 
# with its label. This is defined in the main app at startup.
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
      names(category_labels), 
      # 'category' is an iteration of names(category_labels) 
      function(category){
        # UI element: an inline container with the label and the output computed
        # in the server function
        div(
          # Label = category_labels[[category]] 
          tags$strong(glue("{category_labels[[category]]}: ")),
          # OutputId: uses category name (not the label)
          textOutput(
            outputId = ns(glue("selected_{category}")), 
            inline = TRUE
            )
          )
      })
  )
}

# subset_summary_server
# Arguments
# id: id used to connect module server to module UI
# object: a Seurat object. This can be either the full object or a subset.
# category_labels: a named list of each metadata category in the config file, 
# with its label. This is defined in the main app at startup.
# unique_metadata: a list giving the unique values for each metadata category,
# from the full object. 
subset_summary_server <- function(id,
                                  object,
                                  category_labels,
                                  unique_metadata){
  moduleServer(
    id,
    function(input,output,session){
      # Compute unique values in the object for each category
      lapply(
        # Loop through category IDs (names of `category_labels`)
        names(category_labels),
        function(category, object){
          output[[glue("selected_{category}")]] <-
            renderText({
              # Store unique values for category in full object and subset
              # Uses unique_values() in-house function 
              subset_values <- unique_values(object, category)
              original_values <- unique_metadata[[category]]
              
              if (setequal(subset_values, original_values)){
                # If the unique values for the subset match the unique values
                # in the full object, print "all".
                "All"
              } else {
                # If they do not match, print the values in the current subset.
                vector_to_text(subset_values)
              }
            })
        },
        # Additional variables in the function above are passed to lappy here
        object
      )
    })
}