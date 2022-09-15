#' Metadata Tab in Config App
#'
#' @param id ID to use for module elements.
#'
metadata_tab_ui <- 
  function(
    id,
    object_data
    ){
    # Namespace function: prevents conflicts with IDs defined in other modules
    ns <- NS(id)
    
    sidebarLayout(
      applet_sidebar_panel(
        div(
          class = "input-no-margin",
          uiOutput(
            outputId = "metadata_sortable_bucket"
          )
        )
      ),
      applet_main_panel(
        tagList(
          # Card for generic metadata options 
          div(
            class = "optcard single-space-bottom",
            tags$strong(
              glue("General Options"),
              class = "large center"
            ),
            # Select metadata column to use for patient/sample level metadata
            # analysis
            selectInput(
              inputId = "patient_colname",
              label = 
                "Patient ID column (optional, used for patient-level 
                metadata analysis)",
              # Can select "none" or any categorical metadata column
              choices = c("none", object_data$non_numeric_cols),
              selected = NULL,
              width = "380px"
            )
          ),
          
          # Options specific to each metadata column
          # Options for Numeric metadata (Numeric metadata is currently not
          # displayed)
          
          # Options for Categorical, logical metadata
          # Create a metadata options "card" for each non-numeric metadata 
          # column in the object. Cards below are hidden and display when the
          # corresponding metadata category is selected
          lapply(
            object_data$non_numeric_cols,
            function(colname){
              options_ui(
                id = colname,
                object = object,
                optcard_type = "metadata"
              )
            }
          ),
          # TEMP: add an additional card displaying the outputs 
          # from the metadata tab
          div(
            class = "optcard",
            verbatimTextOutput(outputId = "print_metadata")
          )
        )
      )
    )
    
    }

#' metadata_tab_server
#'
#' @param id ID to use for module server instance.
#'
metadata_tab_server <- 
  function(
    id
    ){
    moduleServer(
      id,
      function(input, output, session){
        
      })
}
