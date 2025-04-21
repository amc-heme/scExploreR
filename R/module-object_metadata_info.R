#' object_metadata_info UI
#'
#' Displays information on object metadata.
#'
#' @noRd
object_metadata_info_ui <- 
  function(
    id
    ){
    ns <- NS(id)
  
    tagList(
      # Search bar for metadata variable
      selectizeInput(
        inputId = ns("metadata_var"),
        multiple = FALSE,
        label = "Enter a metadata variable:",
        choices = NULL,
        selected = NULL,
        # Add remove button to inputs
        options = list(
          # Do not allow user to input features not
          # in the list of options
          'create'= FALSE
          )
        ),
      
      hidden(
        div(
          id = ns("metadata_var_info"),
          # Type of metadata variable
          span(
            tags$b("Type: "),
            textOutput(
              outputId = ns("var_type")
              )
            ),
          # Description of metadata variable (if provided)
          span(
            tags$b("Description: "),
            uiOutput(
              outputId = ns("var_description")
              )
            ),
          # Summary of variable 
          # Either a description of unique values (for categorical variables)
          # or summary statistics (numeric variables)
          uiOutput(
            outputId = ns("var_summary")
            )
          )
        )
      )
    }

#' object_metadata_info Server
#' 
#' Displays information on object metadata.
#'
#' @param id ID to use for module. This must match the id provided to the ui
#' instance of the module for the module to function properly.
#' @param object A single-cell object.
#'
#' @noRd
object_metadata_info_server <- 
  function(
    id,
    object
    ){
    moduleServer(
      id,
      function(input, output, session){
        # Populate search menu with list of metadata options
        observe(
          label = paste0(id, ": Populate metadata list"),
          {
            # Choices: display "featured" metadata from config file first,
            # then all metadata variables left in the object
            #choices <- 
              
          })
        
        # Show/hide summary stats window 
        observe(
          label = paste0(id, ": show/hide summary"),
          {
            target_id <- "metadata_var_info"
            
            if (isTruthy(input$metadata_var)){
              shinyjs::showElement(
                id = target_id,
                anim = TRUE
                )
              } else {
                shinyjs::hideElement(
                  id = target_id,
                  anim = TRUE
                  )
                }
            })
        
        # Fetch metadata for variable
        # (separated into a separate reactive expression so calculation 
        # is only done once when the variable is changed)
        metadata_values <-
          reactive({
            req(input$metadata_var)
            
            SCUBA::fetch_metadata(
              object = isolate(object),
              vars = input$metadata_var,
              return_class = "vector"
              )
          })
        
        # Type (class) of variable
        output$var_type <- 
          renderText({
            class(metadata_values())
          })
      }
    )
  }
