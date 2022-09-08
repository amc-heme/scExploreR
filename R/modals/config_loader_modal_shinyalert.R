#' Config App Modals
#'
#' Displays a modal to either load a new config file, or create a new config 
#' file based on a Seurat object.
#'
#' @param object a reactiveVal object corresponding to the Seurat object .
#' @param config a reactiveVal object for storing the contents of a loaded 
#' config file.
#'
shinyalert_config_loader_modal <- 
  function(
    object,
    config
  ){
    
    
    shinyalert(
      html = TRUE,
      # Modal will only close when either button is clicked
      closeOnEsc = FALSE,
      showCancelButton = TRUE,
      cancelButtonText = "Create new config file",
      showConfirmButton = TRUE,
      confirmButtonText = "Load",
      inputId = "startup_modal",
      class = "scexplorer-modal",
      # Modal content (use html = TRUE to define UI instead of text)
      text =
        tagList(
          selectInput(
            inputId = "config_path",
            label =
              'Choose a config file from the data/ directory,
            or select "new file"',
            choices =
              list.files(
                path = "./data/",
                pattern = "*.yaml$",
                ignore.case = TRUE,
                recursive = TRUE
              )
          ),
        ),
      # Function to run after modal is closed
      callbackR = 
        function(value){
          # Return value is TRUE if confirm ("Load") button is pressed, 
          # and FALSE if cancel ("Create config file) button is pressed
          if (value == TRUE){
            # Load config file based on path selected in the modal
            config_file <-
              read_yaml(
                file =
                  paste0(
                    "./data/",
                    input$config_path
                  )
              )
            
            config(config_file)
          } else {
            # Display a second modal for choosing the object to use as a basis
            # for a new config file
            object_loader_modal(
              object = object,
              config = config
            )
          }
        }
    )
  }