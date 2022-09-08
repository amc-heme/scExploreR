#' Config App Modals
#'
#' Displays a modal to choose the object to use as a basis for a new config file.
#'
#' @param object_path_inputId the input ID used to create the selectInput in the
#' modal, where the user will select a path for the Seurat Object from the .rds 
#' files in `/data`.
#' @param object a reactiveVal object corresponding to the Seurat object .
#' @param config a reactiveVal object for storing the contents of a loaded 
#' config file.
#'
shinyalert_object_loader_modal <-
  function(
    modal_inputId,
    object_path_inputId,
    error_msg = ""
    # object,
    # config
    ){
    shinyalert(
      inputId = modal_inputId,
      html = TRUE,
      # Modal will only close when either button is clicked
      closeOnEsc = FALSE,
      showCancelButton = FALSE,
      #cancelButtonText = "Cancel",
      showConfirmButton = TRUE,
      confirmButtonText = "Confirm",
      class = "scexplorer-modal",
      # Modal content (use html = TRUE to define UI instead of text)
      text =
        tagList(
          selectInput(
            inputId = object_path_inputId,
            label =
              'Choose a Seurat object from the data/ directory.',
            choices =
              list.files(
                path = "./data/",
                pattern = "*.rds$",
                ignore.case = TRUE,
                recursive = TRUE
              )
          ),
          # If an error message is specified, display it underneath the
          # selection window.
          if (isTruthy(error_msg)){
            tags$p(
              class = "error",
              error_msg
            )
          }
        )
      # callbackR = 
      #   function(value){
      #     if (value == TRUE){
      #       # Load object
      #       object <-  
      #         readRDS(
      #           file =
      #             paste0(
      #               "./data/",
      #               input$object_path
      #             )
      #         )
      #       
      #       # Store in reactiveVal for object 
      #       object(object)
      #     } else {
      #       config_loader_modal(
      #         object = object,
      #         config = config
      #       )
      #     }
      #   }
    )
  }
