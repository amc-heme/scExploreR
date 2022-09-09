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
object_loader_modal <-
  function(
    #modal_inputId,
    primaryId,
    ghostId,
    objectPathId,
    error_msg = ""
    # object,
    # config
    ){
    modalDialog(
      title = "Create New Config File",
      footer = 
        tagList(
          actionButton(
            inputId = ghostId,
            label = "Cancel",
            class = "button-ghost extra-space"
          ),
          actionButton(
            inputId = primaryId,
            label = "Load",
            class = "button-primary extra-space"
          )
        ),
      size = "l",
      # Modal content
      div(
        class = "scexplorer-modal-content",
        selectInput(
          inputId = objectPathId,
          label =
            'Choose a Seurat object from the data/ directory. The object will 
             be used as the basis for the new config file.',
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
    )
  }
