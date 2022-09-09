#' Config App Modals
#'
#' Displays a modal to either load a new config file, or create a new config 
#' file based on a Seurat object.
#'
#'
config_loader_modal <- 
  function(
    primaryId,
    ghostId,
    configPathId#,
    #error_msg = ""
    ){
    modalDialog(
      title = "Load Config File",
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
          inputId = configPathId,
          label = 
            'Welcome. Please choose a config file from the data/ directory,
             or select "new file"',
          choices =
            list.files(
              path = "./data/",
              pattern = "*.yaml$",
              ignore.case = TRUE,
              recursive = TRUE
              )
          ),
        # If an error message is specified, display it underneath the
        # selection window.
        # if (isTruthy(error_msg)){
        #   tags$p(
        #     class = "error",
        #     error_msg
        #     )
        #   }
        )
      )
  }