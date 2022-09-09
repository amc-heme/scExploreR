config_start_modal <- 
  function(
    createId,
    loadId
  ){
    modalDialog(
      title = "Welcome",
      size = "l",
      # No footer (must explicitly disable)
      footer = NULL,
      # Modal content
      div(
        class = "scexplorer-modal-content vertical-button-group",
        actionButton(
          inputId = createId,
          label = "Create New Config File",
          class = "button-primary x-large welcome-screen-button"
        ),
        actionButton(
          inputId = loadId,
          label = "Load Config File",
          class = "button-ghost x-large welcome-screen-button"
        )
      )
    )
  }