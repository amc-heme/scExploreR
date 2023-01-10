#' Data Modal
#' 
#' A UI function. Displays a popup (modal) with the available dataset choices.
#'
#' @param datasets a list of the available datasets, generated in the main app.
#' @param selected_key the key of the selected dataset (from the main app).
#'
#' @noRd
data_modal <- function(datasets,
                       selected_key){
  # Test if `selected_key` is reactive 
  if(!is.reactive(selected_key)){
    stop("Argument 'selected_key' must be reactive.")
  }
  
  # Fetch possible keys for datasets (keys are the names for each 
  # dataset in the list)
  keys <- names(datasets)
  
  if (dataset_config_has_info(datasets)){
    # Latest implementation: dataset information defined in the config file
    modalDialog(
      title = "Choose Dataset",
      footer = 
        actionButton(
          inputId = "confirm_selection",
          label = "Confirm Selection"
        ),
      size = "xl",
      # Modal Content
      div(
        style = "height: 500px;",
        # Left-hand side of modal: buttons to choose dataset
        div(
          class = "two_column",
          style = "float: left; height: 500px;",
          radioGroupButtons(
            inputId = "data_key",
            # Names of choices displayed to user: `label` property from the 
            # config file for each dataset in datasets
            choiceNames = 
              lapply(
                keys,
                function(key){
                  datasets[[key]]$config$label
                  }
              ),
            # Values: the server values are keys used to subset the 'datasets' 
            # list for information on each dataset choice (names(datasets))
            choiceValues = keys,
            # Selected value: equal to the last key selected, if it exists. If
            # there is no last key, NULL will be passed and the first dataset 
            # will be selected by default. This value is reactive.
            selected = selected_key(),
            #status = "dataset_choices",
            size = "lg",
            direction = "vertical",
            justified = TRUE,
          )
        ),
        # Right-hand side: will contain a description of the selected dataset
        div(
          class = "two_column",
          style = "float: right; height : 500px;",
          # Dataset plot/image and description
          # This output is defined in the main server for now. In the future, 
          # the modal content should be put in an module for scalability.
          
          # Dataset preview: show either an image or a plot, depending on the 
          # choices the user has made in the config file. The relevant output is
          # shown and the other hidden.
          imageOutput(
            outputId = "dataset_image",
            width = "290px",
            height = "218px"
          ),
          plotOutput(
            outputId = "dataset_dimplot",
            width = "290px",
            height = "218px"
          ),
          # Text description
          tags$strong("Dataset Description"),
          textOutput(outputId = "dataset_description")
        )
      )
    )
  } else {
    # Backwards compatibility: original methods if dataset info is in the 
    # browser config file, and newer methods if datset info is in the dataset
    # config file.
    modalDialog(
      title = "Choose Dataset",
      footer = 
        actionButton(
          inputId = "confirm_selection",
          label = "Confirm Selection"
        ),
      size = "xl",
      # Modal Content
      div(
        style = "height: 500px;",
        # Left-hand side of modal: buttons to choose dataset
        div(
          class = "two_column",
          style = "float: left; height: 500px;",
          radioGroupButtons(
            inputId = "data_key",
            # Names of choices displayed to user: `label` property for each 
            # dataset in datasets
            choiceNames = 
              lapply(
                keys,
                function(key){datasets[[key]]$label}
              ),
            # Values: the server values are keys used to subset the 'datasets' 
            # list for information on each dataset choice (names(datasets))
            choiceValues = keys,
            # Selected value: equal to the last key selected, if it exists. If
            # there is no last key, NULL will be passed and the first dataset 
            # will be selected by default. This value is reactive.
            selected = selected_key(),
            #status = "dataset_choices",
            size = "lg",
            direction = "vertical",
            justified = TRUE,
          )
        ),
        # Right-hand side: will contain a description of the selected dataset
        div(
          class = "two_column",
          style = "float: right; height : 500px;",
          # Dataset image and description
          # This output is defined in the main server for now. In the future, the
          # modal content should be put in an module for scalability.
          # Image: show a dimplot of the dataset, for a visual overview
          imageOutput(
            outputId = "dataset_image",
            width = "290px",
            height = "218px"
          ),
          # Text description
          tags$strong("Dataset Description"),
          textOutput(outputId = "dataset_description")
        )
      )
    )
  }
}