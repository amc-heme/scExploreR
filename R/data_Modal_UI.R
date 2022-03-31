# data_Modal - UI Function

# Displays a popup (modal) with the available dataset choices.

# Arguments
# selected_key (reactive): this 
data_Modal <- function(datasets,
                       selected_key){
  if(!is.reactive(selected_key)){
    stop("Argument 'selected_key' must be reactive.")
  }
  # Fetch possible keys for datasets (keys are the names for each 
  # dataset in the list)
  keys <- names(datasets)
  
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
}