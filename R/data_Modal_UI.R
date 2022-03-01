# data_Modal - UI Function

# Displays a popup (modal) with the available dataset choices.

# Arguments
# selected_key (reactive): this 
data_Modal <- function(selected_key){
  if(!is.reactive(selected_key)){
    stop("Argument 'selected_key' must be reactive.")
  }
  
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
          # Names of choices displayed to user
          choiceNames = 
            list(
              "Longitudinal Data", 
              "AML Dataset"
            ),
          # Values: the server values are keys used to subset the 'datasets' 
          # list for information on each dataset choice
          choiceValues = 
            list(
              "d0_d30",
              "AML_samples"
            ),
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
        verbatimTextOutput(outputId = "buttontest")
      )
    )
  )
}