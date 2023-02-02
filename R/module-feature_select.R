#' feature_select_ui 
#'
#' @param id ID to use for module elements.
#' @param assay_config the assays section of the config file loaded in the main app
#' @param label label to show above the feature 
#' @param feature_guide if TRUE, display a guide showing which types of features
#' may be entered (default is FALSE). 
#' @param placeholder placeholder text to display when no features are selected. 
#' @param multiple if TRUE, allow user to select multiple features (default is 
#' TRUE)
#' @param remove_button if TRUE, show a remove button on each selected feature. 
#' If FALSE, no remove button will be displayed and the user will have to use 
#' the backspace key to remove features.
#' 
#' @noRd
#'
feature_select_ui <- 
  function(
    id,
    assay_config = NULL,
    label = NULL,
    feature_guide = FALSE,
    placeholder = NULL,
    multiple = TRUE,
    remove_button = TRUE
    ){
    # Namespace function: prevents conflicts with IDs defined in other modules
    ns <- NS(id)
    
    # Options list for selectize input
    options_list <- 
      # Named list based on conditional statements: a list for each
      # condition is created, and the results are concatenated
      c(
        # Placeholder text when no features are selected
        if (!is.null(placeholder)){
          list("placeholder" = placeholder)
          },
        # Allow/restrict selection of multiple features
        # Default is to allow an infinite number of selections
        if (multiple == FALSE){
          list("maxItems" = 1)
          },
        # Remove button next to each feature
        if (remove_button == TRUE){
          list("plugins" = list("remove_button"))
        },
        # Do not allow user to enter features not in list
        list("create" = FALSE)
      )
  
    tagList(
      if (!is.null(label)){
        tags$p(tags$b(label))
      } else NULL,
      # Guide of available features, if enabled
      if (feature_guide == TRUE){
        collapsible_panel(
          inputId = ns("which_features"),
          label = "What Can I Enter Here?",
          active = FALSE,
          transparent = TRUE,
          size = "s",
          "The following types of features are available for this object:",
          tags$ul(
            lapply(
              # Fetch assay labels from config file
              assay_config(),
              function (assay_entry) {
                # Use label if defined, otherwise use `assay` field
                if (!is.null(assay_entry$dropdown_title)){
                  tags$li(assay_entry$dropdown_title)
                } else {
                  tags$li(assay_entry$assay)
                }
                
              }
            )
          )
        )
      } else NULL,
      # Selectize input
      # Input defined here; choices populated in server
      selectizeInput(
        inputId = ns("feature"),
        label = NULL,
        choices = NULL,
        selected = character(0),
        options = options_list
      )  
    )
    }

#' feature_select_server
#'
#' @param id ID to use for module server instance.
#' @param valid_features Available features in the current object. This is 
#' computed in the main server function and changes in response to the object.
#'
#' @noRd
feature_select_server <- 
  function(
    id,
    valid_features
    ){
  moduleServer(
    id,
    function(input,output,session){
      # Update feature selection when valid_features() changes (happens upon
      # object change)
      observeEvent(
        valid_features(),
        label = "Render choices for feature selection",
        {
          # Wait until the input exists to update it
          req(input$feature)
          
          updateSelectizeInput(
            session,
            # Do not namespace IDs in update* functions
            inputId = "feature", 
            choices = valid_features(), 
            server = TRUE
          )
        })
      
      # Return selected feature(s)
      reactive({
        input$feature
      })
    })
}
