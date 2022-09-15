#' ADT Threshold Tab in Config App
#'
#' @param id ID to use for module elements.
#'
adt_threshold_tab_ui <- 
  function(
    id
    ){
    # Namespace function: prevents conflicts with IDs defined in other modules
    ns <- NS(id)
    
    sidebarLayout(
      position = "right",
      sidebarPanel = 
        sidebarPanel(
          id = "adt_threshold_sidebar",
          style = "height: 85vh; margin-bottom: 0px;",
          width = 5,
          # Elements in sidebar display conditionally based on which function 
          # the user is performing on threshold data (add new threshold data,
          # edit an existing threshold, or none of the above)
          hidden(
            # Header of options bar: depends on whether the state is "add" 
            # or "edit"
            div(
              # show-on-add: element displays when user is adding a new ADT 
              class = "show-on-add",
              tags$h4("Add ADT Threshold"),
            ),
            div(
              class = "show-on-edit",
              # Header to display when editing feature
              uiOutput(
                outputId = "threshold_header"
              ),
            ),
            # Menu to choose an ADT to add a threshold for
            div(
              class = "show-on-add",
              tags$b("Enter an ADT threshold below to add to table:"),
              selectizeInput(
                inputId = "selected_adt",
                label = NULL,
                choices = NULL,
                selected = character(0),
                options = 
                  list(
                    "placeholder" = "Enter feature",
                    "maxItems" = 1,
                    "plugins" = list("remove_button"),
                    "create" = FALSE
                  )
              )
            ),
            # UI for selecting threshold: displays when a feature is entered 
            # above, or when an existing threshold is being edited. 
            div(
              id = "threshold_picker_div",
              threshold_picker_ui(
                id = "threshold_picker",
                plot_height = "15em"
              )
            ),
            # Buttons to accept or discard threshold
            div(
              class = "show-on-add show-on-edit space-top",
              # Accept button: disabled at first; enabled when a feature 
              # threshold has been selected using the interactive ridge plot
              disabled(
                actionButton(
                  inputId = "accept_threshold",
                  class = "button-primary float-right",
                  style = "margin-left: 10px;",
                  label = "Confirm"
                )
              ),
              # Cancel button: discards feature selection and threshold, and 
              # returns menus to "idle" state
              actionButton(
                inputId = "cancel_threshold",
                class = "button-ghost float-right",
                label = "Cancel"
              )
            )
          )
        ),
      mainPanel = 
        mainPanel(
          id = "adt_threshold_main", 
          width = 7,
          tags$h3(
            "Defined ADT Thresholds",
            class = "Center"
          ),
          
          # Table of thresholds
          # Placeholder for now
          DTOutput(
            outputId = "threshold_table"
          ),
          
          # Button to add a new threshold
          div(
            class = "space-top",
            actionButton(
              inputId = "add_threshold",
              label = "New Threshold",
              class = "button-primary",
              style = "float: right;"
            )
          ),
          # JavaScript for inline edit/delete buttons on table
          # This is rendered as HTML when changes are made to the datatable
          uiOutput(
            outputId = "threshold_table_button_script"
          )
        )
    )
    
    }

#' ADT Threshold Tab in Config App
#'
#' @param id ID to use for module server instance.
#'
adt_threshold_tab_server <- 
  function(
    id
    ){
    moduleServer(
      id,
      function(input, output, session){
        
      })
}
