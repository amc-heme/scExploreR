#' Add object metadata
#' 
#' Creates a modal with an interface to add metadata to the object. Unlike other 
#' modules in scExplorer, this module only has a server component. Modals create 
#' UI in the server function. 
#' 
#' @param id Used to set a namespace for elements created in the module. 
#' Elements with an id property will have the value passed to `id` as a prefix 
#' @param modal_open_button The value of an action button or link that opens 
#' the modal. The value should be passed inside a reactive expression. For 
#' example, if the button has an id "button_id", you would pass
#' `reactive({input$button_id})`.
#'
#' @noRd
add_metadata_server <- 
  function(
    id,
    modal_open_button
    ){
    moduleServer(
      id,
      function(input, output, session) {
        # Server namespace function 
        # Used to add proper namespace to components created in the modal
        ns <- session$ns
        
        # X. Metadata Addition ####
        # Variables for recording state of menu
        add_metadata_state <- reactiveValues()
        # Window state: starts as "closed" and is set to "open" when the link
        # to open the modal is pressed
        add_metadata_state$window_state <- "closed"
        
        ## X.1. Control state of window ####
        # Set state to open when the open link is selected
        observe({
          req(modal_open_button())
          
          modal_open_button()
          
          add_metadata_state$window_state <- "open"
        })
        
        ## X.2. Open/close window based on state ####
        observe({
          req(add_metadata_state$window_state)
          
          if (add_metadata_state$window_state == "open"){
            showModal(
              modalDialog(
                title = "Add New Metadata",
                footer = 
                  tagList(
                    actionButton(
                      inputId = ns("cancel"),
                      label = "Cancel",
                      class = "button-ghost"
                    ),
                    actionButton(
                      inputId = ns("confirm"),
                      label = "Confirm",
                      class = "button-primary"
                    )
                  ),
                size = "xl",
                # Modal Content
                div(
                  style = "height: 500px;",
                  # Left-hand side of modal: buttons to choose dataset
                  tags$p(
                    paste0(
                      "Upload a table below with the IDs of a patient/sample ",
                      "variable as they appear in the app in one column, and ",
                      "group IDs in a second column."
                    )
                  ),
                  # Add interface to download a sample CSV to fill out?
                  # Upload window for CSV
                  fileInput(
                    inputId = ns("file"),
                    label = "Upload a CSV file with groups to add:",
                    accept = ".csv",
                    placeholder = "Upload File"
                  ),
                  checkboxInput(
                    inputId = ns("has_header"), 
                    label = "My file has headers", 
                    value = TRUE
                  ),
                  # Preview and options: shows when upload is complete
                  hidden(
                    # Column *in the uploaded table* to use for the sample 
                    # column
                    selectInput(
                      inputId = ns("upload_sample_colname"),
                      label = "Choose column to map to sample variable:",
                      choices = NULL,
                      selected = NULL
                      ),
                    # Column *in the object* to map the object too
                    selectInput(
                      inputId = ns("object_sample_colname"),
                      label = 
                        "Choose sample variable from object to map to:",
                      choices = NULL,
                      selected = NULL
                      ),
                    uiOutput(outputId = ns("menus"))
                    )
                  )
                )
              )
            
            # Initialize confirm button in a disabled state 
            shinyjs::disable(
              id = "confirm"
            )
          } else if (add_metadata_state$window_state == "closed"){
            # When the window state is set to "closed", remove the modal
            removeModal()
          }
        })
        
        ## X.3. Store uploaded table ####
        metadata_table_upload <-
          reactive({
            req(input$file)
            
            read.csv(
              file = input$file$datapath,
              header = input$has_header
            )
          })
        
        observe({
          req(metadata_table_upload())
          
          print(metadata_table_upload())
        })
        
        ## X.4. Respond to Cancel button ####
        observe({
          req(input$cancel)
          
          add_metadata_state$window_state <- "closed"
        })
        
        ## X.5. Respond to Confirm button ####
        observe({
          req(input$confirm)
          
          add_metadata_state$window_state <- "closed"
        })
      }
    )
  }