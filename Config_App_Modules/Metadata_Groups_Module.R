## B.2 Metadata Groups Module (second level)####
### Module UI
metadata_group_fields_ui <- function(id,remove_button=FALSE,temp_choices=NULL){
  # Namespace function
  ns <- NS(id)
  
  ui <- span(
    # inline-containers class is used to change the display style of all 
    # containers within the element
    class="inline-containers input-no-margin align-containers-top",
    # id: used to delete the line if the remove button is clicked
    # Passing NULL to ns() will make the id equal to the namespace id 
    # passed to the module 
    id=glue("{ns(NULL)}"),
    textInput(inputId = ns("group_name"),
              label = NULL,
              width = "120px",
              placeholder = "Group Name"
    ),
    selectizeInput(
      inputId = ns("group_members"),
      label=NULL,
      # Choices are dynamic and must be updated by the module server
      width = "260px",
      choices = temp_choices,
      selected = NULL,
      multiple= TRUE,
      options = 
        list(
          placeholder="Values in Group",
          size=10
        )
    ),
    if(remove_button == TRUE){
      actionButton(
        inputId = ns("remove_module"),
        label="",
        icon = icon("times"),
        class = "x-button" 
      )
    } else NULL
  )
  
  return(ui)
}

### Module Server
# id: id given to this module for namespacing
# possible selections: a reactive vector of unique values within the 
# metadata category that can be searched in the selectize inputs in this server
metadata_group_fields_server <- function(id,
                                         possible_selections){
  # Initialize module
  moduleServer(
    id,
    function(input, 
             output,
             session){
      # Update the selectize input for this field with valid entries
      # For now, this occurs only once when the module is created
      # With the latest updates to the code structure, updateSelectizeInput is 
      # running each time an entry is made in the metadata group, which erases 
      # input
      # updateSelectizeInput(session,
      #                      #Assuming namespacing is not required
      #                      inputId = "group_members",
      #                      choices = 
      #                         str_sort(possible_selections,numeric=TRUE)
      #                      )
      
      # Deleted: returned to parent modules to notifiy if output has been
      # deleted. When the remove button is clicked, this is set to TRUE 
      deleted <- FALSE
      # Ensures value is reactively updated when the remove button is clicked
      makeReactiveBinding("deleted")
      
      # Code to remove the UI and server instances when the remove 
      # button is clicked
      observeEvent(input$remove_module,
                   ignoreNULL = TRUE,
                   # The once argument will remove the observer when the code 
                   # below is ran (the observer should be deleted to optimize 
                   # performance since the button it connects to will no longer
                   # exist)
                   once = TRUE,
                   # IgnoreInit is set to True to keep the server code from 
                   # running when the observer is created
                   ignoreInit = TRUE,
                   {
                     print(glue("{session$ns(NULL)}: delete button"))
                     # id of the target should be equal to the "full" namespaced 
                     # id of this module (includes all levels of nested modules, 
                     # and retrieved with session$ns())
                     removeUI(selector = glue("#{session$ns(NULL)}"))
                     # Remove shiny input bindings linked to this module 
                     # to optimize performance
                     remove_shiny_inputs(id,input)
                     # Notify parent modules that the field has been deleted 
                     print("set deleted to TRUE")
                     deleted <<- TRUE
                   })
      
      # Return input to the options module as a reactive list
      return(
        reactive(
          label = glue("Return from {session$ns(NULL)}"),
          {
            list(
              `group_name`=input$group_name,
              `group_members`=input$group_members,
              `deleted`=deleted
            )
          })
      )
    })
}
