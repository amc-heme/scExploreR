#' field_ui
#'
#' field_ui is a utility function used by both the UI and server functions in 
#' the metadata_group_fields module. field_ui creates the UI for a group entry
#' field, which consists of a text box where the name of a group is entered, and
#' a selectize input where the members of a group are chosen from a list of 
#' available options.
#'
#' @param inputId The input ID to use for creating inputs for the field
#' @param remove_button If TRUE, add a button to the right hand side of the 
#' inputs that will remove the field when pressed.
#' @param choices A vector with all possible choices that may be sorted into 
#' groups.
#'
field_ui <- 
  function(
    inputId,
    remove_button = FALSE,
    choices = NULL
  ){
    span(
      # inline-containers class is used to change the display style of all 
      # containers within the element
      class = "inline-containers input-no-margin align-containers-top",
      id = glue("{inputId}_field"),
      textInput(
        inputId = glue("{inputId}_name"),
        label = NULL,
        width = "120px",
        placeholder = "Group Name"
      ),
      
      selectizeInput(
        inputId = glue("{inputId}_members"),
        label = NULL,
        # Choices are dynamic and must be updated by the module server
        width = "260px",
        choices = choices,
        selected = NULL,
        multiple = TRUE,
        options = 
          list(
            placeholder = "Values in Group",
            size = 10
          )
      ),
      
      if (remove_button == TRUE){
        actionButton(
          inputId = glue("{inputId}_remove"),
          label = "",
          icon = icon("times"),
          class = "x-button" 
        )
      } else NULL
    )
  }

#' metadata_group_fields_ui 
#' 
#' Creates an interface for specifying any number of metadata groups for the
#' config file. The interface will initially contain a text entry for specifying
#' the name of a metadata group and a selectize input for choosing the members
#' of the group. An "add field" button appears below: when it is pressed, a 
#' duplicate text box/selectize combo will appear for specifying an additional 
#' field. A remove button appears beside the second and subsequent inputs; when
#' this is pressed, the field will disappear and the output will no longer 
#' include the server value of that input.  
#'
#' @param id ID to use for module elements. 
#' @param unique_values Unique values for the metadata category. User will 
#' define group members based on the choices provided to this argument.
#'
metadata_group_fields_ui <- 
  function(
    id,
    unique_values
    ){
    # Namespace function: prevents conflicts with IDs defined in other modules
    ns <- NS(id)

    # Interface returned by module UI function
    tagList(
      # Execute function to create first field
      field_ui(
        # Format for ID: use leading zeros to make number 3 digits
        inputId = ns(sprintf("%03d", 1)),
        remove_button = FALSE,
        choices = unique_values
        ),
      # Add field button (additional fields will be created above this input)
      actionButton(
        inputId = ns("add_group"),
        label = "Add Group",
        width = "100px"
        )
      )
    }


#' metadata_group_fields_server
#'
#' @param id Id to use for module.
#' @param category_name The name of the metadata category for which group 
#' information is being recorded. This is used to update values when loading 
#' a config file.
#' @param unique_values Unique values for the metadata category. User will 
#' define group members based on the choices provided to this argument.
#'
metadata_group_fields_server <- 
  function(
    id,
    category_name,
    unique_values
    ){
    moduleServer(
      id,
      function(input, output, session){
        # Server namespace function 
        ns <- session$ns
        
        # Reactive triggers to programmatically add fields upon 
        # loading config file
        add_trigger <- makeReactiveTrigger()
        
        # Counter: increments when "add group" is pressed and is used to define
        # input ID's for fields
        counter <- reactiveVal(1)
        
        # n_fields: separate from counter. Counts number of fields currently 
        # created in the module, and decrements when the "remove" buttons next
        # to fields are pressed.
        # One field exists upon module creation.
        n_fields <- reactiveVal(1)
        
        # Reactive values object for storing observers, outputs, 
        # reactive triggers
        module_data <- reactiveValues()
        module_data$field_observers <- list()
        module_data$field_values <- list()
        module_data$remove_triggers <- list()
        
        # 1. Add observer for first field --------------------------------------
        # Input ID for first field (not namespaced: no uses of this ID require
        # namespacing)
        field_id_1 <- sprintf("%03d", 1)
        
        observeEvent(
          c(input[[glue("{field_id_1}_name")]], 
            input[[glue("{field_id_1}_members")]]),
          ignoreNULL = FALSE,
          {
            # Add new values to list (module_data$field_values)
            module_data$field_values[[field_id_1]] <-
              list(
                `group_name` = input[[glue("{field_id_1}_name")]],
                `group_members` = input[[glue("{field_id_1}_members")]]
                )
            
            # Sort list so ID's appear in alphanumeric order
            module_data$field_values <-
              module_data$field_values[
                str_sort(names(module_data$field_values), numeric = TRUE)]
          })
        
        # 2. Additional fields ------------------------------------------------- 
        # Add new fields when the "add group" button is pressed
        observeEvent(
          c(input$add_group, add_trigger$depend()),
          label = glue("{ns('add_field')}"),
          ignoreNULL = FALSE,
          ignoreInit = TRUE,
          {
            # Increment counter and form ID based on current value
            counter <- incrementReactiveVal(counter)
            
            # ID of the current field, applied to both the UI and observers
            # Field_id is reactive for subsequent observers and uses counter()
            # to create a new ID for each field created
            field_id <- sprintf("%03d", counter())
            
            # 2.1. Add UI for new field ####
            insertUI(
              # selector: must use jQuery syntax (#<element_id>)
              selector = glue("#{ns('add_group')}"),
              where = "beforeBegin",
              ui = 
                field_ui(
                  # ID is namespaced when adding UI elements
                  inputId = ns(field_id),
                  remove_button = TRUE,
                  choices = unique_values
                  )
              )
            
            # Increment n_fields
            n_fields <- incrementReactiveVal(n_fields)
            
            # 2.2 Reactive Trigger for field ####
            # Allows app to delete fields programmatically when a config file
            # is loaded, if there are more fields than there are groups in the
            # config file
            module_data$remove_triggers[[field_id]] <-
              makeReactiveTrigger()
            
            # TEMP: Prints to console when reactive expression is triggered
            # observe(
            #   #isolate(module_data$remove_triggers)[[field_id]]$depend(),
            #   label = glue("{ns(field_id)}_reactive_trigger_observer"),
            #   # Required ignore combination for reactive triggers
            #   #ignoreNULL = FALSE,
            #   #ignoreInit = TRUE,
            #   {
            #     # Present to get observer to take a depenency on just the 
            #     # particular reactive trigger in question
            #     isolate({module_data$remove_triggers})[[field_id]]$depend()
            #     print(glue("{field_id} removal trigger activated."))
            #   })
            
            # observe(
            #   #isolate(module_data$remove_triggers)[[field_id]]$depend(),
            #   label = glue("{ns(field_id)}_reactive_trigger_observer"),
            #   # Required ignore combination for reactive triggers
            #   #ignoreNULL = FALSE,
            #   #ignoreInit = TRUE,
            #   {
            #     # Present to get observer to take a depenency on just the 
            #     # particular reactive trigger in question
            #     isolate({module_data$remove_triggers})[[field_id]]$depend()
            #     print(glue("{field_id} removal trigger activated."))
            #   })
            
            # 2.3. Observer for field ####
            # Add observer to store data in reactive list when the inputs change
            module_data$field_observers[[field_id]] <- 
              observe(
                label = glue("{ns(field_id)}_record_values"),
                {
                # Add new values to list
                module_data$field_values[[field_id]] <-
                  list(
                    `group_name` = input[[glue("{field_id}_name")]],
                    `group_members` = input[[glue("{field_id}_members")]]
                    )

                # Sort list in alphanumeric order
                module_data$field_values <-
                  module_data$field_values[
                    str_sort(names(module_data$field_values), numeric = TRUE)]
                })
            
            # 2.4. Field deletion observer ####
            # Observer to delete field from data if remove button is pressed
            observeEvent(
              input[[glue("{field_id}_remove")]],
              # c(input[[glue("{field_id}_remove")]],
              #   # Also responds to removal reactive trigger for field
              #   isolate({module_data$remove_triggers[[field_id]]})$depend()),
              label = glue("{ns(field_id)}_delete_field"),
              # Must use ignoreNULL = TRUE, ignoreInit = TRUE to avoid the field
              # being removed as soon as it is created (not yet sure why)
              ignoreNULL = TRUE,
              ignoreInit = TRUE,
              # Observer self-destructs when the removal is complete
              once = TRUE,
              {
                # Remove UI
                removeUI(
                  selector = glue("#{ns(field_id)}_field")
                )
                
                # Decrement n_fields
                n_fields <- decrementReactiveVal(n_fields)
                
                # Destroy field-specific observer
                module_data$field_observers[[field_id]]$destroy()

                # Remove data from list
                module_data$field_values[[field_id]] <- NULL
              })
            
            # 2.5. Deletion observer, triggered programmatically
            # Code is copied to a separate observer since required ignore* 
            # parameters are different for reactive triggers 
            observeEvent(
              isolate({module_data$remove_triggers[[field_id]]})$depend(),
              ignoreNULL = FALSE,
              ignoreInit = TRUE,
              # Observer self-destructs when the removal is complete
              once = TRUE,
              {
                print("Programmatic Trigger of deletion observer")
                # Remove UI
                removeUI(
                  selector = glue("#{ns(field_id)}_field")
                )
                
                # Decrement n_fields
                n_fields <- decrementReactiveVal(n_fields)
                
                # Destroy field-specific observer
                module_data$field_observers[[field_id]]$destroy()
                
                # Remove data from list
                module_data$field_values[[field_id]] <- NULL
                
                # Remove reactive trigger from list
                module_data$remove_triggers[[field_id]] <- NULL
              })
            
            })
        
        observe({
          print("n_fields")
          print(n_fields())
        })
        
        observe(
          label = glue("{ns('print_reactive_triggers')}"),
          {
            print("Reactive triggers:")
            triggerlist <-
              lapply(
                module_data$remove_triggers,
                function(x) x$depend()
              )
            
            names(triggerlist) <- names(module_data$remove_triggers)
            
            print(triggerlist)
            })
        
        # 3. Update fields when config file is loaded --------------------------
        observeEvent(
          session$userData$config(),
          {
            if (category_name %in% names(session$userData$config()$metadata)){
              # Fetch config information for the metadata category matching
              # the current module instance
              config_individual <-
                session$userData$config()$metadata[[category_name]]
              
              if (!is.null(config_individual$groups)){
                # If groups are defined, create fields until the number of
                # fields matches the number of groups in the config file.
                
                # Determine number of groups in loaded file
                n_groups <- length(config_individual$groups)
                
                # Condition 3.A: more groups in loaded file than 
                # there are fields
                if (n_fields() < n_groups){
                  # Create fields until number of fields matches 
                  # number of groups 
                  # Beginning of for loop is the value of n_fields *after* the
                  # first new field is added. i is not used in loop.
                  for (i in (n_fields() + 1):n_groups){
                    # Code below is a copy of 2.1-2.3
                    # Unsure how to functionalize code with this many observers
                    # May require an additional module
                    add_trigger$trigger()
                  }
                } else if (n_fields() == n_groups){
                  # Condition 3.B: as many fields in app as there are 
                  # groups in file
                  
                } else if (n_fields() > n_groups){
                  # Condition 3.C: more fields in app than groups in file
                  # If there are more fields in existence than fields, trigger
                  # the reactive expressions to remove fields until there are
                  # the right number of fields.
                  print("Condition 3C")
                  # Iterate through n_fields-n_groups observers (number that 
                  # must be deleted to create the required number of fields)
                  for (j in 1:(n_fields() - n_groups)){
                    # Trigger reactive triggers for the first n_fields-n_groups
                    # triggers in the list of reactive triggers
                    print("Trigger removal observer")
                    module_data$remove_triggers[[j]]$trigger()
                  }
                  print("For loop complete")
                }
                
              } else {
                # If there are no groups defined, but the user has 
                # created multiple fields
              }
            }
          })
        
        # 4. Return values to parent module ------------------------------------
        return(
          reactive({
            # module_data$field_values is a list
            module_data$field_values
            })
          )
      })
  }
    
 
