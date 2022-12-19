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
        
        # Reactive values object for storing observers, outputs, 
        # reactive triggers
        module_data <- reactiveValues()
        
        module_data$field_observers <- list()
        module_data$field_values <- list()
        module_data$remove_triggers <- list()
        
        # Counter: used to determine ID of new fields created. Increments with
        # the creation of fields (programmatically upon loading or through user
        # interaction)
        module_data$counter <- 1
        # n_fields: tracks the number of fields currently in existence.
        module_data$n_fields <- 1
        
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
            module_data$counter <- module_data$counter + 1
            
            # ID of the current field, applied to both the UI and observers
            # Field_id is reactive for subsequent observers and uses counter()
            # to create a new ID for each field created
            field_id <- sprintf("%03d", module_data$counter)
            
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
            module_data$n_fields <- module_data$n_fields + 1
            
            # 2.2 Reactive Trigger for field ####
            # Allows app to delete fields programmatically when a config file
            # is loaded, if there are more fields than there are groups in the
            # config file
            module_data$remove_triggers[[field_id]] <-
              makeReactiveTrigger()
            
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
                sort_order <- 
                  str_sort(
                    names(module_data$field_values), 
                    numeric = TRUE
                    )
                
                module_data$field_values <-
                  module_data$field_values[sort_order]
                })
            
            # 2.4. Field deletion observer ####
            # Observer to delete field from data if remove button is pressed
            observeEvent(
              input[[glue("{field_id}_remove")]],
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
                module_data$n_fields <- module_data$n_fields - 1 
                
                # Destroy field-specific observer
                module_data$field_observers[[field_id]]$destroy()

                # Remove data from list
                module_data$field_values[[field_id]] <- NULL
              })
            
            # 2.5. Deletion observer, triggered programmatically
            # Code is copied to a separate observer since required ignore* 
            # parameters are different for reactive triggers 
            # observeEvent(
            #   isolate({module_data$remove_triggers[[field_id]]})$depend(),
            #   ignoreNULL = FALSE,
            #   ignoreInit = TRUE,
            #   # Observer self-destructs when the removal is complete
            #   once = TRUE,
            #   {
            #     # Remove UI
            #     removeUI(
            #       selector = glue("#{ns(field_id)}_field")
            #     )
            #     
            #     # Decrement n_fields
            #     module_data$n_fields <- module_data$n_fields - 1
            #     
            #     # Destroy field-specific observer
            #     module_data$field_observers[[field_id]]$destroy()
            #     
            #     # Remove data from list
            #     module_data$field_values[[field_id]] <- NULL
            #     
            #     # Remove reactive trigger from list
            #     module_data$remove_triggers[[field_id]] <- NULL
            #   })

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
                if (module_data$n_fields < n_groups){
                  # Create fields until number of fields matches 
                  # number of groups 
                  
                  # Beginning of for loop is the value of n_fields *after* the
                  # first new field is added. i is not used in loop.
                  for (i in (module_data$n_fields + 1): n_groups){
                    # Code below is a copy of 2.1-2.3
                    # Unsure how to functionalize code with this many observers
                    # May require an additional module
                    
                    # Increment counter and form ID based on current value
                    module_data$counter <- module_data$counter + 1
                    # store field ID's created in a separate vector to avoid 
                    # erasure when more than one field is created
                    field_id <- sprintf("%03d", module_data$counter)
                    
                    # Add UI for new field ####
                    insertUI(
                      # selector: must use jQuery syntax (#<element_id>)
                      # Adds before "add group" button
                      selector = glue("#{ns('add_group')}"),
                      where = "beforeBegin",
                      immediate = TRUE,
                      ui = 
                        field_ui(
                          # ID is namespaced when adding UI elements
                          inputId = ns(field_id),
                          remove_button = TRUE,
                          choices = unique_values
                          )
                      )
                    
                    # Increment n_fields
                    module_data$n_fields <- module_data$n_fields + 1
                    
                    # Reactive Trigger for field ####
                    # Allows app to delete fields programmatically when a 
                    # config file is loaded, if there are more fields than 
                    # there are groups in the config file
                    module_data$remove_triggers[[field_id]] <-
                      makeReactiveTrigger()

                    # Observer for field ####
                    # Add observer to store data in reactive list when the 
                    # inputs change
                    
                    # inject: required for observers in loops. Ensures 
                    # `field_id` is equal to its value at iteration i of the 
                    # loop instead of the last version, avoiding an error with
                    # only the last observer functioning properly
                    rlang::inject({
                      module_data$field_observers[[field_id]] <- 
                        observe(
                          label = glue("{ns(field_id)}_record_values"),
                          {
                            field_id_i <- !!field_id
                            
                            # Add new values to list
                            module_data$field_values[[field_id_i]] <-
                              list(
                                `group_name` = 
                                  input[[glue("{field_id_i}_name")]],
                                `group_members` = 
                                  input[[glue("{field_id_i}_members")]]
                              )
                            
                            # Sort list in alphanumeric order
                            sort_order <- 
                              str_sort(
                                names(module_data$field_values), 
                                numeric = TRUE
                              )
                            
                            module_data$field_values <-
                              module_data$field_values[sort_order]
                          })
                      })
                    
                    # ith Field deletion observer ####
                    # Observer to delete field from data if remove 
                    # button is pressed
                    rlang::inject({
                      observeEvent(
                        input[[glue("{field_id}_remove")]],
                        label = glue("{ns(field_id)}_delete_field"),
                        # Must use ignoreNULL = TRUE, ignoreInit = TRUE to avoid
                        # the field being removed as soon as it is created (not
                        # yet sure why)
                        ignoreNULL = TRUE,
                        ignoreInit = TRUE,
                        # Observer self-destructs when the removal is complete
                        once = TRUE,
                        {
                          field_id_i <- !!field_id
                          
                          # Remove UI
                          removeUI(
                            selector = glue("#{ns(field_id_i)}_field")
                          )

                          # Decrement n_fields
                          module_data$n_fields <- module_data$n_fields - 1

                          # Destroy field-specific observer
                          module_data$field_observers[[field_id_i]]$destroy()

                          # Remove data from list
                          module_data$field_values[[field_id_i]] <- NULL
                        })
                    })
                  }
                  
                  # At end of loop (when n_fields == n_groups),
                  # Update each field with each value in the config file
                  extant_field_ids <- 
                    c("001", 
                      names(module_data$field_observers)
                      )
                
                  # Update fields with information in config file ####
                  for (k in (1: n_groups)){
                    # Use the field ID for the k'th field created
                    field_id <- extant_field_ids[k]
                    
                    # Define the ID for group name and members, 
                    # for the k'th field
                    group_name_id = glue("{field_id}_name")
                    group_members_id = glue("{field_id}_members")
                    
                    # Extract group data for the k'th group in the loaded file
                    group_data <- config_individual$groups[[k]]
                    
                    # Freeze inputs
                    freezeReactiveValue(input, group_name_id)
                    freezeReactiveValue(input, group_members_id)
                    
                    # Update group name and group members field
                    updateTextInput(
                      session = session,
                      inputId = group_name_id, 
                      value = group_data$group_name
                      )
                    
                    updateSelectizeInput(
                      session = session,
                      inputId = group_members_id,
                      selected = group_data$group_members
                      )
                    }
                  } else if (module_data$n_fields == n_groups){
                  # Condition 3.B: as many fields in app as there are 
                  # groups in file
                    # Update fields with information in config file ####
                    # Construct vector of field IDs to update
                    extant_field_ids <- names(module_data$field_values)
                    
                    for (k in (1: n_groups)){
                      # Use the field ID for the k'th field created
                      field_id <- extant_field_ids[k]
                      
                      # Define the ID for group name and members, 
                      # for the k'th field
                      group_name_id = glue("{field_id}_name")
                      group_members_id = glue("{field_id}_members")
                      
                      # Extract group data for the k'th group in the loaded file
                      group_data <- config_individual$groups[[k]]
                      
                      # Freeze inputs
                      freezeReactiveValue(input, group_name_id)
                      freezeReactiveValue(input, group_members_id)
                      
                      # Update group name and group members field
                      updateTextInput(
                        session = session,
                        inputId = group_name_id, 
                        value = group_data$group_name
                      )
                      
                      updateSelectizeInput(
                        session = session,
                        inputId = group_members_id,
                        selected = group_data$group_members
                      )
                    }
                  
                } else if (module_data$n_fields > n_groups){
                  # Condition 3.C: more fields in app than groups in file
                  # If there are more fields in existence than fields, trigger
                  # the reactive expressions to remove fields until there are
                  # the right number of fields.
                  
                  # Define IDs of fields in existance
                  extant_fields <- names(module_data$field_observers)
                  
                  # Iterate through n_fields-n_groups observers 
                  # j = number of fields that must be deleted to create the
                  # required number of fields (n_fields-n_groups)
                  for (j in 1:(module_data$n_fields - n_groups)){
                    # Define ID of jth field to remove
                    field_id <- extant_fields[j]
                    
                    # Remove UI
                    removeUI(
                      selector = glue("#{ns(field_id)}_field")
                    )

                    # Decrement n_fields
                    module_data$n_fields <- module_data$n_fields - 1

                    # Destroy field-specific observer
                    module_data$field_observers[[field_id]]$destroy()

                    # Remove data from list
                    module_data$field_values[[field_id]] <- NULL

                    # Remove reactive trigger from list
                    module_data$remove_triggers[[field_id]] <- NULL
                  }
                  
                  # Update fields with information in config file ####
                  # Construct vector of field IDs to update
                  extant_field_ids <- names(module_data$field_values)
                  
                  for (k in (1: n_groups)){
                    # Use the field ID for the k'th field created
                    field_id <- extant_field_ids[k]
                    
                    # Define the ID for group name and members, 
                    # for the k'th field
                    group_name_id = glue("{field_id}_name")
                    group_members_id = glue("{field_id}_members")
                    
                    # Extract group data for the k'th group in the loaded file
                    group_data <- config_individual$groups[[k]]
                    
                    # Freeze inputs
                    freezeReactiveValue(input, group_name_id)
                    freezeReactiveValue(input, group_members_id)
                    
                    # Update group name and group members field
                    updateTextInput(
                      session = session,
                      inputId = group_name_id, 
                      value = group_data$group_name
                    )
                    
                    updateSelectizeInput(
                      session = session,
                      inputId = group_members_id,
                      selected = group_data$group_members
                    )
                  }
                }
                
              } else {
                if (module_data$n_fields > 1){
                  # If there are no groups defined, but the user has 
                  # created multiple fields, remove the extra fields
                  
                  # Define IDs of fields in existance (besides the first field)
                  extant_fields <- names(module_data$field_observers)
                  
                  # Iterate through all those fields, removing UI and data
                  for (j in 1:length(extant_fields)){
                    # Define ID of jth field to remove
                    field_id <- extant_fields[j]
                    
                    # Remove UI
                    removeUI(
                      selector = glue("#{ns(field_id)}_field")
                    )
                    
                    # Decrement n_fields
                    module_data$n_fields <- module_data$n_fields - 1
                    
                    # Destroy field-specific observer
                    module_data$field_observers[[field_id]]$destroy()
                    
                    # Remove data from list
                    module_data$field_values[[field_id]] <- NULL
                    
                    # Remove reactive trigger from list
                    module_data$remove_triggers[[field_id]] <- NULL
                  }
                  
                  # Also, clear the values in the first field
                  updateTextInput(
                    session = session,
                    inputId = "001_name", 
                    value = ""
                  )
                  
                  updateSelectizeInput(
                    session = session,
                    inputId = "001_members",
                    selected = character(0)
                  )
                }
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
    
 
