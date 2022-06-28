## Options Module (first level) ####
### Options Module UI
# id: the namespace id given to this module. In the config app, this is either 
# the assay name or the metadata type.
# Optcard_type: the type of data to display options for. This can be either 
# "assays" or "metadata", and is used to show the relevant options based 
# on the type.
options_ui <- function(id,
                       object,
                       optcard_type = c("assays", "metadata"),
                       category_name = id
                       ){
  # NS(id): namespace function, defined here and called for every input ID. One
  # namespace is used for each instance of the options module and is defined by
  # the id; use of namespaces allows for the same input id to be used in 
  # different modules without namespace collisions.
  ns <- NS(id)
  
  # Calculations for options UI ---------------------------------------------
  # Metadata-specific calculations used to define UI for each metadata type
  if (optcard_type == "metadata"){
    # Get unique values of metadata field for display of summary statistics
    values <- unique(object@meta.data[[category_name]])
    # Create list of sorted values for display
    values_sorted <- str_sort(values, numeric=TRUE)
    # Determine type of metadata
    metadata_type <- metadata_type(object, category_name)
    
    # Metadata description
    # Display number of unique values if categorical; display range if numeric
    if (metadata_type == "Categorical"){
      n_unique <- length(values)
      metadata_description <- glue("{n_unique} unique values")
    } else if (metadata_type == "Numeric"){
      metadata_description <- ""
      # Previous code for describing numeric metadata: does not work
      # metadata_description <- 
      #   glue("{range: {min(values)} to {max(values)}, avg {mean(values)}}")
    } else {
      # Potential unforseen metadata types: leave the description blank
      metadata_description <- ""
    }
  }
  
  # Create UI for options card ----------------------------------------------
  # Assays UI
  if(optcard_type == "assays"){
    ui <- 
      # Shinyjs hidden class applied initially to assay options cards
      hidden(
        div(
          id = ns("optcard"),
          class = "optcard single-space-bottom",
          tags$strong(
            glue("Options for {category_name}"),
            class = "large half-space-bottom center"
          ),
          
          # Human-readable suffix: appears on plots and search entries
          textInput(
            inputId = ns("hr"),
            label = "Set label for assay (will appear as entered in app)",
            width = "380px"
            ),
          
          # Include assay name on plots: if checked, the label entered will be 
          # displayed on plots and in the feature search results.
          checkboxInput(
            inputId = ns("include_label"),
            label = "Include assay name on plots?"),
          tags$p(
            "(This is usually not required for the default assay in your data)"
            )
        )
      )
    
    # Metadata UI
  } else if (optcard_type == "metadata"){
    ui <- div(
      id = ns("optcard"),
      class = "optcard single-space-bottom",
      tags$strong(glue("Options for {category_name}"),
                  class="large center"),
      
      # Print the type of metadata beneath the title, and a brief description
      tags$p(glue("({metadata_type}, {metadata_description})"), 
             class = "center small half-space-bottom"),
      
      # If the metadata is categorical and there are 15 values or less,
      # print the values to screen
      if (metadata_type == "Categorical" & length(values) <= 15){
        tags$p(glue("Values: {paste(values_sorted, collapse=', ')}"))
      } else NULL,
      
      # Human-readable suffix: appears on plots and search entries
      textInput(inputId = ns("hr"),
                label = "Set label for metadata column (will appear as 
                         entered in app interface)",
                width = "380px"),
      
      # Option to classify metadata into list (ex. group patients 
      # by sample conditions)
      # Only available for categorical metadata columns
      if (metadata_type == "Categorical"){
        tagList(
          materialSwitch(
            inputId =  ns("group_metadata"),
            label = "Group metadata into categories?", 
            value = FALSE,
            right = TRUE,
            status = "default"
            ),
          tags$p(
            "(Choices for possible values in the metadata 
            column will appear in the app)",
            class = "center small"
          ),
          # Metadata groups interface
          # One instance of the fields UI, with a button to 
          # add more fields to the interface
          hidden(
            div(
              id = ns("groups_interface"),
              #Namespace id for module is based on a 
              #numeric id since there are multiple fields.
              metadata_group_fields_ui(
                id = ns("groups"),
                unique_values =
                  str_sort(
                    unique(object@meta.data[[category_name]]),
                    numeric = TRUE
                    )
                )
              
              # metadata_group_fields_ui(
              #   id = ns("groups-1"),
              #   temp_choices = unique(object@meta.data[[category_name]])
              # ),
              # 
              # # Additional fields will be added above "add group" button
              # div(
              #   actionButton(
              #     inputId = ns("add_group"),
              #     label = "Add Group",
              #     width = "100px"
              #   )
              # )
            )#End tagList of input containers
          ),
          
          verbatimTextOutput(
            outputId = ns("test_print")
          )
        )
      } else NULL,
    )
  }
  
  return(ui)
}

### Options module server
# Renders option cards for the variables of assays, metadata, etc. selected by 
# the user, and processes user selections to create a config file for the 
# main app.
# Applies to multiple types of selections (assays, metadata, etc.). One instance 
# of the module is created for every available selection across each tab.

# Arguments
# id: the id passed to the module
# categories_selected: a reactive variable describing the variables (assays, 
# metadata, etc., selected by the user)
# options_type: the type of options to create a server function for. Can be one 
# of "assays" or "metadata"
# category_name: the name of the individual category that the instance of the
# module applies to. This is the id by default, and can be changed.
options_server <- function(id, 
                           object,
                           categories_selected, 
                           options_type = c("assays", "metadata"),
                           category_name = id
                           ){
  # Initialize module
  moduleServer(
    id,
    function(input, 
             output,
             session){
      # Define namespace function for UI elements or references to elements that 
      # are not Shiny inputs or outputs 
      ns <- NS(id)
      
      # Create a counter for group modules (used to set id of modules created)
      # Starts at 1 and counts up when the "add group" button is pressed
      group_counter <- reactiveVal(1)
      
      # Initialize output variables
      # group_choices applies only to metadata variables that are categorical 
      # but is called for export from this module in all cases: therefore, it 
      # must be initialized as NULL to avoid issues when returning for modules
      # not meeting these conditions.
      #group_choices <- NULL
      
      # 1. Show/hide cards based on user selections ----------------------------
      # (Conditionals on non-reactive values such as options_type can be used 
      # outside of server components)
      observeEvent(
        categories_selected(),
        label = glue("Show/Hide Cards: {options_type}"),
        ignoreNULL = FALSE,
        {
          # Examine the list of currently selected categories, and
          # check if the module's category name is selected.
          # If so, show the options card
          # print(glue("show/hide test for {category_name}"))
          # print("categories selected")
          # print(categories_selected())
          # print(glue("{category_name} %in% categories_selected"))
          # print(category_name %in% categories_selected())
          
          if (category_name %in% categories_selected()){
            showElement("optcard")
            # Otherwise, hide the card
          } else {
            hideElement("optcard")
          }
        })
      
      # 2. Metadata-tab specific functions -------------------------------------
      # Determine if metadata field is categorical or numeric
      if (options_type == "metadata"){
        type <- metadata_type(object,id)
      }
      
      ## 2.1. Metadata Groups ####
      # User Interface to define subgroups within a metadata column
      # Only perfomed for categorical inputs
      if (options_type == "metadata"){
        # Test for type of metadata field after testing to see 
        # if the object is a metadata entry
        if (type == "Categorical"){
          ### 2.1.1. Define UI for group selection ####
          observeEvent(
            input$group_metadata,
            label = "Show/Hide Groups UI",
            ignoreNULL = FALSE,
            {
              element_id <- "groups_interface"
              
              #Display the interface when the corresponding 
              #switch is activated
              if (isTruthy(input$group_metadata)){
                showElement(
                  id = element_id
                )
              } else {
                hideElement(
                  id = element_id
                )
              }
            })
          
          ### 2.1.2. Module Server for Choosing Group Values
          group_data <- 
            metadata_group_fields_server(
              id = "groups",
              category_name = category_name,
              unique_values = 
                unique(object@meta.data[[category_name]]) |> 
                str_sort(numeric=TRUE)
              )
          
          output$test_print <- 
            renderPrint(
              group_data()
            )
        }
      }
      
      # 3. Restore inputs when options cards are re-sorted ---------------------
      # (In metadata tab)
      observeEvent(
        session$userData$metadata_sorting_trigger$depend(),
        ignoreNULL = FALSE,
        ignoreInit = TRUE,
        label = glue("{id}: Restore Inputs Upon Rearrangement"),
        {
          # "Update" the input with the previously entered label
          if (!is.null(input$hr)){
            updateTextInput(
              session,
              inputId = "hr",
              # Value for this input is stored in `dropdown_title`
              value = input$hr
            )
          }
          
          # Do the same for the groups switch
          if (!is.null(input$group_metadata)){
            updateMaterialSwitch(
              session,
              inputId = "group_metadata",
              value = input$group_metadata
            )
          }
          
          # Will also need code to update groups input when that is completed
          # for 4.
          
        })
      
      
      # 4. Update Inputs with loaded config file -------------------------------
      if (options_type == "assays"){
        observeEvent(
          session$userData$config(),
          {
            # Search for assay name (module ID) in loaded config file
            if (id %in% names(session$userData$config()$assays)){
              # Get config info for assay matching module ID
              config_individual <- 
                session$userData$config()$assays[[id]]
              
              # Update inputs
              # Text entry for assay label
              updateTextInput(
                session,
                inputId = "hr",
                # Value for this input is stored in `dropdown_title`
                value = config_individual$dropdown_title
              )
              
              # Checkbox to use assay label 
              # If `suffix_human` is defined, a label is desired.
              # When `suffix_human` == "", a label is not desired.
              # isTruthy will cover this case, as well as this field being 
              # NULL (shouldn't happen but it could)
              updateCheckboxInput(
                session,
                inputId = "include_label",
                value =
                  if (isTruthy(config_individual$suffix_human)) TRUE else FALSE
              )
            }
          })
      } else if (options_type == "metadata"){
        observeEvent(
          session$userData$config(),
          {
            # Search for category name in loaded config file
            if (category_name %in% names(session$userData$config()$metadata)){
              # Get config info for matching metadata category
              config_individual <-
                session$userData$config()$metadata[[category_name]]

              # Update inputs using config file
              # Label for metadata column (text input, input$hr)
              updateTextInput(
                session,
                inputId = "hr",
                # Value `label` section of config for category
                value = config_individual$label
              )

              # Switch for defining groups (input$group_metadata)
              # Switch on if `groups` is not NULL
              updateMaterialSwitch(
                session,
                inputId = "group_metadata",
                value =
                  if (!is.null(config_individual$groups)) TRUE else FALSE
                )

              #### Create modules for each group #####
              # One module is already created by updateMaterialSwitch
              # if groups is not NULL
              
              
              # if (!is.null(config_individual$groups)){
              #   n_groups <- length(config_individual$groups)
              #   # If fields have already been created, delete all fields
              #   # if (group_counter() > 1){
              #   #   for (i in 2:group_counter()){
              #   #     module_id <- glue("groups-{i}")
              #   #
              #   #     # Check if groups_i has already been removed
              #   #     if (!is.null(group_choices[[module_id]])){
              #   #       removeUI(
              #   #         # ID of container with ui is
              #   #         selector = glue("groups-{i}")
              #   #       )
              #   #
              #   #       # Remove module output from groups list
              #   #       group_choices[[module_id]] <- NULL
              #   #     }
              #   #
              #   #   }
              #   # }
              # 
              # 
              #   # Determine number of modules to create. This depends on
              #   # the number of fields in the config file and the number of
              #   # group modules already created in the app (as defined by
              #   # group_counter)
              # 
              # 
              #   # Additional modules are created if more than one group is read
              #   # from the config file
              #   if (n_groups > 1){
              #     # Also, check value of the module counter. If it is already
              #     # equal to the number of groups in the config file,
              # 
              #     for (i in 2:n_groups){
              #       print("Create module")
              # 
              #       # Run exact same code as in #2.1.4 (will need to
              #       # re-structure groups model for this to work correctly)
              #       # Increase counter by one
              #       new_value <- group_counter() + 1
              #       group_counter(new_value)
              # 
              #       # Use the action button's value to create an id.
              #       # Use group_counter() for ID creation
              #       nested_id <- glue("groups-{group_counter()}")
              # 
              #       # Add module UI
              #       # For arguments that reference an element by id that is not
              #       # a Shiny input or output, namespacing must be used
              #       insertUI(
              #         selector = glue("#{ns('add_group')}"),
              #         where = "beforeBegin",
              #         # Namespacing should also be used to call the UI
              #         # components of modules, but not the server component
              #         ui =
              #           metadata_group_fields_ui(
              #             ns(nested_id),
              #             remove_button = TRUE,
              #             temp_choices =
              #               str_sort(
              #                 unique(object@meta.data[[category_name]]),
              #                 numeric=TRUE
              #               )
              #           )
              #       )
              # 
              #       # Add module server instance
              #       module_output <-
              #         metadata_group_fields_server(
              #           id = nested_id,
              #           possible_selections = category_values
              #           )
              # 
              #       # Store output in group_choices and use observe() to
              #       # reactively update group_choices when the output changes
              #       # observe(
              #       #   label = glue("Observer for {category_name}-{nested_id}"),
              #       #   {
              #       #     print("ID")
              #       #     print(id)
              #       #     print("Groups module ID")
              #       #     print(nested_id)
              #       #     print("Add new value to reactive list")
              #       #     group_choices[[nested_id]] <-
              #       #       module_output()
              #       #     print("Done")
              #       #   })
              #      }
              #    }
              # }
            }

            })
      }
      
      # 4. Returns from Module ------------------------------------------------- 
      if (options_type == "metadata"){
        # Return options depend on the type of metadata (Categorical metadata 
        # has a reactive list of metadata group choices; numeric and other types 
        # have a non-reactive value of NULL for group_choices)
        if (type == "Categorical"){
          # Return metadata-specific variables as a list
          # Returns 
          # 1. The name of the category (machine-readable and used for 
          #    the values of choices in the app)
          # 2. The user-specified label (human-readable and used for the 
          #   keys of choices)
          # 3. The metadata groups selected, if specified by the user
          return_list_metadata <- reactive({
            list(`meta_colname` = category_name,
                 `label` = input$hr,
                 # groups: defined if the switch to group metadata is turned on, 
                 # and set to NULL otherwise.
                 `groups` = if (isTruthy(input$group_metadata)){
                   group_data()
                 } else NULL
            )
          })
          
          # Numeric metadata and other types: group_choices is NULL
        } else {
          return_list_metadata <- 
            reactive({
              list(
                `meta_colname`= category_name,
                `label`= input$hr,
                `groups`= NULL
              )
            })
        }
        
        return(return_list_metadata)
        
      } else if (options_type=="assays"){
        # For assays, return
        # 1. Assay: the name of the assay as defined in the Seurat object
        # 2. Key: the prefix to be added to features server-side to search for 
        # them from the assay
        # 3. Suffix_human: a suffix that is added to the feature in parentheses 
        # and displayed in the app in dropdown menus and in the titles of plots
        # 4. Dropdown_title: a user-defined label for the assay that will be 
        # added to all dropdown menus in the app that display features from 
        # multiple assays 
        return_list_assays <- 
          reactive({
            list(
              `assay` = category_name,
              `key` = Key(object[[category_name]]),
              `suffix_human` = if (input$include_label==TRUE) input$hr else "",
              `dropdown_title` = input$hr)
          })
        
        return(return_list_assays)
      }
    })
}