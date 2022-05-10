## Options Module (first level) ####
### Options Module UI
# id: the namespace id given to this module. In the config app, this is either 
# the assay name or the metadata type.
# Optcard_type: the type of data to display options for. This can be either 
# "assays" or "metadata", and is used to show the relevant options based 
# on the type.
options_ui <- function(id,
                       object,
                       optcard_type = c("assays","metadata"),
                       category_name = id
                       ){
  # NS(id): namespace function, defined here and called for every input ID. One
  # namespace is used for each instance of the options module and is defined by
  # the id; use of namespaces allows for the same input id to be used in 
  # different modules without namespace collisions.
  ns <- NS(id)
  
  # 1. Calculations for options UI
  # Metadata-specific calculations used to define UI for each metadata type
  if (optcard_type=="metadata"){
    # Get unique values of metadata field for display of summary statistics
    values <- unique(object@meta.data[[category_name]])
    # Create list of sorted values for display
    values_sorted <- str_sort(values, numeric=TRUE)
    # Determine type of metadata
    metadata_type <- metadata_type(object, category_name)
    
    # Metadata description
    # Display number of unique values if categorical; display range if numeric
    if (metadata_type=="Categorical"){
      n_unique <- length(values)
      metadata_description <- glue("{n_unique} unique values")
    } else if (metadata_type=="Numeric"){
      metadata_description <- ""
      # Previous code for describing numeric metadata: does not work
      # metadata_description <- 
      #   glue("{range: {min(values)} to {max(values)}, avg {mean(values)}}")
    } else {
      # Potential unforseen metadata types: leave the description blank
      metadata_description <- ""
    }
  }
  
  # 2. Create UI for options card
  # Assays UI
  if(optcard_type=="assays"){
    ui <- div(
      id=ns("optcard"),
      class="optcard single-space-bottom",
      tags$strong(
        glue("Options for {category_name}"),
        class="large half-space-bottom center"
      ),
      
      # Human-readable suffix: appears on plots and search entries
      textInput(inputId = ns("hr"),
                label="Set label for assay (will appear as entered in app)",
                width = "380px"),
      # Include assay name on plots: if checked, the label entered will be 
      # displayed on plots and in the feature search results.
      # I may put this in the main app instead; it makes more sense to toggle
      # it when making the plots.
      checkboxInput(inputId = ns("include_label"),
                    label = "Include assay name on plots?"),
      tags$p("(This is usually not required for the default assay in your data)")
    )
    
    # Metadata UI
  } else if (optcard_type=="metadata"){
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
      if (metadata_type == "Categorical" & length(values)<=15){
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
      if (metadata_type=="Categorical"){
        tagList(
          materialSwitch(inputId =  ns("group_metadata"),
                         label = "Group metadata into categories?", 
                         value = FALSE,
                         right = TRUE,
                         status = "default"),
          tags$p(
            "(Choices for possible values in the metadata 
            column will appear in the app)",
            class="center small"
          )
        )
      } else NULL,
      
      # Dynamic UI for defining metadata groups
      uiOutput(outputId = ns("groups_list"))
    )
  }
  
  # 3. Add "hidden" shinyjs class to the card to hide each card initially
  ui <- shinyjs::hidden(ui)
  
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
                           options_type=c("assays", "metadata"),
                           category_name=id
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
      
      # Initialize output variables
      # group_choices applies only to metadata variables that are categorical 
      # but is called for export from this module in all cases: therefore, it 
      # must be initialized as NULL to avoid issues when returning for modules
      # not meeting these conditions.
      group_choices <- NULL
      
      # 1. Show/hide cards based on user selections
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
          if (category_name %in% categories_selected()){
            showElement("optcard")
            # Otherwise, hide the card
          } else {
            hideElement("optcard")
          }
        })
      
      # 2. Metadata-tab specific functions
      # Determine if metadata field is categorical or numeric
      if(options_type=="metadata"){
        type <- metadata_type(object,id)
      }
      
      # 2.1. Metadata Groups
      # User Interface to define subgroups within a metadata column
      # Only perfomed for categorical inputs
      if(options_type=="metadata"){
        # Test for type of metadata field after testing to see if the object 
        # is a metadata entry
        if(type=="Categorical"){
          # Create a list for storing reactive outputs of group fields modules
          group_choices <- list()
          # Use makeReactiveBinding to get the list to update when group fields 
          # inputs are changed
          makeReactiveBinding("group_choices")
          
          # 2.1.1. Define UI for group selection
          groups_UI <- 
            eventReactive(
              input$group_metadata,
              label = "Groups UI",
              ignoreNULL = FALSE,
              {
                #Display the interface when the corresponding 
                #switch is activated
                if (input$group_metadata==TRUE){
                  # Interface: one instance of the fields UI, with a button to 
                  # add more fields to the interface
                  tagList(
                    #Namespace id for module is based on a 
                    #numeric id since there are multiple fields.
                    metadata_group_fields_ui(
                      ns("groups-1"),
                      temp_choices = unique(object@meta.data[[category_name]])
                    ),
                    
                    div(
                      actionButton(
                        inputId =ns("add_group"),
                        label = "Add Group",
                        width = "100px"
                      )
                      
                    )
                  )#End tagList of input containers
                } 
              })
          
          # 2.1.2. Create a reactive values object with a vector of the unique 
          # values in the metadata column that can be sorted into groups. This
          # will be used to populate the choices now and will be updated later
          
          # For now, this is not reactive. It will eventually be reactive and 
          # depend on inputs in other metadata fields modules.
          category_values <- unique(object@meta.data[[category_name]]) |> 
            str_sort(numeric=TRUE)
          
          # 2.1.3. After creating the UI for the first group selection field, 
          # create the corresponding server module
          module_output <- metadata_group_fields_server("groups-1",
                                                        possible_selections = category_values)
          
          # Use observe() to reactively update group_choices with 
          # the module output
          observe(
            label=glue("observer for {category_name}-groups-1"),
            {
              # The superassignment operator <<- is used to update group_choices
              # outside of the scope of the observe() function
              group_choices[["groups-1"]] <<- module_output()
            })
          
          # 2.1.4. Add additional fields if the "Add Group" button is clicked
          observeEvent(
            input$add_group,
            label = "{category_name}: add Field Button",
            # When the ns(add-group) button is created or when the options 
            # server module is created it will trigger this observer. The 
            # ignore* parameters are set to TRUE to make the observer respond 
            # only to a click of the button.
            ignoreNULL = TRUE,
            ignoreInit = TRUE,
            {
              # Use the action button's value to create an id. 
              # Add 1 to the value since the first field uses "1" in its 
              # namespace (the first value to be created should have a value 
              # of 2 in the namespace id)
              # A different implementation is recommended since
              # this could be buggy 
              nested_id <- glue("groups-{input$add_group + 1}")
              
              # Add module UI
              # For arguments that reference an element by id that is not 
              # a Shiny input or output, namespacing must be used
              insertUI(
                selector = glue("#{ns('add_group')}"),
                where = "beforeBegin",
                # Namespacing should also be used to call the UI components of 
                # modules, but not the server component
                ui = 
                  metadata_group_fields_ui(
                    ns(nested_id),
                    remove_button = TRUE,
                    temp_choices = 
                      str_sort(
                        unique(object@meta.data[[category_name]]),
                        numeric=TRUE
                      )
                  )
              )
              
              # Add module server instance
              module_output <- 
                metadata_group_fields_server(
                  nested_id,
                  possible_selections = category_values
                )
              
              # Store output in group_choices and use observe() to 
              # reactively update group_choices when the output changes
              observe(
                label=glue("Observer for {category_name}-{nested_id}"),
                {
                  group_choices[[nested_id]] <<- 
                    module_output()
                })
            })
          
          # 2.1.6. Render UI components
          output$groups_list <- 
            renderUI({
              groups_UI()
            })
        }
      }
      
      # 3. Returns from Module: 
      if(options_type == "metadata"){
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
                 `groups` = if(input$group_metadata == TRUE){
                   # Uses the process_group_choices function to remove values 
                   # representing deleted modules
                   process_group_choices(group_choices)
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
        # added to all dropdown menus in the app that display features from multiple assays 
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