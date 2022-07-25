# Initialize libraries
library(shiny)
library(Seurat)

# Shiny add-ons 
library(shinyWidgets)
library(rintrojs)
library(shinydashboard)
library(waiter)
library(shinycssloaders)
library(shinyjs)
library(shinyFeedback)
# Sortable.JS: Creates a drag-and-drop menu
library(sortable)

# Reactlog (for debugging)
library(reactlog)
options(shiny.reactlog=TRUE)

# Tidyverse Packages
library(tidyverse)
library(stringr)
library(dplyr)
library(ggplot2)
library(glue)
library(DT)


# Load functions in ./R directory
# Get list of files
source_files <- 
  list.files(
    path = "./R", 
    pattern="*.R$", 
    full.names=TRUE, 
    ignore.case=TRUE
    )

# Load modules from ./Config_App_Modules
source_files <-
  c(source_files,
    list.files(
      path = "./Config_App_Modules", 
      pattern="*.R$", 
      full.names=TRUE, 
      ignore.case=TRUE
      )
    )

# Add threshold_picker module from main app modules
source_files <-
  c(source_files, "./Modules/threshold_picker.R" )

# Use source() to import files into R
sapply(
  source_files, 
  source
  )

# Load CSS files for app: CSS files are defined and each file is converted to a
# <script> tag using includeCSS(). Each tag defined is passed to a list, which 
# is included in the main UI function.
# Get list of .css files in www/ directory
css_files <- 
  list.files(
    path = "./www", 
    pattern = "*.css$", 
    full.names = TRUE, 
    ignore.case = TRUE
    )

# Create list of style tags for each CSS file
css_list <- 
  lapply(
    css_files, 
    includeCSS
    )

# Load Javascript files for app: find all .js files that apply to the applet and 
# create a list of script() tags using includeScript().
# Files to include: all files in www/applet_js/ directory, and the collapsible_panel.js 
# file in the www/ directory (www/button_wizzard.js must be excluded since it 
# conflicts with 'applet_navbar_wizzard' in the www/applet_js/ directory)
js_files <- 
  list.files(
    path = "./www/", 
    # Use regex to search for files ending in .js (double 
    # backslash used to escape '.' character)
    pattern = ".*\\.js", 
    full.names = TRUE, 
    ignore.case = TRUE
    )

# Add www/collapsible_panel.js file to list
#js_files <- c(js_files,"./www/collapsible_panel.js")

# Create list of style tags for each CSS file 
js_list <- lapply(js_files,includeScript)

# Load object #### 
# (hard-coded for now but will soon be chosen using a file input)
object <- readRDS("./Seurat_Objects/longitudinal_samples_20211025.rds")
# Need a conditional to test if the loaded object is a Seurat object

# Define Config file path for loading ####
# (eventually will use file input)
config_filename <- "./Seurat_Objects/d0-d30-config.rds"

# Version of config app #### 
# Printed in config file. Will be used to alert user if they are using a 
# config file that is not compatible with the current version of the main app
config_version <- "0.2.0"

# Numeric Metadata Columns
meta_columns <- names(object@meta.data)

# is_numeric: a vector of boolean values used to subset meta_columns for 
# numeric metadata
is_numeric <- 
  sapply(
    meta_columns,
    function(x, object){
      class(object@meta.data[[x]]) %in% c("numeric", "integer")
    },
    object
    )
numeric_cols <- meta_columns[is_numeric]
non_numeric_cols <- meta_columns[!is_numeric]

# Main UI and Server Functions ####
## 1. Tabs in Main UI ####
## 1.1. Assays Tab #### 
# (not the assay options module)
assay_tab <- function(){
  sidebarLayout(
    applet_sidebar_panel(
      # input-no-margin class: removes margin of input containers within div
      tagList(
        div(
          class="input-no-margin",
          multiInput(
            inputId = "assays_selected",
            label = "Choose assays to include:",
            width = "100%",
            choices = names(object@assays),
            options = 
              list(
                enable_search = FALSE,
                non_selected_header = "Available Assays",
                selected_header = "Selected Assays",
                "hide_empty_groups" = TRUE
              )
          )
        ), # End multiInput
        
        # Include numeric metadata for plotting
        awesomeCheckbox(
          inputId = "include_numeric_metadata",
          label = "Include numeric metadata for plotting", 
          value = TRUE,
          # Determines style of checkbox (warning is orange)
          status = "warning"
          )
      )
      
      ),
    
    applet_main_panel(
      # Create an instance of the assay options UI for all possible assays. Each 
      # UI creates a "card"; all are hidden at first and are shown when their 
      # corresponding assay is selected by the user. The "id" argument in lapply 
      # is the name of the assay.
      tagList(
        lapply(
          names(object@assays),
          function(assay){
            options_ui(
              id = assay,
              object = object,
              optcard_type = "assays"
            )
            }
          ),
        # TEMP: add an additional card displaying the outputs from all tabs
        div(
          class = "optcard",
          verbatimTextOutput(
            outputId = "assay_options"
            )
        ) # End TEMP
      )
    )
  )
}

## 1.2. Metadata Tab ####
metadata_tab <- 
  function(){
    sidebarLayout(
      applet_sidebar_panel(
        div(
          class = "input-no-margin",
          uiOutput(
            outputId = "metadata_sortable_bucket"
            )
          )
        ),
      applet_main_panel(
        # Options for Numeric metadata
        # 
        
        # Options for Categorical, logical metadata
        # Create a metadata options "card" for each non-numeric metadata column
        # in the object. Cards below are hidden and display when the
        # corresponding metadata category is selected
     
      tagList(
        uiOutput(
          outputId = "metadata_cards"
          ),
        
        # TEMP: add an additional card displaying the outputs 
        # from the metadata tab
        div(
          class = "optcard",
          verbatimTextOutput(outputId = "print_metadata")
          )
        )
      )
  )
    }

## 1.3. ADT Threshold Tab ####
threshold_tab <-
  function(){
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
                tags$h4(
                  "Edit threshold for ",
                  textOutput(
                    outputId = "threshold_header_edit_feature"
                    )
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

## 2. Main UI ####
ui <- fluidPage(
  # Waiter UI: spinners
  useWaiter(),
  # Shinyjs: a Shiny JavaScript extension
  useShinyjs(),
  # Main UI
  navbarPage(
    title = "Object Configuration",
    windowTitle = "Configure Seurat Object",
    position = "fixed-top",
    id = "navbar",
    #Tabs are displayed below
    tabPanel(
      title="Assays",
      assay_tab()
      ),
    tabPanel(
      title = "Metadata",
      metadata_tab()
      ),
    tabPanel(
      title = "ADT Threshold",
      threshold_tab()
    )
    ),
  
  # Elements below will be moved to the navbar using JavaScript
  # Help button
  dropdownButton(
    inputId = "help",
    status = "info",
    right = TRUE,
    label = "",
    size = "sm",
    icon = icon("question"),
    # Dropdown menu content
    tagList(
      tags$p(
        "Help and Background",
        style =
        "color: #888888; 
        margin-bottom: 0px;
        font-size: 1.17em;"
        ),
      
      # Tutorial Document (does not yet exist for the config applet)
      # tags$a("Tutorial Vignette",
      #        href="Shiny_Vignette.html",
      #        class="blue_hover",
      #        target="_blank", #Opens link in new tab
      #        rel="noopener noreferrer" 
      # ),#End tutorial document link
      
      # File issue on github
      tags$a(
        "Report a Bug",
        href = "https://github.com/amc-heme/DataExploreShiny/issues",
        class = "blue_hover",
        # Opens link in new tab
        target = "_blank",
        rel = "noopener noreferrer")
      )# End tagList
    ), # End Help Button
  
  dropdownButton(
    inputId = "options",
    status = "info",
    right = TRUE,
    label = "",
    size = "sm",
    icon = icon("ellipsis-h"),
    downloadLink(
      outputId = "export_selections",
      label = "Export Config File",
      class = "orange_hover"
    ),
    actionLink(
      inputId = "load_config",
      label = "Load Config File",
      class = "orange_hover"
    )
  ),
  
  # Button to create/export config file
  # downloadButton(
  #   outputId = "export_selections",
  #   label="Export Configuration",
  #   class = "float_right",
  #   icon = NULL
  #   ),
  
  # May be more appropriate to use an action button later for more advanced 
  # behavior (save config file and direct user back to app)
  # actionButton(inputId = "export_selections",
  #              label = "Export Configuration",
  #              class="float_right"),
  
  # Include scripts for each JavaScript file in document
  # This is added last to delay running of scripts until the elements to be 
  # moved by scripts are created
  js_list,
  # Apply CSS files (placed last so elements created by scripts can be stylized)
  css_list
) # End fluidPage

## 3. Main Server Function ####
server <- function(input, output, session) {
  # Hide ADT Threshold Tab at startup: this is shown when the user designates
  # an assay as the surface protein assay 
  # Use jQuery selector (tab does not have an ID). Selector is based on content
  # of tab button
  ADT_tab_selector <- "a:contains('ADT Threshold')"

  hideElement(
    selector = ADT_tab_selector
  )
  
  # Initialize Variables
  # all_options: list used to store full record of user selections 
  # across all tabs
  all_options <- 
    list(
      # Append config app version to list that is printed to file 
      `config_version` = config_version
    )
  
  # module_data: reactiveValues object for storing data specific to this module
  module_data <- reactiveValues()
  # Available choices for Sortable drag-and-drop input: defaults to non-numeric
  # metadata. This variable may change upon loading a config file.
  module_data$metadata_sortable_options <- non_numeric_cols
  # Nothing selected by default
  module_data$metadata_sortable_selected <- character(0)
  # Store assays for which ADT thresholding modules have been created 
  # (to avoid duplicates)
  module_data$existing_adt_modules <- c()
  
  # Threshold tab data
  # State of sidebar: different menus are shown depending on what the user
  # is doing at the moment (adding a new threshold, editing a threshold, etc.)
  module_data$threshold_menu_state <- "idle"
  
  # Tibble for storing threshold data: a blank tibble with column names for 
  # the adt name and the value
  module_data$threshold_data <- 
    tibble(
      `adt` = character(0), 
      `value` = numeric(0)
      )
  
  # Variables set when a ADT in the table is being edited and cleared after a
  # new threshold is set
  editing_data <- reactiveValues()
  # Identity of ADT being edited
  editing_data$adt_target <- NULL
  # Previously defined threshold of ADT being edited
  editing_data$previous_threshold <- NULL
  # Index of row being edited
  editing_data$target_row <- NULL
  
  ## 3.1. Assay Panel ####
  ### 3.1.1. Store selected assays as a reactive variable ####
  assays_selected <- 
    eventReactive(
      input$assays_selected,
      ignoreNULL=FALSE,
      {
        input$assays_selected
        })
  
  ### 3.1.2. Create module server instances for each possible assay ####
  # Observe is used to reactively update outputs when inputs in the module and
  # its sub-modules are changed
  observe({
    # <<- is required for all_assay_options to be accessible to other server
    # code (not sure why)
    all_assay_options <<- list()
    
    # Create an assay options module for each assay in the object 
    for (id in names(object@assays)){
      # Must also use <<- here
      all_assay_options[[id]] <<- 
        options_server(
          id = id,
          object = object,
          categories_selected = assays_selected,
          options_type = "assays"
        )
      }
    })
  
  ### 3.1.3. Process list of assay module outputs #### 
  # Filter list of options module outputs and combine into a single 
  # reactive object, which is added to the all_options list. 
  all_options$assays <- 
    reactive({
      #Options list is only processed when metadata columns have been selected
      if (!is.null(input$assays_selected)){
        #Extracts each reactive module output and stores them in a list
        list <- lapply(all_assay_options, function(x) x())
        #Filter list for metadata columns that have been selected by the user
        return(list[names(list) %in% input$assays_selected])
        } else {
          #Return NULL if no columns are selected
          return(NULL)
          }
      })
  
  ### 3.1.4. Determine designated ADT assay ####
  ADT_assay <-
    reactive({
      # Get TRUE/FALSE values for whether each assay is an ADT assay
      is_adt <-
        sapply(
          all_options$assays(),
          function(x){
            x$designated_adt
            }
          )
      
      # Return the name of the assay designated as the ADT assay
      if (any(is_adt == TRUE)){
        return(
          names(
            is_adt[is_adt == TRUE]
            )
          )
      } else {
        # If none are designated, return NULL.
        return(
          NULL
          )
      }
    })
  
  ### 3.1.5. Show/Hide ADT thresholding tab ####
  observe({
    # When an assay is designated as the ADT assay, show the corresponding tab
    if (!is.null(ADT_assay())){
      showElement(
        # ADT_tab_selector is defined at the beginning of the server function
        selector = ADT_tab_selector
      )
    } else {
      # Hide the tab if there is no designated ADT assay
      hideElement(
        selector = ADT_tab_selector
      )
    }
  })
  
  
  # Temp: print assay options to screen ####
  output$assay_options <- 
    renderPrint({
      all_options$assays()
      })
  
  ## 3.2. Metadata Panel ####
  ### 3.2.1. Record Selected Metadata ####
  metadata_selected <- 
    eventReactive(
      input$metadata_selected,
      ignoreNULL = FALSE,
      {
        input$metadata_selected
        })
  
  ### 3.2.2. Options Modules for Metadata ####
  # One server instance is created for each metadata category in the object
  all_metadata_options <- list()
  
  for (id in names(object@meta.data)){
    server_output <- 
      options_server(
        id = id,
        object = object,
        categories_selected = metadata_selected,
        options_type = "metadata"
        )
    
    all_metadata_options[[id]] <- server_output
  }
  
  # observe({
  #   # <<- is required for all_metadata_options to be accessible to other server
  #   # code (variables defined within observers are defined in the local 
  #   # environment of the observer by default, unless superassignment (<<-) 
  #   # is used
  #   all_metadata_options <<- list()
  #   
  #   for (id in names(object@meta.data)){
  #     all_metadata_options[[id]] <<- 
  #       options_server(
  #         id = id,
  #         object = object,
  #         categories_selected = metadata_selected,
  #         options_type = "metadata"
  #       )
  #     }
  # 
  # })
  
  ### 3.2.3. Reactive List of Options Module Outputs ####
  all_options$metadata <- 
    reactive({
      # Options list is only processed when metadata columns have been selected
      if (!is.null(input$metadata_selected)){
        # Extracts each reactive module output and stores them in a list
        list <- lapply(all_metadata_options, function(x) x())
        # Filter list for metadata columns that have been selected by the user
        list <- list[names(list) %in% input$metadata_selected]
        # Sort list according to the order specified by the user in the drag
        # and drop menu
        list <- list[input$metadata_selected]
        return(list)
        } else {
          # Return NULL if no columns are selected
          return(NULL)
          }
      })
  
  ### 3.2.4. Reactive UI ####
  #### 3.2.4.1. UI for sortable menu ####
  # Uses the bucket_list input from the sortable package
  metadata_bucket_ui <-
    eventReactive(
      c(module_data$metadata_sortable_options,
        module_data$metadata_sortable_selected),
      label = "Metadata: define sortable",
      {
        tagList(
          tags$b("Choose Metadata to Include in App"),
          bucket_list(
            header = 
              "Drag metadata categories to the \"Included Metadata\" 
              column to include. Metadata will appear in app menus in 
              the order they appear in the right-hand column.",
            orientation = "horizontal",
            group_name = "metadata_bucket",
            # Use the default class, and a class specific to this app
            # Many sub-classes are tied to the default class, and styling will
            # not be applied to those classes if the default class is not also 
            # passed to this argument.
            class = 
              c("default-sortable", "bucket-select"),
            add_rank_list(
              input_id = "metadata_not_selected",
              text = "Available Metadata",
              labels = module_data$metadata_sortable_options
            ),
            add_rank_list(
              input_id = "metadata_selected",
              text = "Included Metadata",
              labels = module_data$metadata_sortable_selected
            )
          )
        )
        })
  
  #### 3.2.4.2. Set Order of metadata cards based on sortable input ####
  # Sorting trigger: triggers when UI elements are re-arranged; restores inputs
  session$userData$metadata_sorting_trigger <- makeReactiveTrigger()
  
  metadata_cards_ui <-
    reactive(
      label = "Set Order of Metadata Options Cards",
      {
        ui <-
          lapply(
            # Cards made for the selected metadata categories in the order they
            # are selected, then the non-selected categories (which will be
            # hidden)
            c(input$metadata_selected, input$metadata_not_selected),
            function(colname){
              card <- 
                options_ui(
                  id = colname,
                  object = object,
                  optcard_type = "metadata"
                )
              
              # Apply shinyjs hidden class to cards for all metadata
              # categories not currently selected
              if (!colname %in% input$metadata_selected){
                card <- shinyjs::hidden(card)
              }
              
              card
            }
          )
          
        # Trigger sorting trigger to restore inputs lost upon re-creating UI
        session$userData$metadata_sorting_trigger$trigger()
        
        # Return sorted cards
        ui
      })
  
  #### 3.2.4.3. Render reactive UI ####
  # Sortable
  output$metadata_sortable_bucket <-
    renderUI({
      metadata_bucket_ui()
    })
  
  # Metadata cards
  output$metadata_cards <-
    renderUI({
      metadata_cards_ui()
    })
  
  outputOptions(
    output, 
    "metadata_cards", 
    suspendWhenHidden = FALSE,
    priority = 10
    )
  
  # TEMP: print all metadata options
  output$print_metadata <-
    renderPrint({
      all_options$metadata()
      })
  
  ## 3.3 ADT Thresholding Panel ####
  ### 3.3.1. Define/update available ADTs ####
  # Reactive variable will be used for updating the selection menu with
  # ADTs in the designated assay that have not already been added to the table
  available_adts <-
    reactive({
      req(ADT_assay())
      
      # Fetch ADTs in the designated assay (reacts to assay)
      adts <- 
        object[[ADT_assay()]] |> 
        rownames()
      
      # return adts that are not included in the table of defined thresholds
      # (also reacts to changes in the table)
      if (!is.null(module_data$threshold_data)){
        return(adts[!adts %in% module_data$threshold_data$adt])
      } else {
        return(adts)
      }
    })
  
  ### 3.3.2. Populate ADT Choices when designated ADT assay is changed ####
  observeEvent(
    ADT_assay(),
    ignoreNULL = TRUE,
    ignoreInit = TRUE,
    {
      # Fetch features (surface proteins) for the designated ADT assay
      adts <- 
        object[[ADT_assay()]] |> 
        rownames()
      
      # Populate select input with ADT choices 
      updateSelectizeInput(
        session = session,
        inputId = "selected_adt",
        choices = adts,
        selected = character(0),
        server = TRUE
        )
      
      # Also, set state of threshold menus back to "idle" if the designated 
      # ADT assay changes
      module_data$threshold_menu_state <- "idle"
    })
  
  ### 3.3.3. Threshold picker server ####
  #### 3.3.3.1. ADT passed to server ####
  threshold_server_adt <- 
    reactive({
      # Value passed to server depends on state
      if (module_data$threshold_menu_state == "add"){
        # When the menus are in the "add" state, use input$selected_adt
        # Selected ADT must be defined to avoid errors
        req(input$selected_adt)
        req(ADT_assay())
        
        # When adding a new threshold, use the adt selected by the user in
        # the search window
        # Add assay key to ADT (threshold server expects this)
        paste0(
          Key(object[[isolate({ADT_assay()})]]),
          input$selected_adt
          )
      } else if (module_data$threshold_menu_state == "edit") {
        # Will be equal to the ADT requested for editing
        # (this is set using a reactiveValues object)
        paste0(
          # Add assay key
          Key(object[[isolate({ADT_assay()})]]),
          editing_data$adt_target
        )
      } 
    })

  #### 3.3.3.2. Server instance ####
  threshold_value <- 
    threshold_picker_server(
      id = "threshold_picker", 
      # `object` argument must be reactive, but the object is non-reactive
      # in the config file
      object = reactive({object}), 
      feature = threshold_server_adt,
      showhide_animation = TRUE,
      # Used to show the previous threshold on the interactive ridge 
      # plot during editing
      set_threshold = 
        reactive({
          editing_data$previous_threshold
          })
      )
  
  ### 3.3.4. Respond to "Add threshold" Button ####
  # Set state to "add", which will show menus with the class "show-on-add"
  observeEvent(
    input$add_threshold,
    ignoreNULL = FALSE,
    ignoreInit = TRUE,
    {
      module_data$threshold_menu_state <- "add"
    })
  
  ### 3.3.5. Show/hide menus based on state ####
  #### 3.3.5.1. Generic Menus ####
  observe({
    # jQuery selectors for classes that show elements based on state
    add_selector <- "[class *= 'show-on-add']"
    edit_selector <- "[class *= 'show-on-edit']"
    idle_selector <- "[class *= 'show-on-idle']"
    
    if (module_data$threshold_menu_state == "add"){
      showElement(
        selector = add_selector
        )
    } else if (module_data$threshold_menu_state == "edit"){
      showElement(
        selector = edit_selector
        )
    } else if (module_data$threshold_menu_state == "idle") {
      hideElement(
        selector = add_selector
        )
      hideElement(
        selector = edit_selector
        )
      }
  })
  
  #### 3.3.5.2. Show/Hide Threshold Picker UI ####
  # Shown when the state is "add" and an adt is entered in the search input
  # OR when the state is "edit" (feature being edited is provided when changing 
  # to this state)
  observe({
    target_id <- "threshold_picker_div"
    
    if (module_data$threshold_menu_state == "add"){
      if (!is.null(input$selected_adt)){
        showElement(
          id = target_id,
          anim = TRUE
        )
      } else {
        hideElement(
          id = target_id,
          anim = TRUE
        )
      }
    } else if (module_data$threshold_menu_state == "edit"){
      showElement(
        id = target_id
      )
    } else {
      hideElement(
        id = target_id
      )
    }
  })
  
  ### 3.3.6. Confirm feature button ####
  #### 3.3.6.1. Enable "confirm" button when a threshold is selected ####
  observe({
    print("threshold value")
    print(threshold_value())
    
    if (!is.null(threshold_value())){
      enable(
        id = "accept_threshold"
        )
    } else {
        disable(
          id = "accept_threshold"
          )
      }
    })
  
  #### 3.3.6.2. Save data and close menu when the confirm button is pressed ####
  observeEvent(
    input$accept_threshold,
    ignoreNULL = FALSE,
    ignoreInit = TRUE,
    {
      if (module_data$threshold_menu_state == "add"){
        # If the state is "add", add the threshold value for the currently 
        # selected ADT to the table
        module_data$threshold_data <-
          module_data$threshold_data |> 
          add_row(
            adt = input$selected_adt,
            value = threshold_value()
          )
      } else if (module_data$threshold_menu_state == "edit"){
        # Set the "adt" entry of the row being edited to the feature name
        # module_data$threshold_data[editing_data$target_row, 1] <-
        #   editing_data$adt_target
        
        # Set the "value" entry (column 2) of the row being edited to the new 
        # value chosen on the interactive plot
        module_data$threshold_data[editing_data$target_row, 2] <-
          threshold_value()
       
        # Set editing_data variables back to NULL
        editing_data$adt_target <- NULL 
        editing_data$target_row <- NULL
        editing_data$previous_threshold <- NULL
      }
      
      # Update ADT choices to exclude the ADTs currently in the table
      adts <- 
        object[[ADT_assay()]] |> 
        rownames()
      
      updateSelectizeInput(
        session = session,
        inputId = "selected_adt",
        choices = adts[!adts %in% module_data$threshold_data$adt],
        selected = character(0),
        server = TRUE
        )
      
      # Set state of menus back to "idle"
      module_data$threshold_menu_state <- "idle"
    })
  
  ### 3.3.7. Render Table of ADT Thresholds ####
  #### 3.3.7.1. DT Datatable ####
  threshold_DT <-
    reactive({
      DT <- module_data$threshold_data
      
      # Add edit and delete buttons to table
      # Code adapted from https://github.com/AntoineGuillot2/ButtonsInDataTable
      if (nrow(DT) > 0){
        DT[["Actions"]] <-
          glue(
            '<div class = "btn-group" style = "float: right;" role = "group" 
            aria-label = "Options for {DT[[\'adt\']]}">
              <button type="button" class="btn icon-button edit" 
                id = edit_{1:nrow(module_data$threshold_data)}> 
                <i class = "fa fa-pencil" role = "presentation" 
                  aria-label = "Edit" style = "font-size: 1.7em;"></i>
                </button>
              
              <button type="button" class="btn icon-button delete" 
                id = delete_{1:nrow(module_data$threshold_data)}> 
                  <i class = "fa fa-times-circle" role = "presentation" 
                  aria-label = "Delete" style = "font-size: 1.7em;"></i>
              </button>
           </div>'
          )
      }
      
      # Create DT Datatable
      datatable(
        DT,
        # DT classes applied
        # See https://datatables.net/manual/styling/classes
        class = "compact stripe cell-border hover",
        # Disallow selection of rows/cells (currently)
        selection = "none",
        # Remove rownames
        rownames = FALSE,
        colnames = c("ADT" = "adt", "Chosen Threshold" = "value"),
        # Escape set to FALSE so HTML above is rendered properly
        escape = FALSE
        )
      })
  
  output$threshold_table <-
    renderDT({
      threshold_DT()
    })
  
  #### 3.3.7.2. JavaScript for Inline Buttons ####
  button_script <-
    reactive({
      req(module_data$threshold_data)
      
      # Adapted from 
      # https://github.com/AntoineGuillot2/ButtonsInDataTable/blob/master/server.R
      if (nrow(module_data$threshold_data > 0)){
        # Script: when the user clicks a button within the DT table 
        # (#threshold_table button) register the id of the button the user 
        # clicked on as input$lastClickId, and a random number as 
        # input$lastClick (this is the trigger for responding to the click, and 
        # must therefore always change with each click.)
        tags$script(
          "
          $(document).on('click', '#threshold_table button', function () {
              Shiny.onInputChange('lastClickId', this.id);
              Shiny.onInputChange('lastClick', Math.random())
              });
          "
        )
      }
    })
  
  output$threshold_table_button_script <-
    renderUI({
      button_script()
    })
  
  ### 3.3.8. Respond to edit/delete buttons ####
  observeEvent(
    input$lastClick,
    ignoreNULL = FALSE,
    ignoreInit = TRUE,
    {
      # input$lastClickId stores the id of the button that was clicked
      if (grepl("edit", input$lastClickId)){
        # If the button is an edit button, initialize menus for editing.
        # Set the state of the menus to "edit"
        module_data$threshold_menu_state <- "edit"
        
        # Determine which row the edit button was on
        row_selected <- 
          gsub("edit_", "", input$lastClickId) |> 
          as.numeric()
        
        # print(module_data$threshold_data)
        # print(module_data$threshold_data$adt)
        # print("Row selected")
        # print(row_selected)
        # print("ADT selected")
        # print(module_data$threshold_data$adt[row_selected])
        
        # Determine which ADT was on the row selected
        adt_selected <- 
          module_data$threshold_data$adt[row_selected]
        
        if (is.na(adt_selected) | is.null(adt_selected)){
          warning(
            "Threshold table: ADT selected for editing is undefined."
            )
        }
        
        # Fetch previous threshold and pass to interactive ridge plot
        # (will display previous selection )
        editing_data$previous_threshold <- 
          module_data$threshold_data$value[row_selected]
        
        # Pass feature to reactive variable to update the ridge plot
        editing_data$adt_target <- 
          adt_selected
        
        # Store the index of the row selected for 
        editing_data$target_row <-
          row_selected
        
        # Set the selected ADT to the one being edited. 
        # The ADT selectize input is modified and is still accessed, 
        # but it is not visible when the state is "edit"
        # updateSelectizeInput(
        #   session = session,
        #   inputId = "selected_adt",
        #   # Choices do not change, but must be added for update to proceed
        #   choices = available_adts(),
        #   selected = adt_selected,
        #   server = TRUE
        #   )
        
        
      } else if (grepl("delete", input$lastClickId)) {
        # Delete the row corresponding to the button from the table
        # Determine which row the delete button was on
        row_selected <- 
          gsub("delete_", "", input$lastClickId) |> 
          as.numeric()
        
        # Prevents crashing in the event the selected_row is undefined
        if (!is.null(row_selected) | is.na(row_selected)){
          print("Delete Target")
          print(module_data$threshold_data$adt[row_selected])
          
          # Delete the selected row from the table and save the new table
          module_data$threshold_data <-
            module_data$threshold_data[-row_selected,]
        } else {
          warning("Unable to determine the index of the row selected for deletion")
        }
      }
    })
  
  ### 3.3.9. Threshold settings window header text (edit mode) ####
  output$threshold_header_edit_feature <-
    renderText({
      editing_data$adt_target
    })
  
  # TEMP: value of input$selected_adt ####
  observe({
    print("Change in value of input$selected_adt. New value:")
    print(input$selected_adt)
  })
  
  ## 3.4. Config File Download Handler ####
  output$export_selections <- 
    downloadHandler(
      filename = "config.rds",
      content = 
        function(file){
          # Compile config file data from the all_options list 
          config_data <- 
            lapply(
              all_options, 
              function(x){
                # The all_options list contains a mix of reactive and 
                # non-reactive values. Reactive values are unpacked, while 
                # non-reactive values are left as-is.
                if (is.reactive(x)){
                  x()
                  } else {
                    x
                  }
                }
              ) 
          
          # Download above object as .rds 
          saveRDS(
            object = config_data,
            file = file
            )
          })
  
  ## 3.5. Load Config File ####
  ### 3.5.1 Load File ####
  # Loads a previously created config file and imports contents into app
  # storing in session$userdata makes file visible to all modules
  session$userData$config <-
    eventReactive(
      input$load_config,
      ignoreNULL = FALSE,
      ignoreInit = TRUE,
      {
        print("Load config pressed")
        # For now, use a pre-determined config file
        # will soon be chosen with a file input
        showNotification(
          ui =
            div(
              style = "width: 350px;",
              glue('Loading file at {config_filename}')
            ),
          duration = NULL,
          id = "load_config",
          session = session
        )
        
        readRDS(config_filename)
      })
  
  ### 3.5.2. Update inputs in main server function with file contents ####
  # Assays selected
  observeEvent(
    session$userData$config(),
    {
      updateMultiInput(
        session,
        inputId = "assays_selected",
        selected = 
          # Names of assays in config file are the names selected when the 
          # file was created
          names(
            session$userData$config()$assays
          )
      )
    })
  
  # Metadata selected
  observeEvent(
    session$userData$config(),
    {
      updateMultiInput(
        session,
        inputId = "metadata_selected",
        selected = 
          # Names of assays in config file are the names selected when the 
          # file was created
          names(
            session$userData$config()$metadata
          )
      )
    })
  
  # config_file_load <- eventReactive(
  #   input$load_config,
  #   ignoreNULL = FALSE,
  #   ignoreInit = TRUE,
  #   {
  #     print("Load config pressed")
  #     # For now, use a pre-determined config file
  #     # will soon be chosen with a file input
  #     icon_notification_ui(
  #       icon_name = NULL, 
  #       message = 'Loading file at "./Seurat_Objects/AML_TotalVI_config.rds"'
  #       )
  #     readRDS("./Seurat_Objects/AML_TotalVI_config.rds")
  #   })
  
  # observe({
  #   print("Config file exists:")
  #   print(!is.null(session$userData$config()))
  #   print("Contents")
  #   print(session$userData$config())
  # })
  
  # observeEvent(
  #   input$load_config,
  #   ignoreNULL = FALSE,
  #   {
  #     print("Load Config Button")
  #   })
  
  }

shinyApp(ui, server)