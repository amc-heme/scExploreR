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
    path = "./www/applet_js", 
    # Use regex to search for files ending in .js (double 
    # backslash used to escape '.' character)
    pattern = ".*\\.js", 
    full.names = TRUE, 
    ignore.case = TRUE
    )

# Add www/collapsible_panel.js file to list
js_files <- c(js_files,"./www/collapsible_panel.js")

# Create list of style tags for each CSS file 
js_list <- lapply(js_files,includeScript)

# Load object (hard-coded for now but will soon be chosen using a file input)
object <- readRDS("./Seurat_Objects/aml_bmmc_totalvi_20211206_slim1000.rds")
# Need a conditional to test if the loaded object is a Seurat object

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
metadata_tab <- function(){
  sidebarLayout(
    applet_sidebar_panel(
      div(
        class = "input-no-margin",
        multiInput(
          inputId = "metadata_selected",
          label = "Choose metadata to include:",
          width = "100%",
          # Currently, only non-numeric metadata columns are selectable
          choices = non_numeric_cols,
          options = 
            list(
              enable_search = FALSE,
              non_selected_header = "Available Metadata",
              selected_header = "Selected Metadata",
              "hide_empty_groups" = TRUE
              )
          )
        )
      ),
    applet_main_panel(
      # Options for Numeric metadata
      # 
      
      # Options for Categorical, logical metadata
      # Create a metadata options "card" for each non-numeric metadata column in
      # the object. Cards below are hidden and display when the corresponding
      # metadata category is selected
      tagList(
        lapply(
          non_numeric_cols, 
          function(colname){
            options_ui(
              id = colname, 
              object = object,
              optcard_type = "metadata"
              )
            }
          
          ),
        # TEMP: add an additional card displaying the outputs from all tabs
        div(
          class="optcard",
          verbatimTextOutput(outputId = "all_variables")
          )
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
    windowTitle="Configure Seurat Object",
    position="fixed-top",
    id="navbar",
    #Tabs are displayed below
    tabPanel(
      title="Assays",
      assay_tab()
      ),
    tabPanel(
      title = "Metadata",
      metadata_tab()
      )
    ),
  
  # Elements below will be moved to the navbar using JavaScript
  # Help button
  dropdownButton(
    inputId = "help",
    status="info",
    right=TRUE,
    label = "",
    size="sm",
    icon = icon("question"),
    # Dropdown menu content
    tagList(
      tags$p(
        "Help and Background",
        style=
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
        href="https://github.com/amc-heme/DataExploreShiny/issues",
        class="blue_hover",
        # Opens link in new tab
        target="_blank",
        rel="noopener noreferrer")
      )# End tagList
    ), # End Help Button
  
  # Button to create/export config file
  downloadButton(
    outputId = "export_selections",
    label="Export Configuration",
    class = "float_right",
    icon = NULL
    ),
  
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
  # Initialize Variables
  # all_options: list used to store full record of user selections 
  # across all tabs
  all_options <- list()
  
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
  
  ### 3.1.3. Filter list of options module outputs and combine into a single 
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
  
  # Temp: print assay options to screen
  output$assay_options <- 
    renderPrint({
      all_options$assays()
      })
  
  ## 3.2. Metadata Panel ####
  ### 3.2.1. Record Selected Metadata ####
  metadata_selected <- 
    eventReactive(
      input$metadata_selected,
      ignoreNULL=FALSE,
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
        return(list[names(list) %in% input$metadata_selected])
        } else {
          # Return NULL if no columns are selected
          return(NULL)
          }
      })
  
  # TEMP: print all metadata options
  output$all_variables <-
    renderPrint({
      # Lapply: fetches each reactive object on the list, prints the result, and
      # stores all objects in a list
      all_options
      })
  
  ## 3.3. Config File Download Handler ####
  output$export_selections <- 
    downloadHandler(
      filename = "config.rds",
      content=function(file){
        # Extract each reactive output from the options module 
        # and store in a list
        all_options_list <- lapply(all_options, function(x) x()) 
        # Download above object as .rds 
        saveRDS(
          object = all_options_list,
          file = file
          )
      })
  }

shinyApp(ui, server)