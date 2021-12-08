#Initialize libraries
library(shiny)
library(Seurat)

#Shiny add-ons 
library(shinyWidgets)
library(rintrojs)
library(shinydashboard)
library(waiter)
library(shinycssloaders)
library(shinyjs)

#Reactlog (for debugging)
library(reactlog)
options(shiny.reactlog=TRUE)

#Tidyverse Packages
library(tidyverse)
library(stringr)
library(dplyr)
library(ggplot2)
library(glue)
library(DT)

#Load CSS files for app: CSS files are defined and each file is converted to a
#<script> tag using includeCSS(). Each tag defined is passed to a list, which is
#included in the main UI function.
#Get list of .css files in www/ directory
css_files <- list.files(path = "./www", 
                        pattern="*.css$", 
                        full.names=TRUE, 
                        ignore.case=TRUE)
#Create list of style tags for each CSS file
css_list <- lapply(css_files,includeCSS)

#Load Javasctipt files for app: find all .js files in www/ directory and create
#a list of script() tags using includeScript().
#Get list of .js files in www/ directory
js_files <- list.files(path = "./www", 
                       pattern="*.js$", 
                       full.names=TRUE, 
                       ignore.case=TRUE)
#Create list of style tags for each CSS file
js_list <- lapply(js_files,includeScript)

#Load object (hard-coded for now but will soon be chosen using a file input)
sobj <- readRDS("./Seurat_Objects/longitudinal_samples_20211025.rds")
#Need a conditional to test if the loaded object is a Seurat object

# Config applet UI ####
ui <- fluidPage(
  #Place style tags for each CSS file in document
  css_list,
  #Waiter UI: spinners
  useWaiter(),
  #Shinyjs: a Shiny JavaScript extension
  useShinyjs(),
  #Include scripts for each JavaScript file in document
  js_list,
  #Main UI
  tabsetPanel(
    id="config_choices",
    type = "pills",
    tabPanel("Assays",
             column(width = 6,
                    class = "left-column-panel",
                    multiInput(inputId = "select_assays",
                               label = "Choose assays to include:",
                               width = "100%",
                               choices = names(sobj@assays),
                               options = list(enable_search = FALSE,
                                              non_selected_header = "Available Assays",
                                              selected_header = "Selected Assays",
                                              "hide_empty_groups" = TRUE)
                               )
                    ),#End column
             column(width = 6,
                    #Dynamic UI for showing assay choices
                    uiOutput(outputId = "assay_options")
                    ),
    tabPanel("Metadata",
             #Placehoder div
             div()
             )
    )
  )
  )

# Config Applet Server Function ####
server <- function(input, output, session) {
  #1. Assay Panel
  #1.1. Update Assay options
  assay_options_ui <- eventReactive(input$select_assays,
                                    label = "Update Assay Options UI", 
                                    ignoreNULL = FALSE,
                                    {
                                      ui_card <- function(assay_name){
                                        ui <- div(
                                          class="optcard single-space-bottom",
                                          tags$strong(glue("Options for {assay_name}"),
                                                      class="large half-space-bottom center"),
                                          textInput(inputId = glue("{assay_name}_mr"),
                                                    label= "Enter prefix used to access assay 
                                                    in Seurat plotting functions",
                                                    width = "100%"),
                                          textInput(inputId = glue("{assay_name}_hr"),
                                                    label="Set label for assay 
                                                    (will appear in app)",
                                                    width = "100%"),
                                          checkboxInput(inputId = glue("{assay_name}_include_label"),
                                                        label = "Include assay name on plots?")
                                          )
                                        return(ui)
                                      }
                                      
                                      #use lapply to create a panel for each assay selected
                                      panels <- lapply(input$select_assays, function(x) ui_card(x))
                                      
                                      print(panels)
                                      
                                      tagList(panels)
                                    })
  
  output$assay_options <- renderUI({assay_options_ui()})
  
}

shinyApp(ui, server)