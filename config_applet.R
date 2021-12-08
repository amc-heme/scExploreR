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

#Assay options module ####
##UI####
#The "id" argument will be equal to the assay name 
#Problems could result if assay names are not unique. This seems unlikely but 
#needs to be handled if it were to occur. 
assay_options_ui <- function(id){
  #NS(id): namespace function, defined here and called for every input ID. All 
  #inputs created with the namespace function will be created within the namespace 
  #defined by id. Using multiple namespaces allows the inputs to have the same 
  #id in different instances of the module without collisions
  ns <- NS(id)
  #UI created for each assay
  ui <- div(
    id=ns("optcard"),
    class="optcard single-space-bottom",
    tags$strong(glue("Options for {id}"),
                class="large half-space-bottom center"),
    #Human-readable suffix: appears on plots and search entries
    textInput(inputId = ns("hr"),
              label="Set label for assay (will appear as entered in app)",
              width = "100%"),
    #Include assay name on plots: if checked, the label entered will be 
    #displayed on plots and in the feature search results.
    #I may put this in the main app instead; it makes more sense to toggle it when making the plots.
    checkboxInput(inputId = ns("include_label"),
                  label = "Include assay name on plots?"),
    tags$p("(This is usually not required for the default assay in your data)"),
    #Label entered (for testing purposes only)
    textOutput(outputId = ns("label_entered"))
  )
  
  #Add "hidden" shinyjs class to the card to hide each card initially
  ui <- shinyjs::hidden(ui)
  
  return(ui)
}

##Server function for processing assay options ####
assay_options_server <- function(id, assays_selected){
  moduleServer(id, function(input,output,session){
    #Test if input is being passed correctly to the server
    observeEvent(assays_selected(),
                 label = "Update Visibility", 
                 ignoreNULL = FALSE,
                 {
                   print(glue("Assays Selected: {assays_selected()}"))
                   
                   #If the assay id is on the list of selected assays, 
                   #show the options card.
                   if (id %in% assays_selected()){
                     print(glue("Assay {id} is selected"))
                     showElement("optcard")
                     
                   #Otherwise, hide the card.
                   } else {
                     print(glue("Assay {id} is not selected"))
                     hideElement("optcard")
                   }
                 })
    
    output$label_entered <- renderText({glue("input${NS(id,'hr')}: {input$hr}")})
  })
}


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
                    multiInput(inputId = "assays_selected",
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
                    #Create an instance of the assay options UI for all possible 
                    #assays. Each UI creates a "card"; all are hidden at first 
                    #and assays selected by the user are displayed.
                    tagList(lapply(names(sobj@assays), 
                           function(id) assay_options_ui(id))),
                    
                    #Dynamic UI for showing assay choices
                    #uiOutput(outputId = "assay_options")
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
  assays_selected <- eventReactive(input$assays_selected,
                                   ignoreNULL=FALSE,
                                   {
                                     input$assays_selected
                                   })
  
  
  #1.1. Create module server instances for each possible assay
  lapply(names(sobj@assays), 
         function(id) assay_options_server(id,
                                           #Pass reactive value of assays selected to server
                                           assays_selected = assays_selected
                                           )#end assay_options_server
         )#End lapply
 
}

shinyApp(ui, server)