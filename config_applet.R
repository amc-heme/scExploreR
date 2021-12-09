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


#Load functions in ./R directory
#Get list of files
source_files <- list.files(path = "./R", 
                           pattern="*.R$", 
                           full.names=TRUE, 
                           ignore.case=TRUE)
#Use source() to import files into R
sapply(source_files,source)

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
    tags$p("(This is usually not required for the default assay in your data)")
  )
  
  #Add "hidden" shinyjs class to the card to hide each card initially
  ui <- shinyjs::hidden(ui)

  return(ui)
}

#Metadata options module ####
metadata_options_ui <- function(id){
  #Namespace function
  ns <- NS(id)
  #Determine type of metadata (class of values) for display in column
  #Must call @meta.data first with arbitrary metadata 
  #(sobj[[<metadata>]]) will return a dataframe
  type <- class(unique(sobj@meta.data[[id]]))
  
  #Simplify metadata type: "character" and "factor" classes are reported as 
  #"categorical", while "numeric" and "integer" classes are reported as "numeric"
  if (type=="character"||type=="factor"){
    type <- "Categorical"
  } else if (type=="numeric"||type=="integer"){
    type <- "Numeric"
  }
  #(Other metadata classes may exist)
  
  #UI: create an options card for each metadata option selected
  ui <- div(
    id=ns("optcard"),
    class="optcard single-space-bottom",
    tags$strong(glue("Options for {id}"),
                class="large center"),
    
    #Print the type of metadata beneath the title
    tags$p(glue("({type})"),class="center half-space-bottom"),
    
    #Human-readable suffix: appears on plots and search entries
    textInput(inputId = ns("hr"),
              label="Set label for metadata column (will appear as entered in app interface)",
              width = "100%")
  )
  
  #Add "hidden" shinyjs class to the card to hide each card initially
  ui <- shinyjs::hidden(ui)
  
  return(ui)
}

#Server Module for field selection options ####
#Applies to multiple types of selections (assays, metadata, etc.). One instance 
#of the module is applied for every available selection across each tab.
#Renders option cards for the variables of assays, metadata, etc. selected by 
#the user, and processes user selections to create a config file for the main app.
#id: the id passed to the module
#categories_selected: a reactive variable describing the variables (assays, metatdata, etc.
#, selected by the user)
#options_type: the type of options to create a server function for. Can be one 
#of "assays" or "metadata"
#category_name: the name of the individual category that the instance of the
#module applies to. This is the id by default, and can be changed.
options_server <- function(id, 
                           categories_selected, 
                           options_type=c("assays","metadata"),
                           category_name=id){
  #Namespace function
  ns <- NS(id)
  
  #1. Show/hide cards based on user selections
  #(Conditionals on non-reactive values such as options_type can be used 
  #outside of server components)
  observeEvent(categories_selected(),
               label = glue("Show/Hide Cards: {options_type}"), 
               ignoreNULL = FALSE,
               {
                 #Examine the list of currently selected categories, and check
                 #if the module's category name is selected.
                 #If so, show the options card
                 if (category_name %in% categories_selected()){
                   showElement(ns("optcard"))
                   #Otherwise, hide the card
                 } else {
                   hideElement(ns("optcard"))
                 }
               })
}


#UI components ####
##1. UI functions
#applet_sidebar_panel
#Creates a sidebarPanel UI object with formatting common to the applet, and 
#additional classes if specified. Sidebar content is specified to `...`
applet_sidebar_panel <- function(...,class=NULL){
  #Use an empty string to define the class if it is not specified
  if(is.null(class)) class <- ""
  sidebarPanel(
    #The use of width=0 allows the width to be specified for different window 
    #sizes using the class argument (using the width argument will apply the 
    #style for all screens with at least a "medium" size viewport)
    width=0,
    #Column width specifications vary based on viewport size and are given using 
    #Bootstrap classes (R Studio creates a small window by default on a MacBook pro)
    #https://getbootstrap.com/docs/3.3/css/#responsive-utilities
    class = paste0("left-column-panel col-sm-6 col-md-5 col-lg-4 ",class),
    #Pass content to sidebarPanel
    tagList(...)
    )
}

applet_main_panel <- function(...,class=NULL){
  #Use an empty string to define the class if it is not specified
  if(is.null(class)) class <- ""
  #Use width=0 to define column widths using Bootstrap classes
  mainPanel(width=0,
            class = paste0("right-column-panel col-sm-6 col-md-7 col-lg-8 ",class),
            #Pass content to mainPanel 
            tagList(...)
            )
}

##2. Assays *tab* (not the assay options module) ####
assay_tab <- function(){
  sidebarLayout(
    applet_sidebar_panel(
      multiInput(inputId = "assays_selected",
                 label = "Choose assays to include:",
                 width = "100%",
                 choices = names(sobj@assays),
                 options = list(enable_search = FALSE,
                                non_selected_header = "Available Assays",
                                selected_header = "Selected Assays",
                                "hide_empty_groups" = TRUE)
      )#multiInput
    ),
    applet_main_panel(
      #Create an instance of the assay options UI for all possible assays. Each 
      #UI creates a "card"; all are hidden at first and are shown when their 
      #corresponding assay is selected by the user. The "id" argument in lapply 
      #is the name of the assay.
      tagList(lapply(names(sobj@assays),function(id) assay_options_ui(id)))
    )
  )
}

#3. Metadata Tab ####
metadata_tab <- function(){
  sidebarLayout(
    applet_sidebar_panel(
      multiInput(inputId = "metadata_selected",
                 label = "Choose metadata to include:",
                 width = "100%",
                 choices = names(sobj@meta.data),
                 options = list(enable_search = FALSE,
                                non_selected_header = "Available Metadata",
                                selected_header = "Selected Metadata",
                                "hide_empty_groups" = TRUE)
                 )
    ),
    applet_main_panel(
      #Create a metadata options "card" for each metadata column in the object
      #All cards are hidden at first and are displayed when the user selects 
      #the corresponding column. The "id" argument in lapply is the name of the metadata field.
      tagList(lapply(names(sobj@meta.data), function(id) metadata_options_ui(id)))
    )
  )
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
  #CSS style: prevents navbar from appearing on top of content 
  tags$head(tags$style(HTML("body{
                            padding-top: 60px;
                            }"))),
  #Main UI
  navbarPage(title = "Object Configuration",
             windowTitle="Configure Seurat Object",
             position="fixed-top",
             tabPanel(title="Assays",
                      assay_tab()),
             tabPanel(title = "Metadata",
                      metadata_tab())
             )
  )#End fluidPage

# Config Applet Server Function ####
server <- function(input, output, session) {
  #1. Assay Panel
  #1.1. Store selected assays as a reactive variable
  assays_selected <- eventReactive(input$assays_selected,
                                   ignoreNULL=FALSE,
                                   {
                                     input$assays_selected
                                   })
  
  
  #1.1. Create module server instances for each possible assay
  lapply(names(sobj@assays), 
         function(id) options_server(id = id,
                                     categories_selected = assays_selected,
                                     options_type = "assays"
         )
         )#End lapply
 
  #2. Metadata Panel
  #2.1. Store metadata selected as a reactive variable
  metadata_selected <- eventReactive(input$metadata_selected,
                                   ignoreNULL=FALSE,
                                   {
                                     input$metadata_selected
                                   })
  
  
  #2.2. Create options server module instances for each metadata assay
  lapply(names(sobj@meta.data), 
         function(id) options_server(id = id,
                                     categories_selected = metadata_selected,
                                     options_type = "metadata"
                                     )
  )#End lapply
  
}

shinyApp(ui, server)