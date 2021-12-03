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
  
)

# Config Applet Server Function ####
server <- function(input, output, session) {
  
}

shinyApp(ui, server)