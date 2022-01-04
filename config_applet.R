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
library(shinyFeedback)

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

#Load Javascript files for app: find all .js files that apply to the applet and 
#create a list of script() tags using includeScript().
#Files to include: all files in www/applet_js/ directory, and the collapsible_panel.js 
#file in the www/ directory (www/button_wizzard.js must be excluded since it 
#conflicts with 'applet_navbar_wizzard' in the www/applet_js/ directory)
js_files <- list.files(path = "./www/applet_js", 
                       #Use regex to search for files ending in .js (double 
                       #backslash used to escape '.' character)
                       pattern=".*\\.js", 
                       full.names=TRUE, 
                       ignore.case=TRUE)
#Add www/collapsible_panel.js file to list
js_files <- c(js_files,"./www/collapsible_panel.js")

#Create list of style tags for each CSS file 
js_list <- lapply(js_files,includeScript)



#Load object (hard-coded for now but will soon be chosen using a file input)
sobj <- readRDS("./Seurat_Objects/longitudinal_samples_20211025.rds")
#Need a conditional to test if the loaded object is a Seurat object

#Overview####
#
#Section A: Functions for UI and Server Components
#Section B: Modules
#Section C: Main UI and Server Function

#Section A: Functions #### 
##A.1 UI Functions ####
###applet_sidebar_panel 
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
    #Bootstrap classes (R Studio creates a "small" window by default on a MacBook
    #pro) https://getbootstrap.com/docs/3.3/css/#responsive-utilities
    class = paste0("shinysc-sidebar-panel col-sm-6 col-md-5 col-lg-4 ",class),
    #Pass content to sidebarPanel
    tagList(...)
  )
}

applet_main_panel <- function(...,class=NULL){
  #Use an empty string to define the class if it is not specified
  if(is.null(class)) class <- ""
  #Use width=0 to define column widths using Bootstrap classes
  mainPanel(width=0,
            class = paste0("shinysc-main-panel col-sm-6 col-md-7 col-lg-8 ",class),
            #Pass content to mainPanel 
            tagList(...)
  )
}

##A.2 Server Functions ####
#metadata_type: takes a Seurat object and a metadata field, and returns the 
#class of the metadata field
metadata_type <- function(sobj,metadata_field){
  #Get unique values of metadata field for display of summary statistics
  values <- unique(sobj@meta.data[[metadata_field]])
  
  #Determine type of metadata (class of values) for display in column
  #Must call @meta.data first with arbitrary metadata 
  #(sobj[[<metadata>]]) will return a dataframe
  class <- class(values)
  
  #Simplify metadata type: "character" and "factor" classes are reported as 
  #"categorical", while "numeric" and "integer" classes are reported as "numeric"
  if (class=="character"||class=="factor"){
    type <- "Categorical"
  } else if (class=="numeric"||class=="integer"){
    type <- "Numeric"
  } else {
    #Other metadata classes may exist: warn user for unexpected classes
    warning(glue("Unexpected class for metadata column {metadata_field}: {class}."))
    type <- class
  }
  
  return(type)
}

#Section B: Modules ####
#The UI and the server components of each module are displayed
#The level of the module is the number of parent modules it has, including the
#main server function. A module created within a module, which is itself created
#within the main server has a level of 2.

##B.1 Options Module (first level) ####
###Options Module UI
#id: the namespace id given to this module. In the config app, this is either 
#the assay name or the metadata type.
#Optcard_type: the type of data to display options for. This can be either "assays" 
#or "metadata", and is used to show the relevant options based on the type.
options_ui <- function(id,
                       optcard_type=c("assays","metadata"),
                       category_name=id){
  #NS(id): namespace function, defined here and called for every input ID. One
  #namespace is used for each instance of the options module and is defined by
  #the id; use of namespaces allows for the same input id to be used in 
  #different modules without namespace collisions.
  ns <- NS(id)
  
  #1. Calculations for options UI
  #Metadata-specific calculations used to define UI for each metadata type
  if(optcard_type=="metadata"){
    #Get unique values of metadata field for display of summary statistics
    values <- unique(sobj@meta.data[[category_name]])
    #Create list of sorted values for display
    values_sorted <- str_sort(values,numeric=TRUE)
    #Determine type of metadata
    metadata_type <- metadata_type(sobj, category_name)
    
    #Metadata description
    #Display number of unique values if categorical; display range if numeric
    if (metadata_type=="Categorical"){
      n_unique <- length(values)
      metadata_description <- glue("{n_unique} unique values")
    } else if (metadata_type=="Numeric"){
      metadata_description <- ""
      #metadata_description <- glue("{range: {min(values)} to {max(values)}, avg {mean(values)}}")
    } else {
      #Potential unforseen metadata types: leave the description blank
      metadata_description <- ""
    }
  }
  
  #2. Create UI for options card
  #Assays UI
  if(optcard_type=="assays"){
    ui <- div(
      id=ns("optcard"),
      class="optcard single-space-bottom",
      tags$strong(glue("Options for {category_name}"),
                  class="large half-space-bottom center"),
      
      #Human-readable suffix: appears on plots and search entries
      textInput(inputId = ns("hr"),
                label="Set label for assay (will appear as entered in app)",
                width = "380px"),
      #Include assay name on plots: if checked, the label entered will be 
      #displayed on plots and in the feature search results.
      #I may put this in the main app instead; it makes more sense to toggle it when making the plots.
      checkboxInput(inputId = ns("include_label"),
                    label = "Include assay name on plots?"),
      tags$p("(This is usually not required for the default assay in your data)")
    )
    
    #Metadata UI
  } else if (optcard_type=="metadata"){
    ui <- div(
      id=ns("optcard"),
      class="optcard single-space-bottom",
      tags$strong(glue("Options for {category_name}"),
                  class="large center"),
      
      #Print the type of metadata beneath the title, and a brief description
      tags$p(glue("({metadata_type}, {metadata_description})"), 
             class="center small half-space-bottom"),
      
      #If the metadata is categorical and there are 15 values or less, print the values to screen
      if (metadata_type=="Categorical" & length(values)<=15){
        tags$p(glue("Values: {paste(values_sorted, collapse=', ')}"))
        } else NULL,
      
      #Human-readable suffix: appears on plots and search entries
      textInput(inputId = ns("hr"),
                label="Set label for metadata column (will appear as entered in app interface)",
                width = "380px"),
      
      #Option to classify metadata into list (ex. group patients by sample conditions)
      #Only available for categorical metadata columns
      if(metadata_type=="Categorical"){
        tagList(
          materialSwitch(inputId =  ns("group_metadata"),
                         label = "Group metadata into categories?", 
                         value = FALSE,
                         right = TRUE,
                         status = "default"),
          tags$p("(Choices for possible values in the metadata column will appear in the app)",
                 class="center small")
        )
      } else NULL,
      
      #Dynamic UI for defining metadata groups
      uiOutput(outputId = ns("groups_list"))
    )
  }
  
  #3. Add "hidden" shinyjs class to the card to hide each card initially
  ui <- shinyjs::hidden(ui)
  
  return(ui)
}

###Options module server
#Renders option cards for the variables of assays, metadata, etc. selected by 
#the user, and processes user selections to create a config file for the main app.
#Applies to multiple types of selections (assays, metadata, etc.). One instance 
#of the module is created for every available selection across each tab.

#Arguments
#id: the id passed to the module
#categories_selected: a reactive variable describing the variables (assays, metadata, etc.
#, selected by the user)
#options_type: the type of options to create a server function for. Can be one 
#of "assays" or "metadata"
#category_name: the name of the individual category that the instance of the
#module applies to. This is the id by default, and can be changed.
options_server <- function(id, 
                           sobj,
                           categories_selected, 
                           options_type=c("assays","metadata"),
                           category_name=id){
  #Initialize module
  moduleServer(
    id,
    function(input, 
             output,
             session){
      #Define namespace function for UI elements or references to elements that 
      #are not Shiny inputs or outputs 
      ns <- NS(id)
      
      #Initialize output variables
      #group_choices applies only to metadata variables that are categorical but 
      #is called for export from this module in all cases: therefore, it must be 
      #initialized as NULL to avoid issues when returning for modules not meeting 
      #these conditions.
      group_choices <- NULL
      
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
                       showElement("optcard")
                       #Otherwise, hide the card
                     } else {
                       hideElement("optcard")
                     }
                   })
      
      #2. Metadata-tab specific functions
      #Determine if metadata field is categorical or numeric
      if(options_type=="metadata"){
        type <- metadata_type(sobj,id)
      }
      
      #2.1. Metadata Groups: User Interface to define subgroups within a metadata column
      #Only perfomed for categorical inputs
      if(options_type=="metadata"){
        #Test for type of metadata field after testing to see if the object 
        #is a metadata entry
        if(type=="Categorical"){
          #Create a list for storing reactive outputs of group fields modules
          group_choices <- list()
          #Use makeReactiveBinding to get the list to update when group fields 
          #inputs are changed
          makeReactiveBinding("group_choices")
          
          #2.1.1. Define UI for group selection
          groups_UI <- eventReactive(input$group_metadata,
                                     label = "Groups UI",
                                     ignoreNULL = FALSE,
                                     {
                                       #Display the interface when the corresponding 
                                       #switch is activated
                                       if (input$group_metadata==TRUE){
                                         #Interface: one instance of the fields 
                                         #UI, with a button to add more fields 
                                         #to the interface
                                         tagList(
                                           #Namespace id for module is based on a 
                                           #numeric id since there are multiple fields. 
                                           metadata_group_fields_ui(ns("groups-1"),
                                                                    temp_choices = unique(sobj@meta.data[[category_name]])
                                           ),
                                           
                                           div(
                                             actionButton(inputId =ns("add_group"),
                                                          label = "Add Group",
                                                          width = "100px")
                                           )
                                         )#End tagList of input containers
                                       } 
                                     })
          
          #2.1.2. Create a reactive values object with a vector of the unique values 
          #in the metadata column that can be sorted into groups
          #This will be used to populate the choices now and will be updated later
          
          #For now, this is not reactive. It will eventually be reactive and 
          #depend on inputs in other metadata fields modules.
          category_values <- unique(sobj@meta.data[[category_name]]) |> 
            str_sort(numeric=TRUE)
          
          #2.1.3. After creating the UI for the first group selection field, 
          #create the corresponding server module
          module_output <- metadata_group_fields_server("groups-1",
                                         possible_selections = category_values)
          
          #Use observe() to reactively update group_choices with the module output
          observe(
            label=glue("observer for {category_name}-groups-1"),
            {
            #The superassignment operator <<- is used to update group_choices
            #outside of the scope of the observe() function
            group_choices[["groups-1"]] <<- module_output()
          })
          
          #2.1.4. Add additional fields if the "Add Group" button is clicked
          observeEvent(input$add_group,
                       label = "{category_name}: add Field Button",
                       #When the ns(add-group) button is created or when the 
                       #options server module is created it will trigger this 
                       #observer. The ignore* parameters are set to TRUE to make 
                       #the observer respond only to a click of the button.
                       ignoreNULL = TRUE,
                       ignoreInit = TRUE,
                       {
                         #Use the action button's value to create an id
                         #Add 1 to the value since the first field uses "1" 
                         #in its namespace (the first value to be created should 
                         #have a value of 2 in the namespace id)
                         #A different implementation is recommended since 
                         #this could be buggy 
                         nested_id <- glue("groups-{input$add_group + 1}")
                         
                         #Add module UI
                         #For arguments that reference an element by id that is 
                         #not a shiny input or output, namespacing must be used
                         insertUI(selector = glue("#{ns('add_group')}"),
                                  where = "beforeBegin",
                                  #Namespacing should also be used to call the 
                                  #UI components of modules, but not 
                                  #the server component
                                  ui = metadata_group_fields_ui(ns(nested_id),
                                                                remove_button = TRUE,
                                                                temp_choices = str_sort(
                                                                  unique(
                                                                    sobj@meta.data[[category_name]]
                                                                    ),
                                                                  numeric=TRUE
                                                                  )
                                  )
                         )
                         
                         #Add module server instance
                         module_output <- metadata_group_fields_server(nested_id,
                                                                       possible_selections = category_values)
                         #Store output in group_choices and use observe() to 
                         #reactively update group_choices when the output changes
                         observe(label=glue("Observer for {category_name}-{nested_id}"),
                                 {
                                   group_choices[[nested_id]] <<- module_output()
                                 })
                       })
          
          #2.1.6. Render UI components
          output$groups_list <- renderUI({groups_UI()})
          
        }
      }
      
      #3. Returns from Module: 
      if(options_type=="metadata"){
        #Return options depend on the type of metadata (Categorical metadata has 
        #a reactive list of metadata group choices; numeric and other types have 
        #a non-reactive value of NULL for group_choices)
        if (type=="Categorical"){
          #Return metadata-specific variables as a list
          #Returns 
          #1. The name of the category (machine-readable and used for the values 
          #of choices in the app)
          #2. The user-specified label (human-readable and used for the keys of choices)
          #3. The metadata groups selected, if specified by the user
          return_list_metadata <- reactive({
            list(`meta_colname`= category_name,
                 `label`= input$hr,
                 #Uses the process_group_choices function to remove values 
                 #representing deleted modules
                 `groups`= process_group_choices(group_choices)
            )
          })
          
          #Numeric metadata and other types: group_choices is NULL
        } else {
          return_list_metadata <- reactive({
            list(`meta_colname`= category_name,
                 `label`= input$hr,
                 `groups`= NULL
            )
          })
        }
        
        return(return_list_metadata)
        
      } else if (options_type=="assays"){
        #For assays, return
        #1. Assay: the name of the assay as defined in the Seurat object
        #2. Key: the prefix to be added to features server-side to search for 
        #them from the assay
        #3. Suffix_human: a suffix that is added to the feature in parentheses 
        #and displayed in the app in dropdown menus and in the titles of plots
        #4. Dropdown_title: a user-defined label for the assay that will be 
        #added to all dropdown menus in the app that display features from multiple assays 
        return_list_assays <- reactive({
          list(`assay`=category_name,
               `key`=Key(sobj[[category_name]]),
               `suffix_human`=if(input$include_label==TRUE) input$hr else "",
               `dropdown_title`=input$hr)
        })
        
        return(return_list_assays)
      }
    })
}

##B.2 Metadata Groups Module (second level)####
###Module UI
metadata_group_fields_ui <- function(id,remove_button=FALSE,temp_choices=NULL){
  #Namespace function
  ns <- NS(id)
  
  ui <- span(
    #inline-containers class is used to change the display style of all 
    #containers within the element
    class="inline-containers input-no-margin align-containers-top",
    #id: used to delete the line if the remove button is clicked
    #Passing NULL to ns() will make the id equal to the namespace id 
    #passed to the module 
    id=glue("{ns(NULL)}"),
    textInput(inputId = ns("group_name"),
              label = NULL,
              width = "120px",
              placeholder = "Group Name"
    ),
    selectizeInput(inputId = ns("group_members"),
                   label=NULL,
                   #Choices are dynamic and must be updated by the module server
                   width = "260px",
                   choices = temp_choices,
                   selected = NULL,
                   multiple= TRUE,
                   options = list(
                     placeholder="Values in Group",
                     size=10)
    ),
    if(remove_button==TRUE){
      actionButton(inputId = ns("remove_module"),
                   label="",
                   icon = icon("times"),
                   class = "x-button" 
      )
    } else NULL
  )
  
  return(ui)
}

###Module Server
#id: id given to this module for namespacing
#possible selections: a reactive vector of unique values within the 
#metadata category that can be searched in the selectize inputs in this server
metadata_group_fields_server <- function(id,
                                         possible_selections){
  #Initialize module
  moduleServer(
    id,
    function(input, 
             output,
             session){
      #Update the selectize input for this field with valid entries
      #For now, this occurs only once when the module is created
      #With the latest updates to the code structure, updateSelectizeInput is 
      #running each time an entry is made in the metadata group, which erases input
      #updateSelectizeInput(session,
      #                     #Assuming namespacing is not required
      #                     inputId = "group_members",
      #                     choices = str_sort(possible_selections,numeric=TRUE))
      
      #Deleted: returned to parent modules to notifiy if output has been deleted
      #when the remove button is clicked, this is set to TRUE 
      deleted <- FALSE
      #Ensures value is reactively updated when the remove button is clicked
      makeReactiveBinding("deleted")
      
      #Code to remove the UI and server instances when the remove button is clicked
      observeEvent(input$remove_module,
                   ignoreNULL = TRUE,
                   #The once argument will remove the observer when the code 
                   #below is ran (the observer should be deleted to optimize 
                   #performance since the button it connects to will no longer
                   #exist)
                   once = TRUE,
                   #IgnoreInit is set to True to keep the server code from 
                   #running when the observer is created
                   ignoreInit = TRUE,
                   {
                     print(glue("{session$ns(NULL)}: delete button"))
                     #id of the target should be equal to the "full" namespaced 
                     #id of this module (includes all levels of nested modules, 
                     #and retrieved with session$ns())
                     removeUI(selector = glue("#{session$ns(NULL)}"))
                     #Remove shiny input bindings linked to this module 
                     #to optimize performance
                     remove_shiny_inputs(id,input)
                     #Notify parent modules that the field has been deleted 
                     print("set deleted to TRUE")
                     deleted <<- TRUE
                   })
      
      #Return input to the options module as a reactive list
      return(reactive(label = glue("Return from {session$ns(NULL)}"),{
        list(`group_name`=input$group_name,
             `group_members`=input$group_members,
             `deleted`=deleted)
      }))
    })
}

# Section C: Main UI and Server Functions ####
##C.1 Tabs in Main UI ####
### Assays Tab 
#(not the assay options module)
assay_tab <- function(){
  sidebarLayout(
    applet_sidebar_panel(
      #input-no-margin class: removes margin of input containers within div
      div(class="input-no-margin",
          multiInput(inputId = "assays_selected",
                     label = "Choose assays to include:",
                     width = "100%",
                     choices = names(sobj@assays),
                     options = list(enable_search = FALSE,
                                    non_selected_header = "Available Assays",
                                    selected_header = "Selected Assays",
                                    "hide_empty_groups" = TRUE)
          )
      )#multiInput
    ),
    applet_main_panel(
      #Create an instance of the assay options UI for all possible assays. Each 
      #UI creates a "card"; all are hidden at first and are shown when their 
      #corresponding assay is selected by the user. The "id" argument in lapply 
      #is the name of the assay.
      tagList(
        lapply(names(sobj@assays),
               function(assay) options_ui(id=assay,
                                          optcard_type = "assays")
               ),
        #TEMP: add an additional card displaying the outputs from all tabs
        div(class="optcard",
            verbatimTextOutput(outputId = "assay_options")
        ) #End TEMP
      )
    )
  )
}

###Metadata Tab
metadata_tab <- function(){
  sidebarLayout(
    applet_sidebar_panel(
      div(class="input-no-margin",
          multiInput(inputId = "metadata_selected",
                     label = "Choose metadata to include:",
                     width = "100%",
                     choices = names(sobj@meta.data),
                     options = list(enable_search = FALSE,
                                    non_selected_header = "Available Metadata",
                                    selected_header = "Selected Metadata",
                                    "hide_empty_groups" = TRUE)
          )
      )
    ),
    
    applet_main_panel(
      #Create a metadata options "card" for each metadata column in the object
      #All cards are hidden at first and are displayed when the user selects 
      #the corresponding column. The "id" argument in lapply is the name of the metadata field.
      tagList(lapply(names(sobj@meta.data), 
                     function(colname) options_ui(id=colname, 
                                                  optcard_type = "metadata")
                     ),
              #TEMP: add an additional card displaying the outputs from all tabs
              div(class="optcard",
                  verbatimTextOutput(outputId = "all_variables")
              )
      )
    )
  )
}

##C.2 Main UI ####
ui <- fluidPage(
  #Waiter UI: spinners
  useWaiter(),
  #Shinyjs: a Shiny JavaScript extension
  useShinyjs(),
  #CSS style: prevents navbar from appearing on top of content 
  #tags$head(tags$style(HTML(""))),
  #Main UI
  navbarPage(title = "Object Configuration",
             windowTitle="Configure Seurat Object",
             position="fixed-top",
             id="navbar",
             #Tabs are displayed below
             tabPanel(title="Assays",
                      assay_tab()),
             tabPanel(title = "Metadata",
                      metadata_tab())
  ),
  #Elements below will be moved to the navbar using JavaScript
  #Help button
  dropdownButton(inputId = "help",
                 status="info",
                 right=TRUE,
                 label = "",
                 size="sm",
                 icon = icon("question"),
                 tagList(
                   tags$p(
                     "Help and Background",
                     style="color: #888888; 
                     margin-bottom: 0px;
                     font-size: 1.17em;"
                     ),
                   
                   #Tutorial Document
                  # tags$a("Tutorial Vignette",
                  #        href="Shiny_Vignette.html",
                  #        class="blue_hover",
                  #        target="_blank", #Opens link in new tab
                  #        rel="noopener noreferrer" 
                  # ),#End tutorial document link
                 
                   #File issue on github
                   tags$a("Report a Bug",
                          href="https://github.com/amc-heme/DataExploreShiny/issues",
                          class="blue_hover",
                          target="_blank", #Opens link in new tab
                          rel="noopener noreferrer")
                  )#End tagList
  ), #End Help Button
  #Button to create/export config file
  downloadButton(outputId = "export_selections",
                 label="Export Configuration",
                 class = "float_right",
                 icon = NULL),
  
  #May be more appropriate to use an action button later for more advanced 
  #behavior (save config file and direct user back to app)
  #actionButton(inputId = "export_selections",
  #             label = "Export Configuration",
  #             class="float_right"),
  
  #Include scripts for each JavaScript file in document
  #This is added last to delay running of scripts until the elements to be 
  #moved by scripts are created
  js_list,
  #Apply CSS files (placed last so elements created by scripts can be stylized)
  css_list
)#End fluidPage

##C.3 Main Server Function ####
server <- function(input, output, session) {
  #Initialize Variables
  #all_options: list used to store full record of user selections across all tabs
  all_options <- list()
  
  #1. Assay Panel
  #1.1. Store selected assays as a reactive variable
  assays_selected <- eventReactive(input$assays_selected,
                                   ignoreNULL=FALSE,
                                   {
                                     input$assays_selected
                                   })
  
  
  #1.1. Create module server instances for each possible assay
  #Observe is used to reactively update outputs when inputs in the module and
  #its sub-modules are changed
  observe({
    #<<- is required for all_assay_options to be accessible to other server
    #code (not sure why)
    all_assay_options <<- list()
    
    #Create an assay options module for each assay in the object 
    for (id in names(sobj@assays)){
      #Must also use <<- here
      all_assay_options[[id]] <<- options_server(id = id,
                                                sobj=sobj,
                                                categories_selected = assays_selected,
                                                options_type = "assays"
      )
    }
  })
  
  #1.2. Filter list of options module outputs and combine into a single 
  #reactive object, which is added to the all_options list. 
  all_options$assays <- reactive({
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
  
  #Temp: print assay options to screen
  output$assay_options <- renderPrint({
    #Lapply: fetches each reactive object on the list, prints the result, and
    #stores all objects in a list
    all_options$assays()
  })
  
  #2. Metadata Panel
  #2.1. Store metadata selected as a reactive variable
  metadata_selected <- eventReactive(input$metadata_selected,
                                     ignoreNULL=FALSE,
                                     {
                                       input$metadata_selected
                                     })
  
  
  #2.2. Create options server module instances for each metadata assay
  #Use observe() for reactive updates of module output
  observe({
    #<<- is required for all_metadata_options to be accessible to other server
    #code (not sure why)
    all_metadata_options <<- list()
    
    for (id in names(sobj@meta.data)){
      all_metadata_options[[id]] <<- options_server(id = id,
                                                    sobj = sobj,
                                                    categories_selected = metadata_selected,
                                                    options_type = "metadata"
      )
    }

  })
  
  #2.3. Filter list of options selected and combine the individual reactive
  #outputs from each module into a single reactive list 
  all_options$metadata <- reactive({
    #Options list is only processed when metadata columns have been selected
    if (!is.null(input$metadata_selected)){
      #Extracts each reactive module output and stores them in a list
      list <- lapply(all_metadata_options, function(x) x())
      #Filter list for metadata columns that have been selected by the user
      return(list[names(list) %in% input$metadata_selected])
    } else {
      #Return NULL if no columns are selected
      return(NULL)
    }
  })
  
  #TEMP: print all metadata options
  output$all_variables <- renderPrint({
    #Lapply: fetches each reactive object on the list, prints the result, and
    #stores all objects in a list
    all_options
  })
  
  #3. Config File Download Handler
  output$export_selections <- downloadHandler(
    filename = "config.rds",
    content=function(file){
      #Extract each reactive output from the options module and store in a list
      all_options_list <- lapply(all_options, function(x) x()) 
      #Download above object as .rds 
      saveRDS(object = all_options_list,
              file = file)
    })
}

shinyApp(ui, server)