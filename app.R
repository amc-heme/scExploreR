### Load Libraries and Data ####
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

#Additional Backend Packages
library(presto)

#Load functions in ./R directory
#Get list of files
source_files <- list.files(path = "./R", 
                           #Pattern, any set of characters, followed by ".R"
                           #Period is double escaped
                           pattern=".*\\.R", 
                           full.names=TRUE, 
                           ignore.case=TRUE)

#Load .R files in modules directory
source_files <- c(source_files, 
                  list.files(path = "./Modules", 
                             #Pattern, any set of characters, followed by ".R"
                             #Period is double escaped
                             pattern=".*\\.R", 
                             full.names=TRUE, 
                             ignore.case=TRUE)
                  )

#Use source() to import files into R
sapply(source_files, source)

#Load CSS files for app: CSS files are defined and each file is converted to a
#<script> tag using includeCSS(). Each tag defined is passed to a list, which is
#included in the main UI function.
#Get list of .css files in www/ directory
css_files <- list.files(path = "./www", 
                          pattern=".*\\.css", 
                          full.names=TRUE, 
                          ignore.case=TRUE)
#Create list of style tags for each CSS file
css_list <- lapply(css_files,includeCSS)

#Load Javasctipt files for app: find all .js files in www/ directory and create
#a list of script() tags using includeScript().
#Get list of .js files in www/ directory
js_files <- list.files(path = "./www", 
                       #Regex: uses \\. to select for files ending in ".js".
                       #Pattern arguments require double backslashes for eacape 
                       #characters to work (R and regex use the same string 
                       #character)
                       pattern=".*\\.js", 
                       full.names=TRUE, 
                       ignore.case=TRUE,
                       include.dirs = FALSE)
#Create list of style tags for each CSS file
js_list <- lapply(js_files,includeScript)

#Load Seurat object (D0/D30 data, modified to include gene signature scores)
#https://storage.googleapis.com/jv_omics_sandbox/longitudinal_samples_20211025.Rds
sobj <- readRDS("./Seurat_Objects/longitudinal_samples_20211025.rds")

# Object Config ####
#Load config file
config <- readRDS("./Seurat_Objects/d0-d30-config.rds")

#For now, this will be hard-coded. Later, these variables will be defined from
#a .config file created from the config applet.
include_numeric_metadata <- TRUE
numeric_metadata_title <- "Metadata Features"

##Define searchable features and Metadata ####
#Assay list: created using functions in Object_Specific_Processing.R
assay_info <- assay_list(
  #Genes: include even though it is the default assay (it may not be in some objects)
  assay_entry(assay="RNA",
              #machine-readable prefix: in some objects, this is capital; 
              #in others, this is lowercase
              prefix_machine = "rna_",
              #no suffix used in the dropdown menu for genes
              suffix_human = "",
              #dropdown_title: the name that appears in the dividers in the 
              #dropdown menu, which groups search results by assay. 
              dropdown_title = "Genes"),
  
  #ADT assay
  assay_entry(assay = "ADT",
              prefix_machine = "adt_",
              suffix_human = " (Surface Protein)",
              dropdown_title = "Surface Protein Markers"),
  
  #Gene signatures assay
  assay_entry(assay = "SIG", 
              prefix_machine = "sig_", 
              #The signatures do not need a suffix as they are distinct 
              #from gene names. The dropdown menu title should be sufficient
              suffix_human = "",
              dropdown_title = "Gene Signature Scores")
)

#Create a list of valid features using the assays defined above
valid_features <- feature_list_all(sobj,
                                   assay_list = assay_info,
                                   #include_numeric_metadata: a boolean variable 
                                   #that is hard-coded for now and will be 
                                   #defined in the config file
                                   numeric_metadata = include_numeric_metadata, 
                                   #The same is true for numeric_metadata_title
                                   numeric_metadata_title = numeric_metadata_title)

##Define searchable features and Metadata ####
#Gene_expression features
#genes <- rownames(sobj)

### Define function to build feature lists from arbitrary assays
feature_list <- function(assay, prefix_machine, suffix_human) {
  #Fetch features in Seurat object by assay
  features <- rownames(sobj[[assay]])
  #Human-readable feature names
  human_readable <- paste0(features, suffix_human)
  #Machine-readable feature names (format "<prefix>_<feature_name>")
  machine_readable <- paste0(prefix_machine, features)
  #Zip above into a list of key-value pairs (human-readable features as keys, machine-readable features as values)
  split(machine_readable, human_readable)
}

#For future generalization of app: fetch names of all assays to pass to functions
#for generating feature lists
#assays <- names(sobj@assays)

###ADT features
#adt_list <- feature_list("ADT", "adt_", " (Surface Protein)")

###Gene Signatures
#sig_list <- feature_list("SIG", "sig_", " (Gene Signature)")

#Metadata columns (only numeric columns can be plotted)
#meta_cols <- names(sobj@meta.data)

#Select columns that have numeric or integer values
#numeric_cols <- meta_cols[sapply(meta_cols, FUN=function(x){
#  (class(sobj@meta.data[[x]])=="numeric") || (class(sobj@meta.data[[x]])=="integer")
#  })]

#Combine into list
#valid_features <- list(`Genes`=as.list(genes),
#                       `Surface Protein Markers`=adt_list,
#                       `Gene Signature Scores`=sig_list,
#                       `Metadata Features`=as.list(numeric_cols))

#Metadata variables to group and split by in drop down menus


#Specify metadata variables to group and split by in drop down menus
#meta_choices: a named vector with name-value pairs for the display name of the 
#metadata category and the key used to access the category in the Seurat Object

#Construct from config file
#Base vector: contains the "none" option
meta_choices <- c("None"="none")
#Iteratively populate vector using entries in the metadata section of the config file 
for (category in names(config$metadata)){
  #Use setNames from the stats package to add a new name-value pair to the vector
  meta_choices <- setNames(
    #Add `meta_colname` to vector
    object = c(meta_choices, 
               config$metadata[[category]]$meta_colname),
    #Add `label` to the vector as a name
    nm = c(names(meta_choices),
           config$metadata[[category]]$label)
  )
}

#Choices are specific to the D0/D30 object
#meta_choices <- c("None"="none",
#                  "Clusters"="clusters",
#                  "Response"="response",
#                  "Response (Additional Detail)"="best_response",
#                  "Timepoint (Approximate)"="treatment",
#                  "Patient ID"="htb")

##Plots tab ####
#Store UMAP Dimensions of full object
#This is used to allow plotting of subsets with original axes scales
#Plot a UMAP of the full data, store it to memory, and record the
#x and y limits of the plot
umap_orig <- DimPlot(sobj, 
                      group.by = "clusters")
#Record limits
xlim_orig <- layer_scales(umap_orig)$x$range$range
ylim_orig <- layer_scales(umap_orig)$y$range$range

#Store number of cells: used to determine if it is a subset
#TODO: does this apply to non-CITEseq datasets?
n_cells_original <- ncol(sobj)

##Define Valid Metadata Selections####
#The unique values for each metadata category listed in the config file will be 
#stored as vectors in a list 
unique_metadata <- list()
#Store unique values for each metadata category entered
for (category in names(config$metadata)){
  #Use sobj@meta.data[[category]] instead of sobj[[category]] to return a vector
  unique_metadata[[category]] <- unique(sobj@meta.data[[category]])
  #If the metadata category is a factor, convert to a vector with levels() to 
  #avoid integers appearing in place of the unique values themselves
  if(class(unique_metadata[[category]])=="factor"){
    unique_metadata[[category]] <- levels(unique_metadata[[category]])
  }
}

#Clusters dropdown
clusters <- levels(unique(sobj$clusters)) 

#Response dropdown
responses <- unique(sobj$response)

#Treatment dropdown (for d0/d30 object. Shows as 'approximate timepoint')
treatments <- unique(sobj$treatment)

#Patients dropdown
#Create vector and list of patients
#Vector is processed by server; list of patients sorted by dataset type (normal 
#bone marrow, d0/d30, dx/Rl) is displayed to user
#Vector of all patients
patients <- unique(sobj$htb)
#Use function from R/d0-d30_patient_list.R to build list of patients 
patients_categories <- build_patient_list(patients)
#Apply sorting function to patients_categories so this list appears in order initially
patients_categories <- sort_patient_list(patients_categories)

#Compile the valid selections above into a valid choices (vc) list, 
#so choices can be more easily passed to functions
choices <- list("clusters"=clusters,
           "responses"=responses, 
           "treatments"=treatments,
           "patients"=patients,
           "patients_categories"=patients_categories)

#Non-zero proportion threshold: if the proportion of cells for a gene is below 
#this threshold, return a warning to the user.
nonzero_threshold <- 0.10

#Error Handling: define possible errors ####
#Errors are defined in a list using the functions in "./R/error_handling.R". 
#The error_handler() function is executed in a tryCatch() statement and checks
#the error message returned against a list of errors.
## List of errors for subset operations ####
error_list <- list(
  add_error_notification(
    message="cannot allocate vector of size",
    notification_ui=icon_notification_ui_2(
      icon_name = "skull-crossbones",
      tagList(
        "Memory Error: RAM is insufficient for analyzing the specified subset. 
        Please narrow down the subset scope using the restriction criteria to 
        the left, and feel free to", 
        github_link(display_text = "let us know"),
        " ",#Space after link
        "if you repeatedly recieve this error.")#End tagList
      ),#End icon_notification_ui
    notification_id = "mem_error"
    ),#End add_error_notification
  
  #Error 2: Vector memory exhausted
  add_error_notification(
    message="vector memory exhausted",
    notification_ui=icon_notification_ui_2(
      icon_name = "skull-crossbones",
      "Error: vector memory exhausted. If this issue persists, please ",
      github_link("contact us"),
      " with a screenshot of the response criteria selected. For now, narrowing 
      down the subset criteria may resolve the error."
      ),#End icon_notification_ui
    notification_id = "vector_mem_error"
),

#Error 3: No Cells in Subset
add_error_notification(
  message = "No cells found",
  icon_notification_ui_2(
    icon_name = "skull-crossbones",
    "No cells were found matching the defined subset criteria. Please check the 
    subset dropdowns for mutually exclusive selections. If you recieve this error 
    for combinations that should be valid, please",
    github_link("let us know"),
    #Period at end of link
    "."
    ),#End icon_notification_ui
  notification_id = "no_cells_found"
  )#End add_error_notification
)#End list of error definitions (subset_errors)

# Table of Contents #####
# 1. User Interface Functions
#   1.1. Plots
#     1.1.1. Sidebar Panel
#       1.1.1.1. Desired Plots
#       1.1.1.2. UMAP Options
#       1.1.1.3. Feature Plot Options
#       1.1.1.4. Violin Plot Options    
#       1.1.1.5. Dot Plot Options
#       1.1.1.6. Feature Text Entry
#       1.1.1.7. Checkboxes for dot plot features (will be deleted soon)
#     1.1.2. Main Panel
#       1.1.2.1. UMAP Plot
#       1.1.2.2. Feature Plot
#       1.1.2.3. Violin Plot
#       1.1.2.4. Dot Plot
#   1.2. Differential Expression Tables
#     1.2.1. Sidebar Panel
#       1.2.1.1. Metadata Column for Tables
#       1.2.1.2. Ident.1 Choice
#       1.2.1.3. Assay Choice
#     1.2.2. Main Panel (table output)
# 2. Server Function

### 1. User Interface Functions ###
#The user interface is a series of pages; each page is stored in a function for improved readability of code.

### 1.1. Plots Tab ###
plots_tab <- function(unique_metadata,config){
  fluidPage(
    #Sidebar layout: consists of a side panel and a main panel
    sidebarLayout(
      ### 1.1.1. Sidebar panel for user input ####
      sidebarPanel(fluid=FALSE,
                   #### 1.1.1.1 Checkboxes for choosing desired plot ####
                   # Two-column checkboxes: put inside inline block elements 
                   #that span half of the sidebar panel
                   div(class="two_column",
                        style="float: left;",
                        #Specify if UMAP Plot is desired
                        materialSwitch(inputId = "make_umap",
                                       label = "UMAP plot", 
                                       value = TRUE,
                                       right = TRUE,
                                       status = "default"),
                        
                        #Specify if feature plot is desired
                        materialSwitch(inputId = "make_feature",
                                       label = "Feature Plot", 
                                       value = FALSE,
                                       right = TRUE,
                                       status = "default")
                        ),#End div
                    
                    div(class="two_column",
                        #Specify if violin plot is desired
                        materialSwitch(inputId = "make_vln",
                                       label = "Violin Plot", 
                                       value = FALSE,
                                       right = TRUE,
                                       status = "default"),
                        #Specify if dot plot is desired
                        materialSwitch(inputId = "make_dot",
                                       label = "Dot Plot", 
                                       value = FALSE,
                                       right = TRUE,
                                       status = "default")
                        ),#End div
                   
                   #### 1.1.1.2. Feature Text Entry. ####  
                   #Applies to feature, violin, and dot plots unless the user 
                   #specifies the use of different features for each plot 
                   #(this is currently only possible for dot plots) 
                   conditionalPanel(condition="input.make_feature==true | input.make_vln==true | input.make_dot==true",
                                    #Label
                                    tags$p(tags$strong("Enter features to display on plots:")),
                                    #Inline text entry and update button
                                    div(style="vertical-align: top; margin-bottom: 0px;",
                                        selectizeInput(inputId = "text_features",
                                                       multiple = TRUE, 
                                                       label = NULL,
                                                       choices = NULL,
                                                       selected = NULL,
                                                       #Add remove button to inputs
                                                       options = list(
                                                         'plugins' = list('remove_button'),
                                                         'create'=FALSE)) #Do not allow user to 
                                        #input features not in the list of options
                                        )
                                    ),#End 1.1.1.2.
                   
                   #### 1.1.1.3. Subsets for Plots ####
                   collapsible_panel(inputId="plots_subset_collapsible",
                                     label="Subset Options",
                                     active=FALSE,
                                     div(id="plots_subset_panel",
                                         div(id="plots_subset_stats",
                                             tags$strong("Metadata in Displayed Subset",
                                                         id="plots_subset_header"),
                                             div(tags$strong("Clusters: "),
                                                 textOutput(outputId = "plots_selected_clusters", inline = TRUE)),
                                             div(tags$strong("Response criteria: "),
                                                 textOutput(outputId = "plots_selected_response", inline = TRUE)),
                                             div(tags$strong("Timepoints: "),
                                                 textOutput(outputId = "plots_selected_treatment", inline = TRUE)),
                                             div(tags$strong("Patients: "),
                                                 textOutput(outputId = "plots_selected_htb", inline = TRUE))
                                         ),
                                         #Generate subset menus using the 
                                         #config file and unique_metadata
                                         
                                         #subset_selections module
                                         subset_selections_ui("plots_subset",
                                                              unique_metadata,
                                                              metadata_config=config$metadata),
                                         actionButton(inputId = "plots_subset_submit",
                                                      label="Apply Subset")
                                         )
                          ),#End 1.1.1.3
                   
                   #TEMP: text output for subset selections
                   verbatimTextOutput(outputId = "plots_subsets_return",
                                      placeholder = TRUE),
                   
                   ### Plot Specific Options ###
                   #### 1.1.1.4. Options specific to UMAP ####
                   #TEMP: test plot_selections module UI
                   conditionalPanel(
                     condition = "input.make_umap==true",
                     collapsible_panel(
                       inputId="plots_umap_collapsible",
                       label="UMAP Specific Options",
                       active=TRUE,
                       plot_selections_ui(
                         id = "umap",
                         meta_choices = meta_choices,
                         plot_label = "UMAP",
                         group_by =          TRUE,
                         split_by =          TRUE,
                         ncol_slider =       TRUE,
                         label_checkbox =    TRUE,
                         legend_checkbox =   TRUE,
                         limits_checkbox =   TRUE,
                         manual_dimensions = TRUE,
                         download_button =   TRUE
                         )
                       )
                     ),
                   
                   #Panel will display if UMAP is checked
                   conditionalPanel(condition = "input.make_umap==true",
                                    collapsible_panel(inputId="plots_umap_collapsible",
                                                      label="UMAP Specific Options",
                                                      active=TRUE,
                                                      #Choose metadata to group UMAP by
                                                      selectInput(inputId = "umap_group_by", 
                                                                  label = "Metadata to Group by:",
                                                                  #Remove "none" from selectable options to group by
                                                                  choices= meta_choices[!meta_choices %in% "none"], 
                                                                  selected = "clusters"),
                                                      #Choose metadata to split UMAP by
                                                      selectInput(inputId = "umap_split_by", 
                                                                  label = "Metadata to Split By:",
                                                                  choices= meta_choices,  
                                                                  selected = "none"),
                                                      #If split by is specified, control number 
                                                      #of columns with a slider
                                                      uiOutput(outputId = "umap_ncol_slider"),
                                                      #Checkbox: add or remove labels 
                                                      #(labels on by default)
                                                      checkboxInput(inputId ="umap_label",
                                                                    label = "Label Groups",
                                                                    value = TRUE),
                                                      #Checkbox to add or remove Legend
                                                      checkboxInput(inputId = "umap_legend",
                                                                    label = "Include Legend",
                                                                    value = TRUE),
                                                      #If plotting a subset: checkbox to use 
                                                      #original dimensions 
                                                      uiOutput(outputId="umap_limits_checkbox"),
                                                      #UI for user control of plot dimensions, if desired
                                                      manual_dim_UI(plot_type = "umap"),
                                                      #Download button (plot specific)
                                                      downloadButton(outputId = "umap_download", label="Download UMAP")
                                                      )#End collapsible panel
                                    ),#End 1.1.1.4.
                   
                   #### 1.1.1.5. Options specific to feature plot ####
                   conditionalPanel(condition = "input.make_feature==true",
                                    collapsible_panel(inputId="plots_feature_collapsible",
                                                      label = "Feature Plot Specific Options",
                                                      active = FALSE,
                                                      #Feature plots do not have a group.by argument
                                                      #Choose metadata to split feature plot by
                                                      selectInput(inputId = "feature_split_by", 
                                                                  label = "Metadata to split by:",
                                                                  choices=meta_choices, 
                                                                  selected = "none"),
                                                      #Checkbox to add or remove Legend
                                                      checkboxInput(inputId="feature_legend",
                                                                    label="Include Legend",
                                                                    value=TRUE),
                                                      #If plotting a subset: checkbox to use 
                                                      #original dimensions 
                                                      uiOutput(outputId="feature_limits_checkbox"),
                                                      #UI for user control of plot dimensions, if desired
                                                      manual_dim_UI(plot_type = "feature"),
                                                      #Download button (plot specific)
                                                      downloadButton(outputId = "feature_download",
                                                                     label="Download Feature Plot")
                                                      )#End collapsible_panel
                                    ),#End 1.1.1.5
                   
                   #### 1.1.1.6. Options specific to violin plot ####
                   conditionalPanel(condition = "input.make_vln==true",
                                    collapsible_panel(inputId="plots_vln_collapsible",
                                                      label = "Violin Plot Specific Options",
                                                      active=FALSE,
                                                      #Choose metadata to group violin plot by
                                                      selectInput(inputId = "vln_group_by", 
                                                                  label = "Metadata to group by:",
                                                                  #Remove "none" from selectable options to group.by
                                                                  choices=meta_choices[meta_choices %in% "none" == FALSE], 
                                                                  selected = "clusters"),
                                                      
                                                      #Choose metadata to split violin plot by
                                                      selectInput(inputId = "vln_split_by", 
                                                                  label = "Metadata to split by:", 
                                                                  choices=meta_choices,
                                                                  selected = "none"),
                                                      
                                                      #Slider to control number of columns 
                                                      #if multiple features are entered
                                                      uiOutput(outputId = "vln_ncol_slider"),
                                                      #Checkbox to add or remove Legend
                                                      checkboxInput(inputId="vln_legend",
                                                                    label="Include Legend",
                                                                    value=TRUE),
                                                      #UI for user control of plot dimensions, if desired
                                                      manual_dim_UI(plot_type = "vln"),
                                                      #Download button (plot specific)
                                                      downloadButton(outputId = "vln_download",label="Download Violin Plot")
                                                      )#End collapsible panel
                                    ), #End 1.1.1.6.
                   
                   #### 1.1.1.7. Options specific to dot plot ####
                   conditionalPanel(condition = "input.make_dot==true",
                                    collapsible_panel(inputId="plots_dot_collapsible",
                                                      label="Dot Plot Specific Options",
                                                      active=FALSE,
                                                      #Choose metadata to group dot plot by
                                                      selectInput(inputId = "dot_group_by",
                                                                  label = "Metadata to group by:",
                                                                  #Remove "none" from selectable 
                                                                  #options to group by
                                                                  choices=meta_choices[!meta_choices %in% "none"], 
                                                                  selected = "clusters"),
                                                      
                                                      #Choosing different features
                                                      checkboxInput(inputId = "diff_features_dot",
                                                                    label="Use separate features for dot plot", 
                                                                    value=FALSE),
                                                      
                                                      #If the checkbox above is selected, 
                                                      #display a selectize input for feature selection
                                                      conditionalPanel(condition="input.diff_features_dot==true",
                                                                       #Label
                                                                       tags$p(tags$strong("Enter features to display on dot plot:")),
                                                                       #Selectize entry
                                                                       div(style="vertical-align: top; margin-bottom: 0px;",
                                                                           selectizeInput(inputId = "dot_features",
                                                                                          multiple=TRUE,
                                                                                          label=NULL,
                                                                                          choices = NULL,
                                                                                          selected = NULL,
                                                                                          #Add remove button to inputs
                                                                                          options = list(
                                                                                            'plugins' = list('remove_button'),
                                                                                            'create'=FALSE)
                                                                                          )
                                                                           )
                                                                       ),
                                                      #Checkbox to add or remove Legend
                                                      checkboxInput(inputId="dot_legend",
                                                                    label="Include Legend",
                                                                    value=TRUE),
                                                      #UI for user control of plot dimensions, if desired
                                                      manual_dim_UI(plot_type = "dot"),
                                                      #Download button (plot specific)
                                                      downloadButton(outputId = "dot_download",label="Download Dot Plot")
                                                      ) #End collapsible panel
                                    ) #End 1.1.1.7
                   ), #End 1.1.1.
      
      ### 1.1.2. Main panel for displaying plot output ####
      mainPanel(
        #div added to contain Waiter spinner (forces the spinner to cover the full main panel)
        div(id="plots_main_panel", 
            class="spinner-container-main",
            #Panels for plots: display if checkboxes corresponding to each type are checked
            #1.1.2.1. UMAP plot panel
            conditionalPanel(condition = "input.make_umap==true",
                             uiOutput(outputId = "umap_slot")),
            
            #1.1.2.2. Panel for feature plot 
            #Will be a message or a plot, depending on whether features have been entered
            conditionalPanel(condition = "input.make_feature==true",
                             uiOutput(outputId = "feature_slot")),
            
            #1.1.2.3. Panel for violin plot
            #UI displayed will vary based on the entry into the feature text box
            conditionalPanel(condition="input.make_vln==true",
                             uiOutput(outputId = "vln_slot")), 
            
            #1.1.2.4. Dot plot panel
            conditionalPanel(condition = "input.make_dot==true",
                             uiOutput(outputId = "dot_slot"))
            ) #End div
      ) #End 1.1.2
    ) #End sidebarLayout() 
  ) #End fluidPage() 
}#End 1.1.

# Main UI ####
# Navigation panel and references to tabs
ui <- tagList(
  #Add CSS from each .css file in the www/ directory
  #Uses a list of style tags defined at startup
  css_list,
  #Introjs UI: for guided tour
  introjsUI(),
  #Waiter UI: spinners
  useWaiter(),
  #Shinyjs: a Shiny JavaScript extension
  useShinyjs(),
  #CSS style: prevents navbar from appearing on top of content 
  #tags$head(tags$style(HTML("body{
  #                          padding-top: 60px;
  #                          }"))),
  #CSS and JS for collapsible panel
  navbarPage("Shiny scExplorer",
             windowTitle="Shiny scExplorer",
             position="fixed-top",
             tabPanel("Plots",
                      plots_tab(unique_metadata,config)),
             tabPanel("Differential Expression",
                      dge_tab_ui(
                        id = "dge",
                        unique_metadata = unique_metadata,
                        metadata_config = config$metadata
                        )
                      ),
             tabPanel("Gene Correlations",
                      #Use corr_tab_ui module
                      corr_tab_ui(
                        id = "corr",
                        unique_metadata = unique_metadata,
                        metadata_config = config$metadata
                        )
                      )
             ), #End navbarPage()
  #Help button - Creates a Dropdown menu when clicked
  #Button should appear in the upper right hand corner of the navbar menu
  #This will be achieved with the button_wizzard.js script
  #(Help button is wrapped in two introBoxes)
  introBox(data.step=1,
           data.intro='Welcome to the Shiny app.<br><br>Please click the ">" 
           button to continue with the tour, or click "skip" to proceed to the app.',
           data.position = "left",
           #Begin introBox2
           introBox(data.step = 2,
                    data.intro = 'Click this button to view help. For a more 
                    detailed explanation of features, select "detailed walkthrough". 
                    For more information on interpereting the plots in this app, 
                    select "Interpereting scRNA-seq Plots". Click "Guided tour" 
                    to view this tour again.',
                    data.position="left",
                    #Begin Help button
                    dropdownButton(inputId = "help",
                                   status="info",
                                   right=TRUE,
                                   label = "",
                                   size="sm",
                                   icon = icon("question"),
                                   tagList(tags$p("Help and Background",
                                                  style="color: #888888; 
                                                  margin-bottom: 0px;
                                                  font-size: 1.17em;"
                                   ),
                                  
                                   #Guided tour
                                  # actionLink(inputId = "start_intro",
                                  #            class="blue_hover",
                                  #            label = "(introjs help boxes)"),
                                   
                                   #Interpreting scRNA-seq Plots
                                   tags$a("Interpereting scRNA-seq Plots",
                                          href="scRNA_Plots_Explained.html",
                                          class="blue_hover",
                                          #Opens link in new tab
                                          target="_blank", 
                                          #Cybersecurity measure for links that 
                                          #open in new tab: prevents tabnapping
                                          rel="noopener noreferrer" 
                                          ),
                                   
                                   #Tutorial Document
                                   tags$a("Tutorial Vignette",
                                          href="Shiny_Vignette.html",
                                          class="blue_hover",
                                          #Opens link in new tab
                                          target="_blank", 
                                          rel="noopener noreferrer" 
                                   ), #End Detailed Walkthrough link
                                   
                                   #File issue on github
                                   tags$a("Report a Bug",
                                          href="https://github.com/amc-heme/DataExploreShiny/issues",
                                          class="blue_hover",
                                          #Opens link in new tab
                                          target="_blank", 
                                          rel="noopener noreferrer")
                                   )#End tagList
                    )#End Help Button
                    )#End introBox2
           ),#End introBox 1
  #Include list of scripts built from .js files in www/ directory
  js_list
)

# 2. Server function (builds interactive plot to display in UI) #####
server <- function(input,output,session){
  #2.0. Initialize Session
  #2.0.1. Initialize Reactive Values
  rv <- reactiveValues()
  #For correlations tab: report number of cells in a subset, number of cells 
  #where a gene is detected, and the proportion of detected cells
  rv$n_cells <- 0
  rv$n_nonzero <- 0
  rv$prop_nonzero <- 0
  #rv$is_subset: used to tell the plots to plot the subsetted object instead of 
  #the full object. This is FALSE initially and set to TRUE when the user chooses
  #a subset.
  rv$is_subset <- FALSE
  #*_use_original_limits: if TRUE, modify the axes on UMAP and feature plots to
  #match the original UMAPs created from the full dataset
  rv$umap_use_original_limits <- FALSE
  rv$feature_use_original_limits <- FALSE
  #This is true when a subset is selected in the correlations tab
  rv$corr_is_subset <- FALSE
    
  #2.0.2. Render feature choices for text feature selection (plots tab)
  updateSelectizeInput(session,
                       inputId = "text_features", 
                       choices = valid_features, 
                       server = TRUE)
  
  ## 2.1. Plots Tab #####
  #TEMP: plots selections module. Put in plots_tab module when testing complete
  plot_selections_server(id = "umap",
                         subset = plots_subset, #Reactive
                         subset_submit_button = reactive({input$plots_subset_submit}), #Reactive
                         collapsible_panel = reactive({input$plots_umap_collapsible}), #Reactive
                         plot_label = "UMAP", #Non-reactive
                         n_cells_original = n_cells_original #Non-reactive
                         )
  
  ### 2.1.1 Subset for Plots Tab #####
  #2.1.1.1. Module server to process user selections and report to other modules
  plots_subset_selections <- subset_selections_server("plots_subset",
                                                      sobj = sobj,
                                                      unique_metadata = unique_metadata,
                                                      metadata_config = config$metadata)
  
  output$plots_subsets_return <- renderPrint({
    plots_subset_selections()
  })
  
  #2.1.1.2. Update choices in subset selection menu based on user selections
  #Update patients menu based on entries in 'response' or 'treatment' (timepoint)
  observeEvent(c(input$plots_response_selection,input$plots_treatment_selection),
               ignoreNULL = FALSE,
               label = "Plots Update Patients",
               {
                 #Display spinner during computation to keep user from choosing
                 #outdated options
                 #Show a spinner while the valid patient ID's are calculated
                 waiter_show(
                   id = "plots_subset_panel",
                   html = spin_loaders(id = 2, color = "#555588"),
                   color = "#B1B1B188",
                   hide_on_render = FALSE #Gives manual control of showing/hiding spinner
                 )
            
                 #Filter object for treatment and response selections
                 valid_patients <- sobj@meta.data |> 
                   filter(
                     (.data[["response"]] %in% input$plots_response_selection)&
                       (.data[["treatment"]] %in% input$plots_treatment_selection)
                   ) |> 
                   #Select patients metadata column
                   select(.data[["htb"]]) |> 
                   #Return unique values
                   unique() |>
                   #Convert to a character vector
                   unlist()
                 
                 #Form categorized list of valid patients for display in dropdown menu
                 valid_patients_categories <- build_patient_list(valid_patients)
                 #Sort patients categorized list so they appear in order
                 valid_patients_categories <- sort_patient_list(valid_patients_categories)
                 
                 #Update picker input with valid patient IDs
                 updatePickerInput(
                   session,
                   inputId = "plots_htb_selection",
                   label = "Restrict by Patient",
                   choices = valid_patients_categories,
                   selected = valid_patients,
                   options = list(
                     "selected-text-format" = "count > 3",
                     "actions-box" = TRUE
                   )
                 ) #End updatePickerInput
                 
                 #Hide spinner
                 waiter_hide("plots_subset_panel")
               })
  
  # 2.1.1.3. Construct subset after "Apply Subset" button is clicked
  plots_subset <- eventReactive(input$plots_subset_submit,
                                ignoreNULL=FALSE,
                                label = "Plots Subset", 
                                {
                                  print("Executing subset code")
                                  #Display spinner over main window while the 
                                  #subset is being computed
                                  waiter_show(
                                    id = "plots_main_panel",
                                    html = spin_loaders(id = 2, color = "#555588"),
                                    color = "#FFFFFF",
                                    #Gives manual control of showing/hiding spinner
                                    hide_on_render = FALSE 
                                  )
                                  
                                  #Also display a spinner over the text showing
                                  #The metadata in the current subset
                                  waiter_show(
                                    id = "plots_subset_stats",
                                    html = spin_loaders(id = 2, color = "#555588"),
                                    color = "#B1B1B188",
                                    hide_on_render = FALSE #Gives manual control of showing/hiding spinner
                                  )
                                  
                                  plots_s_sub <- tryCatch(error=function(cnd){
                                    #Return errors to user using notifications
                                    #If an error is caught: the function below
                                    #determines the type of error by inspecting 
                                    #message text with grepl (not recommended, 
                                    #but I currently don't know any other way to 
                                    #catch this error type)
                                    error_handler(session,
                                                  cnd_message = cnd$message,
                                                  #Uses a list of 
                                                  #subset-specific errors 
                                                  error_list = error_list,
                                                  #Id prefix for the 
                                                  #notification elements
                                                  id_prefix = "plots")
                                   
                                    #Return "NULL" for subset when an 
                                    #error has occurred
                                    plots_s_sub <- NULL
                                    return(plots_s_sub)
                                 }, #End tryCatch error function
                                 #Begin tryCatch code
                                 {
                                   #Use subsetting function with the output of the 
                                   #subset selections module as `criteria_list`.
                                   plots_s_sub <-
                                     make_subset(
                                       sobj,
                                       criteria_list = plots_subset_selections
                                       )
                                  # subset(
                                  #   sobj,
                                  #   subset =
                                  #     (clusters %in% input$plots_clusters_selection) &
                                  #     (response %in% input$plots_response_selection) &
                                  #     (htb %in% input$plots_htb_selection) &
                                  #     (treatment %in% input$plots_treatment_selection)
                                  # )
                                 }
                                 )#End tryCatch
                                  
                                  #Hide the water
                                  waiter_hide("plots_main_panel")
                                  
                                  #Return subset to the eventReactive variable
                                  plots_s_sub
                                })
  
  #Rendering text for selected subsets
  observeEvent(input$plots_subset_submit, 
               ignoreNULL = FALSE,
               label = "Plots: Render Subset Criteria",
               {
                 #plots_subset() is NULL if errors are found during the subsetting.
                 #Code will only proceed with identifying metadata if no errors 
                 #were encountered
                 if(!is.null(plots_subset())){
                   #Store the current metadata levels stored in the selected subset
                   responses_found <- unique(plots_subset()$response)
                   treatments_found <- unique(plots_subset()$treatment)
                   patients_found <- unique(plots_subset()$htb)
                   clusters_found <- unique(plots_subset()$clusters)
                   
                   #Rendering Selections and Stats for report
                   output$plots_selected_clusters <- renderText({
                     #If all clusters are selected, print "All"
                     if(setequal(clusters_found,clusters)){
                       "All"
                       #Otherwise, print the selected clusters
                     } else { 
                       isolate(vector_to_text(clusters_found))
                     } #End Conditionals
                   }) #End renderText
                   
                   #Selected Response Criteria
                   output$plots_selected_response <- renderText({
                     #Print "All" if all response criteria are selected 
                     if(setequal(responses_found,responses)){
                       "All"
                     }else{
                       #Otherwise, print selected responses
                       isolate(vector_to_text(responses_found))
                     }#End conditionals
                   }) #End renderText
                   
                   #Selected Timepoints
                   output$plots_selected_treatment <- renderText({
                     #Print "All" if all treatment categories (timepoints) are selected
                     if(setequal(treatments_found,treatments)){
                       "All"
                     }else{
                       #Otherwise, print selected treatment categories (timepoints)
                       isolate(vector_to_text(treatments_found))
                     }#End conditionals
                   }) #End renderText
                   
                   #Selected Patients
                   output$plots_selected_htb <- renderText({
                     if(setequal(patients_found,patients)){
                       #Print "all" if all patient IDs are selected
                       "All"
                     }else{
                       #Otherwise, print selected patients
                       isolate(vector_to_text(patients_found))
                     } #End conditionals
                   }) #End renderText
                 }
                 
                 #When finished rendering current metadata, hide the spinner
                 #over the panel
                 waiter_hide("plots_subset_stats")
               })
  
  
  ### 2.1.2. UMAP plot ####
  #### 2.1.2.1. Reactive UMAP plot dimensions ####
  #Width
  #Update text box to match slider when the slider is changed
  observeEvent(input$umap_width,{
    updateSearchInput(session, inputId = "umap_width_text", value=input$umap_width, trigger=TRUE)
  })
  #Update slider based on text entry (search input waits until user presses enter to update)
  observeEvent(input$umap_width_text,{
    updateSliderInput(session, inputId = "umap_width", value=input$umap_width_text)
  })
  #Store plot width from text input if it is changed by user
  umap_width <- eventReactive(c(input$umap_width, input$umap_width_text),{
    input$umap_width
  })
  
  #Height
  #Update text box to match slider when the slider is changed
  observeEvent(input$umap_height,{
    updateSearchInput(session, inputId = "umap_height_text", value=input$umap_height, trigger=TRUE)
  })
  #Update slider based on text entry (search input waits until user presses enter to update)
  observeEvent(input$umap_height_text,{
    updateSliderInput(session, inputId = "umap_height", value=input$umap_height_text)
  })
  #Store plot height from text input if it is changed by user
  umap_height <- eventReactive(c(input$umap_height, input$umap_height_text),{
    input$umap_height
  })
  
  #### 2.1.2.2. ncol slider: appears when a split.by ####
  #Default value depends on the number of values in the metadata object in question.
  #Updates when the split_by argument or the subset is changed
  umap_ncol_slider <- eventReactive(c(input$umap_split_by,input$plots_subset_submit), 
                                    #Do not need to render UI at startup
                                    ignoreInit = TRUE, 
                                    { 
    #Do not render when split.by is "none"
    if (input$umap_split_by=="none"){
      NULL
    } else {
      #Determine number of panels created by split_by choice.
      #Use double-bracket means of accessing the metadata variable (supports 
      #entry of an arbitrary variable). This means of access returns a dataframe. 
      #Slice for the first row (the unique values)
      n_panel <- unique(plots_subset()[[input$umap_split_by]])[,1] |> length()
      
      #Determine initial value for ncol
      #For less than four panels, this is equal to the number of panels. 
      if (n_panel < 4){
        default_col <- n_panel
        #For 4 or more panels, the default value is 2.
      } else {
        default_col <- 2
      }
      
      #Create/update slider input
      sliderInput(inputId = "umap_ncol",
                  label = "Number of Columns: ",
                  min = 1,
                  max = n_panel, #Max value: equal to the number of levels in the given variable
                  step = 1, #Only allow integer values
                  ticks = FALSE,
                  value = default_col)
    } #End else
  })
  
  #### 2.1.2.3. UI to specify original axis limits when a subset is plotted ####
  umap_limits_checkbox <- eventReactive(c(input$plots_subset_submit, input$plots_feature_collapsible),
                                        label = "UMAP Limits UI",
                                        {
                                          #Checkbox will only appear when a subset 
                                          #is selected.The presence of a subset 
                                          #will be tested by observing the number 
                                          #of cells in the subset
                                          if (n_cells_original != ncol(plots_subset())) {
                                            checkboxInput(inputId = "umap_original_limits",
                                                          label = "Use Original Axes Limits",
                                                          value = FALSE)
                                          }else {
                                            #Display nothing when the number of
                                            #cells are equal between the subset
                                            #and the full dataset
                                            NULL
                                          }
                                        })
  
  #### 2.1.2.4. Generate UI for UMAP plot: renders a plotOutput() with either ####
  #automatic or manually specified dimensions based on user specifications
  umap_UI <- reactive({
    if (input$umap_manual_dim==FALSE){
      plotOutput(outputId = "umap_slot_plot")
    } else {
      plotOutput(outputId = "umap_slot_plot",
                 width = umap_width(),
                 height = umap_height())
    }
  })
  
  #### 2.1.3.5. Server Component for Original Axes Checkbox ####
  #Right after a subset is specified, an error appears saying that the condition
  #for computing original axes (input$umap_original_limits) does not exist. This 
  #likely occurs because the input for specifying original axes is created after 
  #the subset is submitted and the new plot drawn, and the conditional relying 
  #on that input is within the plotting function.
  observeEvent(input$umap_original_limits,
               label="Toggle Limits: UMAP Plot",
               {
                 #Set the reactive value based on the state of the input
                 #Reactive value was created on startup so it always has a value
                 if (input$umap_original_limits==TRUE){
                   rv$umap_use_original_limits=TRUE
                 } else{
                   rv$umap_use_original_limits=FALSE
                 }
               })
  
  #### 2.1.2.6. Define UMAP Plot ####
  #Plot content is defined separately in a reactive context, to be rendered later with the UI.
  umap_plot_content <- reactive({
    #validate will keep plot code from running if the subset is NULL 
    #(no cells in subset)
    validate(
      need(plots_subset(),
           #No message displayed (a notification is already displayed)
           message = "")
    )
    #Produce a single UMAP plot if no features to split by are specified
    if (input$umap_split_by=="none"){
      #Use full object if is_subset is FALSE, and use the subset otherwise
      umap_plot <- DimPlot(plots_subset(),
                           group.by = input$umap_group_by,
                           label = input$umap_label, #TRUE if "label groups" is checked, FALSE otherwise
                           reduction = "umap") 
    } else {
      #UMAP with split.by defined and no special subset
      umap_plot <- DimPlot(plots_subset(),
                           group.by = input$umap_group_by,
                           split.by = input$umap_split_by,
                           label = input$umap_label,
                           ncol = input$umap_ncol,
                           reduction = "umap") 
    }
    
    #Modify plot after creation with ggplot layers according to user input
    #'layers' is a list of layers that is applied to the plot
    #List format works more effectively with conditional statements
    layers <- list(
      #Element A 
      #Legend position: "right" if a legend is desired, and "none" if not
      theme(legend.position = if (input$umap_legend==TRUE) "right" else "none"),
      
      #B-C. Axis limits: use limits from full dataset if specified
      #Element B
      #Must first test to see if subset is present
      #Input container does not exist if there is no subset
      if(n_cells_original != ncol(plots_subset())){
        #Add original limits to the list if the 
        #corresponding checkbox is checked
        #The conditional is tied to a reactive value instead of the input to avoid
        #An error that occurs when this function is evaluated before the input is 
        #defined. 
        if (rv$umap_use_original_limits==TRUE) scale_x_continuous(limits=xlim_orig)
      },
      #Element C
      #Check for subset (input container in 
      #child conditional does not exist 
      #before a subset is created)
      if(n_cells_original != ncol(plots_subset())){
        #Add original limits to the list if the 
        #corresponding checkbox is checked
        if(rv$umap_use_original_limits==TRUE) scale_y_continuous(limits=ylim_orig) 
      }
    )
    
    #Modify the plot using the layers defined above
    umap_plot <- umap_plot &
      layers
    
    #Return plot to umap_plot_content()
    umap_plot
  })
  
  #### 2.1.2.7. Render UI Components ####
  output$umap_slot <- renderUI({
    umap_UI()
    })
  
  output$umap_ncol_slider <- renderUI({
    umap_ncol_slider()
    })
  
  output$umap_limits_checkbox <- renderUI({
    umap_limits_checkbox()
    })
  
  #### 2.1.2.8. Render UMAP plot, with manual or automatic dimensions as specified #####
  #ObserveEvent will respond to the check box and the slider/text box pairs 
  #(other variables involved in plot construction will also cause the plot to
  #re-render)
  observeEvent(
    c(input$umap_manual_dim, 
                 input$umap_width, 
                 input$umap_width_text, 
                 input$umap_height,
                 input$umap_height_text),
    {
      if (input$umap_manual_dim==FALSE){
        output$umap_slot_plot <- renderPlot(umap_plot_content())
        } else {
          output$umap_slot_plot <- renderPlot(umap_plot_content(), 
                                              width = umap_width(), 
                                              height = umap_height())
          }
      }
    )
  
  #### 2.1.2.9. Download UMAP Plot ####
  output$umap_download <- downloadHandler(
    filename = "UMAP_plot.png",
    content = function(file){
      if (input$umap_manual_dim==TRUE){
        ggsave(file, 
               plot=umap_plot_content(), 
               device="png",
               width=umap_width(),
               height=umap_height(),
               dpi=72,
               units="px")
      } else {
        ggsave(file, 
               plot=umap_plot_content(), 
               device="png")
      }
    },#End content function
    contentType = "image/png"
  ) #End downloadHandler function
  
  
  #Feature and Violin Plots: choose whether to render a plot or a message based on user inputs
  
  ### 2.1.3. Feature Plot ##### 
  #### 2.1.3.1 Reactive dimensions ####
  ##Sync width inputs
  #Update text box to match slider value when slider is changed
  observeEvent(input$feature_width,{
    updateSearchInput(session, inputId = "feature_width_text", value=input$feature_width, trigger=TRUE)
  })
  
  #Update slider when text box value is changed (search input waits until user presses enter to update)
  observeEvent(input$feature_width_text,{
    updateSliderInput(session, inputId = "feature_width", value=input$feature_width_text)
  })
  
  #Store the plot width value specified by the user 
  feature_width <- eventReactive(c(input$feature_width, input$feature_width_text),{
    #Store the value from the slider (will be the same as the text box value since the syncing operations above run first)
    input$feature_width
  })
  
  ##Sync height inputs
  #Update text box to match slider value when slider is changed
  observeEvent(input$feature_height,{
    updateSearchInput(session, inputId = "feature_height_text", value=input$feature_height, trigger=TRUE)
  })
  
  #Update slider when text box value is changed (search input waits until user presses enter to update)
  observeEvent(input$feature_height_text, {
    updateSliderInput(session, inputId = "feature_height", value=input$feature_height_text)
  })
  
  #Store the plot height value specified by the user 
  feature_height <- eventReactive(c(input$feature_height, input$feature_height_text),{
    input$feature_height
  })
  
  #### 2.1.3.2 UI to specify origional access limits ####
  #Appears only when a subset is plotted (reacts to submit button and clicks on
  #the collapsible panel header for feature plots)
  feature_limits_checkbox <- eventReactive(c(input$plots_subset_submit, input$plots_feature_collapsible),
                                        label = "Feature Limits UI",
                                        ignoreNULL = FALSE,
                                        {
                                          #Checkbox will only appear when a subset 
                                          #is selected.The presence of a subset 
                                          #will be tested by observing the number 
                                          #of cells in the subset
                                          print("Code to build feature limits checkbox")
                                          if (n_cells_original != ncol(plots_subset())) {
                                            checkboxInput(inputId = "feature_original_limits",
                                                          label = "Use Original Axes Limits",
                                                          value = FALSE)
                                          } else {
                                            NULL
                                          }
                                        })
  
  #### 2.1.3.3 Feature UI ####
  feature_slot_UI <- reactive({
    #Condition A: no features have been entered yet
    if (length(input$text_features)==0){
      #If this is the case, generate a message instructing the user to enter features.
      tags$h3("Please enter a feature to view plot.", style="margin-bottom: 10em;")
    }

    #Condition B: Features are selected
    else {
      #Generate a plot. Only the UI for the plot is shown here; content is in next eventReactive call.
      #plotOutput: will have width and height arguments specified if the user indicates manual control of plot dimensions
      if (input$feature_manual_dim==TRUE){
        plotOutput(outputId = "feature_slot_plot",
                   width = feature_width(), 
                   height= feature_height())
      } else {
        plotOutput(outputId = "feature_slot_plot")
      }
    }
  })
  
  #### 2.1.3.4. Server Component for Original Axes Checkbox ####
  #Right after a subset is specified, an error appears saying that the condition
  #for computing original axes does not exist. This is likely due to the fact that
  #the input for specifying original axes is created after the subset is submitted
  #and the new plot drawn, and the conditional relying on that input is within the
  #plotting function.
  observeEvent(input$feature_original_limits,
               label="Toggle Limits: Feature Plot",
               {
                 #Set the reactive value based on the state of the input
                 #Reactive value was created on startup so it always has a value
                 if (input$feature_original_limits==TRUE){
                   rv$feature_use_original_limits=TRUE
                 } else{
                   rv$feature_use_original_limits=FALSE
                 }
               })
  
  #### 2.1.3.5. Generate content for plot (but only if features are entered) ####
  feature_plot_content <- reactive({
    if (length(input$text_features)>0){
      #If no split.by variable is specified, create a feature plot without 
      #the split.by argument
      if (input$feature_split_by=="none"){
        feature_plot <- FeaturePlot(plots_subset(),
                                    features=input$text_features)
        #Clean up title: this changes the feature names on each plot 
        #to a human-readable format
        #Determine number of plots created
        n_patches <- n_patches(feature_plot)
        #Iterate through each plot, correcting the title
        feature_plot <- hr_title(feature_plot,n_patches,assay_info)
      }
      #Otherwise, split by the user-specified variable
      else {
        feature_plot <- FeaturePlot(plots_subset(), 
                                    features=input$text_features,
                                    split.by = input$feature_split_by)
      }
      
      #Add ggplot layers to modify plot
      #Layers: a list of ggplot layers, based on user input (list works well 
      #with conditionals)
      layers <- list(
        #Element A 
        #Legend position: "right" if a 
        #legend is desired, and "none" if not
        theme(legend.position = if (input$feature_legend==TRUE)"right" else "none"),
        
        #B-C. Axis limits: use limits from full dataset if specified
        #Element B
        #Must first test to see if subset is present
        #Input container does not exist if there is no subset
        if(n_cells_original != ncol(plots_subset())){
          #Add original limits to the list if the 
          #corresponding checkbox is checked
          if (rv$feature_use_original_limits==TRUE) scale_x_continuous(limits=xlim_orig)
        },
        #Element C
        #Check for subset (input container in 
        #child conditional does not exist 
        #before a subset is created)
        if(n_cells_original != ncol(plots_subset())){
          #Add original limits to the list if the 
          #corresponding checkbox is checked
          if(rv$feature_use_original_limits==TRUE) scale_y_continuous(limits=ylim_orig) 
        }
      )
      
      #Modify the plot created in eventReactive
      #function using the layers defined above
      feature_plot <- feature_plot &
        layers
      
      #Return plot to feature_plot_content()
      feature_plot
    }
  })
  
  #### 2.1.3.6. Render the UI and plot objects created above ####
  #UI
  output$feature_slot <- renderUI({
    feature_slot_UI()
    })
  
  #Limits checkbox
  output$feature_limits_checkbox <- renderUI({
    feature_limits_checkbox()
  })
  
  #Plot: width of plot will be either automatically determined or manually set to user specifications based on whether user requests manual control of dimensions.
  observeEvent(c(input$feature_manual_dim, input$feature_width, input$feature_width_text, input$feature_height,input$feature_height_text),{
    if (input$feature_manual_dim==TRUE){
      #Manual dimensions for plot, using values from 2.3.1.
      output$feature_slot_plot <- renderPlot({feature_plot_content()}, 
                                             width = feature_width(), 
                                             height= feature_height())
    } else {
      #Use automatic dimensions for renderUI and renderPlot (no width or height specified)
      output$feature_slot_plot <- renderPlot({feature_plot_content()})
    }
  })
  
  #### 2.1.3.7. Feature Plot Download ####
  output$feature_download <- downloadHandler(
    filename = "Feature_plot.png",
    content = function(file){
      if (input$feature_manual_dim==TRUE){
        ggsave(file, 
               plot=feature_plot_content(), 
               device="png",
               width=feature_width(),
               height=feature_height(),
               dpi=72,
               units="px")
      } else {
        ggsave(file, 
               plot=feature_plot_content(), 
               device="png")
      }
    },#End content function
    contentType = "image/png"
  ) #End downloadHandler function
  
  ### 2.1.4. Violin plot ######
  #### 2.1.4.1 Reactive plot dimensions ####
  ##Sync width inputs
  #Update text box to match slider value when slider is changed
  observeEvent(input$vln_width,{
    updateSearchInput(session, inputId = "vln_width_text", value=input$vln_width, trigger=TRUE)
  })
  
  #Update slider when text box value is changed (search input waits until user presses enter to update)
  observeEvent(input$vln_width_text,{
    updateSliderInput(session, inputId = "vln_width", value=input$vln_width_text)
  })
  
  #Store the plot width value specified by the user 
  vln_width <- eventReactive(c(input$vln_width, input$vln_width_text),{
    #Store the value from the slider (will be the same as the text box value since the syncing operations above run first)
    input$vln_width
  })
  
  ##Sync height inputs
  #Update text box to match slider value when slider is changed
  observeEvent(input$vln_height,{
    updateSearchInput(session, inputId = "vln_height_text", value=input$vln_height, trigger=TRUE)
  })
  
  #Update slider when text box value is changed (search input waits until user presses enter to update)
  observeEvent(input$vln_height_text, {
    updateSliderInput(session, inputId = "vln_height", value=input$vln_height_text)
  })
  
  #Store the plot height value specified by the user 
  vln_height <- eventReactive(c(input$vln_height, input$vln_height_text),{
    input$vln_height
  })
  
  #### 2.1.4.2. Slider to control number of columns when multiple features are entered ####
  vln_ncol_slider <- eventReactive(input$text_features, ignoreNULL = FALSE,{
    #Only display slider when there is more than one feature
    if (length(input$text_features) <= 1){
      ui <- NULL
    } else {
      #Default number of columns: equal to the number of panels if there are 
      #less than four, otherwise equal to two
      #Number of panels equals number of features for violin plots
      if (length(input$text_features)<4){
        default_col <- length(input$text_features)
      } else {
        default_col <- 2
      }
      
      #Create/update slider input
      ui<- sliderInput(inputId = "vln_ncol",
                  label = "Number of columns: ",
                  min = 1,
                  max = length(input$text_features), #Max value: equal to the number of features entered
                  step = 1, #Only allow integer values
                  ticks = FALSE,
                  value = default_col)
    }
    ui
  })
  
  #### 2.1.4.3. Code for conditional UI ####
  vln_slot_UI <- reactive({
    #Condition A: no features have been entered yet
    if (length(input$text_features)==0){
      #Generate a message instructing the user to enter features.
      tags$h3("Please enter a feature to view violin plot.", style="margin-bottom: 10em;")
    }
    
    #Condition B: one or more features are entered
    else {
      #Generate a plot. Use automatic or user-specified dimensions with plotOutput() based on user specification of manual dimensions.
      if (input$vln_manual_dim==TRUE){
        plotOutput(outputId = "vln_slot_plot",
                   width = vln_width(), 
                   height= vln_height())
      }else{
        plotOutput(outputId = "vln_slot_plot")
      }
    }    
    
  })
  
  #### 2.1.4.4. Code for content ####
  vln_plot_content <- reactive({
    if(length(input$text_features>=1)){
      vln_plot <- VlnPlot(plots_subset(),
                          features=input$text_features,
                          group.by = input$vln_group_by,
                          #Split.by: NULL if user selects "none", otherwise equal to user selection
                          split.by = if (input$vln_split_by=="none") NULL else input$vln_split_by,
                          #ncol: NULL if only one feature is entered. If there are multiple features,
                          #this is equal to what the user specifies
                          ncol = if (length(input$text_features)==1) NULL else input$vln_ncol
                          ) +
                  #Legend position: "right" if a legend is desired, and "none" if not
                  theme(legend.position = if (input$vln_legend==TRUE) "right" else "none")
      
      #Correct titles: change machine-readable name to human-readable name
      #Determine number of plots created
      n_patches <- n_patches(vln_plot)
      #Iterate through each plot, correcting the title
      vln_plot <- hr_title(vln_plot,n_patches,assay_info)
      
      #Return the plot
      vln_plot
    }
  })
  
  #### 2.1.4.5. Render UI components and content for violin plot ####
  output$vln_slot <- renderUI({vln_slot_UI()})
  
  #ncol slider
  output$vln_ncol_slider <- renderUI({vln_ncol_slider()})
  
  #Render Plot: use automatic or manual width/height based on user specifications
  observeEvent(c(input$vln_manual_dim, input$vln_width, input$vln_width_text, input$vln_height,input$vln_height_text),{
    if (input$vln_manual_dim==TRUE){
      #Manual dimensions for UI and plot, using values from 2.4.1.
      output$vln_slot_plot <- renderPlot({vln_plot_content()}, 
                                             width = vln_width(), 
                                             height= vln_height())
    } else {
      output$vln_slot_plot <- renderPlot({vln_plot_content()})
    }
  })
  
  #### 2.1.4.6. Violin Plot Download ####
  output$vln_download <- downloadHandler(
    filename = "Violin_plot.png",
    content = function(file){
      if (input$vln_manual_dim==TRUE){
        ggsave(file, 
               plot=vln_plot_content(), 
               device="png",
               width=vln_width(),
               height=vln_height(),
               dpi=72,
               units="px")
      } else {
        ggsave(file, 
               plot=vln_plot_content(), 
               device="png")
      }
    },#End content function
    contentType = "image/png"
  ) #End downloadHandler function
  
  ### 2.1.5. Dot plot #####
  #### 2.1.5.1. Reactive plot dimensions ######
  ##Sync width inputs
  #Update text box to match slider value when slider is changed
  observeEvent(input$dot_width,{
    updateSearchInput(session, inputId = "dot_width_text", value=input$dot_width, trigger=TRUE)
  })
  
  #Update slider when text box value is changed (search input waits until user presses enter to update)
  observeEvent(input$dot_width_text,{
    updateSliderInput(session, inputId = "dot_width", value=input$dot_width_text)
  })
  
  #Store the plot width value specified by the user 
  dot_width <- eventReactive(c(input$dot_width, input$dot_width_text),{
    #Store the value from the slider (will be the same as the text box value since the syncing operations above run first)
    input$dot_width
  })
  
  ##Sync height inputs
  #Update text box to match slider value when slider is changed
  observeEvent(input$dot_height,{
    updateSearchInput(session, inputId = "dot_height_text", value=input$dot_height, trigger=TRUE)
  })
  
  #Update slider when text box value is changed (search input waits until user presses enter to update)
  observeEvent(input$dot_height_text, {
    updateSliderInput(session, inputId = "dot_height", value=input$dot_height_text)
  })
  
  #Store the plot height value specified by the user 
  dot_height <- eventReactive(c(input$dot_height, input$dot_height_text),{
    input$dot_height
  })
  
  #### 2.1.5.2. Feature choices #####
  #First observeEvent() function
  #The function below responds to each feature entered while the "use separate features for dot plot" checkbox is not checked. It is designed to load the selected options in the background before the user checks the box, making them immediately available when the box is checked 
  observeEvent(input$text_features,
               {req(input$text_features) #prevents code from running at startup (waits until something is entered in text_features)
                 if (input$diff_features_dot==FALSE){
                 updateSelectizeInput(session,
                                      inputId = "dot_features",
                                      choices = valid_features,
                                      selected = input$text_features,
                                      server = TRUE)
               }
               })
  
  #Second observeEvent() function
  #When the user checks the box to specify different features, sync the selected options for the dot plot with the generic text entry.
  #Prevents an error that arises when the user enters features in the generic entry while the box is checked, unchecks the box, then checks it again (in this case, features do not reset to be equal to the ones the user entered in the generic entry)
  observeEvent(input$diff_features_dot,
                {if (!setequal(input$text_features, input$dot_features)){
                    updateSelectizeInput(session,
                                         inputId = "dot_features",
                                         choices = valid_features,
                                         selected = input$text_features,
                                         server=TRUE)}
                  })
  
  #### 2.1.5.3. Generate UI for dot plot #####
  #Use reactive instead of eventReactive since the update button is no longer in use
  dot_slot_UI <- reactive({
    #Condition A: no features are entered, and use of generic features is selected
    if ((input$diff_features_dot==FALSE)&(length(input$text_features)==0)){
      #If this is the case, generate a message instructing the user to enter features.
      tags$h3("Please enter a feature to view dot plot.", style="margin-bottom: 10em;")
    }
    
    #Condition B: Use of dot-specific features is selected, but no features have been entered into the corresponding text box
    else if ((input$diff_features_dot==TRUE)&(length(input$dot_features)==0)){
      tags$h3('Please specify dot-plot specific features to view plot. To use the same features as for other plots, please uncheck "use separate features for dot plot".', style="margin-bottom: 10em;")
    }
    
    #Condition C: One or more generic features entered if separate features checkbox is unchecked, 
    #Or one or more dot-plot specific features entered if separate features checkbox is checked
    else if (((input$diff_features_dot==FALSE)&(length(input$text_features)>=1))|((input$diff_features_dot==TRUE)&(length(input$dot_features)>=1))){
      if (input$dot_manual_dim==TRUE){
        plotOutput(outputId = "dot_slot_plot",
                   width = dot_width(), 
                   height= dot_height())
      } else {
        plotOutput(outputId = "dot_slot_plot")
      }
    }
    })
  
  #### 2.1.5.4. Generate dot plot content #####
  dot_plot_content <- reactive({
    #Only renders if condition C in 2.5.2 is met
    if (((input$diff_features_dot==FALSE)&(length(input$text_features)>=1))|((input$diff_features_dot==TRUE)&(length(input$dot_features)>=1))){
      #If user specifies the use of different features, use the dot plot-specific features instead of the generic text entry features
      if (input$diff_features_dot==TRUE){
        DotPlot(plots_subset(),
                features = input$dot_features,
                group.by = input$dot_group_by) + 
          RotatedAxis() +
          #Legend position: "right" if a legend is desired, and "none" if not
          theme(legend.position = if (input$dot_legend==TRUE)"right" else "none")
      }
      else {
        #Check if split.by is specified
        DotPlot(plots_subset(), 
                features = input$text_features,
                group.by = input$dot_group_by) + 
          RotatedAxis() +
          #Legend position: "right" if a legend is desired, and "none" if not
          theme(legend.position = if (input$dot_legend==TRUE)"right" else "none")
      }
    }
  })
  
  #### 2.1.5.5. Render dot plot UI and content ####
  #UI
  output$dot_slot <- renderUI({dot_slot_UI()})
  
  #Plot content
  observeEvent(c(input$dot_manual_dim, input$dot_width, input$dot_width_text, input$dot_height,input$dot_height_text),{
    if (input$dot_manual_dim==TRUE){
      #Manual dimensions for plot, using values from 2.4.1.
      output$dot_slot_plot <- renderPlot({dot_plot_content()}, 
                                         width = dot_width(), 
                                         height= dot_height())
    } else {
      #Use automatic dimensions (no width or height specifed) if box is unchecked
      output$dot_slot_plot <- renderPlot({dot_plot_content()})
    }
  })
  
  #### 2.1.5.6. Dot Plot Download #####
  output$dot_download <- downloadHandler(
    filename = "Dot_plot.png",
    content = function(file){
      if (input$dot_manual_dim==TRUE){
        ggsave(file, 
               plot=dot_plot_content(), 
               device="png",
               width=dot_width(),
               height=dot_height(),
               dpi=72,
               units="px",
               #Explicitly state white background color (plots were transparent)
               bg="#FFFFFF")
      } else {
        ggsave(file, 
               plot=dot_plot_content(), 
               device="png",
               #White background
               bg="#FFFFFF")
      }
    },#End content function
    contentType = "image/png"
  ) #End downloadHandler function
  
  ## 2.2. DGE Tab Server Module ####
  dge_tab_server(id = "dge",
                 sobj = sobj,
                 metadata_config = config$metadata,
                 unique_metadata = unique_metadata,
                 meta_choices = meta_choices)
  
  ## 2.3. Correlations Tab Server Module ####
  corr_tab_server(id = "corr",
                  sobj = sobj,
                  metadata_config = config$metadata,
                  unique_metadata = unique_metadata,
                  n_cells_original = n_cells_original, 
                  nonzero_threshold = nonzero_threshold, 
                  meta_choices = meta_choices,
                  valid_features = valid_features,
                  error_list = error_list)
}

# Run the application 
shinyApp(ui = ui, server = server)
