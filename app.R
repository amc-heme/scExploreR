### Load Libraries and Data; Define Variables ###
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

#Load Seurat object (D0/D30 data, modified to include gene signature scores)
#https://storage.googleapis.com/jv_omics_sandbox/longitudinal_samples_20211025.Rds
sobj <- readRDS("./Seurat_Objects/longitudinal_samples_20211025.rds")

##Define searchable features and Metadata ####
#Gene_expression features
genes <- rownames(sobj)

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
adt_list <- feature_list("ADT", "adt_", " (Surface Protein)")

###Gene Signatures
sig_list <- feature_list("SIG", "sig_", " (Gene Signature)")

#Metadata columns (only numeric columns can be plotted)
meta_cols <- names(sobj@meta.data)

#Select columns that have numeric or integer values
numeric_cols <- meta_cols[sapply(meta_cols, FUN=function(x){
  (class(sobj@meta.data[[x]])=="numeric") || (class(sobj@meta.data[[x]])=="integer")
  })]

#Combine into list
valid_features <- list(`Genes`=as.list(genes),
                       `Surface Protein Markers`=adt_list,
                       `Gene Signature Scores`=sig_list,
                       `Metadata Features`=as.list(numeric_cols))

#Specify metadata variables to group and split by in drop down menus
#Choices are specific to the D0/D30 object
meta_choices <- c("None"="none",
                  "Clusters"="clusters",
                  "Response"="response",
                  "Response (Additional Detail)"="best_response",
                  "Timepoint (Approximate)"="treatment",
                  "Patient ID"="htb")

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
n_cells_original <- ncol(sobj)

##Correlations tab: define valid metadata selections ####
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

#Non-zero proportion threshold: if the proportion of cells for a gene is below this threshold, return a warning to the user.
nonzero_threshold <- 0.10

#Error Handling: define possible errors ####
#Errors are defined in a list using the functions in "./R/error_handling.R". 
#The error_handler() function is executed in a tryCatch() statement and checks
#the error message returned against a list of errors.
## List of errors for subset operations ####
subset_error_list<- list(add_error_notification(message="cannot allocate vector of size",
                                                notification_ui=icon_notification_ui_2(icon_name = "skull-crossbones",
                                                                                       tagList(
                                                                                         "Memory Error: RAM is insufficient for analyzing the specified subset. Please narrow down the subset scope using the restriction criteria to the left, and feel free to", 
                                                                                         github_link(display_text = "let us know"),
                                                                                         " ",#Space after link
                                                                                         "if you repeatedly recieve this error.")#End tagList
                                                ),#End icon_notification_ui
                                                notification_id = "mem_error"
),#End add_error_notification

#Error 2: Vector memory exhausted
add_error_notification(message="vector memory exhausted",
                       notification_ui=icon_notification_ui_2(icon_name = "skull-crossbones",
                                                              "Error: vector memory exhausted. If this issue persists, please ",
                                                              github_link("contact us"),
                                                              " with a screenshot of the response criteria selected. For now, narrowing down the subset criteria may resolve the error."
                       ),#End icon_notification_ui
                       notification_id = "vector_mem_error"
),

#Error 3: No Cells in Subset
add_error_notification(message = "No cells found",
                       icon_notification_ui_2(
                         icon_name = "skull-crossbones",
                         "No cells were found matching the defined subset criteria. Please check the subset dropdowns for mutually exclusive selections. If you recieve this error for combinations that should be valid, please",
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
plots_tab <- function(){
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
                                             tags$strong("Metadata in Displayed Subset",id="plots_subset_header"),
                                             div(tags$strong("Clusters: "),
                                                 textOutput(outputId = "plots_selected_clusters", inline = TRUE)),
                                             div(tags$strong("Response criteria: "),
                                                 textOutput(outputId = "plots_selected_response", inline = TRUE)),
                                             div(tags$strong("Timepoints: "),
                                                 textOutput(outputId = "plots_selected_treatment", inline = TRUE)),
                                             div(tags$strong("Patients: "),
                                                 textOutput(outputId = "plots_selected_htb", inline = TRUE))
                                         ),
                                         subset_menus(input_prefix = "plots",choices=choices),
                                         actionButton(inputId = "plots_subset_submit",
                                                      label="Apply Subset")
                                         )
                          ),#End 1.1.1.3
                   
                   ### Plot Specific Options ###
                   #### 1.1.1.4. Options specific to UMAP ####
                   #Panel will display if UMAP is checked
                   conditionalPanel(condition = "input.make_umap==true",
                                    collapsible_panel(inputId="plots_umap_collapsible",
                                                      label="UMAP Specific Options",
                                                      active=TRUE,
                                                      #Choose metadata to group UMAP by
                                                      selectInput(inputId = "umap_group_by", 
                                                                  label = "Metadata to Group by:",
                                                                  #Remove "none" from selectable options to group by
                                                                  choices=meta_choices[!meta_choices %in% "none"], 
                                                                  selected = "clusters"),
                                                      #Choose metadata to split UMAP by
                                                      selectInput(inputId = "umap_split_by", 
                                                                  label = "Metadata to Split By:",
                                                                  choices=meta_choices,  
                                                                  selected = "none"),
                                                      #If split by is specified, control number 
                                                      #of columns with a slider
                                                      uiOutput(outputId = "umap_ncol_slider"),
                                                      #Checkbox: add or remove labels 
                                                      #(labels on by default)
                                                      checkboxInput(inputId = "umap_label",
                                                                    label="Label Groups",
                                                                    value=TRUE),
                                                      #Checkbox to add or remove Legend
                                                      checkboxInput(inputId="umap_legend",
                                                                    label="Include Legend",
                                                                    value=TRUE),
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

## 1.2 Tables Tab ####
tables_tab <- function(){
  fluidPage(
    sidebarLayout(
      #1.2.1. Options Panel
      sidebarPanel(
        #Add a block to display a waiter over when the options are updating
        div(id="dge_sidebar",
            #1.2.1.1. Restrict correlation table by metadata
            tags$h3("Differential Gene Expression"),
            tags$p("Use the dropdown menus below to select the desired test, the groups to use, and the subset on which to perform the test."),
            selectInput(inputId = "dge_mode",
                        label = "Choose test to perform",
                        #User chooses either dge or marker identification.
                        #Human- and computer- readable names given for both options
                        choices = c("Marker Identification"="mode_marker",
                                   "Differential Expression"="mode_dge"),
                        #Marker selection on clusters is displayed at startup
                        selected = "mode_marker"),
            #Further dropdown menus are specific to the chosen mode
            uiOutput(outputId = "dge_conditional_menus"),
            #Checkbox to return positive markers only (shown for both modes)
            checkboxInput(inputId = "dge_pos",
                          label="Positive Markers Only",
                          value = TRUE),
            #Submit button
            actionButton(inputId = "dge_submit",
                         label = "Update"),
            
            #Download Button
            uiOutput(outputId = "dge_downloads_ui")
        )#End dge-sidebar div
      ),#End sidebarPanel (1.3.1)
      #1.3.2 Main Pane
      mainPanel(
        div(id="dge_main_panel", 
            class="spinner-container-main", 
            #Div added to contain Waiter spinner (forces the spinner to 
            #cover the full main panel)
            uiOutput(outputId = "dge_ui")
           )#End dge_main_panel
      )#End MainPanel
    )#End sidebarLayout
  )#End fluidPage
}#End 1.2.

## 1.3 Correlation Tab ####
corr_tab <- function(){
  fluidPage(
    sidebarLayout(
      #1.3.1. Options Panel
      sidebarPanel(
        #Add a block to display a waiter over when the options are updating
        div(id="corr_sidebar",
            #1.3.1.1. Restrict correlation table by metadata
            tags$h3("Correlation Coefficients"),
            tags$p("Enter one feature to view the top features positively and negatively correlated with the feature in the data. You may optionally restrict the correlation analysis by metadata variables using the dropdown menus below."),
            #Feature selection: only one feature can be entered
            selectizeInput(inputId="corr_feature_selection",
                           label = "Gene Selection",
                           #Feature choices populated in server, as in the plots tab
                           choices = NULL,
                           selected = character(0),
                           options = list("placeholder"="Enter gene name")),
            pickerInput(inputId = "clusters_selection",
                        label = "Restrict by Cluster",
                        choices = clusters,
                        selected = clusters,
                        multiple = TRUE,
                        options = list(
                          "selected-text-format" = "count > 5",
                          #Define max options to show at a time to keep menu from being cut off
                          "size" = 10, 
                          "actions-box"=TRUE)),
            pickerInput(inputId = "response_selection",
                        label = "Restrict by Response",
                        choices = responses,
                        selected = responses,
                        multiple = TRUE),
            pickerInput(inputId="treatment_selection",
                        label = "Restrict by Timepoint (approximate)",
                        choices = treatments,
                        selected = treatments, 
                        multiple = TRUE,
                        options = list(
                          "selected-text-format" = "count > 3",
                          "actions-box"=TRUE
                        )),
            pickerInput(inputId = "htb_selection",
                        label = "Restrict by Patient",
                        choices = patients_categories,
                        selected = patients, 
                        multiple = TRUE,
                        options = list(
                          "selected-text-format" = "count > 3",
                          "size" = 10, 
                          "actions-box"=TRUE
                        )),
            actionButton(inputId = "corr_submit",
                         label = "Submit",
                         #Display inline with download button when it appears
                         class = "inline-block"),
            
            #Download buttons: render after the correlations table 
            #and scatterplots are computed
            uiOutput(outputId = "corr_downloads_ui", inline=TRUE),
            
            #Options for scatterplot: a collapsible panel of options
            #appears after a scatterplot is displayed
            uiOutput(outputId = "corr_scatter_options_ui")
            )#End corr-sidebar div
        ),#End sidebarPanel (1.3.1)
      #1.3.2 Main Pane
      mainPanel(
        div(id="corr_main_panel", class="spinner-container-main", #Div added to contain Waiter spinner (forces the spinner to cover the full main panel)
            uiOutput(outputId = "corr_ui"))
        
        )#End MainPanel
      )#End sidebarLayout
  )#End fluidPage
}#End 1.3.

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
  #CSS Defined in Header
  tags$head(tags$style(HTML("body{
                            padding-top: 60px;
                            }"))),
  #CSS and JS for collapsible panel
  navbarPage("Shiny scExplorer",
             windowTitle="Shiny scExplorer",
             position="fixed-top",
             tabPanel("Plots",
                      plots_tab()),
             tabPanel("DE Tables",
                      tables_tab()),
             tabPanel("Gene Correlations",
                      corr_tab())
             ),#End navbarPage()
  #Help button - Creates a Dropdown menu when clicked
  #Button should appear in the upper right hand corner of the navbar menu
  #This will be achieved with the button_wizzard.js script
  #(Help button is wrapped in two introBoxes)
  introBox(data.step=1,
           data.intro='Welcome to the Shiny app.<br><br>Please click the ">" button to continue with the tour, or click "skip" to proceed to the app.',
           data.position = "left",
           #Begin introBox2
           introBox(data.step = 2,
                    data.intro = 'Click this button to view help. For a more detailed explanation of features, select "detailed walkthrough". For more information on interpereting the plots in this app, select "Interpereting scRNA-seq Plots". Click "Guided tour" to view this tour again.',
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
                                          target="_blank", #Opens link in new tab
                                          rel="noopener noreferrer" #Cybersecurity measure for links that open in new tab: prevents tabnapping
                                          ),
                                   
                                   #Tutorial Document
                                   tags$a("Tutorial Vignette",
                                          href="Shiny_Vignette.html",
                                          class="blue_hover",
                                          target="_blank", #Opens link in new tab
                                          rel="noopener noreferrer" 
                                   ),#End Detailed Walkthrough link
                                   
                                   #File issue on github
                                   tags$a("Report a Bug",
                                          href="https://github.com/amc-heme/DataExploreShiny/issues",
                                          class="blue_hover",
                                          target="_blank", #Opens link in new tab
                                          rel="noopener noreferrer")
                                   )#End tagList
                    )#End Help Button
                    )#End introBox2
           ),#End introBox 1
  #Include list of scripts build from .js files in www/ directory
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
  
  #2.0.3. Render feature choices for feature selection in the correlations tab
  updateSelectizeInput(session,
                       inputId = "corr_feature_selection", 
                       choices = genes,
                       selected = character(0),
                       server = TRUE)
  
  ## 2.1. Plots Tab #####
  ### 2.1.1 Define subset for plots #####
  #2.1.1.1. Update choices in subset selection menu based on user selections
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
  
  # 2.1.1.2. Construct subset after "Apply Subset" button is clicked
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
                                                  cnd_message=cnd$message,
                                                  #Uses a list of 
                                                  #subset-specific errors 
                                                  error_list = subset_error_list,
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
                                   #make_subset(input,sobj)
                                   subset(
                                     sobj,
                                     subset =
                                       (clusters %in% input$plots_clusters_selection) &
                                       (response %in% input$plots_response_selection) &
                                       (htb %in% input$plots_htb_selection) &
                                       (treatment %in% input$plots_treatment_selection)
                                   )
                                 }
                                 )#End tryCatch
                                  
                                  #Hide the water
                                  waiter_hide("plots_main_panel")
                                  
                                  #Return subset to the eventReactive variable
                                  print("about to request plots_s_sub")
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
      theme(legend.position = if (input$umap_legend==TRUE)"right" else "none"),
      
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
  
  #### 2.1.2.7. Render UI Components ###
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
      #If no split.by variable is specified, create a feature plot without the split.by argument
      if (input$feature_split_by=="none"){
        feature_plot <- FeaturePlot(plots_subset(),
                                    features=input$text_features)
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
    #If/else if structure: code runs when one or more features are entered.
    #One feature entered: do not need ncol argument
    if (length(input$text_features)==1){
      #No ncol, no split.by
      if (input$vln_split_by=="none"){
        VlnPlot(plots_subset(), 
                features = input$text_features,
                group.by = input$vln_group_by) +
          #Legend position: "right" if a legend is desired, and "none" if not
          theme(legend.position = if (input$vln_legend==TRUE)"right" else "none")
      #No ncol, split.by
      } else {
        VlnPlot(plots_subset(), 
                features = input$text_features,
                group.by = input$vln_group_by,
                split.by = input$vln_split_by) +
          #Legend position: "right" if a legend is desired, and "none" if not
          theme(legend.position = if (input$vln_legend==TRUE)"right" else "none")
      }
    #More than one feature entered: use ncol since there are multiple panels
    } else if (length(input$text_features)>1){
      #ncol and no split.by
      if (input$vln_split_by=="none"){
        VlnPlot(plots_subset(), 
                features = input$text_features,
                group.by = input$vln_group_by,
                ncol=input$vln_ncol) +
          #Legend position: "right" if a legend is desired, and "none" if not
          theme(legend.position = if (input$vln_legend==TRUE) "right" else "none")
      #ncol and split.by
      } else {
        VlnPlot(plots_subset(), 
                features = input$text_features,
                group.by = input$vln_group_by,
                split.by = input$vln_split_by,
                ncol=input$vln_ncol) +
          #Legend position: "right" if a legend is desired, and "none" if not
          theme(legend.position = if (input$vln_legend==TRUE) "right" else "none")
      }
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
  
  ## 2.2. DGE Tab #####
  ### 2.2.1. Reactive dropdown menus ####
  #### 2.2.1.1. Reactive dropdown menu for patient ####
  # The code in this section is duplicated from section 2.3.1, and should be rewritten as a function to avoid redundancy.
  #Since patients fall into either the sensitive or resistant category, the patients dropdown will need to be updated to keep the user from choosing invalid combinations.
  #Menu will be updated in the future when variables such as treatment and time after diagnosis are added (ignoreInit prevents this from happening when app is initialized.
  #Running of code at startup is disabled with "ignoreInit=TRUE"
  # This (above line) is broken for DGE, but the behaviour is tolerable as-is. Should be formalized or fixed.
  observeEvent(
    c(input$dge_response_selection, input$dge_treatment_selection),
    ignoreInit = TRUE,
    label = "Reactive Patient Dropdown",
    {
      #Show a spinner while the valid patient ID's are calculated
      waiter_show(
        id = "dge_sidebar",
        html = spin_loaders(id = 2, color = "#555588"),
        color = "#B1B1B188",
        hide_on_render = FALSE #Gives manual control of showing/hiding spinner
      )
      
      ###### DGE Metadata Subset #####
      #Valid for all modes except dge with response or treatment as the group.by variable
      #Filter object for treatment and response selections
      valid_patients <- sobj@meta.data |> 
        filter(
          (.data[["response"]] %in% input$dge_response_selection)&
            (.data[["treatment"]] %in% input$dge_treatment_selection)
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
      
      #Update picker input with valid patient ID's
      updatePickerInput(
        session,
        inputId = "dge_htb_selection",
        #If patients is the group by variable, do not change the label
        #(a special label is used for the group by category)
        label = 
          if (input$dge_group_by=="htb") {NULL}
          else {"Restrict by Patient"},
        choices = valid_patients_categories,
        selected = valid_patients,
        options = list(
          "selected-text-format" = "count > 3",
          "actions-box" = TRUE
        )
      )
      
      #Hide waiter
      waiter_hide(id = "dge_sidebar")
    }
  )
  
  #### 2.2.1.2. Reactive dropdown: DGE and either patient or timepoint as the group by variable ####
  observeEvent(
    c(input$dge_group_1, input$dge_group_2),
    ignoreInit = TRUE,
    label = "Reactive Patient Dropdown (DGE Groups)",
    {
      #Only update patients when response or treatment is the group_by variable
      #If patient ID is the group by variable, update response and treatment menus
      #See else if at end of if statement below
      if(input$dge_group_by %in% c("response","treatment")){
        #Show a spinner while the valid patient ID's are calculated
        waiter_show(
          id = "dge_sidebar",
          html = spin_loaders(id = 2, color = "#555588"),
          color = "#B1B1B188",
          hide_on_render = FALSE #Gives manual control of showing/hiding spinner
        )
        
        ###### DGE Metadata Subset (groups mode) ####
        #Filter object using the two group selections for either response or
        #treatment, and the picker input selections for the other variable
        if (input$dge_group_by=="treatment"){
          valid_patients <- sobj@meta.data |> 
            filter(
              #Use two treatment selections and picker input for response
              (.data[["response"]] %in% input$dge_response_selection)&
                (.data[["treatment"]] %in% c(input$dge_group_1,input$dge_group_2))
            ) |> 
            #Select patients metadata column
            select(.data[["htb"]]) |> 
            #Return unique values
            unique() |>
            #Convert to a character vector
            unlist()
        } else if (input$dge_group_by=="response"){
          valid_patients <- sobj@meta.data |> 
            filter(
              #Use two response selections and picker input for treatment
              (.data[["response"]] %in% c(input$dge_group_1,input$dge_group_2))&
                (.data[["treatment"]] %in% input$dge_treatment_selection)
            ) |> 
            #Select patients metadata column
            select(.data[["htb"]]) |> 
            #Return unique values
            unique() |>
            #Convert to a character vector
            unlist() 
        }

        #Form categorized list of valid patients for display in dropdown menu
        valid_patients_categories <- build_patient_list(valid_patients)
        #Sort patients categorized list so they appear in order
        valid_patients_categories <- sort_patient_list(valid_patients_categories)
        
        #Update picker input with valid patient IDs
        updatePickerInput(
          session,
          inputId = "dge_htb_selection",
          label = "Restrict by Patient",
          choices = valid_patients_categories,
          selected = valid_patients,
          options = list(
            "selected-text-format" = "count > 3",
            "actions-box" = TRUE
          )
        )
        
        #Hide waiter
        waiter_hide(id = "dge_sidebar")
        
      } else if (input$dge_group_by=="htb") {
        #In thus case, update *treatment* and *response* selections to reflect
        #selected patients
        #Show a spinner while the valid patient ID's are calculated
        waiter_show(
          id = "dge_sidebar",
          html = spin_loaders(id = 2, color = "#555588"),
          color = "#B1B1B188",
          hide_on_render = FALSE #Gives manual control of showing/hiding spinner
        )
        
        valid_choices <- sobj@meta.data |> 
          filter(
            #Use two treatment selections and picker input for response
            (.data[["htb"]] %in% c(input$dge_group_1,input$dge_group_2))
            ) |> 
          #Select treatment and response columns
          select(.data[["treatment"]],.data[["response"]]) #|> 
        
        #From the filtered metadata table above, fetch unique
        #treatment and response values and process to correct input
        valid_response <- valid_choices$response |> 
          #Return unique values
          unique() |> 
          #Convert to a character vector
          unlist() |> 
          #Sort values (use function from stringr 
          #package for natural sorting)
          str_sort(numeric=TRUE)
        
        valid_treatment <- valid_choices$treatment |> 
          #Return unique values
          unique() |> 
          #Convert to a character vector
          unlist() |> 
          #Sort values
          str_sort(numeric=TRUE)
        
        #Update response input with valid choices
        updatePickerInput(session,
                          inputId = "dge_response_selection",
                          label = "Restrict by Response",
                          choices = valid_response,
                          selected = valid_response)
        
        updatePickerInput(session,
                          inputId = "dge_treatment_selection",
                          label = "Restrict by Timepoint (approximate)",
                          choices = valid_treatment,
                          selected = valid_treatment)
        
        #Hide waiter
        waiter_hide(id = "dge_sidebar")
      }
      
    }
    )
  
  #### 2.2.1.3. Update group 2 menu after DGE group 1 selection ####
  observeEvent(
    c(input$dge_group_1),
    ignoreInit = TRUE,
    label = "Reactive Patient Dropdown (DGE Groups)",
    {
      #Define valid choices for group 2 (excludes the choice currently
      #selected for group 1)
      new_choices<- rv$dge_group_choices[rv$dge_group_choices!=input$dge_group_1]  |> 
      #Sort choices
      str_sort(numeric=TRUE)
      
      updateSelectInput(
        session,
        inputId = "dge_group_2",
        label = "Group 2",
        #Update choices to exclude the current group 1 choice
        choices = new_choices,
        #Preserve current selection in group 2, unless it is invalid
        selected= 
          if(input$dge_group_2!=input$dge_group_1) {input$dge_group_2} 
          else {new_choices[1]}
      )
    })
  
  
  ### 2.2.2. DGE table for selected metadata and restriction criteria ####
  #Table updates only when the "Update" button is clicked
  #### 2.2.2.1. Store table content as reactive value ####
  #The tibble generated here is converted to a DT in 2.2.2.2. for viewing, and
  #Is passed to the download handler when the "download table" button is clicked.
  dge_table_content <- eventReactive(
    input$dge_submit,
    label = "DGE Table Content",
    ignoreNULL = FALSE,
    {
      #Show loading screen above main panel while table is computed (takes about a minute)
      waiter_show(
        id = "dge_main_panel",
        html = spin_loaders(id = 2, color = "#555588"),
        color = "#FFFFFF",
        hide_on_render = FALSE #Gives manual control of showing/hiding spinner
      )
      
      #Error handling: errors are frequent in this script, often due to memory 
      #limitations, and they will result in the spinner not disappearing from 
      #the main window since waiter_hide() exists at the end this code block. 
      #Therefore, the code in this block must be handled with tryCatch() to 
      #capture errors. This error handling code has been duplicated verbatim 
      #from section 2.3.2.1 - likely isn't necessary here; should be removed 
      #if warranted by testing.
      dge_table <- tryCatch(
        #If an error is caught: attempt to determine type of 
        #error by inspecting message text with grepl (not recommended, 
        #but I currently don't know any other way to catch this error type)
        error = function(cnd) {
          #Use error_handler function to display notifications to the 
          #user based on the error message
          error_handler(session,
                        cnd_message=cnd$message,
                        #The error handling function uses a list 
                        #of subset-specific errors 
                        error_list = subset_error_list,
                        #Id prefix for notification elements
                        id_prefix = "dge")
          
          #Return nothing if an error occurs
          dge_table <-
            NULL 
          return(dge_table)
        },
        #End error function
        #Begin tryCatch code
        {
          #Form subset based on chosen criteria
          #Store in reactive variable so it can be accessed by 
          #UMAP eventReactive() function
          print("Computing Subset")
          
          ## Pick the appropriate input variables bases on the group_by selection
          ## Not clear why the listing and unlisting is required for a character vector
          dge_clusters_int = unlist(ifelse(
            input$dge_group_by == "clusters",
            list(input$dge_clusters_selection_group),
            list(input$dge_clusters_selection)
          ))
          
          dge_response_int = unlist(ifelse(
            input$dge_group_by == "response",
            list(input$dge_response_selection_group),
            list(input$dge_response_selection)
          ))
          
          dge_htb_int = unlist(ifelse(
            input$dge_group_by == "htb",
            list(input$dge_htb_selection_group),
            list(input$dge_htb_selection)
          ))
          
          dge_treatment_int = unlist(ifelse(
            input$dge_group_by == "treatment",
            list(input$dge_treatment_selection_group),
            list(input$dge_treatment_selection)
          ))
          
          #Subset method depends on whether marker identification or
          #dge is selected
          if (input$dge_mode=="mode_marker"){
            #Marker identification: create subset based on the subset 
            #dropdown menus, and include the group_by metadata in the 
            #subset. The group_by dropdown is named reactively according
            #to user choice, such that its id matches one of the four below. 
            rv$dge_s_sub <- subset(
              sobj,
              subset =
                (clusters %in% dge_clusters_int) &
                (response %in% dge_response_int) &
                (htb %in% dge_htb_int) &
                (treatment %in% dge_treatment_int)
            )#End subset
          }else{
            #For DGE, define subset conditionally based on the group by
            #variable selection (VERY messy, but works for now)
            if (input$dge_group_by=="clusters"){
              #Subset will use the subset criteria chosen, as well as the two
              #classes chosen for the group by variable 
              
              #Create a vector with the two classes chosen for DGE
              clusters_selected_vector <- c(input$dge_group_1, input$dge_group_2)
              
              #Subset as usual for all variables except for the group by variable
              rv$dge_s_sub <- subset(
                sobj,
                subset =
                  #Use the vector above for the group_by_variable
                  (clusters %in% clusters_selected_vector) &
                  (response %in% dge_response_int) &
                  (htb %in% dge_htb_int) &
                  (treatment %in% dge_treatment_int)
              )#End subset
              
            } else if (input$dge_group_by=="response"){
              #Create a vector with the two response classes chosen for DGE
              response_selected_vector <- c(input$dge_group_1, input$dge_group_2)
              
              #Subset as usual for all variables except for response
              rv$dge_s_sub <- subset(
                sobj,
                subset =
                  #Use the vector above for the group_by_variable
                  (clusters %in% dge_clusters_int) &
                  (response %in% response_selected_vector) &
                  (htb %in% dge_htb_int) &
                  (treatment %in% dge_treatment_int)
              )#End subset
              
            } else if (input$dge_group_by=="htb"){
              #Create a vector with the two htb classes chosen for DGE
              htb_selected_vector <- c(input$dge_group_1, input$dge_group_2)
              
              #Subset as usual for all variables except for htb
              rv$dge_s_sub <- subset(
                sobj,
                subset =
                  #Use the vector above for the group_by_variable
                  (clusters %in% dge_clusters_int) &
                  (response %in% dge_response_int) &
                  (htb %in% htb_selected_vector) &
                  (treatment %in% dge_treatment_int)
              )#End subset
              
            } else if (input$dge_group_by=="treatment"){
              #Create a vector with the two treatment (timepoint) classes chosen for DGE
              treatment_selected_vector <- c(input$dge_group_1, input$dge_group_2)
              
              #Subset as usual for all variables except for treatment
              rv$dge_s_sub <- subset(
                sobj,
                subset =
                  #Use the vector above for the group_by_variable
                  (clusters %in% dge_clusters_int) &
                  (response %in% dge_response_int) &
                  (htb %in% dge_htb_int) &
                  (treatment %in% treatment_selected_vector)
              )#End subset
            }

          }
          
          print("Subset Stats")
          ###Subset Stats
          #Subset stats
          #compute_subset_stats(input,output,session,rv,nonzero_threshold)
            
          #Cells in subset
          rv$dge_n_cells <-
            length(Cells(rv$dge_s_sub))
          
          ## DGE-specific Stats
          #Number of classes in selection
          rv$dge_n_classes <- length(unique(rv$dge_s_sub@meta.data[,input$dge_group_by]))
          #Mode selection
          rv$dge_mode <- ifelse(
            rv$dge_n_classes == 2,
            paste0(
              "Differential Expression (",
              unique(rv$dge_s_sub@meta.data[, input$dge_group_by])[1],
              " vs. ",
              unique(rv$dge_s_sub@meta.data[, input$dge_group_by])[2],
              ")"
            ),
            paste0("Marker Identification (", rv$dge_n_classes, " classes)")
          )
          
          print("n_by_class")
          #Cells per class
          #Calculate number of cells per class using metadata table of subset
          n_by_class <- rv$dge_s_sub@meta.data |>
            #Group by the specified metadata variable (.data and double braces 
            #used so group_by can properly interpret the character vector input)
            group_by(.data[[input$dge_group_by]]) |> 
            summarise(n=n()) #Calculate number of cells per group 
          
          #Print class names and cell counts per class
          #Convert class names and cell counts from tibble to character vector 
          #Class names in first column of tibble
          class_names <- as.character(n_by_class[[1]]) 
          #Cell counts are in second column of tibble
          n_cells <- n_by_class[[2]]
          
          #Build list of classes and the number of cells in each
          n_list=list()
          for (i in 1:nrow(n_by_class)){
            n_list[[i]]<-glue("{class_names[i]}: {n_cells[i]}")
          }
          #Collapse list of class/count pairs into a string, 
          #and store in reactive variable
          rv$dge_n_by_class<-paste(n_list,collapse = "\n")
          #\n is the separator (will be read by verbatimTextOutput())
          
          print("Presto")
          #Run Presto
          dge_table <-
            #Run presto on the subsetted object and indicated metadata slot
            wilcoxauc(rv$dge_s_sub, group_by = input$dge_group_by) %>% 
            #Explicitly coerce to tibble
            as_tibble() %>%  
            #remove stat and auc from the output table
            select(-c(statistic, auc)) %>%  
            #Using magrittr pipes here because the following statement 
            #doesn't work with base R pipes
            #remove negative logFCs if box is checked
            {if (input$dge_pos) filter(., logFC > 0) else .} %>%
            #Arrange in ascending order for padj, pval (lower values are more
            #"significant"). Ascending order is used for the log fold-change
            arrange(padj, pval, desc(abs(logFC))) 
        }
      )#End tryCatch
      
      print("Code to hide waiter")
      #Hide loading screen
      waiter_hide(id = "dge_main_panel")
      waiter_hide(id = "dge_sidebar")
      
      #Return table for storage in dge_table_content()
      dge_table
    }
  )
  
  #### 2.2.2.2. DGE table, as DT for viewing ####
  dge_DT_content <- eventReactive(input$dge_submit, 
                                  label = "DGE DT Generation",
                                  ignoreNULL=FALSE, 
                                  {
                                    print("Output as DT")
    datatable(
      dge_table_content(),
      class = "compact stripe cell-border hover",
      selection = "none",
      filter = "top",
      rownames = FALSE
    ) %>%
      formatSignif(3:8, 5) # This is more than enough - 3 is probably fine
  })
  
  ### 2.2.3. DGE UI Components ####
  #### 2.2.3.1. DGE Main UI (Stats and Table/UMAP Outputs) ####
  #IgnoreNULL set to false to get UI to render at start up
  dge_ui <- eventReactive(input$dge_submit,
                          label = "DGE Main UI (Define Content)",
                          ignoreInit=TRUE,
                          ignoreNULL = FALSE, 
                          {
                            print("DGE UI Function")
                            waiter_show(
                              id = "dge_main_panel",
                              html = spin_loaders(id = 2, color = "#555588"),
                              color = "#FFFFFF",
                              hide_on_render = FALSE #Gives manual control of showing/hiding spinner
                            )
                            
                            #Also display spinner over the options menu to keep user from being able to click download buttons before content is ready
                            waiter_show(
                              id = "dge_sidebar",
                              html = spin_loaders(id = 2, color = "#555588"),
                              color = "#B1B1B188",
                              hide_on_render = FALSE #Gives manual control of showing/hiding spinner
                            )
                            
                            #UI to display
                            div(
                              tags$h2(
                                glue(
                                  "Differential Expression/Marker Genes by {input$dge_group_by} in Subset"
                                ),
                                class="center"
                              ),
                              #Table Metadata section
                              tags$h3("Test Summary", class="center"),
                              #Make each input criteria appear inline
                              div(
                                div(
                                  tags$strong("Test selected", class="x-large inline-block"),
                                  textOutput(outputId = "dge_print_mode", inline=FALSE)
                                ),
                                tags$strong("Subset Used for Test", class="x-large inline-block space-top"),
                                div(
                                  tags$strong("Clusters: "),
                                  textOutput(outputId = "dge_selected_clusters", inline = TRUE)
                                ),
                                div(
                                  tags$strong("Response criteria: "),
                                  textOutput(outputId = "dge_selected_response", inline = TRUE)
                                ),
                                div(
                                  tags$strong("Timepoints (approximate): "),
                                  textOutput(outputId = "dge_selected_treatment", inline = TRUE)
                                ),
                                div(
                                  tags$strong("Patients: "),
                                  textOutput(outputId = "dge_selected_htb", inline = TRUE)
                                ),
                                div(
                                  tags$strong("Number of cells in subset: ",
                                              class="space-top inline-block"),
                                  textOutput(outputId = "dge_print_n_cells", inline = TRUE)
                                ),
                                div(
                                  tags$strong("Number of cells per class: "),
                                  verbatimTextOutput(outputId = "dge_print_n_by_class") 
                                )
                                
                              ),
                              
                              #Correlations table and plots
                              tags$h3("DGE Table", class="center"),
                              #Add table container
                              div(#Use a DT data table
                                DTOutput(outputId = "dge_table")),
                              
                              #UMAP plot
                              #Label: depends on mode
                              #The conditional below is passed as an argument 
                              #To the div() function to generate the HTML for the
                              #DGE UI. hasName() is used to avoid errors arising
                              #From the rv$dge_n_classes not yet existing
                              #This happens at startup when the UI is computed 
                              #Before the table, where rv$dge_n_classes is undefined.
                              if(hasName(rv,"dge_n_classes")){
                                #Title for differential gene expression mode
                                if (rv$dge_n_classes == 2){
                                  tags$h3("UMAP of groups being compared")
                                }else{
                                  #Title for marker identification mode
                                  tagList(
                                    #Center text
                                    tags$h3("UMAP by class", 
                                            class="center"),
                                    tags$p("(Markers are computed for each group shown)", 
                                           class="center")
                                  )
                                }
                              } else {
                                #This conditional will be run at startup.
                                #The default is to compute markers for each cluster,
                                #so the text for marker identification will be shown.
                                tagList(
                                  #Center text
                                  tags$h3("UMAP by class",
                                          class="center"),
                                  tags$p("(Markers are computed for each group shown)", 
                                         class="center")
                                )
                              }, #This comma is very important (conditional is an argument)
                              
                              #UMAP container
                              plotOutput(outputId = "dge_umap",
                                         height = "600px")
                              
                            )#End UI div
                          })
  
  #### 2.2.3.2. Conditional Dropdown Menus ####
  #display based on whether DGE or marker identification is selected
  #2.2.3.2.1. Main Dropdown interface
  dge_conditional_menus <- 
    eventReactive(
      c(input$dge_mode),
      label = "DGE Conditional Menus",
      ignoreNULL = FALSE,
      {
        if (input$dge_mode=="mode_marker"){
          #UI for marker selection
          #Metadata to use for marker identification
          ui <- tagList(
            selectInput(inputId = "dge_group_by",
                        label = "Choose metadata to use for marker identification:",
                        #Remove "none" and "best_response" from selectable options to group by
                        choices = meta_choices[!meta_choices %in% c("none", "best_response")],
                        #At startup, marker selection is ran with clusters as the
                        #group by variable.
                        selected="clusters"),
            #Choice of classes to include in marker identification 
            #Based on group_by selection above
            uiOutput(outputId = "dge_marker_selection"),
            #Subset choices: depend on what the user selects for the group by variable
            uiOutput(outputId = "dge_subset_selection")
          )#end tagList 
          
        } else {
          #UI for differential gene expression
          #Metadata to use for dge: text displayed to user is different
          ui <- tagList(
            selectInput(inputId = "dge_group_by",
                        label = "Choose metadata to use for differential gene expression:",
                        #Remove "none" and "best_response" from selectable options to group by
                        choices = meta_choices[!meta_choices %in% c("none", "best_response")],
                        #Clusters is selected by default
                        selected = "clusters"
            ),#End selectInput
            #Choice of groups to compare: depends on what metadata type is selected
            uiOutput(outputId = "dge_group_selection"),
            #Subset choices: also depends on user selection
            uiOutput(outputId = "dge_subset_selection")
          )#End tagList
        }
        
        #Return the ui defined by the conditionals above
        ui
      })
  
  #2.2.3.2.2. UI to pick groups to compare for DGE
  dge_group_selection <- eventReactive(input$dge_group_by,
                                       label="DGE: groups for DGE test",
                                       ignoreNULL = FALSE,
                                       {
                                         print("Running code for DGE selections")
                                         #UI is only displayed when dge is selected
                                         if (input$dge_mode=="mode_dge"){
                                           #Use metadata type to determine choices 
                                           #for dropdown menu. Store in a reactive
                                           #variable so group 2 can be reactively
                                           #updated to exclude the selection in
                                           #group 1
                                           rv$dge_group_choices = sobj@meta.data |>
                                             #Get unique values for the metadata type entered
                                             select(.data[[input$dge_group_by]]) |> 
                                             unique() |>
                                             #Convert to vector
                                             unlist() |> 
                                             #Remove names from vector 
                                             unname() |> 
                                             #Convert factor of choices to character vector
                                             as.character() |> 
                                             #Sort choices
                                             str_sort(numeric=TRUE)
                                           
                                           #Choose two groups for dge from the metadata
                                           #type specified by the user. 
                                           #tagList: combines the elements below to 
                                           #Output them together
                                           tagList(
                                             #Put choices beside one another in two-column format
                                             div(class="two_column float_left",
                                                 selectInput(inputId = "dge_group_1",
                                                             label = "Group 1",
                                                             choices = rv$dge_group_choices,
                                                             selected = rv$dge_group_choices[1])
                                             ), #End div
                                             div(class="two_column float_right",
                                                 selectInput(inputId = "dge_group_2",
                                                             label = "Group 2",
                                                             choices = rv$dge_group_choices,
                                                             selected = rv$dge_group_choices[2])
                                             )
                                           )#End tagList
                                           
                                           #Do not display UI if mode is not dge
                                         } else NULL
                                       })
  
  #2.2.3.2.3. UI to pick classes for marker identification
  dge_marker_selection <- eventReactive(input$dge_group_by,
                                        label="DGE: groups for marker identificaiton",
                                        ignoreNULL = FALSE,
                                        {
                                          #UI is only displayed when marker identification is selected
                                          if (input$dge_mode=="mode_marker"){
                                            
                                            #Use metadata type to determine choices 
                                            #for picker menu
                                            marker_choices = sobj@meta.data |>
                                              #Get unique values for the metadata type entered
                                              select(.data[[input$dge_group_by]]) |> 
                                              unique() |>
                                              #Convert to vector
                                              unlist() |> 
                                              #Remove names from vector 
                                              unname() |> 
                                              #Convert factor of choices to character vector
                                              as.character() |> 
                                              #Sort choices
                                              str_sort(numeric=TRUE)
                                            
                                            #If the group by variable is patient
                                            #ID, form the categorized list of 
                                            #patients
                                            if (input$dge_group_by=="htb"){
                                              valid_patients_categorized <- build_patient_list(marker_choices)
                                              valid_patients_categorized <- sort_patient_list(valid_patients_categorized)
                                            }
                                          
                                            #Choose classes to include in marker search
                                            #Input id: use group by variable and syntax
                                            #Used for subsetting in other tabs to simplify
                                            #Subset computation when the table is computed
                                            pickerInput(inputId = glue("dge_{input$dge_group_by}_selection_group"),
                                                        label = "Choose classes to include in marker computation",
                                                        #Choices: if patient ID
                                                        #is the group by variable,
                                                        #Use the categorized patient list
                                                        choices = 
                                                          if (input$dge_group_by!="htb") {marker_choices} 
                                                        else {valid_patients_categorized},
                                                        #At startup, marker_choices 
                                                        #is equal to all clusters in the
                                                        #object. All are selected by 
                                                        #default.
                                                        selected = marker_choices,
                                                        multiple = TRUE,
                                                        options = list(
                                                          "selected-text-format" = "count > 3",
                                                          "size" = 10, 
                                                          #Define max options to show at 
                                                          #a time to keep menu from being cut off
                                                          "actions-box"=TRUE))
                                            #Do not display UI if the mode is not marker identification
                                          } else NULL 
                                        })
  
  #2.2.3.2.4. UI to select subset, for both DGE and marker identification
  dge_subset_selection <- eventReactive(input$dge_group_by,
                                        label="DGE: Subset Menu",
                                        ignoreNULL = FALSE,
                                        {
                                          print("Computing subset menus UI")
                                          #Subset menus: one menu for each metadata 
                                          #category that can be selected for the group
                                          #by variable, minus the current selected 
                                          #category
                                          menu_categories <- meta_choices[!meta_choices %in% c("none", "best_response", input$dge_group_by)]
                                          
                                          #Use the subset_menus function to create the
                                          #subset dropdowns UI
                                          subset_menus(input_prefix = "dge",
                                                       #Choices is a list defined at 
                                                       #Startup
                                                       choices = choices,
                                                       menus = menu_categories)
                                        })
  #### 2.2.3.3. Download Buttons for Table and Plots ####
  dge_downloads_ui <-
    eventReactive(
      c(input$submit, input$dge_table_rows_selected),
      label = "DGE Download Buttons UI",
      ignoreNULL = FALSE,
      {
        #Conditional level one, !hasName(): TRUE before table is created, FALSE after
        if (!hasName(input, "dge_table_rows_selected")) {
          #!hasName()==TRUE
          #Display nothing before table is created
          NULL
        } else {
          #!hasName()==FALSE (table created)
          #Display button to download table after table is created
          div(
            downloadButton(
              outputId = "dge_download_table",
              label = "Download Table",
              icon = icon("table")
            )
          )
        }
      }
    )

  #### 2.2.4. UMAP of DE selected groups ####
  dge_umap <- eventReactive(input$dge_submit, 
                            ignoreNULL=FALSE, 
                            label="DGE UMAP", {
                              print("DGE UMAP")
                              #ncol_argument: number of columns based on number
                              #of classes being analyzed in the subset.
                              #Use double-bracket means of accessing the 
                              #metadata variable (supports entry of an arbitrary variable)
                              #This means of access returns a dataframe. 
                              #Slice for the first row (the unique values)
                              n_panel <- unique(rv$dge_s_sub[[input$dge_group_by]])[,1] |>
                                length()
                              
                              #Set ncol to number of panels if less than four
                              #Panels are created
                              if (n_panel<4){
                                ncol=n_panel
                              }
                              #Use three columns for 4-9 panels
                              else if (n_panel>=4 & n_panel<9){
                                ncol=3
                              }
                              #Use four columns for 9+ panels
                              else if (n_panel>=9){
                                ncol=4
                              }
                              
                              #Create UMAP of subsetted object, split by metadata
                              #for object calculation, colored by cluster
                              DimPlot(rv$dge_s_sub,
                                      split.by = input$dge_group_by,
                                      group.by = "clusters",
                                      ncol=ncol)
                            })
  
  #### 2.2.5. Render DGE UI, table, and statistics ####
  #Main UI
  output$dge_ui <- renderUI({
    dge_ui()
  })
  
  #Sidebar conditional UI
  #Main conditional UI
  output$dge_conditional_menus <- renderUI({
    dge_conditional_menus()
  })
  
  #Marker selection, if indicated
  output$dge_marker_selection <- renderUI({
    dge_marker_selection()
  })
  
  #Group selection, if indicated
  output$dge_group_selection <- renderUI({
    dge_group_selection()
  })
  
  #Subset selection (depends on entries for marker and group selection)
  output$dge_subset_selection <- renderUI({
    dge_subset_selection()
  })
  
  #Download buttons
  output$dge_downloads_ui <- renderUI({
    dge_downloads_ui()
  })
  
  #Table
  output$dge_table <- renderDT({
    dge_DT_content()
  })
  
  #UMAP plot
  output$dge_umap <- renderPlot({
    dge_umap()
    })
  
  #Render Statistics
  observeEvent(input$dge_submit, 
               ignoreNULL= FALSE,
               label = "DE Render Statistics", 
               {
                 #New stats computation: find unique classes in the subset 
                 #created (rather than the user input)
                 #Render Clusters in subset
                 output$dge_selected_clusters <-
                   renderText({
                     rv$dge_s_sub@meta.data |> 
                       #Get unique clusters in subset
                       select(clusters) |> 
                       unique() |>
                       #Convert to vector (returns a factor)
                       unlist() |> 
                       #Remove names from vector 
                       unname() |> 
                       #Convert factor of clusters to character vector
                       as.character() |> 
                       #Convert to grammatically correct output
                       vector_to_text()
                   })
                 
                 #Render Response Criteria
                 output$dge_selected_response <-
                   renderText({
                     rv$dge_s_sub@meta.data |> 
                       #Get unique response criteria in subset
                       select(response) |> 
                       unique() |>
                       #Convert to vector
                       unlist() |> 
                       #Remove names from vector 
                       unname() |> 
                       #Convert factor of criteria to character vector
                       as.character() |> 
                       #Convert to grammatically correct output
                       vector_to_text()
                   })
                 
                 #Render Patients
                 output$dge_selected_htb <-
                   renderText({
                     rv$dge_s_sub@meta.data |> 
                       #Get unique response criteria in subset
                       select(htb) |> 
                       unique() |>
                       #Convert to vector
                       unlist() |> 
                       #Remove names from vector 
                       unname() |> 
                       #Convert factor of criteria to character vector
                       as.character() |> 
                       #Sort patient vector 
                       str_sort(numeric=TRUE) |> 
                       #Convert to grammatically correct output
                       vector_to_text()
                   })
                 
                 #Render timepoints
                 output$dge_selected_treatment <- 
                   renderText({
                     rv$dge_s_sub@meta.data |> 
                       #Get unique treatment (timepoints) in subset
                       select(treatment) |> 
                       unique() |>
                       #Convert to vector
                       unlist() |> 
                       #Remove names from vector 
                       unname() |> 
                       #Convert factor to character vector
                       as.character() |> 
                       #Convert to grammatically correct output
                       vector_to_text()
                   })
                 
                 #N cells in subset
                 output$dge_print_n_cells <-
                   renderText(isolate(rv$dge_n_cells))
                 
                 #N by class
                 output$dge_print_n_by_class<- renderText(isolate(rv$dge_n_by_class))
                 
                 #Current Test
                 output$dge_print_mode <-
                   renderText(isolate(rv$dge_mode))
               })
  
  #### 2.2.6. Download Handlers ####
  #Correlations Table
  output$dge_download_table <- downloadHandler(
    filename = function() {
      glue("DGE_table_{input$dge_group_by}.csv")
    },
    content = function(file) {
      write.csv(dge_table_content(),
                file = file,
                row.names = FALSE)
    },
    contentType = "text/csv"
  )#End downloadHandler 

  
  ## 2.3. Correlations Tab #####
  
  ### 2.3.1 Reactive dropdown menu for patient ####
  #Since patients fall into either the sensitive or resistant category, the patients dropdown will need to be updated to keep the user from choosing invalid combinations.
  #Menu will be updated in the future when variables such as treatment and time after diagnosis are added (ignoreInit prevents this from happening when app is initialized)
  #Running of code at startup is disabled with "ignoreInit=TRUE"
  observeEvent(c(input$response_selection, input$treatment_selection),ignoreInit = TRUE,label="Reactive Patient Dropdown",{ 
    #Show a spinner while the valid patient ID's are calculated
    waiter_show(
      id = "corr_sidebar",
      html = spin_loaders(id=2, color = "#555588"),
      color = "#B1B1B188",
      hide_on_render = FALSE #Gives manual control of showing/hiding spinner
    )
    
    #Corr dplyr subset ####
    #Filter object for treatment and response selections, and find valid patients
    valid_patients <- sobj@meta.data |> 
      filter(
        (.data[["response"]] %in% input$response_selection)&
          (.data[["treatment"]] %in% input$treatment_selection)
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
    #####
    
    #Update picker input with valid patient ID's
    updatePickerInput(session,
                      inputId = "htb_selection",
                      label = "Restrict by Patient",
                      choices = valid_patients_categories,
                      selected = valid_patients,
                      options = list(
                        "selected-text-format" = "count > 3",
                        "actions-box"=TRUE
                      ))
    
    #Hide waiter
    waiter_hide(id = "corr_sidebar")
  })
 
  ### 2.3.2. Correlation table for selected feature and restriction criteria ####
  #Table updates only when the "Submit" button is clicked
  #### 2.3.2.1. Compute table content ####
  #The table in this function is accessed by the download handler,
  #and converted to DT format in 2.3.2.2. for display in app
  corr_table_content <- eventReactive(input$corr_submit,
                                      label="Corelation Table Content",
                                      ignoreInit = FALSE, 
                                      ignoreNULL = FALSE, 
                                      {

    # Only run the correlation table code if a feature has been specified
    if (input$corr_feature_selection != ""){
      #Hide the main screen UI while calculations are performed
      #Selector argument: a jQuery selector. The .find() method will hide all 
      #elements contained within corr_main_panel.
      hideElement(id="corr_ui")
      #Show loading screen above main panel while table is computed
      waiter_show(
        id = "corr_main_panel",
        html = spin_loaders(id=2, color = "#555588"),
        color = "#FFFFFF",
        hide_on_render = FALSE #Gives manual control of showing/hiding spinner
      )
      
      #Error handling: the code in this block must be handled with tryCatch() to 
      #capture errors that may arise from selecting subsets with zero cells, or 
      #from memory limitations that may be reached with larger datasets.
      corr_table <- tryCatch(
        #If an error is caught: attempt to determine type of error by
        #inspecting message text with grepl (not recommended, but I
        #currently don't know any other way to catch this error type)
        error = function(cnd){
          error_handler(session, 
                        cnd_message=cnd$message,
                        #Uses a list of subset-specific errors 
                        error_list=subset_error_list,
                        #Id prefix for notification elements
                        id_prefix = "plots")
          
          #Return nothing for the correlation table if an error occurs
          corr_table <- NULL 
          return(table)
        },#End error function
        #Begin tryCatch code
        {
          print("Make subset")
          #Form subset based on chosen criteria (store in reactive value so the 
          #subset can be accessed in the scatterplot function)
          rv$s_sub <- make_subset(input,sobj)

          #Determine if the subset created is a subset (if it is the full data,
          #use different procedures for creating/rendering the table and plots)
          if (n_cells_original!=ncol(rv$s_sub)){
            rv$corr_is_subset <- TRUE
          } else {
            rv$corr_is_subset <- FALSE
          }
          
          ###Subset Stats
          print("Subset Stats")
          #Determine the proportion of cells with nonzero reads for the selected 
          #gene. If it is below the threshold defined at the top of this script,
          #return a warning to the user.
          compute_subset_stats(input,output,session,rv,nonzero_threshold)
          
          #Compute correlations
          #If a subset has been selected, correlation coefficients between the 
          #selected feature and others will be computed for both the full data 
          #and the subset, and both will be displayed. If a subset is not selected, 
          #correlation coefficients will only be computed for the full data.
          if (rv$corr_is_subset==TRUE){
            #Subset is selected: compute both tables and merge
            table_full <- compute_correlation(input,
                                              object = sobj,
                                              colnames=c("Feature","Correlation_Global")
                                              )
            
            table_subset <- compute_correlation(input,
                                                object = rv$s_sub,
                                                colnames=c("Feature","Correlation_Subset")
                                                )
            
            #Merge individual tables and arrange in descending order by the 
            #subset correlation coefficient
            corr_table <- merge(table_full,table_subset, by = "Feature") |>
              arrange(desc(.data[["Correlation_Subset"]]))
            
          }else{
            #A subset is not present: compute the table for the subset
            corr_table <- compute_correlation(input,
                                         object = rv$s_sub,
                                         colnames=c("Feature","Correlation_Subset")
                                         )
          }
          
          #Return corr_table to eventReactive() function
          corr_table
          
#          print("Make Matrix")
#          #Convert subset data to matrix and transpose so columns are gene names
#          mat <- t(as.matrix(rv$s_sub@assays$RNA@data))
#          
#          print("Compute correlations")
#          #Form correlation matrix
#          table <- cor(mat[,input$corr_feature_selection],mat) |> #Compute correlation between selected feature and others
#            t() |> #Code returns coefficients for each feature in rows (want columns) 
#            enframe("Feature","Correlation_Coefficient") |> #Convert matrix to tibble
#            filter(Feature != input$corr_feature_selection) |> #Filter out selected feature
#            arrange(desc(Correlation_Coefficient)) #Arrange in descending order by correlation coeff
          
          })#End tryCatch
      
      #Hide loading screen
      waiter_hide(id = "corr_main_panel")
      waiter_hide(id="corr_sidebar")
      #Show content in main panel
      showElement(id="corr_ui")
      
      #Return table for storage in corr_table_content()
      corr_table
    }
  })
  
  #### 2.3.2.2. Store table in DT format for display in app ####
  corr_DT_content <- eventReactive(c(input$corr_submit,rv$corr_is_subset),
                                   label = "Corr DT Content",
                                   ignoreNULL = FALSE,
                                   {
                                     #Define header for datatable using HTML
                                     if(rv$corr_is_subset==TRUE){
                                       #If a subset is selected, the header will 
                                       #have three columns for the feature, the 
                                       #global correlation coefficients, and the 
                                       #correlation coefficients for the subset
                                       header <- tags$table(
                                         #center-colnames class: centers the 
                                         #column names in the header
                                         class = "compact stripe cell-border hover center-colnames",
                                         tags$thead(
                                           tags$tr(
                                             tags$th("Feature"),
                                             tags$th(
                                               tagList("Correlation",
                                                       tags$br(),
                                                       "(Global)"
                                               )
                                             ),
                                             tags$th(
                                               tagList("Correlation",
                                                       tags$br(),
                                                       "(Subset)")
                                             ) #End th
                                           ) #End tr
                                         ) #End thead
                                       ) #End table
                                       
                                     } else {
                                       header <- tags$table(
                                         #center-colnames class: centers the 
                                         #column names in the header
                                         class = "compact stripe cell-border hover center-colnames",
                                         tags$thead(
                                           tags$tr(
                                             tags$th("Feature"),
                                             tags$th(
                                               tagList("Correlation Coefficient",
                                                       tags$br(),
                                                       "(Global)")
                                             ) #End th
                                           ) #End tr
                                         ) #End thead
                                       ) #End table tag
                                       }
                                     
                                     datatable(
                                       corr_table_content(),
                                       class = "compact stripe cell-border hover",
                                       selection = "single",
                                       filter = "top",
                                       rownames = FALSE,
                                       container = header
                                     ) %>%
                                       #Use 5 sig figs for pearson coefficient 
                                       #column(s). If a subset is used, this 
                                       #will be columns 2 and 3; if not, 
                                       #this will be column 2.
                                       formatSignif(columns= if(rv$corr_is_subset==TRUE) c(2,3) else 2, 
                                                    digits=5)
                                   })
  
  ### 2.3.3. Correlations UI ####
  #### 2.3.3.1 Main UI ####
  #IgnoreNULL set to false to get UI to render at start up
  corr_ui <- eventReactive(input$corr_submit, 
                           label = "Correlation Main UI (Define Content)",
                           ignoreNULL = FALSE, 
                           {
    print("Correlation UI Function")
    #UI: if the feature selection menu is empty (default state at initialization), prompt user to enter features
    if (input$corr_feature_selection == ""){
      tags$h3("Enter a feature and press submit to view correlated features. You may also specify restriction criteria using the dropdown menus.")
    }
    #After a feature is applied and the submit button is pressed, display the table
    else {
      #Display the loading screen (screen will show until the end of the 
      #corr_table_content calculation is reached).
      waiter_show(
        id = "corr_main_panel",
        html = spin_loaders(id=2, color = "#555588"),
        color = "#FFFFFF",
        hide_on_render = FALSE #Gives manual control of showing/hiding spinner
      )
      
      #Also display spinner over the options menu to keep user from being able to click download buttons before content is ready
      waiter_show(
        id = "corr_sidebar",
        html = spin_loaders(id=2, color = "#555588"),
        color = "#B1B1B188",
        hide_on_render = FALSE #Gives manual control of showing/hiding spinner
      )
      
      #UI to display 
      tagList(
        tags$h2(glue("Genes correlated with {input$corr_feature_selection} in Subset"), class="center"),
        #Restriction criteria section
        tags$h3("Selected Restriction Criteria", class="center"),
        #Make each input criteria appear inline
        tagList(div(tags$strong("Clusters: "),textOutput(outputId = "selected_clusters", inline = TRUE)),
                div(tags$strong("Response criteria: "),textOutput(outputId = "selected_response", inline = TRUE)),
                div(tags$strong("Patients: "),textOutput(outputId = "selected_htb", inline = TRUE))
                ),
        
        #Statistics section
        tags$h3("Quality Statistics for Gene and Subset", class="center"),
        tagList(div("(Subset created based on defined restriction criteria)"),
            div(tags$strong("Number of cells in subset: "),textOutput(outputId = "print_n_cells", inline = TRUE)),
            div(tags$strong(glue("Cells with non-zero reads for {input$corr_feature_selection}:")),textOutput(outputId = "print_nonzero", inline = TRUE))
            ),
        
        #Correlations table and plots
        tags$h3("Correlated Genes", class="center"),
        #Output: table is rendered inline with scatterplot, which is defined in a separate eventReactive function
        #Scatterplot only appears when the user makes a selection on the table
        #Add table container
        div(class="two-column",
            style="width: 40%; float: left;",
            tags$strong("Correlation Table", class="center single-space-bottom"),
            #Use a DT data table
            DTOutput(outputId = "corr_table")
        ),
        
        div(class="two-column",
            style="width: 60%; float: right;",
            #UI for scatterplot rendered in separate eventReactive function
            uiOutput(outputId = "corr_scatter_ui")
            )
        )#End tagList
      }
  })
  
  #### 2.3.3.2. Correlations scatterplot UI ####
  #Computed separately from main UI since it responds to a different user input (clicking table)
  corr_scatter_ui <- eventReactive(c(input$corr_table_rows_selected,rv$corr_is_subset),
                                      label="Correlation Scatterplot UI",
                                      ignoreNULL = FALSE,
                                   {
                                     #Display the graph if rows are selected
                                      if (length(input$corr_table_rows_selected)>0){
                                        #If a subset is selected, display two 
                                        #plots: one for the subset and one for 
                                        #the full data.
                                        if (rv$corr_is_subset==TRUE){
                                          tagList(
                                            tags$strong("Scatterplot for Subset", 
                                                        class="center single-space-bottom"),
                                            plotOutput(outputId = "corr_scatter_subset", 
                                                       height = "400px", 
                                                       width="400px"),
                                            tags$strong("Scatterplot for Full Data", 
                                                        class="center single-space-bottom"),
                                            plotOutput(outputId = "corr_scatter_global", 
                                                       height = "400px", 
                                                       width="400px")
                                          )
                                        } else {
                                          tagList(
                                            tags$strong("Scatterplot", 
                                                        class="center single-space-bottom"),
                                            plotOutput(outputId = "corr_scatter_global", 
                                                       height = "400px", 
                                                       width="400px")
                                            )
                                          }
                                        }
                                     })
  
  #### 2.3.3.3. UI for customizing the scatterplot ####
  #This appears in the sidebar and displays a list of options used for customizing
  #the scatterplot
  corr_scatter_options <- eventReactive(c(input$corr_table_rows_selected, rv$corr_is_subset),
                                        label="Corr. Scatterplot Options UI",
                                        ignoreNULL = FALSE,
                                        {
                                          #If a selection in the table is made,
                                          #display a collapsible_panel with a 
                                          #a list of options for customization
                                          if (length(input$corr_table_rows_selected)>0){
                                            collapsible_panel(
                                              inputId = "corr_scatter_options",
                                              label = "Scatterplot Options",
                                              active = TRUE,
                                              #group.by selection
                                              selectInput(
                                                inputId = "corr_scatter_group_by",
                                                label = "Metadata to Group by:",
                                                #Remove "none" from selectable 
                                                #options to group by
                                                choices=meta_choices[!meta_choices %in% "none"], 
                                                selected = "clusters"
                                              ),
                                              #Download button for scatterplot
                                              #Subset (displays only if a subset is selected)
                                              if(rv$corr_is_subset==TRUE){
                                                downloadButton(
                                                  outputId = "corr_download_scatter_subset",
                                                  label = "Download Scatterplot (Subset)",
                                                  #Adds space before button
                                                  class="space-top",
                                                  icon = icon("poll")
                                                )
                                              } else NULL, #End downloadButton tag
                                              #Download button for scatterplot
                                              #Full data
                                              downloadButton(
                                                outputId = "corr_download_scatter_global",
                                                #Label changes based on whether 
                                                #a subset is selected
                                                label = if(rv$corr_is_subset==TRUE){
                                                  "Download Scatterplot (Full Data)"
                                                  } else {
                                                    "Download Scatterplot"},
                                                #Adds space before button
                                                #This is only needed when a 
                                                #subset is selected and there 
                                                #are two buttons
                                                class=if(rv$corr_is_subset==TRUE){
                                                  "space-top"
                                                  } else NULL,
                                                icon = icon("poll")
                                              ) #End downloadButton
                                            ) #End collapsible_panel  
                                          } #End if statement
                                        })
  
  #### 2.3.3.4. Download Button for Table ####
  corr_downloads_ui <- eventReactive(c(input$submit,input$corr_table_rows_selected),
                                     label="Correlation Table Download Button UI",
                                     ignoreNULL = FALSE, 
                                     {
                                       #Condition !hasName(): TRUE before table 
                                       #is created, FALSE after
                                       if (!hasName(input,"corr_table_rows_selected")){
                                         #Display nothing before table is created
                                         NULL 
                                       } else {
                                         #Display download button after table is
                                         #created
                                         downloadButton(
                                           outputId = "corr_download_table", 
                                           label = "Download Table",
                                           #Adds space before button
                                           class="inline-block",
                                           icon = icon("table")
                                         )
                                       } #End else
                                     })
  
  ### 2.3.4. Server Value for Rows Selected from Table ####
  #Becuase input$corr_table_rows_selected is NULL before the table is clicked,
  #An error flickers where the correlation plots are before displaying the plots,
  #giving the user the impression that an error has occurred. 
  corr_rows_selected <- eventReactive(input$corr_table_rows_selected,
               label = "Rows Selected: Server Value",
               {
                 #If the number of rows selected is not NULL and not equal to 
                 #`character(0)` (Value assigned by Shiny when no rows are 
                 #selected), set rv$rows_selected to TRUE
                 print(glue("Rows are selected: {(!identical(input$corr_table_rows_selected,character(0)))&
                     (!is.null(input$corr_table_rows_selected))}"))
                 if ((!identical(input$corr_table_rows_selected,character(0)))&
                     (!is.null(input$corr_table_rows_selected))){
                   corr_rows_selected=TRUE
                 } else {
                   #If a row is deselected or the table is re-computed, this must
                   #be set back to FALSE to keep the scatterplot from running when
                   #a feature is not selected, which will cause an error
                   corr_rows_selected=FALSE
                 }
                 
                 return(corr_rows_selected)
               })
  
  
  ### 2.3.5. Plot of feature selected from table ####
  #### 2.3.5.1. Correlation scatterplot for subset
  #Row index of user selection from table is stored in input$corr_table_rows_selected.
  #Reactive variable responds to input$corr_table_rows_selected and rv$corr_table_rows_selected
  #input$corr_table_rows_selected is the index of the row selected, while
  #corr_rows_selected() is the boolean generated in 2.3.4. This value is required 
  #to prevent the code from running when the user has de-selected values 
  corr_scatter_subset <- eventReactive(c(input$corr_table_rows_selected,
                                         corr_rows_selected(),
                                         input$corr_scatter_group_by),
                                  label="Correlation Scatterplot Content (Subset)",
                                  {
                                    row_idx <- input$corr_table_rows_selected
                                    print(glue("corr_rows_selected(): {corr_rows_selected()}"))
                                    #Take action only if a row is selected
                                    if (corr_rows_selected()==TRUE){
                                      #Record gene name of row selected
                                      rv$gene_selected <- as.character(corr_table_content()[row_idx,1])
        
                                      #Make and store scatterplot
                                      print(glue("For scatterplot: input$corr_feature_selection: {input$corr_feature_selection}"))
                                      FeatureScatter(rv$s_sub, 
                                                     feature1 = input$corr_feature_selection, 
                                                     feature2 = rv$gene_selected,
                                                     #group.by and split.by according to user input
                                                     group.by = input$corr_scatter_group_by)
                                      }
                                    })
  
  #### 2.3.5.2. Correlation plot for full data
  corr_scatter_global <- eventReactive(c(input$corr_table_rows_selected, 
                                         corr_rows_selected(),
                                         input$corr_scatter_group_by), 
                                       label="Correlation Scatterplot Content (Global)",
                                       {
                                         row_idx <- input$corr_table_rows_selected
                                         #Take action only if a row is selected 
                                         if (corr_rows_selected()==TRUE){
                                           #Record gene name of row selected
                                           rv$gene_selected <- as.character(corr_table_content()[row_idx,1])
                                           
                                           #Make and store scatterplot 
                                           #Use full object
                                           FeatureScatter(sobj, 
                                                          feature1 = input$corr_feature_selection, 
                                                          feature2 = rv$gene_selected,
                                                          #group.by and split.by according to user input
                                                          group.by = input$corr_scatter_group_by)
                                         }
                                       })
  
  ### 2.3.6. Render Correlation UI, table, scatterplot, and statistics ####
  #Main UI
  output$corr_ui <- renderUI({
    corr_ui()
    })
  
  #Container for scatterplots (main panel)
  output$corr_scatter_ui <- renderUI({
    corr_scatter_ui()
    })

  #Scatterplot (main panel, in UI container)
  observeEvent(input$corr_table_rows_selected, 
               label = "Render Corr Scatter (Subset)", {
                 output$corr_scatter_subset <- renderPlot({corr_scatter_subset()})
               })
  
  #Scatterplot (main panel, in UI container)
  observeEvent(input$corr_table_rows_selected, 
               label = "Render Corr Scatter (Global)", {
                 output$corr_scatter_global <- renderPlot({corr_scatter_global()})
               })
  
  #Options for scatterplot (sidebar panel)
  output$corr_scatter_options_ui <- renderUI({
    corr_scatter_options()
  })
  
  #Download button for Table
  output$corr_downloads_ui <- renderUI({
    corr_downloads_ui()
    })
  
  #Table
  output$corr_table <- renderDT({
    corr_DT_content()
    })

  #Render Statistics
  observeEvent(input$corr_submit,
               label = "Render Statistics",{
                 render_statistics(input,output,session,rv)
               })
  
  ### 2.3.7. Download Handlers ####
  #Correlations Table
  output$corr_download_table <- downloadHandler(
    filename=function(){
      glue("Corr_table_{input$corr_feature_selection}.csv")
    },
    content=function(file){
      write.csv(corr_table_content(),
                file=file,
                row.names=FALSE)
    },
    contentType="text/csv"
  )#End downloadHandler 
  
  #Scatterplot (for selected subset)
  output$corr_download_scatter_subset <- downloadHandler(
    filename=function(){
      glue("Corr_scatter_{input$corr_feature_selection}-vs-{rv$gene_selected}_subset.png")
    },
    content=function(file){
      ggsave(file, 
             plot=corr_scatter_subset(), 
             device="png",
             bg="#FFFFFF")
    },
    contentType = "image/png"
  )#End downloadHandler
  
  #Scatterplot (for full data)
  output$corr_download_scatter_global <- downloadHandler(
    filename=function(){
      glue("Corr_scatter_{input$corr_feature_selection}-vs-{rv$gene_selected}_global.png")
    },
    content=function(file){
      ggsave(file, 
             plot=corr_scatter_global(), 
             device="png",
             bg="#FFFFFF")
    },
    contentType = "image/png"
  )#End downloadHandler
}

# Run the application 
shinyApp(ui = ui, server = server)
