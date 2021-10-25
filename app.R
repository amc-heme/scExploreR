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

#Reactlog (for debugging)
library(reactlog)
options(shiny.reactlog=TRUE)

#Tidyverse Packages
library(tidyverse)
library(dplyr)
library(ggplot2)
library(glue)
library(DT)

#Load Seurat object (D0/D30 data)
#https://drive.google.com/file/d/1S7iGNzfmLX5g00zEgVX_Z98c6Xm0Bifb/view
sobj <- readRDS("./Seurat_Objects/longitudinal_samples_updated_2021-10-20.rds")

#Define subsets for 
#Diagnosis/follow-up (longitudinal samples, patients 1325, 1650, 1510, 1526, 1378, and 1724)
d0_d30 <- subset(sobj, sub=htb %in% c("1325","1650","1510","1526","1378","1724"))

#Define searchable features
#Gene_expression features
genes <- rownames(sobj)

### ADT features
#Fetch ADTs in Seurat object
adts <- rownames(sobj[["ADT"]])
#Human-readable ADT values
adt_human_readable <- paste0(adts," (Surface Protein)") 

#Machine-readable ADT value (format "ADT_<gene_name>")
adt_machine_readable <-paste0("ADT_",adts) #Capital letters are used for this object

#Zip above into a list of key-value pairs (human-readable features as keys, machine-readable features as values)
adt_list <- split(adt_machine_readable, adt_human_readable)
###

#Metadata columns (only numeric columns can be plotted)
meta_cols <- names(sobj@meta.data)

#Select columns that have numeric or integer values
numeric_cols <- meta_cols[sapply(meta_cols, FUN=function(x){
  (class(sobj@meta.data[[x]])=="numeric") || (class(sobj@meta.data[[x]])=="integer")
  })]

#Combine into list
valid_features <- list(`Genes`=as.list(genes),
                       `Surface Protein Markers`=adt_list,
                       `Metadata Features`=as.list(numeric_cols))

#Specify metadata variables to group and split by in drop down menus
#Choices are specific to the D0/D30 object
meta_choices <- c("None"="none",
                  "Clusters"="clusters",
                  "Response"="response",
                  "Response (Additional Detail)"="best_response",
                  "Treatment"="treatment",
                  "Patient ID"="htb")

split_by_choices <- c("None"="none",
                      "Clusters"="clusters",
                      "Response"="response",
                      "Response (Additional Detail)"="best_response",
                      "Patient ID"="htb",
                      #"sub" options below involve both a subset and a split.by on the treatment column
                      "D0 vs. D30"="sub-d0_d30")#,
                      #"Dignosis vs. Relapse"="sub-dx_rl")

###Correlations tab: define valid metadata selections
#Clusters dropdown
clusters <- levels(unique(sobj$clusters)) 

#Response dropdown
responses <- unique(sobj$response)

#Treatment dropdown (for d0/d30 object. Shows as 'approximate timepoint')
treatments <- unique(sobj$treatment)

#Patients dropdown
#The list of lists below displays patients in their respective groups
#This should be passed to 'choices', but not 'selected' (selected must be a vector)
patients_categories <- list(`d0/d30`=list("1325","1650","1510","1526","1378","1724"),
                 `Dx/Rl`=list("1261","1467","719"),
                 `Normal Bone Marrow`=list("BMMC_1","BMMC_2","BMMC_3"))
#Vector of all patients, to be passed to 'selected' in dropdown menus
patients <- unique(sobj$htb)

#Non-zero proportion threshold: if the proportion of cells for a gene is below this threshold, return a warning to the user.
nonzero_threshold <- 0.10

### Functions Used 
### Manual_dim_UI ###
#Creates two slider-text box pairs for manual control of the height and width of a plot.
manual_dim_UI <- function(plot_type,
                               initial_width=700,
                               max_width=2000,
                               min_width=200,
                               initial_height=400,
                               max_height=2000,
                               min_height=200){
  #Plot_dimension_input: used within manual_dim_UI. Creates a slider and text box intended to input either the width or height of a plot when manual dimensions are desired
  plot_dimension_input <-function(slider_input_id,
                                  box_input_id,
                                  label=NULL,
                                  slider_min=100,
                                  slider_max=1500,
                                  initial_value=350,
                                  style=NULL){
    div(style=style,
        #Label (if indicated)
        #Additional instructions are printed with label
        if (!is.null(label)){
          tags$p(tags$strong(label),
                 tags$br(),
                 "(Press enter to update text box value)")
        },
        
        
        #Slider (takes up 60% of element width)
        span(style="display: inline-block; vertical-align:top; width: 60%",
             sliderInput(inputId=slider_input_id,
                         label=NULL,
                         min = slider_min,
                         value= initial_value,
                         max= slider_max,
                         ticks=FALSE,
                         post=" px")
        ),
        
        #Text box
        span(style="display: inline-block; width: 60px; margin-bottom:0px; margin-left:5px;",
             searchInput(inputId=box_input_id,
                         value = initial_value,
                         label=NULL)),
        #px suffix after text box
        span(style="display: inline-block;",
             "px"))
  }
  
  #Form inputId values for each UI component created
  checkbox_id <- paste0(plot_type,"_manual_dim")
  width_slider_id <- paste0(plot_type,"_width")
  width_textbox_id <- paste0(plot_type,"_width_text")
  height_slider_id <- paste0(plot_type,"_height")
  height_textbox_id <-paste0(plot_type,"_height_text")
  
  #Create string for the "condition" argument in the conditionalpanel
  manual_dimensions_desired <- paste0("input.",checkbox_id,"==true")
  
  div(#Create checkbox: if checked, manual dimensions for the plot will be used according to inputs below.
    checkboxInput(inputId = checkbox_id,
                  label="Manually adjust plot dimensions",
                  value=FALSE),
    
    #Panel below displays when box is checked.
    conditionalPanel(condition=manual_dimensions_desired,
                     #Slider/text box for specifying width
                     plot_dimension_input(slider_input_id = width_slider_id,
                                          box_input_id = width_textbox_id,
                                          label = paste0("Use slider or text box to adjust plot width"), 
                                          initial_value = initial_width, 
                                          slider_min = min_width,
                                          slider_max=max_width),
                     
                     #Slider/text box for height
                     plot_dimension_input(slider_input_id = height_slider_id,
                                          box_input_id = height_textbox_id,
                                          label = paste0("Use slider or text box to adjust plot height"), 
                                          initial_value = initial_height, 
                                          slider_min = min_height, 
                                          slider_max = max_height)
    )#End conditional panel
    )#End div
}

### Collapsible panel UI function ###
#Will create a panel with a header that will toggle between hiding and showing its contents when the user clicks the header. 
#Must include the files "collapsible_panel.css" and "collapsible_panel.js" in the UI function for this to work properly.
collapsible_panel <- function(...,label=NULL,active=FALSE){
  #Use taglist to return button tag for header and div tag for content
  tagList( 
    #Header of panel: built with button tag. The label the user enters will be header text 
    button_html <- tags$button(type="button",
                               class=if (active==FALSE) "collapsible" else "collapsible active", #collapsible: starts closed; collapsible active: starts open
                               #Pass the user-provided label to the button text
                               if (!is.null(label)) as.character(label)),
    
    #Pass all content to div tag
    #If active==TRUE, the style attribute display will be set to "block" to display the content upon loading
    if (active==TRUE){
      content_html <- div(...,class="content",style="display:block;")
    } else {
      #Otherwise, the default value of none will be used to hide content initially
      content_html <- div(...,class="content")
    }
  )#End taglist
}
###

### Icon Notification Function
#Defines the HTML to be printed within a notification box. The function takes the name of a Font Awesome icon and a message as input, and will display the icon and the message inline.
icon_notification_ui <- function(icon_name,message){
  span(
    #Icon (inline and enlarged)
    icon(icon_name, style="display: inline-block; font-size: 1.7em;"),
    #Message (inline with icon, font slightly enlarged)
    span(message,style="font-size: 1.17em;")
    )
  }
###

### Vector to Text
# Prints the contents of a vector as a string with commas separating each element. 
# Used to create the formal report giving the subset criteria in the gene correlations tab
vector_to_text <- function(vector){
  if (is.null(vector)){
    "NULL"
  }
  #For one entry, report the choice.
  else if (length(vector)==1){
    paste(vector)
  }
  #For two entries, use 'and' between choices
  else if (length(vector)==2){
    paste0(vector[1]," and ",vector[2])
  }
  #Multiple entries: iteratively integrate elements in a single string using paste0.
  else {
    #For more than 2 entries, must list with commas between each choice, and 'and' in front of the last choice
    for (i in (1:(length(vector)))){
      #First element: create string_return string and store the first element in the string. 
      #Add a comma at the end.
      if (i==1){
        string_return <- paste0(vector[i],",")
      }
      #For all entries except the last entry, add the ith entry to the text vector with a comma at the end.
      else if (i!=length(vector)){
        string_return <- paste0(string_return," ",vector[i],",",collapse = "")
      }
      #Last entry: add ', and', then the last entry, then a period. 
      else{
        string_return <- paste0(string_return," and ",vector[i],collapse="")
      }
    }
    #When finished iterating through all entries, print the result.
    string_return
  }
}
###

### Sort patient list
#For the list of patients that display in the dropdown for the d0/d30 object,
#Sort each sublist so the patients appear in order.
sort_patient_list <- function(patient_list){
  #Patients in d0/d30 and Dx/Rl are numeric values that can be sorted easily
  patient_list$`d0/d30` <- patient_list$`d0/d30` |> 
    as.numeric() |> 
    sort() |> 
    as.character() |> #Convert back to character values to avoid issues with further subsetting 
    as.list()
  patient_list$`Dx/Rl` <- patient_list$`Dx/Rl` |> 
    as.numeric() |> 
    sort() |> 
    as.character() |> 
    as.list()
  #Normal bone marrow column consists of character IDs that are sorted properly with sort()
  patient_list$`Normal Bone Marrow` <- patient_list$`Normal Bone Marrow` |> 
    as.character() |> 
    sort() |> 
    as.list()
  #Return patient list
  patient_list
}
#Apply function to patients_categories so this list appears in order initially
patients_categories <- sort_patient_list(patients_categories)
###

### Table of Contents
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
      
      ### 1.1.1. Sidebar panel for user input ###
      sidebarPanel(fluid=FALSE,
        ### 1.1.1.1 Checkboxes for choosing desired plot
        # Two-column checkboxes: put inside inline block elements that span half of the sidebar panel
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
        
        ### 1.1.1.2. Feature Text Entry. Applies to feature, violin, and dot plots unless the user specifies the use of different features for each plot (currently only possible for dot plots) 
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
                                              'create'=FALSE)) #Do not allow user to input features not in the list of options
                         ),
                         #Error message: displayed if inalid features are entered (currently unused)
                         div(style="margin-top: 0px; margin-bottom: 10px;",uiOutput(outputId = "feature_error"))
        ),#End 1.1.1.2.
        
        ###1.1.1.3. Subsets for Plots
        collapsible_panel(label="Subset Options",
                           active=FALSE,
                           uiOutput(outputId="plots_selected_subset"),
                           pickerInput(inputId = "plots_cluster_selection",
                                       label = "Restrict by Cluster",
                                       choices = clusters,
                                       selected = clusters,
                                       multiple = TRUE,
                                       options = list(
                                         "selected-text-format" = "count > 5",
                                         "size" = 10, #Define max options to show at a time to keep menu from being cut off
                                         "actions-box"=TRUE)),
                           pickerInput(inputId = "plots_response_selection",
                                       label = "Restrict by Response",
                                       choices = responses,
                                       selected = responses,
                                       multiple = TRUE),
                           pickerInput(inputId = "plots_htb_selection",
                                       label = "Restrict by Patient",
                                       choices = patients_categories, #Display patient groups to user
                                       selected = patients, 
                                       multiple = TRUE,
                                       options = list(
                                         "selected-text-format" = "count > 3",
                                         "size" = 10, 
                                         "actions-box"=TRUE
                                       )),
                           actionButton(inputId="plots_subset_submit",
                                         label="Apply Criteria")
                           ),#End 1.1.1.3
        
        ### Plot Specific Options ###
        #1.1.1.4. Options specific to UMAP: panel will display if UMAP is checked
        conditionalPanel(condition = "input.make_umap==true",
                         collapsible_panel(label="UMAP Specific Options",
                                           active=TRUE,
                                           #Choose metadata to group UMAP by
                                           selectInput(inputId = "umap_group_by", 
                                                       label = "Metadata to group by:",
                                                       choices=meta_choices[meta_choices %in% "none" == FALSE], #Remove "none" from selectable options to group by
                                                       selected = "clusters"),
                                           #Choose metadata to split UMAP by
                                           selectInput(inputId = "umap_split_by", 
                                                       label = "Metadata to split by:", 
                                                       choices=split_by_choices, #Experimental 
                                                       selected = "none"),
                                           #If split by is specified, control number of columns with a slider
                                           uiOutput(outputId = "umap_ncol_slider"),
                                           #UI for user control of plot dimensions, if desired
                                           manual_dim_UI(plot_type = "umap"),
                                           #Download button (plot specific)
                                           downloadButton(outputId = "umap_download",label="Download UMAP")
                                           )#End collapsible panel
                         ),#End 1.1.1.4.
        
        #1.1.1.5. Options specific to feature plot
        conditionalPanel(condition = "input.make_feature==true",
                         collapsible_panel(label = "Feature Plot Specific Options",
                                           active = FALSE,
                                           #Feature plots do not have a group.by argument
                                           #Choose metadata to split feature plot by
                                           selectInput(inputId = "feature_split_by", 
                                                       label = "Metadata to split by:", 
                                                       choices=meta_choices, 
                                                       selected = "none"),
                                           #UI for user control of plot dimensions, if desired
                                           manual_dim_UI(plot_type = "feature"),
                                           #Download button (plot specific)
                                           downloadButton(outputId = "feature_download",label="Download Feature Plot")
                                           )#End collapsible_panel
                         ),#End 1.1.1.5
        
        #1.1.1.6. Options specific to violin plot
        conditionalPanel(condition = "input.make_vln==true",
                         collapsible_panel(label = "Violin Plot Specific Options",
                                           active=FALSE,
                                           #Choose metadata to group violin plot by
                                           selectInput(inputId = "vln_group_by", 
                                                       label = "Metadata to group by:", 
                                                       choices=meta_choices[meta_choices %in% "none" == FALSE], #Remove "none" from selectable options to group.by 
                                                       selected = "clusters"),
                                           
                                           #Choose metadata to split violin plot by
                                           selectInput(inputId = "vln_split_by", 
                                                       label = "Metadata to split by:", 
                                                       choices=meta_choices, 
                                                       selected = "none"),
                                           
                                           #Slider to control number of columns if multiple features are entered
                                           uiOutput(outputId = "vln_ncol_slider"),
                                           #UI for user control of plot dimensions, if desired
                                           manual_dim_UI(plot_type = "vln"),
                                           #Download button (plot specific)
                                           downloadButton(outputId = "vln_download",label="Download Violin Plot")
                                           )#End collapsible panel
                         ), #End 1.1.1.6.
        
        #1.1.1.7. Options specific to dot plot
        conditionalPanel(condition = "input.make_dot==true",
                         collapsible_panel(label="Dot Plot Specific Options",
                                           active=FALSE,
                                           #Choose metadata to group dot plot by
                                           selectInput(inputId = "dot_group_by", 
                                                       label = "Metadata to group by:", 
                                                       choices=meta_choices[meta_choices %in% "none" == FALSE], #Remove "none" from selectable options to group by
                                                       selected = "clusters"),
                                              
                                           #Choosing different features
                                           checkboxInput(inputId = "diff_features_dot",label="Use separate features for dot plot", value=FALSE),
                                           
                                           #If the checkbox above is selected, display a selectize input for feature selection
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
                                                                               options = list('plugins' = list('remove_button'),'create'=FALSE)))
                                           ),
                                           #UI for user control of plot dimensions, if desired
                                           manual_dim_UI(plot_type = "dot"),
                                           #Download button (plot specific)
                                           downloadButton(outputId = "dot_download",label="Download Dot Plot")
                                           ) #End collapsible panel
                         ) #End 1.1.1.7 
      ), #End 1.1.1.
      
      ###1.1.2. Main panel for displaying plot output###
      mainPanel(
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
      ) #End 1.1.2
    ) #End sidebarLayout() 
  ) #End fluidPage() 
}#End 1.1.

### 1.2 Tables Tab ###
tables_tab <- function(){
  fluidPage(
    #As with the plots tab, create a sidebar layout with options to select on the left and the table in the center
    sidebarLayout(
      
      ###1.2.1. Options Sidebar
      sidebarPanel(
        
        #1.2.1.1. Metadata to group by
        selectInput(inputId = "table_group_by", label = "Select Variable to view gene expression data by", choices = c("Response"="response","Patient ID"= "htb")),
        
        #1.2.1.2. Ident.1 selections: choose which metadata variable to display differential expression for based on metadata selection
        ### TODO: USE SERVER FUNCTION TO UPDATE SECOND DROPDOWN TO AVOID NEEDING ONE IDENT_1 INPUT FOR EACH METADATA TYPE 
        #1.2.1.2.1. If response is chosen, show selection to display resistant vs. sensitive or sensitive vs. resistant
        conditionalPanel(condition = "input.table_group_by=='response'",
                         selectInput(inputId = "response_ident_1", 
                                     choices = c("Resistant","Sensitive") ,
                                     label="Choose response classification to view differential expression data for:"),
                         "(Differential Expression results will be for the selected response classification relative to the other.)"),
        
        #1.2.1.2.2. If patient id is chosen, show selection input for patient id to compare relative to the others (ident.1 argument in FindMarkers())
        conditionalPanel(condition = "input.table_group_by=='htb'",
                         selectInput(inputId = "htb_ident_1",
                                     choices = unique(sobj@meta.data[["htb"]]),
                                     label ="Choose patient ID to view differential expression data for:"),
                         
                         #Additional text below group by choice panel
                         "(Differential Expression results will be for the selected patient ID relaitve to all other patients)"),
        
        #1.2.1.2.3. If clusters is chosen as the group.by variable, show selection input for cluster id to compare to the others (ident.1 argument in FindMarkers())
        conditionalPanel(condition="input.table_group_by=='clusters'",
                         selectInput(inputId = "cluster_ident_1",
                                     choices = levels(unique(sobj@meta.data[["clusters"]])),#called levels() to make choices show in numerical order
                                     label="Choose cluster ID to view differential expression data for:"),
                         
                         #Additional text below group by choice panel
                         "(Differential Expression results will be for the selected cluster ID relaitve to all other patients)"),
        
        tags$br(),
        
        #1.2.1.3. Ask user to specify whether gene expression or ADT data is desired
        selectInput(inputId = "table_assay",
                    choices = c("Gene"="RNA","Surface Protein"="ADT"),
                    selected = "Gene",
                    label = "Choose assay to view (gene or surface protein expression)"),

        #1.2.1.4. Download Button for Table 
        downloadButton(outputId = "de_download", label="Download Table")
      ),
      
      ###1.2.2. Main Panel with Table
      mainPanel(DTOutput(outputId = "de_table"))
    )
  )
}#End 1.2.

### 1.3 Correlation Tab ###
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
            pickerInput(inputId = "cluster_selection",
                        label = "Restrict by Cluster",
                        choices = clusters,
                        selected = clusters,
                        multiple = TRUE,
                        options = list(
                          "selected-text-format" = "count > 5",
                          "size" = 10, #Define max options to show at a time to keep menu from being cut off
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
                         label = "Submit")#,
            #Subset Stats Panel
            #uiOutput(outputId = "sub_stats")
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

### Define user interface: code for navigation panel and references to tabs
ui <- tagList(
  #CSS for Collapsible Panels
  includeCSS("www/collapsible_panel.css"),
  #CSS for Custom Scrollbars
  includeCSS("www/fancy_scroll.css"),
  #CSS for help button and dropdown menu
  includeCSS("www/help_button_and_dropdown.css"),
  #Introjs UI: for guided tour
  introjsUI(),
  #Waiter UI: spinners
  useWaiter(),
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
  includeScript("www/collapsible_panel.js"),
  includeScript("www/button_wizzard.js")
)

### 2. Server function (builds interactive plot to display in UI) ###
server <- function(input,output,session){
  #2.0. Initialize Session
  #2.0.1. Initialize Reactive Values
  rv <- reactiveValues()
  #For correlations tab: report number of cells in a subset, number of cells where a gene is detected, and the proportion of detected cells
  rv$n_cells <- 0
  rv$n_nonzero <- 0
  rv$prop_nonzero <- 0
    
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
  
  #Open Help Dropdown and initialize guided tour
#  introjs(session,
#          options = list("nextLabel" = ">",
#                         "prevLabel" = "<",
#                         "skipLabel" = "skip",
#                         "overlayOpacity" = -1
#                         ),
#          events = list("onbeforechange" = readCallback("switchTabs")) ##End list
#          ) #End introjs
  
  #Open the intro every time the user clicks the "guided tour" button
  observeEvent(input$start_intro, {
    print("Detected link")
    introjs(session,
            options = list("nextLabel" = ">",
                           "prevLabel" = "<",
                           "skipLabel" = "skip",
                           "overlayOpacity" = -1
                           ),
            events = list("onbeforechange" = readCallback("switchTabs"))
            )
  })
  
  ###2.1. Plots Tab
  #2.1.1 Define subset for plots 
  #UI for defining the subset currently selected.
  current_subset <- eventReactive(input$plots_subset_submit, label = "Plots: Current Subset UI", {
    #Print current subset criteria, inline with captions
    div(div(tags$strong("Clusters: "),textOutput(outputId = "plots_selected_clusters", inline = TRUE)),
        div(tags$strong("Response criteria: "),textOutput(outputId = "plots_selected_response", inline = TRUE)),
        div(tags$strong("Patients: "),textOutput(outputId = "plots_selected_htb", inline = TRUE))
    )
  })
  
  #Rendering text for selected subsets
  observeEvent(input$plots_subset_submit, ignoreNULL = FALSE,
               label = "Plots: Render Subset Criteria",{
                 #print("Clusters selected",input$plots_cluster_selection)
                 #print("Clusters",clusters)
                 #print("Logic test:",setequal(input$plots_cluster_selection,clusters))
                 
                 #Rendering Selections and Stats for report
                 output$plots_selected_clusters <- renderText({
                   #If all clusters are selected, print "All"
                   if(setequal(input$plots_cluster_selection,clusters)){
                     print("All")
                     #Otherwise, print the selected clusters
                   } else { 
                     isolate(vector_to_text(input$plots_cluster_selection))
                     } #End Conditionals
                 }) #End renderText
                 
                 #Selected Response Criteria
                 output$plots_selected_response <- renderText({
                   #Print "All" if all clusters are selected, otherwise print selected responses
                   if(setequal(input$plots_response_selection,responses)){
                     print("All")
                     }else{
                       isolate(vector_to_text(input$plots_response_selection))
                       }#End conditionals
                   }) #End renderText
                 
                #Selected Patients
                 output$plots_selected_htb <- renderText({
                   if(setequal(input$plots_htb_selection,patients)){
                   #Print "all" if all patient IDs are selected, otherwise print selected patients
                   print("All")
                   }else{
                     isolate(vector_to_text(input$plots_htb_selection))
                     } #End conditionals
                   }) #End renderText
               })
  
  #2.1.2. UMAP plot
  #2.1.2.1. Reactive UMAP plot dimensions
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
  
  #2.1.2.2. ncol slider: appears when a split.by 
  #Default value depends on the number of values in the metadata object in question. 
  umap_ncol_slider <- eventReactive(input$umap_split_by, ignoreInit = TRUE, { #Do not need to render UI at startup
    #Do not render when split.by is "none"
    if (input$umap_split_by=="none"){
      NULL
    } else {
      #Determine number of panels created by split_by choice.
      #Use double-bracket means of accessing the metadata variable (supports entry of an arbitrary variable)
      #This means of access returns a dataframe. Slice for the first row (the unique values)
      n_panel <- unique(sobj[[input$umap_split_by]])[,1] |> length()
      
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
                  label = "Number of columns: ",
                  min = 1,
                  max = n_panel, #Max value: equal to the number of levels in the given variable
                  step = 1, #Only allow integer values
                  ticks = FALSE,
                  value = default_col)
    } #End else
  })
  
  #2.1.2.3. Generate UI for UMAP plot: renders a plotOutput() with either automatic or manually specified dimensions based on user specifications
  umap_UI <- reactive({
    if (input$umap_manual_dim==FALSE){
      plotOutput(outputId = "umap_slot_plot")
    } else {
      plotOutput(outputId = "umap_slot_plot",
                 width = umap_width(),
                 height = umap_height())
    }
  })
  
  #2.1.2.4. Define UMAP Plot Content
  #Plot content is defined separately in a reactive context, to be rendered later with the UI.
  umap_plot_content <- reactive({
    #Produce a single UMAP plot if no features to split by are specified
    if (input$umap_split_by=="none"){
      DimPlot(sobj, 
              group.by = input$umap_group_by, 
              label = TRUE, 
              reduction = "umap")
    } else if (input$umap_split_by=="sub-d0_d30"){
      #Special case: if a subset option is passed, make the plot based on the pertinent subset
      DimPlot(d0_d30,
              group.by = input$umap_group_by, 
              split.by = "treatment", #The Treatment column shows the diagnosis vs. D30 data
              label = TRUE, 
              ncol = input$umap_ncol,
              reduction = "umap")
    } else {
      #UMAP with split.by defined and no special subset
      DimPlot(sobj, 
              group.by = input$umap_group_by, 
              split.by = input$umap_split_by, 
              label = TRUE, 
              ncol = input$umap_ncol,
              reduction = "umap")
    }
  })
  
  #2.1.2.5. Render UI Components
  output$umap_slot <- renderUI({umap_UI()})
  
  output$umap_ncol_slider <- renderUI({umap_ncol_slider()})
  
  #2.1.2.6. Render UMAP plot, with manual or automatic dimensions as specified
  #ObserveEvent will respond to the check box and the slider/text box pairs (other variables involved in plot construction are updated separately in the reactive function above)
  observeEvent(c(input$umap_manual_dim, input$umap_width, input$umap_width_text, input$umap_height,input$umap_height_text),{
    if (input$umap_manual_dim==FALSE){
      output$umap_slot_plot <- renderPlot(umap_plot_content())
    } else {
      output$umap_slot_plot <- renderPlot(umap_plot_content(), 
                                width = umap_width(), 
                                height = umap_height())
    }
  })
  
  #2.1.2.7. Download UMAP Plot
  output$umap_download <- downloadHandler(
    filename = "UMAP_plot.png",
    content = function(file){
      if (input$umap_manual_dim==TRUE){
        ggsave(file, 
               plot=umap_plot_content(), 
               device="png",
               width=umap_width()*5,
               height=umap_height()*5,
               dpi=300,
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
  
  #2.1.3. Feature Plot 
  #2.1.3.1 Reactive dimensions
  
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
  
  #2.1.3.2 Feature UI
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
  
  #2.1.3.3. Generate content for plot (but only if features are entered)
  feature_plot_content <- reactive({
    if (length(input$text_features)>0){
      #If no split.by variable is specified, create a feature plot without the split.by argument
      if (input$feature_split_by=="none"){
        FeaturePlot(sobj,
                    features=input$text_features)
      }
      #Otherwise, split by the user-specified variable
      else {
        FeaturePlot(sobj, 
                    features=input$text_features,
                    split.by = input$feature_split_by)
      }
    }
  })
  
  #2.1.3.4. Render the UI and plot objects created above
  #UI
  output$feature_slot <- renderUI({feature_slot_UI()})
  
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
  
  #2.1.3.5. Feature Plot Download
  output$feature_download <- downloadHandler(
    filename = "Feature_plot.png",
    content = function(file){
      if (input$feature_manual_dim==TRUE){
        ggsave(file, 
               plot=feature_plot_content(), 
               device="png",
               width=umap_width()*5,
               height=umap_height()*5,
               dpi=300,
               units="px")
      } else {
        ggsave(file, 
               plot=feature_plot_content(), 
               device="png")
      }
    },#End content function
    contentType = "image/png"
  ) #End downloadHandler function
  
  #2.1.4. Violin plot
  #2.1.4.1 Reactive plot dimensions
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
  
  #2.1.4.2. Slider to control number of columns when multiple features are entered
  vln_ncol_slider <- eventReactive(input$text_features, ignoreNULL = FALSE,{
    #Only display slider when there is more than one feature
    if (length(input$text_features) <= 1){
      ui <- NULL
    } else {
      #Default number of columns: equal to the number of panels if there are less than four, otherwise equal to two
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
  
  #2.1.4.3. Code for conditional UI
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
  
  #2.1.4.4. Code for content
  vln_plot_content <- reactive({
    #If/else if structure: code runs when one or more features are entered.
    #One feature entered: do not need ncol argument
    if (length(input$text_features)==1){
      #No nol, no split.by
      if (input$vln_split_by=="none"){
        VlnPlot(sobj, 
                features = input$text_features,
                group.by = input$vln_group_by)
      #No ncol, split.by
      } else {
        VlnPlot(sobj, 
                features = input$text_features,
                group.by = input$vln_group_by,
                split.by = input$vln_split_by) 
      }
    #More than one feature entered: use ncol since there are multiple panels
    } else if (length(input$text_features)>1){
      #ncol and no split.by
      if (input$vln_split_by=="none"){
        VlnPlot(sobj, 
                features = input$text_features,
                group.by = input$vln_group_by,
                ncol=input$vln_ncol)
      #ncol and split.by
      } else {
        VlnPlot(sobj, 
                features = input$text_features,
                group.by = input$vln_group_by,
                split.by = input$vln_split_by,
                ncol=input$vln_ncol) 
      }
    }
  })
  
  #2.1.4.5. Render UI components and content for violin plot
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
  
  #2.1.4.6. Violin Plot Download
  output$vln_download <- downloadHandler(
    filename = "Violin_plot.png",
    content = function(file){
      if (input$vln_manual_dim==TRUE){
        ggsave(file, 
               plot=vln_plot_content(), 
               device="png",
               width=umap_width()*5,
               height=umap_height()*5,
               dpi=300,
               units="px")
      } else {
        ggsave(file, 
               plot=vln_plot_content(), 
               device="png")
      }
    },#End content function
    contentType = "image/png"
  ) #End downloadHandler function
  
  #2.1.5. Dot plot
  #2.1.5.1. Reactive plot dimensions
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
  
  #2.1.5.2. Feature choices
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
  
  #2.1.5.3. Generate UI for dot plot
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
  
  #2.1.5.4. Generate dot plot content
  dot_plot_content <- reactive({
    #Only renders if condition C in 2.5.2 is met
    if (((input$diff_features_dot==FALSE)&(length(input$text_features)>=1))|((input$diff_features_dot==TRUE)&(length(input$dot_features)>=1))){
      #If user specifies the use of different features, use the dot plot-specific features instead of the generic text entry features
      if (input$diff_features_dot==TRUE){
        DotPlot(sobj,
                features = input$dot_features,
                group.by = input$dot_group_by) + RotatedAxis()
      }
      else {
        #Check if split.by is specified
        DotPlot(sobj, 
                features = input$text_features,
                group.by = input$dot_group_by) + RotatedAxis()
      }
    }
  })
  
  #2.1.5.5. Render dot plot UI and content
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
  
  #2.1.5.6. Dot Plot Download
  output$dot_download <- downloadHandler(
    filename = "Dot_plot.png",
    content = function(file){
      if (input$dot_manual_dim==TRUE){
        ggsave(file, 
               plot=dot_plot_content(), 
               device="png",
               width=umap_width()*5,
               height=umap_height()*5,
               dpi=300,
               units="px")
      } else {
        ggsave(file, 
               plot=dot_plot_content(), 
               device="png")
      }
    },#End content function
    contentType = "image/png"
  ) #End downloadHandler function
  
  #2.2. Create table with differential expression data
  #2.2.1. Define table content in reactive variable
  de_table_content <- reactive({
    #Determine which file to load based on user selections
    #First layer of conditionals: choice of assay
    if (input$table_assay=="RNA"){
      #Second layer of conditionals: choice of group.by variable
      if (input$table_group_by=="response"){
        #If response is chosen, display either resistant vs. sensitive or sensitive vs. resistant based on user selection
        if (input$response_ident_1=="Resistant"){filename <- "./Feature_Tables/uhg_resistant_vs_sensitive.tsv.gz"} 
        else {filename <- "./Feature_Tables/uhg_sensitive_vs_resistant.tsv.gz"}
      }
      else if (input$table_group_by=="htb"){
        #If patient id (htb) is chosen, display the table corresponding to the patient id desired for comparison
        #file name format: "<htb>_vs_all.tsv.gz"
        filename <- paste0("./Feature_Tables/",input$htb_ident_1,"_vs_all.tsv.gz")
      }
    }
    
    #Conditionals for ADT assay
    else if (input$table_assay=="ADT"){
      if (input$table_group_by=="response"){
        #If response is chosen, display either resistant vs. sensitive or sensitive vs. resistant based on user selection
        if (input$response_ident_1=="Resistant"){filename <- "./Feature_Tables/uhg_resistant_vs_sensitive_adt.tsv.gz"} 
        else {filename <- "./Feature_Tables/uhg_sensitive_vs_resistant_adt.tsv.gz"}
      }
      else if (input$table_group_by=="htb"){
        #If patient id (htb) is chosen, display the table corresponding to the patient id desired for comparison
        #file name format: "<htb>_vs_all.tsv.gz"
        filename <- paste0("./Feature_Tables/",input$htb_ident_1,"_vs_all_adt.tsv.gz")}
    }
    
    #Load file and display table
    table <- read_tsv(filename, show_col_types = FALSE) #show_col_types is set to FALSE to quiet a message printed to the console every time a table is loaded.
    #Creates a column ranked by adjusted p value
    table$p_adj_rank <- order(table$p_val_adj)
    
    #Format columns of table
    #p_val_adj, p_val: print in scientific format
    table$p_val_adj <- format(table$p_val_adj, digits=5, nsmall=2, scientific=TRUE) |> as.numeric()
    table$p_val <- format(table$p_val, digits=5, nsmall=2, scientific=TRUE) |> as.numeric()
    #avg_log2FC: float with 5 digits
    table$avg_log2FC <- format(table$avg_log2FC, digits=5, nsmall=2, scientific=FALSE) |> as.numeric()
    
    table
  })
  
  
  
  #2.2.2 Define download button UI
  de_button_ui<- eventReactive(input$de_submit,{ 
    
    })
  
  #2.2.3. Render DE Table and download button UI
  output$de_table <- renderDT({de_table_content()}, 
                              class="compact stripe cell-border",
                              #Sorts by adjusted p value column (6th column, zero index for Javascript)
                              options=list(order = list(list(5, 'asc'))),
                              rownames=FALSE,
                              selection='none',
                              escape = FALSE)

  output$de_download_button <- renderUI({de_button_ui()})
  
  #2.2.4. Download Handler for DE Table
  output$de_download <- downloadHandler(
    #Filename function: filename determined based on current user selections
    filename=function(){
      if (input$table_group_by=="response"){
        #If response is chosen, display either resistant vs. sensitive or sensitive vs. resistant based on user selection
        if (input$response_ident_1=="Resistant"){
          filename <- glue("DE_table_R-vs-S_{input$table_assay}.csv")
        } 
        else {
          filename <- glue("DE_table_S-vs-R_{input$table_assay}.csv")
        }
      }
      else if (input$table_group_by=="htb"){
        #If patient id (htb) is chosen, display the table corresponding to the patient id desired for comparison
        #file name format: "<htb>_vs_all.tsv.gz"
        filename <- glue("DE_Table_{input$htb_ident_1}-vs-all_{input$table_assay}.csv")
      }
      
      #Return filename computed above
      filename
    },
    content=function(file) {
      write.csv(de_table_content(), 
                file = file, 
                row.names = FALSE)
      },
    contentType = "text/csv"
  )
  
  ###2.3. Correlations Tab 
  
  #2.3.1 Reactive dropdown menu for patient 
  #Since patients fall into either the sensitive or resistant category, the patients dropdown will need to be updated to keep the user from choosing invalid combinations.
  #Menu will be updated in the future when variables such as treatment and time after diagnosis are added (ignoreInit prevents this from happening when app is initialized)
  #Running of code at startup is disabled with "ignoreInit=TRUE"
  observeEvent(c(input$response_selection),ignoreInit = TRUE,label="Reactive Patient Dropdown",{ 
    #Show a spinner while the valid patient ID's are calculated
    waiter_show(
      id = "corr_sidebar",
      html = spin_loaders(id=2, color = "#555588"),
      color = "#B1B1B188",
      hide_on_render = FALSE #Gives manual control of showing/hiding spinner
    )
    
    #Subset Seurat object for the selected response type and return vector of patients included in that type
    valid_patients <- unique(subset(sobj, subset = response %in% input$response_selection)$htb)
    #List of valid patients: indicates group of patient in dropdown menu
    valid_patients_categories=list(`d0/d30`=list(),
                                    `Dx/Rl`=list(),
                                    `Normal Bone Marrow`=list())
    #Sort valid patients into above framework
    for (patient in valid_patients){
      #Iterate through valid_patients vector and place each choice in the relevant category
      if(patient %in% c("1325","1650","1510","1526","1378","1724")){
        #Append above patient ids to d0/d30 category
        valid_patients_categories$`d0/d30` <- append(valid_patients_categories$`d0/d30`,patient)
      } else if (patient %in% c("1261","1467","719")){
        #Append above patient ids to Dx/Rl category
        valid_patients_categories$`Dx/Rl` <- append(valid_patients_categories$`Dx/Rl`,patient)
      } else if (patient %in% c("BMMC_1","BMMC_2","BMMC_3")){
        #Append above patient (sample) ids to normal bone marrow category
        valid_patients_categories$`Normal Bone Marrow` <- append(valid_patients_categories$`Normal Bone Marrow`,patient)
      }
    }
    #Sort patients in each list to display in order
    #Patients in d0/d30 and Dx/Rl are numeric values that can be sorted easily
    valid_patients_categories$`d0/d30` <- valid_patients_categories$`d0/d30` |> 
      as.numeric() |> 
      sort() |> 
      as.character() |> #Convert back to character values to avoid issues with further subsetting 
      as.list()
    valid_patients_categories$`Dx/Rl` <- valid_patients_categories$`Dx/Rl` |> 
      as.numeric() |> 
      sort() |> 
      as.character() |> 
      as.list()
    #Normal bone marrow column consists of character IDs that are sorted properly with sort()
    valid_patients_categories$`Normal Bone Marrow` <- valid_patients_categories$`Normal Bone Marrow` |> 
      as.character() |> 
      sort() |> 
      as.list()
    
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
 
  #2.3.2. Correlation table for selected feature and restriction criteria
  #Table updates only when the "Submit" button is clicked
  #2.3.2.1. Store table content as reactive value
  corr_table_content <- eventReactive(input$corr_submit,
                                      label="Corelation Table Content",
                                      ignoreInit = FALSE, 
                                      ignoreNULL = FALSE, {
    print("Running correlation table content code")
    #Reactive value for identifying a memory error (defined here and reset to FALSE each time the correlation table code is ran)
    rv$memory_error=FALSE
    rv$vector_mem_error=FALSE
    rv$other_error=FALSE
    
    # Only run the correlation table code if a feature has been specified
    if (input$corr_feature_selection != ""){
      #Show loading screen above main panel while table is computed (takes about a minute) 
      waiter_show(
        id = "corr_main_panel",
        html = spin_loaders(id=2, color = "#555588"),
        color = "#FFFFFF",
        hide_on_render = FALSE #Gives manual control of showing/hiding spinner
      )
      
      #Error handling: errors are frequent in this script, often due to memory limitations, and they will result in the spinner not disappearing from the main window since waiter_hide() exists at the end this code block. Therefore, the code in this block must be handled with tryCatch() to capture errors.
      tryCatch(
        #If an error is caught: attempt to determine type of error by inspecting message text with grepl (not recommended, but I currently don't know any other way to catch this error type)
        error = function(cnd){
          print(class(cnd$message))
          #Error 1: RAM error
          if (grepl("cannot allocate vector of size",cnd$message)){
            #This reactive value will instruct the correlation table UI to display differently based on the error
            rv$memory_error=TRUE
            #Define notification to be displayed to user upon memory error
            mem_err_ui <- icon_notification_ui(icon_name = "skull-crossbones",
                                               message = tagList(
                                                 "Memory Error: RAM is insufficient for analyzing the specified subset. Please narrow down the subset scope using the restriction criteria to the left, and feel free to ",
                                                 tags$a("let us know",
                                                        href="https://github.com/amc-heme/DataExploreShiny/issues",
                                                        target="_blank", #Opens link in new tab
                                                        rel="noopener noreferrer"),
                                                 " ",#Space after link 
                                                 "if you repeatedly recieve this error.")#End tagList
                                               )
            
            #Display notification
            showNotification(ui=mem_err_ui, 
                             #Duration=NULL will make the message persist until dismissed
                             duration = NULL,
                             id = "corr_mem_error",
                             session=session)
          }
          #Error 2: vector memory exhausted
          if (grepl("vector memory exhausted",cnd$message)){
            rv$vector_mem_error=TRUE
            
            #Define Notification UI
            vector_err_ui <- icon_notification_ui(icon_name = "skull-crossbones",
                                               message = tagList(
                                                 "Error: vector memory exhausted. Please ",
                                                 tags$a("report this issue",
                                                        href="https://github.com/amc-heme/DataExploreShiny/issues",
                                                        target="_blank", #Opens link in new tab
                                                        rel="noopener noreferrer"),
                                                 " ", #Space after link
                                                 "with a screenshot of the response criteria selected, and please narrow down the subset criteria for now.")#End tagList
            )
            
            #Display Notification
            showNotification(ui=vector_err_ui, 
                             #Duration=NULL will make the message persist until dismissed
                             duration = NULL,
                             id = "corr_vector_mem_error",
                             session=session)
          }
          #Notification for any unforseen error type
          else {
            rv$other_error=TRUE
            
            #Define Notification UI
            other_err_ui <- icon_notification_ui(icon_name = "skull-crossbones",
                                                  message = tagList(
                                                    glue("Error: {cnd$message}. Please "),
                                                    tags$a("report this issue ",
                                                           href="https://github.com/amc-heme/DataExploreShiny/issues",
                                                           target="_blank", #Opens link in new tab
                                                           rel="noopener noreferrer"),
                                                    "with a screenshot of the app window.")#End tagList
            )
            
            #Display Notification
            showNotification(ui=other_err_ui, 
                             #Duration=NULL will make the message persist until dismissed
                             duration = NULL,
                             id = "corr_other_error",
                             session=session)
          }
          
          #This will eventually be replaced with an error message to display to the user
          print("An error ocurred while computing correlation table code.")
          print(cnd$message)
          table <- NULL #Return nothing if an error occurs
        },#End error function
        #Begin tryCatch code
        {
          print("Make subset")
          #Form subset based on chosen criteria (store in reactive value so the subset can be accessed in the scatterplot function)
          rv$s_sub <- subset(sobj, 
                          subset=(clusters %in% input$cluster_selection) & 
                            (response %in% input$response_selection) & 
                            (htb %in% input$htb_selection) &
                            (treatment %in% input$treatment_selection)
          )
          
          ###Subset Stats
          print("Subset Stats")
          #Determine the proportion of cells with nonzero reads for the selected gene. If it is below the threshold defined at the top of this script, return a warning to the user.
          #Cells in subset
          rv$n_cells <- length(Cells(rv$s_sub))
          #Cells with nonzero reads
          rv$n_nonzero <- sum(rv$s_sub@assays$RNA@counts[input$corr_feature_selection,] != 0)
          #Proportion of nonzero reads
          rv$prop_nonzero <- rv$n_nonzero/rv$n_cells
          #Store as a percentage (format to show at least two digits after decimal point, and at least three sig figs)
          rv$percent_nonzero <- format(rv$prop_nonzero*100, digits=3, nsmall=2, scientific=FALSE)
          print(paste0("Percent nonzero: ",rv$percent_nonzero,"%"))
        
          #Notification if nonzero proportion is too low
          if (rv$prop_nonzero < nonzero_threshold){
            #Define notification UI (warning icon plus text)
            notification_ui <- span(
              #Warning icon (inline and enlarged)
              icon("exclamation-triangle", style="display: inline-block; font-size: 1.7em;"),
              #Notification text with proportion and number of non-zero cells
              span(glue("Low gene coverage: the selected feature was detected in {rv$percent_nonzero}% of cells within the selection restriction criteria ({rv$n_nonzero}/{rv$n_cells} cells). Correlation results may be inaccurate."),
                   #Font size of notification text 
                   style="font-size: 1.17em;")#End span
            )#End notification_ui span
            
            #Display notification UI
            showNotification(ui=notification_ui, 
                             #Duration=NULL will make the message persist until dismissed
                             duration = NULL,
                             id = "corr_high_zero_content",
                             session=session)
          } 
          ###
          
          print("Make Matrix")
          #Convert subset data to matrix and transpose so columns are gene names
          mat <- t(as.matrix(rv$s_sub@assays$RNA@data))
          
          print("Compute correlations")
          #Form correlation matrix
          table <- cor(mat[,input$corr_feature_selection],mat) |> #Compute correlation between selected feature and others
            t() |> #Code returns coefficients for each feature in rows (want columns) 
            enframe("Feature","Correlation_Coefficient") |> #Convert matrix to tibble
            filter(Feature != input$corr_feature_selection) |> #Filter out selected feature
            arrange(desc(Correlation_Coefficient)) #Arrange in descending order by correlation coeff
          
          #Round correlation coefficients to 5 digits
          table$Correlation_Coefficient <- format(table$Correlation_Coefficient, digits=5, nsmall=2, scientific=FALSE) |> as.numeric()
          
          })#End tryCatch
      
      #Hide loading screen
      waiter_hide(id = "corr_main_panel")
      
      #Return table for storage in corr_table_content()
      table
    }
  })
  
  #2.3.3. Correlations UI
  #2.3.3.1 Main UI
  #IgnoreNULL set to false to get UI to render at startup
  corr_ui <- eventReactive(input$corr_submit, 
                           label = "Correlation Main UI (Define Content)",
                           ignoreNULL = FALSE, {
    print("Correlation UI Function")
    #UI: if the feature selection menu is empty (default state at initialization), prompt user to enter features
    if (input$corr_feature_selection == ""){
      tags$h3("Enter a feature and press submit to view correlated features. You may also specify restriction criteria using the dropdown menus.")
    }
    #After a feature is applied and the submit button is pressed, display the table
    else {
      #Display the loading screen (screen will show until the end of the corr_table_content calculation is reached).
      waiter_show(
        id = "corr_main_panel",
        html = spin_loaders(id=2, color = "#555588"),
        color = "#FFFFFF",
        hide_on_render = FALSE #Gives manual control of showing/hiding spinner
      )
      
      #UI to display 
      div(
        tags$h2(glue("Genes correlated with {input$corr_feature_selection} in Subset")),
        #Restriction criteria section
        tags$h3("Selected Restriction Criteria"),
        #Make each input criteria appear inline
        div(div(tags$strong("Clusters: "),textOutput(outputId = "selected_clusters", inline = TRUE)),
            div(tags$strong("Response criteria: "),textOutput(outputId = "selected_response", inline = TRUE)),
            div(tags$strong("Patients: "),textOutput(outputId = "selected_htb", inline = TRUE))
            ),
        
        #Statistics section
        tags$h3("Quality Statistics for Gene and Subset"),
        div(div("(Subset created based on defined restriction criteria)"),
            div(tags$strong("Number of cells in subset: "),textOutput(outputId = "print_n_cells", inline = TRUE)),
            div(tags$strong(glue("Cells with non-zero reads for {input$corr_feature_selection}:")),textOutput(outputId = "print_nonzero", inline = TRUE))
            ),
        
        #Correlations table and plots
        tags$h3("Correlated Genes"),
        #Output: table is rendered inline with scatterplot, which is defined in a separate eventReactive function
        #Scatterplot only appears when the user makes a selection on the table
        #Add table container
        div(class="two-column",
            style="width: 40%; float: left;",
            #Use a DT data table
            DTOutput(outputId = "corr_table")
        ),
        
        div(class="two-column",
            style="width: 60%; float: right;",
            #UI for scatterplot rendered in separate eventReactive function
            uiOutput(outputId = "corr_scatter_ui")
            )
        )#End UI div
      }
  })
  
  #2.3.3.2 Correlations scatterplot UI
  #Computed separately from main UI since it responds to a different user input (clicking table)
  corr_scatter_ui <- eventReactive(input$corr_table_rows_selected,
                                      label="Correlation Scatterplot UI",
                                      ignoreNULL = FALSE,{
                                        #Display the graph if rows are selected
                                        if (length(input$corr_table_rows_selected)>0){
                                          #Use two-column CSS class for inline display
                                          plotOutput(outputId = "corr_scatter",height = "400px")     
                                        }
                                        })
  
  #2.3.4. Plot of feature selected from table
  #Row index of user selection from table is stored in input$corr_table_rows_selected.
  corr_scatter <- eventReactive(input$corr_table_rows_selected,
                                label="Correlation Scatterplot Content",{
    row_idx <- input$corr_table_rows_selected
    #Take action only if a row is selected (length of selection > 0) 
    if (row_idx>0){
      #Record gene name of row selected
      gene_selected <- as.character(corr_table_content()[row_idx,1])
      
      #Make and store scatterplot
      FeatureScatter(rv$s_sub, 
                     feature1 = input$corr_feature_selection, 
                     feature2 = gene_selected, 
                     group.by = "clusters")
    }
  })
  
  #2.3.5. Render Correlation UI, table, scatterplot, and statistics
  #Main UI
  output$corr_ui <- renderUI({corr_ui()})
  
  output$corr_scatter_ui <- renderUI({corr_scatter_ui()})

  #Table
  output$corr_table <- renderDT({corr_table_content()},
                                class="compact stripe cell-border hover",
                                selection="single",
                                filter="top",
                                colnames=c("Gene","Correlation Coefficient"),
                                rownames=FALSE)
  
  #Render correlation scatterplot
  observeEvent(input$corr_table_rows_selected, 
               label = "Render Corr Scatter", {
    output$corr_scatter <- renderPlot({corr_scatter()})
  })

  #Render Statistics
  observeEvent(input$corr_submit,
               label = "Render Statistics",{
                 #Rendering Selections and Stats for report
                 output$selected_clusters <- renderText(isolate(vector_to_text(input$cluster_selection)))
                 output$selected_response <- renderText(isolate(vector_to_text(input$response_selection)))
                 output$selected_htb <- renderText(isolate(vector_to_text(input$htb_selection)))
                 output$print_n_cells <- renderText(isolate(rv$n_cells))
                 output$print_nonzero <- renderText(isolate(glue("{rv$n_nonzero} ({rv$percent_nonzero}%)")))
               })

    
}

# Run the application 
shinyApp(ui = ui, server = server)