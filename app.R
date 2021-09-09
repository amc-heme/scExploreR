### Load Libraries and Data; Define Variables###
#Initialize libraries
library(shiny)
library(Seurat)
library(ggplot2)
library(tidyverse)
library(dplyr)

#Load Seurat object 
#Currently using the sample AML dataset
#https://drive.google.com/file/d/1S7iGNzfmLX5g00zEgVX_Z98c6Xm0Bifb/view
sobj <- readRDS("./Seurat_Objects/uhg_seurat_intro.Rds")

#Features for dot plot checkboxes: for now I will include the top 15 genes differentially expressed in resistant patients vs. controls. 
features <- c("RPS26","RNASE1","PRSS21","CES1","PPP1R27","RND3","LAMP5","SAMHD1","ADGRG6","XIST","KCNE5","LY86","FCER2","BANK1")

#Define searchable features (for now this will be the row names in the Seurat object) 
valid_features <- rownames(sobj)

#Specify metadata variables to group and split by in drop down menus
meta_choices <- c("None"="none","Clusters"="clusters","Response"="response","Treatment"="treatment","Patient ID"="htb","Capture Number"="capture_num","Run"="run")

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
      sidebarPanel(
        ### 1.1.1.1 Checkboxes for choosing desired plot
        #Specify is UMAP Plot is desired
        checkboxInput(inputId = "make_umap",label = "Add UMAP plot", value = TRUE),
        #Specify if feature plot is desired
        checkboxInput(inputId = "make_feature",label = "Add feature plot",value=FALSE),
        #Specify if violin plot is desired
        checkboxInput(inputId = "make_vln",label = "Add violin plot", value=FALSE),
        #Specify if dot plot is desired
        checkboxInput(inputId = "make_dot",label = "Add dot plot", value=FALSE),
        
        ### 1.1.1.2. Feature Text Entry. Applies to feature, violin, and dot plots unless the user specifies the use of different features for each plot (currently only possible for dot plots) 
        conditionalPanel(condition="input.make_feature==true | input.make_vln==true | input.make_dot==true",
                         #Label
                         tags$p(tags$strong("Enter features to display on plots:")),
                         #Inline text entry and update button
                         div(style="vertical-align: top; margin-bottom: 0px;",
                             selectizeInput(inputId = "text_features", 
                                            multiple = TRUE, 
                                            label=NULL,
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
        
        ### Plot Specific Options ###
        #1.1.1.3. Options specific to UMAP: panel will display if UMAP is checked
        conditionalPanel(condition = "input.make_umap==true",
                         tags$h4("UMAP Specific Options"),
                         #Choose metadata to group UMAP by
                         selectInput(inputId = "umap_group_by", label = "Metadata to group by:", choices=meta_choices, selected = "clusters"),
                         #Choose metadata to split UMAP by
                         selectInput(inputId = "umap_split_by", label = "Metadata to split by:", choices=meta_choices, selected = "none")
                         ),#End 1.1.1.3.
        
        #1.1.1.4. Options specific to feature plot
        conditionalPanel(condition = "input.make_feature==true",
                         tags$h4("Feature Plot Specific Options"),
                         #Feature plots do not have a group.by argument
                         #Choose metadata to split feature plot by
                         selectInput(inputId = "feature_split_by", 
                                     label = "Metadata to split by:", 
                                     choices=meta_choices, 
                                     selected = "none")
                         ),#End 1.1.1.4
        
        #1.1.1.5. Options specific to violin plot
        conditionalPanel(condition = "input.make_vln==true",
                         tags$h4("Violin Plot Specific Options"),
                         #Choose metadata to group violin plot by
                         selectInput(inputId = "vln_group_by", 
                                     label = "Metadata to group by:", 
                                     choices=meta_choices[meta_choices %in% "none" == FALSE], #Remove "none" from selectable options to group.by 
                                     selected = "clusters"),
                         
                         #Choose metadata to split violin plot by
                         selectInput(inputId = "vln_split_by", 
                                     label = "Metadata to split by:", 
                                     choices=meta_choices, 
                                     selected = "none")
                         ), #End 1.1.1.5.
        
        #1.1.1.6. Options specific to dot plot
        conditionalPanel(condition = "input.make_dot==true",
                         tags$h4("Dot Plot Specific Options"),
                         
                         #Choose metadata to group dot plot by
                         selectInput(inputId = "dot_group_by", label = "Metadata to group by:", choices=meta_choices, selected = "clusters"),
                         
                         #Choose metadata to split dot plot by
                         selectInput(inputId = "dot_split_by", label = "Metadata to split by:", choices=meta_choices, selected = "none"),
                         
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
                                          )
                         ) #End 1.1.1.6 
      ), #End 1.1.1.
      
      ###1.1.2. Main panel for displaying plot output###
      mainPanel(
        #Panels for plots: display if checkboxes corresponding to each type are checked
        #1.1.2.1. UMAP plot panel
        conditionalPanel(condition = "input.make_umap==true",
                         plotOutput(outputId = "umap")),
        
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
                    choices = c("RNA","ADT"),
                    selected = "RNA",
                    label = "Choose assay to view"),
        
        #Additional text below assay choice panel
        "(RNA for diffential gene expression or ADT for surface protein expresssion)"
      ),
      
      ###1.2.2. Main Panel with Table
      mainPanel(dataTableOutput(outputId = "de_table"))
    )
  )
}

### Define user interface: code for navigation panel and references to tabs
ui <- navbarPage("Shiny scExplorer",
                 windowTitle="Shiny scExplorer",
                 tabPanel("Plots",
                          plots_tab()),
                 tabPanel("DE Tables",
                          tables_tab()))

### 2. Server function (builds interactive plot to display in UI) ###
server <- function(input,output,session){
  #2.1. Render feature choices for text feature selection
  updateSelectizeInput(session,inputId = "text_features", choices = valid_features, server = TRUE)
  
  #2.2. Create UMAP plot
  output$umap <- renderPlot({
    #Produce a single UMAP plot if no features to split by are specified
    if (input$umap_split_by=="none"){
      DimPlot(sobj, 
              group.by = input$umap_group_by, 
              label = TRUE, 
              reduction = "umap_harmony")
      
      #Otherwise, produce a UMAP split by the specified variable
    } else {
      DimPlot(sobj, 
              group.by = input$umap_group_by, 
              split.by = input$umap_split_by, 
              label = TRUE, 
              reduction = "umap_harmony")
    }
    
    
    
  })
  
  #Feature and Violin Plots: choose whether to render a plot or a message based on user inputs
  
  #2.3. Feature Plot 
  #2.3.1 Feature UI: Generate UI each time the feature submit button is clicked, the "make feature plot" check box is checked, or the split.by setting is updated. 
  feature_slot_UI <- eventReactive(c(input$make_feature, input$feature_split_by, input$text_features),{
    #Condition A: no features have been entered yet
    if (length(input$text_features)==0){
      #If this is the case, generate a message instructing the user to enter features.
      tags$h3("Please enter a feature to view plot.", style="margin-bottom: 10em;")
    }

    #Condition B: Features are selected
    else {
      #Generate a plot. Only the UI for the plot is shown here; content is in next eventReactive call.
      plotOutput(outputId = "feature_slot_plot")
    }
  })
  
  #2.3.2. Generate content for plot (but only if features are entered)
  feature_plot_content <- eventReactive(c(input$make_feature, input$feature_split_by, input$text_features),{
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
  
  #2.3.3. Render the UI and plot objects created above
  #Plot UI
  output$feature_slot <- renderUI({feature_slot_UI()})
  
  #Plot Content
  output$feature_slot_plot <- renderPlot({feature_plot_content()})
  
  #2.4. Violin plot
  #2.4.1. Code for conditional UI
  vln_slot_UI <- eventReactive(c(input$make_vln, input$vln_split_by, input$vln_group_by, input$text_features),{
    #Condition A: no features have been entered yet
    if (length(input$text_features)==0){
      #Generate a message instructing the user to enter features.
      tags$h3("Please enter a feature to view violin plot.", style="margin-bottom: 10em;")
    }
    
    #Condition B: one or more features are entered
    else {
      #Generate a plot. Only the UI for the plot is shown here; content is in next eventReactive call.
      plotOutput(outputId = "vln_slot_plot")
    }    
    
  })
  
  #2.4.2. Code for content
  vln_plot_content <- eventReactive(c(input$make_vln, input$vln_split_by, input$vln_group_by, input$text_features), {
    #Code will only run if features are entered
    if (length(input$text_features)>0){
      if (input$vln_split_by=="none"){
        VlnPlot(sobj, 
                features = input$text_features,
                group.by = input$vln_group_by)
      } else {
        VlnPlot(sobj, 
                features = input$text_features,
                group.by = input$vln_group_by,
                split.by = input$vln_split_by)
      }
    }
  })
  
  #2.4.3. Render UI and content for violin plot
  #Conditional UI
  output$vln_slot <- renderUI({vln_slot_UI()})
  
  #Plot content
  output$vln_slot_plot <- renderPlot({vln_plot_content()})
  
  #2.5. Dot plot
  #2.5.1. Feature choices
  #First observeEvent() function
  #The function below responds to each feature entered while the "use separate features for dot plot" checkbox is not checked. It is designed to load the selected options in the background before the user checks the box, making them immediately available when the box is checked 
  observeEvent(input$text_features,
               {if (input$diff_features_dot==FALSE){
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
  
  #2.5.2. Generate UI for dot plot
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
      plotOutput(outputId = "dot_slot_plot")
    }
    })
  
  #2.5.3. Generate dot plot content
  dot_plot_content <- reactive({
    #Only renders if condition C in 2.5.2 is met
    if (((input$diff_features_dot==FALSE)&(length(input$text_features)>=1))|((input$diff_features_dot==TRUE)&(length(input$dot_features)>=1))){
      #If user specifies the use of different features, use the dot plot-specific features instead of the generic text entry features
      if (input$diff_features_dot==TRUE){
        #Check if split.by is specified
        if (input$dot_split_by=="none"){
          DotPlot(sobj, 
                  features = input$dot_features,
                  group.by = input$dot_group_by) + RotatedAxis()
        }
        else {
          DotPlot(sobj, 
                  features = input$dot_features,
                  group.by = input$dot_group_by,
                  split.by = input$dot_split_by) + RotatedAxis()
        }
      }
      else {
        #Check if split.by is specified
        if (input$dot_split_by=="none"){
          DotPlot(sobj, 
                  features = input$text_features,
                  group.by = input$dot_group_by) + RotatedAxis()
        }
        else{
          DotPlot(sobj, 
                  features = input$text_features,
                  group.by = input$dot_group_by,
                  split.by = input$dot_split_by) + RotatedAxis()
        }
      }
    }
  })
  
  #2.5.4. Render dot plot UI and content
  #UI
  output$dot_slot <- renderUI({dot_slot_UI()})
  
  #Content
  output$dot_slot_plot <- renderPlot({dot_plot_content()})
  
  #2.6. Create table with differential expression data
  output$de_table <- renderDataTable({
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
    read_tsv(filename, show_col_types = FALSE) #show_col_types is set to FALSE to quiet a message printed to the console every time a table is loaded.
    
  }, escape = FALSE)

  
}

# Run the application 
shinyApp(ui = ui, server = server)