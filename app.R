### Load Libraries and Data; Define Variables ###
#Initialize libraries
library(shiny)
library(Seurat)
library(ggplot2)
library(tidyverse)
library(dplyr)
library(shinyWidgets)
library(rintrojs)
library(shinydashboard)

#Load Seurat object 
#Currently using the sample AML dataset
#https://drive.google.com/file/d/1S7iGNzfmLX5g00zEgVX_Z98c6Xm0Bifb/view
sobj <- readRDS("./Seurat_Objects/uhg_seurat_intro.Rds")

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
meta_choices <- c("None"="none",
                  "Clusters"="clusters",
                  "Response"="response",
                  "Treatment"="treatment",
                  "Patient ID"="htb",
                  "Capture Number"="capture_num",
                  "Run"="run")

### Functions Used 
### Manual_dim_UI ###
#Creates two slider-text box pairs for manual control of theheight and width of a plot.
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
                         collapsible_panel(label="UMAP Specific Options",
                                           active=TRUE,
                                           #Choose metadata to group UMAP by
                                           selectInput(inputId = "umap_group_by", 
                                                       label = "Metadata to group by:",
                                                       choices=meta_choices[meta_choices %in% "none" == FALSE], #Remove "none" from selectable options to group by
                                                       selected = "clusters"),
                                           #Choose metadata to split UMAP by
                                           selectInput(inputId = "umap_split_by", label = "Metadata to split by:", choices=meta_choices, selected = "none"),
                                           #UI for user control of plot dimensions, if desired
                                           manual_dim_UI(plot_type = "umap"),
                                           #Download button (plot specific)
                                           downloadButton(outputId = "umap_download",label="Download UMAP")
                                           )#End collapsible panel
                         ),#End 1.1.1.3.
        
        #1.1.1.4. Options specific to feature plot
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
                         ),#End 1.1.1.4
        
        #1.1.1.5. Options specific to violin plot
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
                                           
                                           #UI for user control of plot dimensions, if desired
                                           manual_dim_UI(plot_type = "vln"),
                                           #Download button (plot specific)
                                           downloadButton(outputId = "vln_download",label="Download Violin Plot")
                                           )#End collapsible panel
                         ), #End 1.1.1.5.
        
        #1.1.1.6. Options specific to dot plot
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
                         ) #End 1.1.1.6 
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
}#End 1.2.

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
                      tables_tab())),
  #Help button - Creates a Dropdown menu when clicked
  #Button should appear in the upper right hand corner of the navbar menu
  #This will be achieved with the button_wizzard.js script
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
                         #Guided tour link: wrapped in introBox
                         introBox(data.step=1,
                                  data.intro='Welcome to the Shiny app.<br><br>Please click the ">" button to continue with the tour, or click "skip" to proceed to the app.',
                                  data.position = "left",
                                  #introBox content
                                  actionLink(inputId = "start_intro",
                                             class="blue_hover",
                                             label = "Guided Tour")
                                  ), #End introBox
                         introBox(data.step = 2,
                                  data.intro = 'For more detailed help, select "detailed walkthrough". For more information on interpereting the plots in this app, select "Interpereting scRNA-seq Plots".',
                                  data.position="left",
                                  data.hint = "Hint text",
                                  #introBox content
                                  tags$a(href="#",
                                         class="blue_hover",
                                         "Interpereting scRNA-seq Plots"),
                                  
                                  tags$a("Detailed Walkthrough",
                                         href="Shiny_Vignette.html",
                                         class="blue_hover",
                                         target="_blank", #Opens link in new tab
                                         rel="noopener noreferrer", #Cybersecurity measure for links that open in new tab: prevents tabnapping
                                         )
                                  )#End introBox
             
                 )#End tagList
                 ),#End dropdownButton
  includeScript("www/collapsible_panel.js"),
  includeScript("www/button_wizzard.js")
)

### 2. Server function (builds interactive plot to display in UI) ###
server <- function(input,output,session){
  #2.1. Initialize Session
  #2.1.1. Render feature choices for text feature selection
  updateSelectizeInput(session,inputId = "text_features", choices = valid_features, server = TRUE)
  
  #Open Help Dropdown and initialize guided tour
  toggleDropdownButton(inputId="help", session = session)
  introjs(session,
          options = list("nextLabel" = ">",
                         "prevLabel" = "<",
                         "skipLabel" = "skip",
                         "overlayOpacity" = -1
                         ),
          events = list(onbeforechange = readCallback("switchTabs"))
          )
  
  #Open the intro every time the user clicks the "guided tour" button
  observeEvent(input$start_intro, {
    print("Detected link")
    introjs(session,
            options = list("nextLabel" = ">",
                           "prevLabel" = "<",
                           "skipLabel" = "skip",
                           "overlayOpacity" = -1
                           ),
            events = list(onbeforechange = readCallback("switchTabs"))
            )
  })
  
  #2.2. UMAP plot
  #2.2.1. Reactive UMAP plot dimensions
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
  
  #2.2.2. Generate UI for UMAP plot: renders a plotOutput() with either automatic or manually specified dimensions based on user specifications
  umap_UI <- reactive({
    if (input$umap_manual_dim==FALSE){
      plotOutput(outputId = "umap_slot_plot")
    } else {
      plotOutput(outputId = "umap_slot_plot",
                 width = umap_width(),
                 height = umap_height())
    }
  })
  
  #2.2.3. Define UMAP Plot Content
  #Plot content is defined separately in a reactive context, to be rendered later with the UI.
  umap_plot_content <- reactive({
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
  
  #2.2.4. Render UI
  output$umap_slot <- renderUI({umap_UI()})
  
  #2.2.5. Render UMAP plot, with manual or automatic dimensions as specified
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
  
  #2.2.6. Download UMAP Plot
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
  
  #2.3. Feature Plot 
  #2.3.1 Reactive dimensions
  
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
  
  #2.3.2 Feature UI
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
  
  #2.3.3. Generate content for plot (but only if features are entered)
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
  
  #2.3.4. Render the UI and plot objects created above
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
  
  #2.3.5. Feature Plot Download
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
  
  #2.4. Violin plot
  #2.4.1 Reactive plot dimensions
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
  
  #2.4.2. Code for conditional UI
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
  
  #2.4.3. Code for content
  vln_plot_content <- reactive({
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
  
  #2.4.4. Render UI and content for violin plot
  output$vln_slot <- renderUI({vln_slot_UI()})
  
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
  
  #2.4.5. Violin Plot Download
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
  
  #2.5. Dot plot
  #2.5.1. Reactive plot dimensions
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
  
  #2.5.2. Feature choices
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
  
  #2.5.3. Generate UI for dot plot
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
  
  #2.5.4. Generate dot plot content
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
  
  #2.5.5. Render dot plot UI and content
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
  
  #2.5.6. Dot Plot Download
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