#Initialize libraries
library(shiny)
library(Seurat)
library(ggplot2)

#Load Seurat object 
#Currently using the sample AML dataset
#https://drive.google.com/file/d/1S7iGNzfmLX5g00zEgVX_Z98c6Xm0Bifb/view
sobj <- readRDS("./Seurat_Objects/uhg_seurat_intro.Rds")

#Selectable features: for now I will include the top 15 genes differentially expressed in resistant patients vs. controls (will hard code this because calculating the features takes about 3 minutes on my computer)
features <- c("RPS26","RNASE1","PRSS21","CES1","PPP1R27","RND3","LAMP5","SAMHD1","ADGRG6","XIST","KCNE5","LY86","FCER2","BANK1")

#Define user interface
ui <- fluidPage(
  titlePanel(tags$h1(HTML("<center>scRNA-seq Visualization With Shiny</center>"))),
  
  #Sidebar layout: consists of a side panel and a main panel
  sidebarLayout(
    
    #Sidebar panel for user input
    sidebarPanel(
      #Menu for choosing plot options
      #Specify is UMAP Plot is desired
      checkboxInput(inputId = "make_umap",label = "Add UMAP plot", value = TRUE),
      
      #Specify if feature plot is desired
      checkboxInput(inputId = "make_feature",label = "Add feature plot",value=FALSE),
      
      #Specify if violin plot is desired
      checkboxInput(inputId = "make_vln",label = "Add violin plot", value=FALSE),
      
      #Menu for choosing metadata to group by
      selectInput(inputId = "group_by", label = "Choose metadata to group plot by:", choices=c("Clusters"="clusters","Response"="response","Treatment"="treatment","HTB"="htb","Capture Number"="capture_num","Run"="run"), selected = "clusters"),
      
      #Menu for choosing metadata to split by
      selectInput(inputId = "split_by", label = "Choose metadata to split plot by:", choices=c("None"="none","Clusters"="clusters","Response"="response","Treatment"="treatment","HTB"="htb","Capture Number"="capture_num","Run"="run"), selected = "none"),
      
      #Feature checkboxes for UMAP plot
      checkboxGroupInput(inputId = "features",label = "Choose feature(s) to display on plot:",choices = features),
    ),
    
    #Main panel for displaying plot output
    mainPanel(
      #Panels for plots: display if checkboxes corresponding to each type are checked
      #UMAP plot panel
      conditionalPanel(condition = "input.make_umap==true",
                       plotOutput(outputId = "umap")),
      
      #Panel for feature plot
      conditionalPanel(condition = "input.make_feature==true",
                       plotOutput(outputId = "feature")),
      
      #Violin plot panel
      conditionalPanel(condition = "input.make_vln==true",
                       plotOutput(outputId = "vln")),
      
      #If a violin plot is selected and no features are selected, direct the user to choose features for the plot
      conditionalPanel(condition = "input.plot_type=='vln' & input.features.length==0", tags$h4("Please select features to view on the violin plot.")),
      )  
  )
)  

#Server function (builds interactive plot to display in UI)
server <- function(input,output){
  
  #Create UMAP plot
  output$umap <- renderPlot({
    #Produce a single UMAP plot if no features to split by are specified
    if (input$split_by=="none"){
      DimPlot(sobj, 
              group.by = input$group_by, 
              label = TRUE, 
              reduction = "umap")
      #Otherwise, produce a UMAP split by the specified variable
    } else {
      DimPlot(sobj, 
              group.by = input$group_by, 
              split.by = input$split_by, 
              label = TRUE, 
              reduction = "umap")
    }
    
    
    
  })
  
  #Render Feature plot 
  output$feature <- renderPlot({
    #To avoid errors, only render plot if features are selected.
    if (!is.null(input$features)){
      #Split plot by variable if it is specified
      if (input$split_by=="none"){
        FeaturePlot(sobj, 
                    features=input$features)
      } else {
        FeaturePlot(sobj, 
                    features=input$features,
                    split.by = input$split_by)
      }
    }
  })
  
  
  #Create Violin plot
  output$vln <- renderPlot({
    if (!is.null(input$features)){
      VlnPlot(sobj, 
              features = input$features,
              group.by = input$group_by,
              split.by = input$split_by)
    }
    
  })
  
}

# Run the application 
shinyApp(ui = ui, server = server)