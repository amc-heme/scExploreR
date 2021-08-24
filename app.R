#Initialize libraries
library(shiny)
library(Seurat)
library(ggplot2)
#Load Seurat object 
#The PMBC3k data will be used in the initial version of the app
sobj <- readRDS("./Seurat_Objects/pbmc3k_final.RDS")
#Load selectable features
features <- c("LYZ", "CCL5", "IL32", "PTPRCAP", "FCGR3A", "PF4")

#Define user interface
ui <- fluidPage(
  titlePanel("scRNA-seq visualization with Shiny"),
  #Sidebar layout: store the selectable options in the sidebar panel, and the plots in the main panel
  sidebarLayout(
    #Sidebar panel
    sidebarPanel(
      #Feature checkboxes for UMAP plot
      checkboxGroupInput(inputId = "features",label = "Choose feature to display/split UMAP by:",choices = features),
    ),
    #Main panel
    mainPanel(plotOutput(outputId = "umap"))  
  )
)  

#Server function (builds interactive plot to display in UI)
server <- function(input,output){
  
  #Print Features selected (for debugging purposes only)
  output$selected <- renderText({
    if (is.null(input$features)){
      print("No features selected.")
    } else {
      paste0(c("Features selected:\n",paste(input$features, sep=","))
      )
    }
  })
  
  #Build UMAP plot based on checkbox selections
  output$umap <- renderPlot({
    #If no feature checkboxes are selected, print a standard UMAP plot separated by clusters
    if (is.null(input$features)){
      DimPlot(sobj, label = TRUE, reduction = "umap")
      #Otherwise, plot a feature plot using the checked features
      } else {
      FeaturePlot(sobj,features = input$features)
    }
  })
}

# Run the application 
shinyApp(ui = ui, server = server)