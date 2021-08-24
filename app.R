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
  titlePanel("scRNA-seq Visualization With Shiny"),
  
  #Sidebar layout: consists of a side panel and a main panel
  sidebarLayout(
    
    #Sidebar panel for user input
    sidebarPanel(
      #Menu for choosing plot options
      selectInput(inputId = "plot_type",label = "Choose Visualization",choices = c("UMAP"="umap","Violin Plot"="vln"),selected = "umap"),
      #Feature checkboxes for UMAP plot
      checkboxGroupInput(inputId = "features",label = "Choose feature(s) to display on plot:",choices = features),
    ),
    
    #Main panel for displaying plot output
    mainPanel(
      #If a violin plot is selected and no features are selected, direct the user to choose features for the plot
      conditionalPanel(condition = "input.plot_type=='vln' & input.features.length==0", tags$h4("Please select features to view on the violin plot.")),
      #If a valid selection is made for plot type and features, display the plot.
      conditionalPanel(condition = "!(input.plot_type=='vln' & input.features.length==0)",plotOutput(outputId = "main_plot")))  
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
  
  #Build plot based on checkbox selections
  output$main_plot <- renderPlot({
    #Determine plot type based on user choice in selection menu
    if (input$plot_type=="umap"){
      #UMAP Plot: if no feature checkboxes are selected, print a standard UMAP plot separated by clusters
      if (is.null(input$features)){
        DimPlot(sobj, label = TRUE, reduction = "umap")
        #Otherwise, plot a feature plot using the checked features
      } else {
        FeaturePlot(sobj,features = input$features)
      }
    } else if (input$plot_type=="vln"){
      #Violin plot, split by selected features
      #Only display plot if features are selected
      if (!is.null(input$features)){
        VlnPlot(sobj,features=input$features)
      }
    }
  })
}

# Run the application 
shinyApp(ui = ui, server = server)