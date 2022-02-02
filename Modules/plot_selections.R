#Plot Selections Module

#Displays selections menus for an individual plot and processes user entries
plot_selections_ui <- function(id,
                               meta_choices,
                               plot_label = "",
                               #TEMP: conditionals vertically aligned
                               #for multi-cursor editing
                               group_by =           FALSE,
                               split_by =           FALSE,
                               ncol_slider =        FALSE,
                               label_checkbox =     FALSE,
                               legend_checkbox =    FALSE,
                               limits_checkbox =    FALSE,
                               manual_dimensions =  FALSE,
                               download_button =    FALSE
                               ){
  #Namespace function: prevents conflicts with IDs defined in other modules 
  ns <- NS(id)
  
  #UI: elements are added to tagList if specified when calling the module ui 
  #Attempted to use ifelse() for this; ifelse() did not print Shiny tags 
  #properly and was unable to process NULL
  tagList(
    #Group by menu
    if (group_by == TRUE){
      #If TRUE, add element
      selectInput(
        inputId = ns("group_by"), 
        label = "Metadata to Group by:",
        #Can select all options except "none"
        choices= meta_choices[!meta_choices %in% "none"], 
        #First option selected by default 
        selected = meta_choices[1]
      )
      #Do not add element if FALSE
      } else NULL,
    
    #Split by menu
    if (split_by == TRUE){
      selectInput(
        inputId = ns("split_by"), 
        label = "Metadata to Split By:",
        #Use vector of included metadata category names from the config file
        choices= meta_choices,  
        #"none" selected by default
        selected = "none"
      )
      } else NULL,
    
    #Slider to adjust number of columns
    if (ncol_slider == TRUE){
      #Dynamic UI (appears when split_by != "none")
      uiOutput(outputId = ns("ncol_slider"))
      } else NULL,
    
    #Checkbox to add/remove labels
    if (label_checkbox == TRUE){
      checkboxInput(
        inputId = ns("label"),
        label = "Label Groups",
        value = TRUE
      )
      } else NULL,
    
    #Checkbox to add or remove Legend
    if (legend_checkbox == TRUE){
      checkboxInput(
        inputId = ns("legend"),
        label = "Include Legend",
        value = TRUE
      )
      } else NULL,
    
    #Checkbox to specify original axes limits
    if (limits_checkbox == TRUE){
      #Dynamic UI: displays when a subset is selected
      uiOutput(outputId=ns("limits_checkbox"))
      } else NULL,
    
    #UI for user control of plot dimensions
    if (manual_dimensions == TRUE){
      #If TRUE, call function for manual adjustment of plot dimensions
      #Todo: modify function to accomodate module namespacing
      tags$p("Manual dimensions")
      #manual_dim_UI(plot_type = "umap"),
      } else NULL,
    
    #UI for download button
    if (download_button == TRUE){
      downloadButton(
        outputId = ns("download"), 
        label=glue("Download {plot_label}")
        )
      } else NULL
    )
}

plot_selections_server <- function(id,
                                   subset, #Reactive
                                   subset_submit_button, #Reactive
                                   collapsible_panel, #Reactive
                                   plot_label, #Non-reactive
                                   n_cells_original #Non-reactive
                                   ){
  moduleServer(id,
               function(input,output,session){
                 #1. Record plot options 
                 group_by <- reactive({
                   req(input$group_by)
                   input$group_by
                   })
                 
                 split_by <- reactive({
                   req(input$split_by)
                   input$split_by
                   })
                 
                 labels <- reactive({
                   req(input$label)
                   input$label
                   })
                 
                 legend <- reactive({
                   req(input$legend)
                   input$legend
                   })
                 
                 download_button <- reactive({
                   req(input$download)
                   input$download
                   })
                 
                 #Values below gathered from conditional UI in 2.
                 ncol <- reactive({
                   req(input$ncol)
                   input$ncol
                   })
                 
                 original_limits <- reactive({
                   req(input$original_limits)
                   input$original_limits
                   })
                 
                 #2. Conditional UI
                 #2.1. ncol slider: appears when split_by != "none"
                 ncol_slider <-
                   eventReactive(
                     c(split_by(), 
                       subset_submit_button()
                       ),
                     ignoreNULL = TRUE,
                     {
                       #Do not render when split_by is "none"
                       if (split_by() == "none"){
                         NULL
                       } else {
                         #Number of panels: used to set bounds of ncol slider
                         #Number of panels is equal to the number of unique 
                         #values for the chosen metadata category
                         n_panel <-
                           plots_subset()@meta.data[[split_by()]] |> 
                           unique() |> 
                           length()

                         #Determine default value for ncol
                         #For less than four panels, this is equal to the 
                         #number of panels. 
                         if (n_panel < 4){
                           default_col <- n_panel
                           #For 4 or more panels, the default value is 2
                         } else {
                           default_col <- 2
                         }
                         
                         #Create slider input
                         sliderInput(
                           inputId = ns("ncol"),
                           label = "Number of Columns: ",
                           min = 1,
                           #Max value: equal to the number of levels 
                           #in the given variable
                           max = n_panel,
                           #Only allow integer values
                           step = 1, 
                           ticks = FALSE,
                           value = default_col
                         )
                         } #End else
                     })
                 
                 #2.2. Checkbox to Specify Original Axis Limits
                 limits_checkbox <- 
                   eventReactive(
                     c(subset_submit_button(), 
                       collapsible_panel()
                       ),
                     label = glue("{plot_label} Limits UI"),
                     {
                       #Checkbox will only appear when a subset is selected. 
                       #The presence of a subset will be tested by observing 
                       #the number of cells in the subset
                       if (n_cells_original != ncol(subset())) {
                         checkboxInput(
                           inputId = "original_limits",
                           label = "Use Original Axes Limits",
                           value = FALSE
                           )
                         } else {
                           #Display nothing when the number of cells are equal 
                           #between the subset and the full dataset
                           NULL
                         }
                       })
                 
                 #2.3. Render Dynamic UI
                 output$ncol_slider <- renderUI({
                   ncol_slider()
                 })
                 
                 output$limits_checkbox <- renderUI({
                   limits_checkbox()
                 })
                 
                 #3. Return values from server
                 
                 # return_list <- 
                 #   reactive({
                 #     list(
                 #       if(group_by()){}
                 #         )
                 #     })
                 
                 })
  }
