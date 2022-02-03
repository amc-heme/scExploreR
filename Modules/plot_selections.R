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
      #If TRUE, call module for manual adjustment of plot dimensions
      manual_dimensions_ui(id = ns("manual_dim"))
      } else NULL,
    
    #UI for download button
    if (download_button == TRUE){
      downloadButton(
        outputId = ns("download"), 
        label=glue("Download {plot_label}")
        )
      } else NULL,
    
    #TEMP: Print outputs
    "Manual Dimensions Output",
    verbatimTextOutput(outputId = ns("manual_dim_output")),
    
    verbatimTextOutput(outputId = ns("truthy_output")),
    
    verbatimTextOutput(outputId = ns("selections_output"))
    )
}


#manual_dimensions: creates a server instance for specifying manual dimensions 
#if TRUE. This should be set to TRUE if manual_dimensions is also true in the UI
plot_selections_server <- function(id,
                                   subset, #Reactive
                                   subset_submit_button, #Reactive
                                   collapsible_panel, #Reactive
                                   plot_label, #Non-reactive
                                   n_cells_original, #Non-reactive
                                   manual_dimensions = TRUE #Non-reactive
                                   ){
  moduleServer(id,
               function(input,output,session){
                 #Server namespace function: for dynamic UI and modules
                 ns <- session$ns
                 
                 #1. Manual Dimensions Module Server ---------------------------
                 manual_dim <- manual_dimensions_server(id = "manual_dim")
                 
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
                 
                 # download_button <- reactive({
                 #   req(input$download)
                 #   input$download
                 #   })
                 
                 #Values below gathered from conditional UI in 2.
                 # ncol <- reactive({
                 #   req(input$ncol)
                 #   input$ncol
                 #   })
                 
                 original_limits <- reactive({
                   req(input$original_limits)
                   input$original_limits
                   })
                 
                 #Reactive list for storing selected inputs
                 plots_selections <- reactive({
                   list(
                     #If an input is created for the property 
                     #(hasName(input, property) == TRUE), add it to the list.
                     #Otherwise, put "NULL" for the property.
                     `group_by` = 
                       if(hasName(input,"group_by")){
                         input$group_by
                         } else NULL,
                     `split_by` = 
                       if(hasName(input,"split_by")){
                         input$split_by
                       } else NULL,
                     `ncol` = 
                       #Consider a special conditional for ncol 
                       #(input$split_by != "none") 
                       #input$ncol will still have a value if input$split by is 
                       #changed from a metadata category to "none" 
                       if(hasName(input,"ncol")){
                         input$ncol
                       } else NULL,
                     `legend` = 
                       if(hasName(input,"legend")){
                         input$legend
                       } else NULL,
                     `label` = 
                       if(hasName(input,"label")){
                         input$label
                       } else NULL,
                     `limits` = 
                       if(hasName(input,"original_limits")){
                         input$original_limits
                       } else NULL
                   )
                 })
                 
                 #TEMP: "truthy" test to automatically 
                 #Idea is similar to shiny::req(), but I don't want to stop 
                 #downstream execution (what req() does by default)
                 output$truthy_output <- renderPrint({
                   print(glue("group_by: {isTruthy(input$group_by)}"))
                   print(glue("split_by: {isTruthy(input$split_by)}"))
                   print(glue("label: {isTruthy(input$label)}"))
                   print(glue("legend: {isTruthy(input$legend)}"))
                   print(glue("label: {isTruthy(input$ncol)}"))
                   print(glue("limits: {isTruthy(input$original_limits)}"))
                   print(glue("collapsible panel: {isTruthy(collapsible_panel())}"))
                   print(glue("nonsense: {isTruthy(input$xyz)}"))
                   print("\n hasName outputs")
                   print(glue("hasName (nonsense): {hasName(input,'xyz')}"))
                   print(glue("hasName (group_by): {hasName(input,'group_by')}"))
                   print(glue("split_by: {hasName(input,'split_by')}"))
                   print(glue("ncol: {hasName(input,'ncol')}"))
                   print(glue("label: {hasName(input,'label')}"))
                   print(glue("legend: {hasName(input,'legend')}"))
                   print(glue("limits: {hasName(input,'original_limits')}"))
                   print(glue("collapsible panel: {!is.null(collapsible_panel())}"))
                 })
                 
                 output$selections_output <- renderPrint({
                   plots_selections()
                 })
              
                 # print_manual_dim <- reactive({
                 #   list(
                 #     `width` = manual_dim$width(),
                 #     `height` = manual_dim$height()
                 #     )
                 # })
                 
                 output$manual_dim_output <- renderPrint({
                   manual_dim
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
                           subset()@meta.data[[split_by()]] |> 
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
                       n_cells_subset <- 
                         subset() |> 
                         Cells() |> 
                         length()
                       
                       if (n_cells_original != n_cells_subset) {
                         checkboxInput(
                           inputId = ns("original_limits"),
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
