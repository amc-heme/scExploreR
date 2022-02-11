# Plot Selections Module

# Displays selections menus for an individual plot and processes user entries
plot_selections_ui <- function(id,
                               # The plot_selections UI consists of the options
                               # panels and the plot output, which exist in 
                               # different places in the app. This argument will
                               # allow the module server to update components in
                               # different locations
                               ui_component = c("options", "plot"),
                               meta_choices = NULL,
                               plot_label = "",
                               # TEMP: conditionals vertically aligned
                               # for multi-cursor editing
                               group_by =           FALSE,
                               split_by =           FALSE,
                               ncol_slider =        FALSE,
                               label_checkbox =     FALSE,
                               legend_checkbox =    FALSE,
                               limits_checkbox =    FALSE,
                               manual_dimensions =  FALSE,
                               download_button =    FALSE
                               ){
  # Namespace function: prevents conflicts with IDs defined in other modules 
  ns <- NS(id)
  
  if (ui_component == "options"){
    # UI for plot options
    # Elements are added to tagList if specified when calling the module ui 
    # Attempted to use ifelse() for this; ifelse() did not print Shiny tags 
    # properly and was unable to process NULL
    tagList(
      # Group by menu
      if (group_by == TRUE){
        # If TRUE, add element
        selectInput(
          inputId = ns("group_by"), 
          label = "Metadata to Group by:",
          # Can select all options except "none"
          choices= meta_choices[!meta_choices %in% "none"], 
          # First option selected by default 
          selected = meta_choices[1]
        )
        # Do not add element if FALSE
      } else NULL,
      
      # Split by menu
      if (split_by == TRUE){
        selectInput(
          inputId = ns("split_by"), 
          label = "Metadata to Split By:",
          # Use vector of included metadata category names from the config file
          choices= meta_choices,  
          #"none" selected by default
          selected = "none"
        )
      } else NULL,
      
      # Slider to adjust number of columns
      if (ncol_slider == TRUE){
        #Dynamic UI (appears when split_by != "none")
        uiOutput(outputId = ns("ncol_slider"))
      } else NULL,
      
      # Checkbox to add/remove labels
      if (label_checkbox == TRUE){
        checkboxInput(
          inputId = ns("label"),
          label = "Label Groups",
          value = TRUE
        )
      } else NULL,
      
      # Checkbox to add or remove Legend
      if (legend_checkbox == TRUE){
        checkboxInput(
          inputId = ns("legend"),
          label = "Include Legend",
          value = TRUE
        )
      } else NULL,
      
      # Checkbox to specify original axes limits
      if (limits_checkbox == TRUE){
        # Dynamic UI: displays when a subset is selected
        uiOutput(outputId=ns("limits_checkbox"))
      } else NULL,
      
      # UI for user control of plot dimensions
      if (manual_dimensions == TRUE){
        #If TRUE, call module for manual adjustment of plot dimensions
        manual_dimensions_ui(id = ns("manual_dim"))
      } else NULL,
      
      # UI for download button
      if (download_button == TRUE){
        downloadButton(
          outputId = ns("download"), 
          label=glue("Download {plot_label}")
        )
      } else NULL,
      
      #TEMP
      verbatimTextOutput(outputId = ns("temp"))
    )
    
  } else if (ui_component == "plot"){
    # UI for plot output 
    # Conditional UI: displays if the "make *" switch for the plot is turned on 
    # in the plots tab
    uiOutput(outputId = ns("plot_output_ui"))
  }
}


# manual_dimensions: creates a server instance for specifying manual dimensions 
# if TRUE. This should be set to TRUE if manual_dimensions is also true in the UI
# Object: the Seurat object to be used for plotting. It may be a subset or the 
# full object.
# plot_switch: Switch in the plots tab specifying whether the user wishes to see
# the plot created in this server function 
# plot_label: The name for this plot type, in a format desired for display
# n_cells_original: The number of cells 
# manual_dimensions: a boolean specifying whether to create an instance of the 
# manual_dimensions server. This should be true when manual_dimensions is TRUE
# in the UI function for this module.
# plot_type: the type of plot to create from the selected options.
# xlim_orig: the x limits of the dimplot of the full Seurat object (before a
# subset is created). This only applies to dimplots and feature plots.
# ylim_orig: the y limits of the dimplot of the full Seurat object.
plot_selections_server <- function(id,
                                   object, #Reactive
                                   plot_switch, #Reactive
                                   plot_label, #Non-reactive
                                   n_cells_original, #Non-reactive
                                   features_entered = NULL, #Reactive 
                                   manual_dimensions = TRUE, #Non-reactive
                                   plot_type = c("dimplot",
                                                 "feature",
                                                 "violin",
                                                 "dot"), #Non-reactive
                                   xlim_orig = NULL, #Non-reactive
                                   ylim_orig = NULL, #Non-reactive
                                   #Currently only needed for feature plots
                                   assay_info = NULL #Non-reactive
                                   ){
  moduleServer(id,
               function(input,output,session){
                 # Server namespace function: for dynamic UI and modules
                 ns <- session$ns
                 
                 # 1. Manual Dimensions Module Server --------------------------
                 if (manual_dimensions == TRUE){
                   manual_dim <- manual_dimensions_server(id = "manual_dim")
                   }
                 
                 output$temp <- renderPrint(print_reactive_list(manual_dim))
                 
                 # 2. Record plot options --------------------------------------
                 #list of reactives for storing selected inputs
                 plot_selections <- 
                   list(
                     # Group_by
                     `group_by` = reactive({
                       if("group_by" %in% isolate(names(input))){
                         input$group_by
                         } else NULL
                       }),
                     
                     # Split_by
                     `split_by` = reactive({
                       if("split_by" %in% isolate(names(input))){
                         input$split_by
                         } else NULL
                     }),
                     
                     # Number of columns in multi-panel plot
                     # Special conditional used 
                     # input$ncol will still have a value if input$split by is 
                     # changed from a metadata category to "none" 
                     `ncol` = reactive({
                       if (input$split_by != "none"){
                         input$ncol
                       } else NULL
                     }),
                     
                     # Include legend
                     `legend` = reactive({
                       if("legend" %in% isolate(names(input))){
                         input$legend
                       } else NULL
                     }),
                     
                     # Label groups
                     `label` = reactive({
                       if("label" %in% isolate(names(input))){
                         input$label
                       } else NULL
                     }),
                     
                     # Original axes limits
                     `limits` = reactive({
                       if("original_limits" %in% isolate(names(input))){
                         input$original_limits
                       } else NULL
                     })
                   )
                 
                 # 3. Determine if a subset has been used  ----------------------
                 # This variable will be a boolean used in downstream 
                 # computations
                 is_subset <- eventReactive(
                   label = glue("{plot_label}: Test if Object is a Subset"),
                   object(),
                   ignoreNULL = FALSE,
                   {
                     # Throw an error if the subset does not exist or is NULL
                     validate(
                       need(
                         object(),
                         message = "subset is NULL"
                         )
                     )

                     # Compute number of cells in subset
                     n_cells_subset <-
                       object() |>
                       Cells() |>
                       length()

                     # Test if the number of cells in the subset differs from
                     # the number of cells in the original object. If this
                     # conditional is TRUE, then the object read is a subset
                     n_cells_original != n_cells_subset
                 })
                 
                 # 4. Conditional UI -------------------------------------------
                 ## 4.1. ncol slider ####
                 # Conditions under which ncol slider appear differ based on 
                 # plot type
                 if (plot_type == "dimplot"){
                   # UMAP plots: appears when split_by != "none"
                   ncol_slider <-
                     eventReactive(
                       c(plot_selections$split_by(),
                         object()
                       ),
                       label = glue("{plot_label}: Make ncol Slider"),
                       ignoreNULL = TRUE,
                       {
                         #Do not render when split_by is "none"
                         if (plot_selections$split_by() == "none"){
                           NULL
                         } else {
                           # Number of panels: used to set bounds of ncol slider
                           # Number of panels is equal to the number of unique
                           # values for the chosen metadata category
                           n_panel <-
                             object()@meta.data[[plot_selections$split_by()]] |>
                             unique() |>
                             length()
                           
                           # Determine default value for ncol
                           # For less than four panels, this is equal to the
                           # number of panels.
                           if (n_panel < 4){
                             default_col <- n_panel
                             # For 4 or more panels, the default value is 2
                           } else {
                             default_col <- 2
                           }
                           
                           # Create slider input
                           sliderInput(
                             inputId = ns("ncol"),
                             label = "Number of Columns: ",
                             min = 1,
                             # Max value: equal to the number of levels
                             # in the given variable
                             max = n_panel,
                             # Only allow integer values
                             step = 1,
                             ticks = FALSE,
                             value = default_col
                           )
                         } # End else
                       })
                   
                 } else if (plot_type == "violin"){
                   # Violin plots: appears when multiple features are entered
                   ncol_slider <-
                     eventReactive(
                       features_entered(),
                       label = glue("{plot_label}: Make ncol Slider"),
                       ignoreNULL = TRUE,
                       {
                         # Number of panels equals number of features for violin 
                         # plots. Slider is needed only when more than one 
                         # feature is entered
                         if(length(features_entered()) > 1){
                           # Default number of columns: equal to the number of
                           # panels if there are less than four, otherwise equal 
                           # to two
                           if (length(features_entered())<4){
                             default_col <- length(features_entered())
                           } else {
                             default_col <- 2
                           }
                           
                           # Create/update slider input
                           sliderInput(
                             inputId = ns("ncol"),
                             label = "Number of columns: ",
                             min = 1,
                             #Max value: equal to the number of features entered
                             max = length(features_entered()),
                             #Only allow integer values
                             step = 1, 
                             ticks = FALSE,
                             value = default_col
                             )
                         } else NULL
                       })
                 }
    
                 ## 4.2. Checkbox to Specify Original Axis Limits ####
                 limits_checkbox <-
                   reactive(
                     label = glue("{plot_label}: Limits UI"),
                     {
                     # Checkbox will only appear when a subset is selected.
                     # The presence of a subset will be tested by observing
                     # the number of cells in the subset
                     if (is_subset()) {
                       checkboxInput(
                         inputId = ns("original_limits"),
                         label = "Use Original Axes Limits",
                         value = FALSE
                       )
                     } else {
                       # Display nothing when the number of cells are equal
                       # between the subset and the full dataset
                       NULL
                       }

                       })
                 
                 ## 4.3. Dynamic UI for plot output ####
                 # UMAP UI is slightly different than other plots because it 
                 # does not depend on features being entered (other plots 
                 # instruct user to enter features if none are chosen)
                 if (plot_type == "dimplot"){
                   plot_output_ui <- 
                     reactive(
                       label = glue("{plot_label}: Plot Output UI"),
                       {
                         # UI only computes if the switch for the plot is enabled  
                         req(plot_switch())
                         
                         # If manual dimensions are specified, they must be 
                         # specified here. If they are only given to renderPlot,
                         # the plot will overlap with other elements on the page 
                         # if its dimensions are changed with the manual 
                         # dimensions inputs.
                         if (
                           (!is.null(manual_dim$width())) && 
                           (!is.null(manual_dim$height()))
                         ){
                           # If manual dimensions are specified, pass the values
                           # specified by the user to plotOutput
                           plotOutput(
                             outputId = ns("plot"),
                             width = manual_dim$width(),
                             height = manual_dim$height()
                           )
                         } else {
                           # Otherwise, call plotOutput without defining 
                           # width and height
                           plotOutput(
                             outputId = ns("plot")
                           )
                         }
                       })
                 } else {
                   # UI for all other plot types 
                   plot_output_ui <- 
                     reactive(
                       label = glue("{plot_label}: Plot Output UI"),
                       {
                         # UI only computes if the switch for the plot is enabled  
                         req(plot_switch())
                         
                         # Test if features have been entered
                         if (length(features_entered())==0){
                           # If no features are entered, generate a message 
                           # instructing the user to enter features.
                           # The string passed to plot_label should make sense
                           # when written on it's own (i.e. 'violin plot' 
                           # instead of 'violin')
                           tags$h3(
                             glue(
                               "Please enter a feature to view 
                               {tolower(plot_label)}."
                               ), 
                             style="margin-bottom: 10em;"
                             )
                         } else {
                           # Display UI as normal if features are entered
                           
                           # Second conditional: check if manual dimensions are
                           # specified
                           if (
                             (!is.null(manual_dim$width())) && 
                             (!is.null(manual_dim$height()))
                           ){
                             # If manual dimensions are specified, pass the values
                             # specified by the user to plotOutput
                             plotOutput(
                               outputId = ns("plot"),
                               width = manual_dim$width(),
                               height = manual_dim$height()
                             )
                           } else {
                             # Otherwise, call plotOutput without defining 
                             # width and height
                             plotOutput(
                               outputId = ns("plot")
                             )
                           }
                         }
                       })
                 }
                 
                 
                 ## 4.4. Render Dynamic UI ####
                 output$ncol_slider <- renderUI({
                   ncol_slider()
                 })

                 output$limits_checkbox <- renderUI({
                   limits_checkbox()
                 })
                 
                 output$plot_output_ui <- renderUI({
                   plot_output_ui()
                 })
                 
                 # 5. Plot -----------------------------------------------------
                 ## 5.1 Construct Plot ####
                 # Plot created based on the type specified when this server 
                 # function is called
                 if (plot_type == "dimplot"){
                   plot <- reactive(
                     label = glue("{plot_label}: Create Plot"),
                     {
                     # Create a UMAP plot using shiny_umap()
                     shiny_umap(
                       object = object,
                       group_by = plot_selections$group_by,
                       split_by = plot_selections$split_by,
                       show_label = plot_selections$label,
                       show_legend = plot_selections$legend,
                       ncol = plot_selections$ncol,
                       is_subset = is_subset,
                       original_limits = plot_selections$limits,
                       xlim_orig = xlim_orig,
                       ylim_orig = ylim_orig
                     )
                   })
                 } else if (plot_type == "feature") {
                   plot <- reactive(
                     label = glue("{plot_label}: Create Plot"),
                     {
                       #Feature plot using arguments relevant to shiny_feature()
                       shiny_feature(
                         object = object,
                         features_entered = features_entered, 
                         split_by = plot_selections$split_by,
                         show_label = plot_selections$label,
                         show_legend = plot_selections$legend,
                         is_subset = is_subset,
                         original_limits = plot_selections$limits,
                         assay_info = assay_info,
                         xlim_orig = xlim_orig,
                         ylim_orig = ylim_orig
                         )
                       })
                 } else if (plot_type == "violin") {
                   plot <- reactive(
                     label = glue("{plot_label}: Create Plot"),
                     {
                       #Violin plot using arguments relevant to shiny_vln()
                       shiny_vln(
                         object = object,
                         features_entered = features_entered, 
                         group_by = plot_selections$group_by,
                         split_by = plot_selections$split_by,
                         show_label = plot_selections$label,
                         show_legend = plot_selections$legend,
                         ncol = plot_selections$ncol,
                         assay_info = assay_info,
                         xlim_orig = xlim_orig,
                         ylim_orig = ylim_orig
                       )
                     })
                 } else if (plot_type == "dot") {

                 }
                 
                 ## 5.2 Render plot ####
                 # Height and width arguments are left undefined
                 # If undefined, they will use the values from plotOutput, which
                 # respond to the manual dimensions inputs.
                 output$plot <- renderPlot({
                   plot()
                 })
                   
                 })
  }
