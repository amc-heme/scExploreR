# Plot Module
# Displays selections menus for an individual plot, builds the plot, and 
# displays it to the screen

# plot_module_ui
# The module UI has two components: "options", which builds the selection menus 
# for plot options, and "plot", which contains the output for the plot itself. 
# The UI component needs to be called twice, once in the desired location for 
# the menus using ui_component="options", and again in the desired location for 
# the plot output with ui_component="plot". For the plot component, the only 
# arguments that need to be set are id and the ui_component; all other arguments
# are used for the options tab

# Arguments
# id: ID to use for module elements. IDs for the options and plot UI components,
# and the server component, must match.
# ui_component: determines which UI component is to be plotted. Use "options" 
# to create the input menus for plot options, and "plot" to create the output
# container for the plot.
# plot_label: a human readable name for the plot that will appear in the menus.
# The name should make sense by itself (i.e. "Feature Plot" should be entered
# instead of "Feature").
# group_by: if TRUE, display a menu with metadata categories to group by.
# split_by: if TRUE, display a menu with metadata categories to split by.
# ncol_slider: if TRUE, display a slider to specify the number of columns to use
# for the plot. This only works for UMAP and violin plots. 
# label_checkbox: if TRUE, display a checkbox for including labels on the plot.
# legend_checkbox: if TRUE, display a checkbox for including a legend.
# limits_checkbox: if TRUE, display a checkbox for the use of original axes.
# limits when a subset is plotted. This only works for UMAP and Feature plots.
# manual_dimensions: if TRUE, display an interface to specify manual height and
# width parameters for the plot.
# separate_features: if TRUE, display a checkbox to enter separate features 
# for the plot. A text entry will be created if the checkbox is selected.
# download_button: if TRUE, display a download button that will save a .png 
# image of the plot to disk.
plot_module_ui <- function(id,
                           # The plot_module UI consists of the options
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
                           separate_features =  FALSE,
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
        uiOutput(outputId = ns("limits_checkbox"))
      } else NULL,
      
      # UI for user control of plot dimensions
      if (manual_dimensions == TRUE){
        #If TRUE, call module for manual adjustment of plot dimensions
        manual_dimensions_ui(id = ns("manual_dim"))
      } else NULL,
      
      # UI to request use of separate features for plot
      if (separate_features == TRUE){
        # Store namespaced ID of use separate features check box
        # for facilitated entry in condionalPanel element
        sep_id <- ns("use_separate_features")
        
        tagList(
          # Checkbox to use separate features
          checkboxInput(
            inputId = sep_id,
            label = glue("Use separate features for {tolower(plot_label)}"), 
            value = FALSE
            ),
          
          # Use conditional panel (rendering UI will reset the text entry)
          conditionalPanel(
            condition = glue("input['{sep_id}'] == true"),
            # Text Entry Element
            tagList(
              # Label
              tags$p(
                HTML(
                  glue("<strong> Enter features <br> 
                       (specific to {tolower(plot_label)}): </strong>")
                  )
                ),
              
              # Selectize input for separate features
              div(
                style =
                  "vertical-align: top; margin-bottom: 0px;",
                selectizeInput(
                  inputId = ns("separate_features"),
                  multiple = TRUE,
                  label = NULL,
                  choices = NULL,
                  selected = NULL,
                  # Add remove button to inputs
                  options =
                    list(
                      'plugins' = list('remove_button'),
                      'create' = FALSE
                    )
                  )
                )
              )
            )
          )
      } else NULL,
      
      # UI for download button
      if (download_button == TRUE){
        downloadButton(
          outputId = ns("download"), 
          label=glue("Download {plot_label}")
          )
      } else NULL
    )
    
  } else if (ui_component == "plot"){
    # UI for plot output 
    # Conditional UI: displays if the "make *" switch for the plot is turned on 
    # in the plots tab
    uiOutput(outputId = ns("plot_output_ui"))
  }
}

#plot_module_server

# Arguments
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
# assay_info: list of assay information generated in main server at startup
# separate_features_separate: a boolean giving whether server code to process 
# separate features (features specific to the plot created by this module) 
# should be ran
plot_module_server <- function(id,
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
                                   valid_features = NULL, #Non-reactive
                                   xlim_orig = NULL, #Non-reactive
                                   ylim_orig = NULL, #Non-reactive
                                   #Currently only needed for feature plots
                                   assay_info = NULL, #Non-reactive
                                   separate_features_server =  FALSE #Non-reactive
                                   ){
  moduleServer(id,
               function(input,output,session){
                 # Server namespace function: for dynamic UI
                 ns <- session$ns
                 
                 # 1. Manual Dimensions Module Server --------------------------
                 if (manual_dimensions == TRUE){
                   manual_dim <- manual_dimensions_server(id = "manual_dim")
                   }
                 
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
                     `ncol` = 
                       if (plot_type == "dimplot"){
                         # Condition to record ncol for UMAP
                         # Equal to conditions where there are multiple panels
                         reactive({
                           if (input$split_by != "none"){
                             input$ncol
                             } else NULL
                           })
                         } else if (plot_type == "violin"){
                           # Condition to record ncol for violin plot
                           # Equal to conditions where there are multiple panels
                           reactive({
                             if (length(features_entered()) > 1){
                               input$ncol
                               } else NULL
                             })
                           },
                     
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
                     # isolate(names(input)) does not work for testing the
                     # existence of the original axes limits checkbox. 
                     # input$original_limits will be either TRUE or FALSE if
                     # the checkbox exists, or NULL if it does not. A suitable
                     # existence test is !is.null(input$original_limits)
                     `limits` = reactive({
                       if(!is.null(input$original_limits)){
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
                           if (length(features_entered()) < 4){
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
                 # UI display depends on the plot type and whether the plot 
                 # has a separate features option
                 if (
                   plot_type == "dimplot"
                   ){
                   # UI for dimplots (separate features are not possible for 
                   # this type)
                   plot_output_ui <- 
                     reactive(
                       label = glue("{plot_label}: Plot Output UI"),
                       {
                         # UI only computes if the switch for the plot is
                         # enabled  
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
                 } else if (
                   plot_type!="dimplot" &
                   separate_features_server == FALSE
                   ){
                   # UI for all other plot types that do not have a separate
                   # features option
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
                 } else if (
                   plot_type != "dimplot" &
                   separate_features_server == TRUE
                 ){
                   # Reactive to use for plots other than dimplots with a 
                   # separate features option
                   plot_output_ui <- 
                     reactive(
                       label = glue("{plot_label}: Plot Output UI"),
                       {
                         # UI only computes if the switch for the plot is enabled  
                         req(plot_switch())
                         
                         # Display either the plot or a message depending on 
                         # whether features have been entered and whether 
                         # the use of separate features is indicated
                         if (
                           input$use_separate_features == FALSE &
                           length(features_entered()) == 0
                           ){
                           # If no features are entered, generate a message 
                           # instructing the user to enter features.
                           tags$h3(
                             glue(
                               "Please enter a feature to view 
                               {tolower(plot_label)}."
                             ), 
                             style="margin-bottom: 10em;"
                           )
                         } else if (
                           input$use_separate_features == TRUE &
                           length(input$separate_features) == 0
                         ){
                           # Screen to display when separate features are 
                           # desired, but none are entered
                           tags$h3(
                             glue(
                               'Please specify at least one 
                               {tolower(plot_label)} specific feature to view 
                               plot. To use the same features as for other 
                               plots, please uncheck "use separate features".'
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
                 output$ncol_slider <- 
                   renderUI({
                     ncol_slider()
                     })

                 output$limits_checkbox <- 
                   renderUI({
                     limits_checkbox()
                     })
                 
                 output$plot_output_ui <- 
                   renderUI({
                     plot_output_ui()
                     })

                 # 5. Separate Features Entry: Dynamic Update ------------------
                 # Observers for separate features only update for server 
                 # instances where features_entered
                 if (separate_features_server ==  TRUE){
                   ## 5.1 Update Separate Features in Background ####
                   # Before the checkbox to select separate features is checked, 
                   # update the text entry in the background so it is synced
                   # when it appears after the box is checked. 
                   # This process ensures the features are instantly available in
                   # the separate features text box when the checkbox is checked
                   observeEvent(
                     features_entered(),
                     label = 
                       glue("{plot_label}: Update Separate 
                            Features Text Entry"),
                     {
                       if (input$use_separate_features == FALSE){
                         updateSelectizeInput(
                           session,
                           inputId = "separate_features",
                           choices = valid_features,
                           selected = features_entered(),
                           server = TRUE
                           )
                         }
                     })
                   
                   ## 5.2 Reset Separate Features Upon Checkbox Toggle ####
                   # If the "use separate features" checkbox is toggled and the
                   # features entered in the separate features text entry differ
                   # from the general features selected, update the separate
                   # features text entry to match the general features. 
                   # This is necessary to ensure that selections for general 
                   # features appear in the separate features text entry in the 
                   # event that the user checks the box, unchecks it, changes
                   # general features, and checks the box again.
                   observeEvent(
                     input$use_separate_features,
                     label = 
                       glue("{plot_label}: Set Separate Features Input"),
                     {
                       if (
                         # Check if general feature and separate feature text 
                         # entries are not in sync
                         !setequal(
                           features_entered(), 
                           input$separate_features
                           )
                         ){
                         updateSelectizeInput(
                           session,
                           inputId = "separate_features",
                           choices = valid_features,
                           selected = features_entered(),
                           server=TRUE
                         )
                       }
                     })
                 }
                 
                 # 6. Plot -----------------------------------------------------
                 ## 6.1 Define Features to use (all plots except UMAP)
                 # Uses either the general feature entry (features_entered()),
                 # or the separate features text entry depending on whether
                 # separate features are used in the module and whether the 
                 # checkbox to use them is selected.
                 if (plot_type != "dimplot"){
                   features <-
                     reactive(
                       label = glue("{plot_label}: Features for Plot"),            
                       {
                         print("Conditionals in features argument")
                         # Test for separate_features_server first
                         # input$use_separate_features does not exist if 
                         # separate_features_server == FALSE
                         if (separate_features_server == TRUE){
                           # If separate features are used in this module,
                           # input them if the user checks the box to use
                           # them 
                           if(input$use_separate_features == TRUE){
                             #Use separate features
                             input$separate_features
                           } else if (input$use_separate_features == FALSE){
                             #Use general features
                             features_entered()
                           }
                         } else if (separate_features_server == FALSE){
                           # Otherwise, pass features_entered() to 
                           # shiny_dot() (general features)
                           features_entered()
                         }
                       })
                 }
                 
                 ## 6.2 Construct Plot ####
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
                       # Feature plot using arguments relevant to 
                       # shiny_feature()
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
                       # Violin plot using arguments relevant to shiny_vln()
                       shiny_vln(
                         object = object,
                         features_entered = features_entered, 
                         group_by = plot_selections$group_by,
                         split_by = plot_selections$split_by,
                         show_legend = plot_selections$legend,
                         ncol = plot_selections$ncol,
                         assay_info = assay_info
                         )
                     })
                 } else if (plot_type == "dot") {
                   # Dot plot using arguments relevant to shiny_dot()
                   plot <- reactive(
                     label = glue("{plot_label}: Create Plot"),
                     {
                       shiny_dot(
                         object = object,
                         # Features argument: uses value returned by reactive
                         features = features,
                         # use_separate_features = 
                         #   reactive({input$use_separate_features}),
                         # separate_features = 
                         #   reactive({input$separate_features}),
                         group_by = plot_selections$group_by,
                         show_legend = plot_selections$legend
                         )
                     })
                   }
                 
                 ## 6.2. Render plot ####
                 # Height and width arguments are left undefined
                 # If undefined, they will use the values from plotOutput, which
                 # respond to the manual dimensions inputs.
                 output$plot <- renderPlot({
                   plot()
                 })
                 
                 # 7. Download Handler -----------------------------------------
                 output$download <- downloadHandler(
                   # Filename: takes the label and replaces 
                   # spaces with underscores
                   filename = glue("{sub(' ','_',plot_label)}.png"),
                   content = function(file){
                     # Conditional: manual dimensions are specified
                     if (
                       (!is.null(manual_dim$width())) && 
                       (!is.null(manual_dim$height()))
                     ){
                       # If manual dimensions are specified, apply them to 
                       # height and width arguments
                       ggsave(
                         file,
                         plot = plot(),
                         device = "png",
                         width = manual_dim$width(),
                         height = manual_dim$height(),
                         # Set dpi to 72 so proportions of downloaded plot 
                         # match the plot in the app
                         dpi = 72,
                         units = "px",
                         # Set background color to white (background is 
                         # transparent on some plots)
                         bg="#FFFFFF"
                         )
                     } else {
                       ggsave(
                         file,
                         plot = plot(),
                         device = "png",
                         # Set background color to white (background is 
                         # transparent on some plots)
                         bg ="#FFFFFF"
                         )
                       }
                     },#End content function
                   contentType = "image/png"
                 ) #End downloadHandler function
                 
                 })
  }