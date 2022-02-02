#Plots Tab Module

plots_tab_ui <- function(id){
   #Namespace function: prevents conflicts with 
   #inputs/outputs defined in other modules 
   ns <- NS(id)
   
   #UI for plots tab
   fluidPage(
     #Sidebar layout: consists of a side panel and a main panel
     sidebarLayout(
       ### 1.1.1. Sidebar panel for user input ####
       sidebarPanel(
         fluid=FALSE,
         #### 1.1.1.1 Checkboxes for choosing desired plot ####
         # Two-column checkboxes: put inside inline block elements 
         #that span half of the sidebar panel
         div(
           class="two_column",
           style="float: left;",
           #Specify if UMAP Plot is desired
           materialSwitch(
             inputId = "make_umap",
             label = "UMAP plot", 
             value = TRUE,
             right = TRUE,
             status = "default"
             ),
           
           #Specify if feature plot is desired
           materialSwitch(
             inputId = "make_feature",
             label = "Feature Plot", 
             value = FALSE,
             right = TRUE,
             status = "default"
             )
           ),#End div
         
         div(
           class="two_column",
           #Specify if violin plot is desired
           materialSwitch(
             inputId = "make_vln",
             label = "Violin Plot", 
             value = FALSE,
             right = TRUE,
             status = "default"
             ),
           #Specify if dot plot is desired
           materialSwitch(
             inputId = "make_dot",
             label = "Dot Plot", 
             value = FALSE,
             right = TRUE,
             status = "default"
             )
           ),#End div
         
         #### 1.1.1.2. Feature Text Entry. #### 
         #Applies to feature, violin, and dot plots unless the user specifies 
         #the use of different features for each plot (this is currently only 
         #possible for dot plots) 
         conditionalPanel(
           condition="input.make_feature==true | 
           input.make_vln==true | 
           input.make_dot==true",
           #Label
           tags$p(tags$strong("Enter features to display on plots:")),
           #Inline text entry and update button
           div(
             style="vertical-align: top; margin-bottom: 0px;",
             selectizeInput(
               inputId = "text_features",
               multiple = TRUE, 
               label = NULL,
               choices = NULL,
               selected = NULL,
               #Add remove button to inputs
               options = list(
                 'plugins' = list('remove_button'),
                 #Do not allow user to input features not in the list of options
                 'create'=FALSE)
               ) 
             )
           ),#End 1.1.1.2.
         
         #### 1.1.1.3. Subsets for Plots ####
         collapsible_panel(
           inputId="plots_subset_collapsible",
           label="Subset Options",
           active=FALSE,
           div(
             id="plots_subset_panel",
             div(
               id="plots_subset_stats",
               tags$strong(
                 "Metadata in Displayed Subset",
                 id="plots_subset_header"
                 ),
               div(
                 tags$strong("Clusters: "),
                 textOutput(
                   outputId = "plots_selected_clusters", 
                   inline = TRUE
                   )
                 ),
               div(tags$strong("Response criteria: "),
                   textOutput(
                     outputId = "plots_selected_response", 
                     inline = TRUE
                     )
                   ),
               div(tags$strong("Timepoints: "),
                   textOutput(
                     outputId = "plots_selected_treatment", 
                     inline = TRUE
                     )
                   ),
               div(tags$strong("Patients: "),
                   textOutput(
                     outputId = "plots_selected_htb", 
                     inline = TRUE
                     )
                   )
               ),
             #Generate subset menus using the config file and unique_metadata
             
             #subset_selections module
             subset_selections_ui(
               "plots_subset",
               unique_metadata,
               metadata_config=config$metadata
               ),
             actionButton(
               inputId = "plots_subset_submit",
               label="Apply Subset"
               )
             )
           ),#End 1.1.1.3
         
         #TEMP: text output for subset selections
         verbatimTextOutput(
           outputId = "plots_subsets_return",
           placeholder = TRUE
           ),
         
         ### Plot Specific Options ###
         #### 1.1.1.4. Options specific to UMAP ####
         #Panel will display if UMAP is checked
         conditionalPanel(
           condition = "input.make_umap==true",
           collapsible_panel(
             inputId="plots_umap_collapsible",
             label="UMAP Specific Options",
             active=TRUE,
             
             #Choose metadata to group UMAP by
             selectInput(
               inputId = "umap_group_by", 
               label = "Metadata to Group by:",
               #Remove "none" from selectable options to group by
               choices= meta_choices[!meta_choices %in% "none"], 
               selected = "clusters"),
             #Choose metadata to split UMAP by
             selectInput(
               inputId = "umap_split_by", 
               label = "Metadata to Split By:",
               choices= meta_choices,  
               selected = "none"
               ),
             #If split by is specified, control number of columns with a slider
             uiOutput(outputId = "umap_ncol_slider"),
             
             #Checkbox: add or remove labels (labels on by default)
             checkboxInput(
               inputId ="umap_label",
               label = "Label Groups",
               value = TRUE
               ),
             #Checkbox to add or remove Legend
             checkboxInput(
               inputId = "umap_legend",
               label = "Include Legend",
               value = TRUE
               ),
             
             #If plotting a subset: checkbox to use original dimensions 
             uiOutput(outputId="umap_limits_checkbox"),
             
             #UI for user control of plot dimensions, if desired
             manual_dim_UI(plot_type = "umap"),
             #Download button (plot specific)
             downloadButton(
               outputId = "umap_download", 
               label="Download UMAP"
               )
             )#End collapsible panel
           ),#End 1.1.1.4.
         
         #### 1.1.1.5. Options specific to feature plot ####
         conditionalPanel(
           condition = "input.make_feature==true",
           collapsible_panel(inputId="plots_feature_collapsible",
                             label = "Feature Plot Specific Options",
                             active = FALSE,
                             #Feature plots do not have a group.by argument
                             #Choose metadata to split feature plot by
                             selectInput(
                               inputId = "feature_split_by",
                               label = "Metadata to split by:",
                               choices=meta_choices, 
                               selected = "none"
                               ),
                             
                             #Checkbox to add or remove Legend
                             checkboxInput(
                               inputId="feature_legend",
                               label="Include Legend",
                               value=TRUE
                               ),
                             
                             #If plotting a subset: checkbox to use 
                             #original dimensions 
                             uiOutput(outputId="feature_limits_checkbox"),
                             #UI for user control of plot dimensions, if desired
                             manual_dim_UI(plot_type = "feature"),
                             #Download button (plot specific)
                             downloadButton(
                               outputId = "feature_download",
                               label="Download Feature Plot"
                               )
                             )#End collapsible_panel
           ),#End 1.1.1.5
         
         #### 1.1.1.6. Options specific to violin plot ####
         conditionalPanel(
           condition = "input.make_vln==true",
           collapsible_panel(
             inputId="plots_vln_collapsible",
             label = "Violin Plot Specific Options",
             active=FALSE,
             #Choose metadata to group violin plot by
             selectInput(
               inputId = "vln_group_by", 
               label = "Metadata to group by:",
               #Remove "none" from selectable options to group.by
               choices=meta_choices[meta_choices %in% "none" == FALSE],
               selected = "clusters"
               ),
             
             #Choose metadata to split violin plot by
             selectInput(
               inputId = "vln_split_by", 
               label = "Metadata to split by:", 
               choices=meta_choices,
               selected = "none"
               ),
             
             #Slider to control number of columns if multiple features are entered
             uiOutput(outputId = "vln_ncol_slider"),
             #Checkbox to add or remove Legend
             checkboxInput(
               inputId="vln_legend",
               label="Include Legend",
               value=TRUE
               ),
             
             #UI for user control of plot dimensions, if desired
             manual_dim_UI(plot_type = "vln"),
             #Download button (plot specific)
             downloadButton(
               outputId = "vln_download",
               label="Download Violin Plot"
               )
             )#End collapsible panel
           ), #End 1.1.1.6.
         
         #### 1.1.1.7. Options specific to dot plot ####
         conditionalPanel(
           condition = "input.make_dot==true",
           collapsible_panel(
             inputId="plots_dot_collapsible",
             label="Dot Plot Specific Options",
             active=FALSE,
             #Choose metadata to group dot plot by
             selectInput(
               inputId = "dot_group_by",
               label = "Metadata to group by:",
               #Remove "none" from selectable options to group by
               choices=meta_choices[!meta_choices %in% "none"], 
               selected = "clusters"),
             
             #Choosing different features
             checkboxInput(
               inputId = "diff_features_dot",
               label="Use separate features for dot plot", 
               value=FALSE
               ),
             
             #If the checkbox above is selected, display a selectize input for 
             #feature selection
             conditionalPanel(
               condition="input.diff_features_dot==true",
               #Label
               tags$p(tags$strong("Enter features to display on dot plot:")),
               #Selectize entry
               div(
                 style="vertical-align: top; margin-bottom: 0px;",
                 selectizeInput(
                   inputId = "dot_features",
                   multiple=TRUE,
                   label=NULL,
                   choices = NULL,
                   selected = NULL,
                   #Add remove button to inputs
                   options = list(
                     'plugins' = list('remove_button'),
                     'create'=FALSE
                     )
                   ) #End selectizeInput
                 )
               ), #End conditionalPanel
             #Checkbox to add or remove Legend
             checkboxInput(
               inputId="dot_legend",
               label="Include Legend",
               value=TRUE
               ),
             #UI for user control of plot dimensions, if desired
             manual_dim_UI(plot_type = "dot"),
             #Download button (plot specific)
             downloadButton(outputId = "dot_download",label="Download Dot Plot")
             ) #End collapsible panel
           ) #End 1.1.1.7
         ), #End 1.1.1.
       
       ### 1.1.2. Main panel for displaying plot output ####
       mainPanel(
         #div added to contain Waiter spinner (forces the spinner to cover 
         #the full main panel)
         div(
           id="plots_main_panel", 
           class="spinner-container-main",
           #Panels for plots: display if checkboxes corresponding to 
           #each type are checked
           #1.1.2.1. UMAP plot panel
           conditionalPanel(
             condition = "input.make_umap==true",
             uiOutput(outputId = "umap_slot")
             ),
           
           #1.1.2.2. Panel for feature plot 
           #Will be a message or a plot, depending on whether features have 
           #been entered
           conditionalPanel(
             condition = "input.make_feature==true",
             uiOutput(outputId = "feature_slot")
             ),
           
           #1.1.2.3. Panel for violin plot
           #UI displayed will vary based on the entry into the feature text box
           conditionalPanel(
             condition="input.make_vln==true",
             uiOutput(outputId = "vln_slot")
             ), 
           
           #1.1.2.4. Dot plot panel
           conditionalPanel(
             condition = "input.make_dot==true",
             uiOutput(outputId = "dot_slot")
             )
         ) #End div
       ) #End 1.1.2
     ) #End sidebarLayout() 
   ) #End fluidPage() 
   
}

plots_tab_server <- function(id){
  moduleServer(id,
               function(input,output,session){
                 #Namespace function: for dynamic UI and modules
                 ns <- session$ns
                 
                 #2.0.1. Initialize Reactive Values
                 rv <- reactiveValues()
                 
                 #rv$is_subset: used to tell the plots to plot the subsetted 
                 #object instead of the full object. This is FALSE initially 
                 #and set to TRUE when the user chooses a subset.
                 rv$is_subset <- FALSE
                 
                 #*_use_original_limits: if TRUE, modify the axes on UMAP and feature plots to
                 #match the original UMAPs created from the full dataset
                 rv$umap_use_original_limits <- FALSE
                 rv$feature_use_original_limits <- FALSE
                 #This is true when a subset is selected in the correlations tab
                 rv$corr_is_subset <- FALSE
                 
                 #2.0.2. Render feature choices for text feature selection (plots tab)
                 updateSelectizeInput(session,
                                      inputId = "text_features", 
                                      choices = valid_features, 
                                      server = TRUE)
                 
                 ### 2.1.1 Subset for Plots Tab #####
                 #2.1.1.1. Module server to process user selections and report 
                 #to other modules
                 plots_subset_selections <- 
                   subset_selections_server("plots_subset",
                                            sobj = sobj,
                                            unique_metadata = unique_metadata,
                                            metadata_config = config$metadata)
                 
                 #TEMP: Print selections made
                 output$plots_subsets_return <- renderPrint({
                   plots_subset_selections()
                 })
                 
                 #2.1.1.2. Update choices in subset selection menu based on 
                 #user selections
                 #Update patients menu based on entries in 'response' or 
                 #'treatment' (timepoint)
                 observeEvent(
                   c(input$plots_response_selection,
                     input$plots_treatment_selection),
                   ignoreNULL = FALSE,
                   label = "Plots Update Patients",
                   {
                     #Display spinner during computation to keep user from 
                     #choosing outdated options
                     waiter_show(
                       id = "plots_subset_panel",
                       html = spin_loaders(id = 2, color = "#555588"),
                       color = "#B1B1B188",
                       #Gives manual control of showing/hiding spinner
                       hide_on_render = FALSE 
                       )
                     
                     #Filter object for treatment and response selections
                     valid_patients <- 
                       sobj@meta.data |> 
                       filter(
                         (.data[["response"]] %in% 
                            input$plots_response_selection)&
                           (.data[["treatment"]] %in% 
                              input$plots_treatment_selection)
                                  ) |> 
                       #Select patients metadata column
                       select(.data[["htb"]]) |> 
                       #Return unique values
                       unique() |>
                       #Convert to a character vector
                       unlist()
                     
                     #Form categorized list of valid patients for display 
                     #in dropdown menu
                     valid_patients_categories <- 
                       build_patient_list(valid_patients)
                     #Sort patients categorized list so they appear in order
                     valid_patients_categories <- 
                       sort_patient_list(valid_patients_categories)
                     
                     #Update picker input with valid patient IDs
                     updatePickerInput(
                       session,
                       inputId = "plots_htb_selection",
                       label = "Restrict by Patient",
                       choices = valid_patients_categories,
                       selected = valid_patients,
                       options = list(
                         "selected-text-format" = "count > 3",
                         "actions-box" = TRUE
                         )
                       ) #End updatePickerInput
                     
                     #Hide spinner
                     waiter_hide("plots_subset_panel")
                     })
                 
                 # 2.1.1.3. Construct subset after "Apply Subset" button is clicked
                 plots_subset <- 
                   eventReactive(
                     input$plots_subset_submit,
                     ignoreNULL=FALSE,
                     label = "Plots Subset", 
                     {
                       print("Executing subset code")
                       #Display spinner over main window while the 
                       #subset is being computed
                       waiter_show(
                         id = "plots_main_panel",
                         html = spin_loaders(id = 2, color = "#555588"),
                         color = "#FFFFFF",
                         #Gives manual control of showing/hiding spinner
                         hide_on_render = FALSE 
                         )
                       
                       #Also display a spinner over the text showing the 
                       #metadata in the current subset
                       waiter_show(
                         id = "plots_subset_stats",
                         html = spin_loaders(id = 2, color = "#555588"),
                         color = "#B1B1B188",
                         #Gives manual control of showing/hiding spinner
                         hide_on_render = FALSE 
                         )
                       
                       plots_s_sub <- 
                         tryCatch(
                           error=function(cnd){
                             #Return errors to user using notifications
                             #If an error is caught: the function below
                             #determines the type of error by inspecting 
                             #message text with grepl (not recommended, 
                             #but I currently don't know any other way to 
                             #catch this error type)
                             error_handler(
                               session,
                               cnd_message = cnd$message,
                               #Uses a list of subset-specific errors 
                               error_list = error_list,
                               #Id prefix for the notification elements
                               id_prefix = "plots"
                               )
                             
                             #Return "NULL" for subset when an error has occurred
                             plots_s_sub <- NULL
                             return(plots_s_sub)
                             }, #End tryCatch error function
                           #Begin tryCatch code
                           {
                             #Use subsetting function with the output of the 
                             #subset selections module as `criteria_list`.
                             plots_s_sub <-
                               make_subset(
                                 sobj,
                                 criteria_list = plots_subset_selections
                                 )
                             }
                           )#End tryCatch
                       
                       #Hide the water
                       waiter_hide("plots_main_panel")
                       
                       #Return subset to the eventReactive variable
                       plots_s_sub
                       })
                 
                 #Rendering text for selected subsets
                 observeEvent(
                   input$plots_subset_submit, 
                   ignoreNULL = FALSE,
                   label = "Plots: Render Subset Criteria",
                   {
                     #plots_subset() is NULL if errors are found during the 
                     #subsetting. Code will only proceed with identifying 
                     #metadata if no errors were encountered
                     if(!is.null(plots_subset())){
                       #Store the current metadata levels stored in the 
                       #selected subset
                       responses_found <- unique(plots_subset()$response)
                       treatments_found <- unique(plots_subset()$treatment)
                       patients_found <- unique(plots_subset()$htb)
                       clusters_found <- unique(plots_subset()$clusters)
                       
                       #Rendering Selections and Stats for report
                       output$plots_selected_clusters <- renderText({
                         #If all clusters are selected, print "All"
                         if(setequal(clusters_found,clusters)){
                           "All"
                           #Otherwise, print the selected clusters
                           } else { 
                             isolate(vector_to_text(clusters_found))
                             } #End Conditionals
                         }) #End renderText
                                  
                       #Selected Response Criteria
                       output$plots_selected_response <- renderText({
                         #Print "All" if all response criteria are selected 
                         if(setequal(responses_found,responses)){
                           "All"
                           }else{
                             #Otherwise, print selected responses
                             isolate(vector_to_text(responses_found))
                             }#End conditionals
                         }) #End renderText
                       
                       #Selected Timepoints
                       output$plots_selected_treatment <- renderText({
                         #Print "All" if all treatment categories 
                         #(timepoints) are selected
                         if(setequal(treatments_found,treatments)){
                           "All"
                           }else{
                             #Otherwise, print selected treatment 
                             #categories (timepoints)
                             isolate(vector_to_text(treatments_found))
                             }#End conditionals
                         }) #End renderText
                       
                       #Selected Patients
                       output$plots_selected_htb <- renderText({
                         if(setequal(patients_found,patients)){
                           #Print "all" if all patient IDs are selected
                           "All"
                           }else{
                             #Otherwise, print selected patients
                             isolate(vector_to_text(patients_found))
                             } #End conditionals
                       }) #End renderText
                       }
                     
                     #When finished rendering current metadata, hide the spinner
                     #over the panel
                     waiter_hide("plots_subset_stats")
                     })
                 
                 ### 2.1.2. UMAP plot ####
                 #### 2.1.2.1. Reactive UMAP plot dimensions ####
                 #Width
                 #Update text box to match slider when the slider is changed
                 observeEvent(
                   input$umap_width,
                   {
                     updateSearchInput(
                       session, 
                       inputId = "umap_width_text", 
                       value=input$umap_width, 
                       trigger=TRUE)
                     })
                 
                 #Update slider based on text entry (search input waits until 
                 #user presses enter to update)
                 observeEvent(
                   input$umap_width_text,
                   {
                     updateSliderInput(
                       session, 
                       inputId = "umap_width", 
                       value=input$umap_width_text
                       )
                     })
                 
                 #Store plot width from text input if it is changed by user
                 umap_width <- 
                   eventReactive(
                     c(input$umap_width, 
                       input$umap_width_text),
                     {
                       input$umap_width
                       })
                 
                 #Height
                 #Update text box to match slider when the slider is changed
                 observeEvent(
                   input$umap_height,
                   {
                     updateSearchInput(
                       session, 
                       inputId = "umap_height_text", 
                       value=input$umap_height, 
                       trigger=TRUE)
                     })
                 
                 #Update slider based on text entry (search input waits until 
                 #user presses enter to update)
                 observeEvent(
                   input$umap_height_text,
                   {
                     updateSliderInput(
                       session, 
                       inputId = "umap_height", 
                       value=input$umap_height_text
                       )
                     })
                 
                 #Store plot height from text input if it is changed by user
                 umap_height <- 
                   eventReactive(
                     c(input$umap_height, 
                       input$umap_height_text),
                     {
                       input$umap_height
                       })
                 
                 #### 2.1.2.2. ncol slider: appears when a split.by ####
                 #Default value depends on the number of values in the metadata 
                 #object in question.
                 #Updates when the split_by argument or the subset is changed
                 umap_ncol_slider <- 
                   eventReactive(
                     c(input$umap_split_by,
                       input$plots_subset_submit), 
                     #Do not need to render UI at startup
                     ignoreInit = TRUE, 
                     {
                       #Do not render when split.by is "none"
                       if (input$umap_split_by=="none"){
                         NULL
                         } else {
                           #Determine number of panels created by split_by 
                           #choice. Use double-bracket means of accessing the 
                           #metadata variable (supports entry of an arbitrary 
                           #variable). This means of access returns a dataframe. 
                           #Slice for the first row (the unique values)
                           n_panel <- 
                             unique(plots_subset()[[input$umap_split_by]])[,1] |> 
                             length()
                                                    
                           #Determine initial value for ncol
                           #For less than four panels, this is equal to the 
                           #number of panels. 
                           if (n_panel < 4){
                             default_col <- n_panel
                             #For 4 or more panels, the default value is 2
                             } else {
                               default_col <- 2
                               }
                           
                           #Create/update slider input
                           sliderInput(
                             inputId = "umap_ncol",
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
                 
                 #### 2.1.2.3. UI to specify original axis limits ####
                 #Appears when a subset is plotted
                 umap_limits_checkbox <- 
                   eventReactive(
                     c(input$plots_subset_submit,
                       input$plots_feature_collapsible),
                     label = "UMAP Limits UI",
                     {
                       #Checkbox will only appear when a subset is selected. 
                       #The presence of a subset will be tested by observing the 
                       #number of cells in the subset
                       if (n_cells_original != ncol(plots_subset())) {
                         checkboxInput(
                           inputId = "umap_original_limits",
                           label = "Use Original Axes Limits",
                           value = FALSE
                           )
                         } else {
                           #Display nothing when the number of cells are equal 
                           #between the subset and the full dataset
                           NULL
                           }
                       })
                 
                 #### 2.1.2.4. Generate UI for UMAP plot ####
                 #Renders a plotOutput() with either automatic or manually 
                 #specified dimensions based on user specifications
                 umap_UI <- reactive({
                   if (input$umap_manual_dim==FALSE){
                     plotOutput(outputId = "umap_slot_plot")
                   } else {
                     plotOutput(outputId = "umap_slot_plot",
                                width = umap_width(),
                                height = umap_height())
                   }
                 })
                 
                 #### 2.1.3.5. Server Component for Original Axes Checkbox ####
                 #Right after a subset is specified, an error appears saying that the condition
                 #for computing original axes (input$umap_original_limits) does not exist. This 
                 #likely occurs because the input for specifying original axes is created after 
                 #the subset is submitted and the new plot drawn, and the conditional relying 
                 #on that input is within the plotting function.
                 observeEvent(input$umap_original_limits,
                              label="Toggle Limits: UMAP Plot",
                              {
                                #Set the reactive value based on the state of the input
                                #Reactive value was created on startup so it always has a value
                                if (input$umap_original_limits==TRUE){
                                  rv$umap_use_original_limits=TRUE
                                } else{
                                  rv$umap_use_original_limits=FALSE
                                }
                              })
                 
                 #### 2.1.2.6. Define UMAP Plot ####
                 #Plot content is defined separately in a reactive context, to be rendered later with the UI.
                 umap_plot_content <- reactive({
                   #validate will keep plot code from running if the subset is NULL 
                   #(no cells in subset)
                   validate(
                     need(plots_subset(),
                          #No message displayed (a notification is already displayed)
                          message = "")
                   )
                   #Produce a single UMAP plot if no features to split by are specified
                   if (input$umap_split_by=="none"){
                     #Use full object if is_subset is FALSE, and use the subset otherwise
                     umap_plot <- DimPlot(plots_subset(),
                                          group.by = input$umap_group_by,
                                          label = input$umap_label, #TRUE if "label groups" is checked, FALSE otherwise
                                          reduction = "umap") 
                   } else {
                     #UMAP with split.by defined and no special subset
                     umap_plot <- DimPlot(plots_subset(),
                                          group.by = input$umap_group_by,
                                          split.by = input$umap_split_by,
                                          label = input$umap_label,
                                          ncol = input$umap_ncol,
                                          reduction = "umap") 
                   }
                   
                   #Modify plot after creation with ggplot layers according to user input
                   #'layers' is a list of layers that is applied to the plot
                   #List format works more effectively with conditional statements
                   layers <- list(
                     #Element A 
                     #Legend position: "right" if a legend is desired, and "none" if not
                     theme(legend.position = if (input$umap_legend==TRUE) "right" else "none"),
                     
                     #B-C. Axis limits: use limits from full dataset if specified
                     #Element B
                     #Must first test to see if subset is present
                     #Input container does not exist if there is no subset
                     if(n_cells_original != ncol(plots_subset())){
                       #Add original limits to the list if the 
                       #corresponding checkbox is checked
                       #The conditional is tied to a reactive value instead of the input to avoid
                       #An error that occurs when this function is evaluated before the input is 
                       #defined. 
                       if (rv$umap_use_original_limits==TRUE) scale_x_continuous(limits=xlim_orig)
                     },
                     #Element C
                     #Check for subset (input container in 
                     #child conditional does not exist 
                     #before a subset is created)
                     if(n_cells_original != ncol(plots_subset())){
                       #Add original limits to the list if the 
                       #corresponding checkbox is checked
                       if(rv$umap_use_original_limits==TRUE) scale_y_continuous(limits=ylim_orig) 
                     }
                   )
                   
                   #Modify the plot using the layers defined above
                   umap_plot <- umap_plot &
                     layers
                   
                   #Return plot to umap_plot_content()
                   umap_plot
                 })
                 
                 #### 2.1.2.7. Render UI Components ####
                 output$umap_slot <- renderUI({
                   umap_UI()
                 })
                 
                 output$umap_ncol_slider <- renderUI({
                   umap_ncol_slider()
                 })
                 
                 output$umap_limits_checkbox <- renderUI({
                   umap_limits_checkbox()
                 })
                 
                 #### 2.1.2.8. Render UMAP plot, with manual or automatic dimensions as specified #####
                 #ObserveEvent will respond to the check box and the slider/text box pairs 
                 #(other variables involved in plot construction will also cause the plot to
                 #re-render)
                 observeEvent(
                   c(input$umap_manual_dim, 
                     input$umap_width, 
                     input$umap_width_text, 
                     input$umap_height,
                     input$umap_height_text),
                   {
                     if (input$umap_manual_dim==FALSE){
                       output$umap_slot_plot <- renderPlot(umap_plot_content())
                     } else {
                       output$umap_slot_plot <- renderPlot(umap_plot_content(), 
                                                           width = umap_width(), 
                                                           height = umap_height())
                     }
                   }
                 )
                 
                 #### 2.1.2.9. Download UMAP Plot ####
                 output$umap_download <- downloadHandler(
                   filename = "UMAP_plot.png",
                   content = function(file){
                     if (input$umap_manual_dim==TRUE){
                       ggsave(file, 
                              plot=umap_plot_content(), 
                              device="png",
                              width=umap_width(),
                              height=umap_height(),
                              dpi=72,
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
                 
                 ### 2.1.3. Feature Plot ##### 
                 #### 2.1.3.1 Reactive dimensions ####
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
                 
                 #### 2.1.3.2 UI to specify origional access limits ####
                 #Appears only when a subset is plotted (reacts to submit button and clicks on
                 #the collapsible panel header for feature plots)
                 feature_limits_checkbox <- eventReactive(c(input$plots_subset_submit, input$plots_feature_collapsible),
                                                          label = "Feature Limits UI",
                                                          ignoreNULL = FALSE,
                                                          {
                                                            #Checkbox will only appear when a subset 
                                                            #is selected.The presence of a subset 
                                                            #will be tested by observing the number 
                                                            #of cells in the subset
                                                            print("Code to build feature limits checkbox")
                                                            if (n_cells_original != ncol(plots_subset())) {
                                                              checkboxInput(inputId = "feature_original_limits",
                                                                            label = "Use Original Axes Limits",
                                                                            value = FALSE)
                                                            } else {
                                                              NULL
                                                            }
                                                          })
                 
                 #### 2.1.3.3 Feature UI ####
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
                 
                 #### 2.1.3.4. Server Component for Original Axes Checkbox ####
                 #Right after a subset is specified, an error appears saying that the condition
                 #for computing original axes does not exist. This is likely due to the fact that
                 #the input for specifying original axes is created after the subset is submitted
                 #and the new plot drawn, and the conditional relying on that input is within the
                 #plotting function.
                 observeEvent(input$feature_original_limits,
                              label="Toggle Limits: Feature Plot",
                              {
                                #Set the reactive value based on the state of the input
                                #Reactive value was created on startup so it always has a value
                                if (input$feature_original_limits==TRUE){
                                  rv$feature_use_original_limits=TRUE
                                } else{
                                  rv$feature_use_original_limits=FALSE
                                }
                              })
                 
                 #### 2.1.3.5. Generate content for plot (but only if features are entered) ####
                 feature_plot_content <- reactive({
                   if (length(input$text_features)>0){
                     #If no split.by variable is specified, create a feature plot without 
                     #the split.by argument
                     if (input$feature_split_by=="none"){
                       feature_plot <- FeaturePlot(plots_subset(),
                                                   features=input$text_features)
                       #Clean up title: this changes the feature names on each plot 
                       #to a human-readable format
                       #Determine number of plots created
                       n_patches <- n_patches(feature_plot)
                       #Iterate through each plot, correcting the title
                       feature_plot <- hr_title(feature_plot,n_patches,assay_info)
                     }
                     #Otherwise, split by the user-specified variable
                     else {
                       feature_plot <- FeaturePlot(plots_subset(), 
                                                   features=input$text_features,
                                                   split.by = input$feature_split_by)
                     }
                     
                     #Add ggplot layers to modify plot
                     #Layers: a list of ggplot layers, based on user input (list works well 
                     #with conditionals)
                     layers <- list(
                       #Element A 
                       #Legend position: "right" if a 
                       #legend is desired, and "none" if not
                       theme(legend.position = if (input$feature_legend==TRUE)"right" else "none"),
                       
                       #B-C. Axis limits: use limits from full dataset if specified
                       #Element B
                       #Must first test to see if subset is present
                       #Input container does not exist if there is no subset
                       if(n_cells_original != ncol(plots_subset())){
                         #Add original limits to the list if the 
                         #corresponding checkbox is checked
                         if (rv$feature_use_original_limits==TRUE) scale_x_continuous(limits=xlim_orig)
                       },
                       #Element C
                       #Check for subset (input container in 
                       #child conditional does not exist 
                       #before a subset is created)
                       if(n_cells_original != ncol(plots_subset())){
                         #Add original limits to the list if the 
                         #corresponding checkbox is checked
                         if(rv$feature_use_original_limits==TRUE) scale_y_continuous(limits=ylim_orig) 
                       }
                     )
                     
                     #Modify the plot created in eventReactive
                     #function using the layers defined above
                     feature_plot <- feature_plot &
                       layers
                     
                     #Return plot to feature_plot_content()
                     feature_plot
                   }
                 })
                 
                 #### 2.1.3.6. Render the UI and plot objects created above ####
                 #UI
                 output$feature_slot <- renderUI({
                   feature_slot_UI()
                 })
                 
                 #Limits checkbox
                 output$feature_limits_checkbox <- renderUI({
                   feature_limits_checkbox()
                 })
                 
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
                 
                 #### 2.1.3.7. Feature Plot Download ####
                 output$feature_download <- downloadHandler(
                   filename = "Feature_plot.png",
                   content = function(file){
                     if (input$feature_manual_dim==TRUE){
                       ggsave(file, 
                              plot=feature_plot_content(), 
                              device="png",
                              width=feature_width(),
                              height=feature_height(),
                              dpi=72,
                              units="px")
                     } else {
                       ggsave(file, 
                              plot=feature_plot_content(), 
                              device="png")
                     }
                   },#End content function
                   contentType = "image/png"
                 ) #End downloadHandler function
                 
                 ### 2.1.4. Violin plot ######
                 #### 2.1.4.1 Reactive plot dimensions ####
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
                 
                 #### 2.1.4.2. Slider to control number of columns when multiple features are entered ####
                 vln_ncol_slider <- eventReactive(input$text_features, ignoreNULL = FALSE,{
                   #Only display slider when there is more than one feature
                   if (length(input$text_features) <= 1){
                     ui <- NULL
                   } else {
                     #Default number of columns: equal to the number of panels if there are 
                     #less than four, otherwise equal to two
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
                 
                 #### 2.1.4.3. Code for conditional UI ####
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
                 
                 #### 2.1.4.4. Code for content ####
                 vln_plot_content <- reactive({
                   if(length(input$text_features>=1)){
                     vln_plot <- VlnPlot(plots_subset(),
                                         features=input$text_features,
                                         group.by = input$vln_group_by,
                                         #Split.by: NULL if user selects "none", otherwise equal to user selection
                                         split.by = if (input$vln_split_by=="none") NULL else input$vln_split_by,
                                         #ncol: NULL if only one feature is entered. If there are multiple features,
                                         #this is equal to what the user specifies
                                         ncol = if (length(input$text_features)==1) NULL else input$vln_ncol
                     ) +
                       #Legend position: "right" if a legend is desired, and "none" if not
                       theme(legend.position = if (input$vln_legend==TRUE) "right" else "none")
                     
                     #Correct titles: change machine-readable name to human-readable name
                     #Determine number of plots created
                     n_patches <- n_patches(vln_plot)
                     #Iterate through each plot, correcting the title
                     vln_plot <- hr_title(vln_plot,n_patches,assay_info)
                     
                     #Return the plot
                     vln_plot
                   }
                 })
                 
                 #### 2.1.4.5. Render UI components and content for violin plot ####
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
                 
                 #### 2.1.4.6. Violin Plot Download ####
                 output$vln_download <- downloadHandler(
                   filename = "Violin_plot.png",
                   content = function(file){
                     if (input$vln_manual_dim==TRUE){
                       ggsave(file, 
                              plot=vln_plot_content(), 
                              device="png",
                              width=vln_width(),
                              height=vln_height(),
                              dpi=72,
                              units="px")
                     } else {
                       ggsave(file, 
                              plot=vln_plot_content(), 
                              device="png")
                     }
                   },#End content function
                   contentType = "image/png"
                 ) #End downloadHandler function
                 
                 ### 2.1.5. Dot plot #####
                 #### 2.1.5.1. Reactive plot dimensions ######
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
                 
                 #### 2.1.5.2. Feature choices #####
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
                 
                 #### 2.1.5.3. Generate UI for dot plot #####
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
                 
                 #### 2.1.5.4. Generate dot plot content #####
                 dot_plot_content <- reactive({
                   #Only renders if condition C in 2.5.2 is met
                   if (((input$diff_features_dot==FALSE)&(length(input$text_features)>=1))|((input$diff_features_dot==TRUE)&(length(input$dot_features)>=1))){
                     #If user specifies the use of different features, use the dot plot-specific features instead of the generic text entry features
                     if (input$diff_features_dot==TRUE){
                       DotPlot(plots_subset(),
                               features = input$dot_features,
                               group.by = input$dot_group_by) + 
                         RotatedAxis() +
                         #Legend position: "right" if a legend is desired, and "none" if not
                         theme(legend.position = if (input$dot_legend==TRUE)"right" else "none")
                     }
                     else {
                       #Check if split.by is specified
                       DotPlot(plots_subset(), 
                               features = input$text_features,
                               group.by = input$dot_group_by) + 
                         RotatedAxis() +
                         #Legend position: "right" if a legend is desired, and "none" if not
                         theme(legend.position = if (input$dot_legend==TRUE)"right" else "none")
                     }
                   }
                 })
                 
                 #### 2.1.5.5. Render dot plot UI and content ####
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
                 
                 #### 2.1.5.6. Dot Plot Download #####
                 output$dot_download <- downloadHandler(
                   filename = "Dot_plot.png",
                   content = function(file){
                     if (input$dot_manual_dim==TRUE){
                       ggsave(file, 
                              plot=dot_plot_content(), 
                              device="png",
                              width=dot_width(),
                              height=dot_height(),
                              dpi=72,
                              units="px",
                              #Explicitly state white background color (plots were transparent)
                              bg="#FFFFFF")
                     } else {
                       ggsave(file, 
                              plot=dot_plot_content(), 
                              device="png",
                              #White background
                              bg="#FFFFFF")
                     }
                   },#End content function
                   contentType = "image/png"
                 ) #End downloadHandler function
                 
               })
  }
