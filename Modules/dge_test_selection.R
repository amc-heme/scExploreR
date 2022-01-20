#DGE Interface Module

#Processes Inputs for subsetting and specifying groups for marker analysis or 
#differential gene expression.

dge_test_selection_ui <- function(id,
                             dge_mode=c("mode_dge","mode_marker")
                             ){
  #Namespace function: prevents conflicts with IDs defined in other modules 
  ns <- NS(id)
  
  #UI components: 
  #1. choice of group by variable 
  if(dge_mode == "mode_dge"){
    group_by_menu <- 
      selectInput(
        inputId = ns("group_by"),
        label = "Choose metadata to use for differential gene expression:",
        #Remove "none" and "best_response" from selectable options to group by
        choices = meta_choices[!meta_choices %in% "none"],
        #Clusters is selected by default
        selected = "clusters"
        )
  } else if (dge_mode == "mode_marker"){
    group_by_menu <- 
      selectInput(
        inputId = ns("group_by"),
        label = "Choose metadata to use for marker identification:",
        #Remove "none" and "best_response" from selectable options to group by
        choices = meta_choices[!meta_choices %in% "none"],
        #At startup, marker selection is ran with clusters as the
        #group by variable.
        selected="clusters"
      )
  }
  
  #2. Selection of marker classes for group by variable, or groups if mode==dge
  #(dynamically updated based on group_by choice)
  classes_menu <- uiOutput(outputId = "test_classes")
  
  #Combine elements above into tagList and return for display in app
  ui <- tagList(
    group_by_menu,
    classes_menu,
    subset_menus
  )
}

#Server function
#Arguments
#id: the namespace to use for the module. UI-server function pairs should use the same id.
#sobj: The Seurat Object defined in the main server function
#metadata_config: the metadata section of the config file. This does not need to
#be specified if the config list is stored as "config" in the global environment.
#dge_mode: The chosen mode for differential expression analysis (marker selection 
#or differential expression). 
dge_test_selection_server <- function(id,
                                 sobj,
                                 unique_metadata,
                                 metadata_config,
                                 meta_choices,
                                 dge_mode){
  moduleServer(id,
               function(input,output,session){
                 
                 #1. Process group_by choice -----------------------------------
                 group_by_category <- reactive({
                   input$group_by
                 })
                 
                 #Subset_menu_categories (may or may not be used)
                 #Menus are created based on the metadata category specified for 
                 #the group_by argument. The current group_by menu is tracked, and 
                 #all categories that are not the curreny group_by menu are 
                 #recorded in this variable.
                 subset_menu_categories <- reactive({
                   all_categories <- names(metadata_config)
                   #Return all metadata categories except for the current group by selection
                   all_categories[!all_categories %in% input$group_by]
                 })
                 
                 #2. Determine unique values for the group by category ---------
                 #Will be options for marker classes or groups in DGE). This 
                 #process will be the same regardless of whether marker 
                 #selection or differential gene expression is the chosen test
                 group_choices <- 
                   eventReactive(input$group_by,
                                 ignoreNULL = FALSE,
                                 ignoreInit = TRUE,
                                 {
                                   sobj@meta.data |>
                                     #Get unique values for the group by category
                                     select(.data[[input$group_by]]) |> 
                                     unique() |>
                                     #Convert to vector
                                     unlist() |> 
                                     #Remove names from vector 
                                     unname() |> 
                                     #Convert factor of choices to character vector
                                     as.character() |> 
                                     #Sort choices
                                     str_sort(numeric=TRUE)
                                   })
                
                 #3. UI for marker classes or group selection ------------------
                 ##3.1 Define UI
                 test_classes <- 
                   #UI updates after group choices are computed 
                   #(may need to change this)
                   eventReactive(
                     group_choices(),
                     ignoreNULL = FALSE,
                     ignoreInit = TRUE,
                     {
                       if (dge_mode=="dge"){
                         #DGE mode: display menus to select two classes from the 
                         #metadata category chosen as the group_by variable. 
                         #This can be expanded to include groups based on 
                         #multiple metadata selections
                         tagList(
                           #Put choices beside one another in two-column format
                           div(
                             class="two_column float_left",
                             selectInput(
                               inputId = ns("group_1"),
                               label = "Group 1",
                               choices = group_choices(),
                               selected = group_choices()[1]
                               )
                             ), #End div
                           div(
                             class="two_column float_right",
                             selectInput(
                               inputId = ns("group_2"),
                               label = "Group 2",
                               choices = group_choices(),
                               selected = group_choices()[2]
                               )
                             )
                           )#End tagList
                       } else if (dge_mode=="marker"){
                         #Marker mode: display menu to choose values from the 
                         #group by metadata category to include as classes in 
                         #marker identification. 
                         pickerInput(
                           inputId = ns("marker_class_selection"),
                           label = "Choose classes to include in marker computation",
                           #Choices: if there are groups defined in the 
                           #config file for the metadata category, build a list 
                           #with the defined groups for the menu
                           choices = 
                             if (!is.null(metadata_config[[group_by_category()]]$groups)) {
                               #Use group_metadata_choices() to generate list
                               group_metadata_choices(
                                 group_info=
                                   metadata_config[[group_by_category()]]$groups,
                                 choices = 
                                   unique_metadata[[group_by_category()]]
                               )
                             } else {
                               #Otherwise, use the group_choices() vector 
                               group_choices()
                             },
                           
                           #Select all unique values at startup
                           selected = group_choices(),
                           multiple = TRUE,
                           options = list(
                             "selected-text-format" = "count > 3",
                             "size" = 10,
                             #Define max options to show at 
                             #a time to keep menu from being cut off
                             "actions-box"=TRUE
                             )
                           ) #End pickerInput
                         }
                       })
                 
                 ##3.2. Render UI
                 output$test_classes <- renderUI({
                   test_classes()
                 })
                 
                 #4. Update Group 2 Menu Based on Group 1 Selection ------------
                 #Lower priority, add later
                 
                 #5. Process Test Selections -----------------------------------
                 #Record selections for 
                 if (dge_mode == "dge"){
                   #For dge: record two classes selected
                   group_1 <- input$group_1
                   group_2 <- input$group_2
                 } else if (dge_mode == "marker"){
                   #For marker identification: record classes selected
                   classes_selected <- input$marker_class_selection
                 }
                 
                 #Processes the selections for the group by variable, and either 
                 #group or marker selections depending on the mode of DGE selected.
                 selections <- reactive({
                   #Store selections in a list
                   selections <- list()
                   #Record the group by variable
                   selections$group_by <- group_by_category()
                   
                   if (dge_mode == "dge"){
                     #For dge: record two classes selected
                     #Parentheses at end: immediately extracts value from reactive 
                     #context for storage in list (list is reactive, which is sufficient)
                     selections$group_1 <- reactive({input$group_1})()
                     selections$group_2 <- input$group_2
                   } else if (dge_mode == "marker"){
                     #For marker identification: record classes selected
                     selections$classes_selected <- reactive({input$marker_class_selection})()
                   }
                 })
                 
                 #Return selections info from module
                 return(selections)
               })
}
