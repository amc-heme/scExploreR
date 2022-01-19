#Subset Selections Module

#subset_selections_ui
#arguments
#id: namespace to use for this module. Reccomended id is "<tab_name>_subset_menus".
#unique_metadata: a list of the unique metadata values for each of the metadata 
#categories listed in the config file. This is generated in the main server function 
#at startup.
#metadata_config: the metadata section of the config file. This does not need to
#be specified if the config list is stored as "config" in the global environment.
subset_selections_ui <- function(id,
                                 unique_metadata,
                                 tab = c("plots", "dge", "corr"),
                                 metadata_config=config$metadata){
  #Namespace function: prevents conflicts with IDs defined in other modules 
  ns <- NS(id)
  
  #UI for plots and correlations tab
  if (tab %in% c("plots","corr")){
    #Create a list for storing the Shiny tags from each menu 
    menus<-tagList()
    
    for(category in names(metadata_config)){
      #Create menu for the current category
      menu_tag <- pickerInput(
        #input_prefix: will become unnecessary once the subset menus are placed 
        #within a module
        inputId=ns(glue("{category}_selection")),
        #label: uses the label defined for the category in the config file
        label=glue("Restrict by {metadata_config[[category]]$label}"),
        #choices: filled using the unique_metadata list
        #If the metadata category has defined groups, sort choices into a named 
        #list based on the groups. This will show choices divided by group in 
        #the pickerInput menu.
        choices= if(!is.null(metadata_config[[category]]$groups)){
          #Use group_metadata_choices() to generate list
          group_metadata_choices(
            group_info=metadata_config[[category]]$groups,
            choices = unique_metadata[[category]]
          )
        } else {
          #If groups are not defined, use the vector of choices from unique_metadata
          unique_metadata[[category]]
        },
        #selected: all choices selected by default
        selected=unique_metadata[[category]],
        multiple=TRUE,
        #Options for pickerInput
        options=list(
          #Display number of items selected instead of their names 
          #when more than 5 values are selected
          "selected-text-format" = "count > 5",
          #Define max options to show at a time to keep menu from being cut off
          "size" = 7, 
          #Add "select all" and "deselect all" buttons
          "actions-box"=TRUE
        )
      )#End pickerInput
      
      #Append tag to list using tagList (append() will modify the HTML of the tag)
      menus <- tagList(menus,menu_tag)
    }
    
    #Last element: a reset button that will appear when subset menus are filtered 
    #to remove criteria that are mutually exclusive with current selections
    menus <- tagList(menus,
                     uiOutput(outputId = ns("reset_filter_button"))
    )
    
    #Return list of menu tags
    return(menus)

  } else if (tab=="dge") {
    #DGE tab: UI is dynamically generated
    uiOutput(outputId = ns("selections_ui"))
  }
  
}

#Server function
#Arguments
#id: the namespace to use for the module. UI-server function pairs should use the same id.
#sobj: The Seurat Object defined in the main server function
#metadata_config: the metadata section of the config file. This does not need to
#be specified if the config list is stored as "config" in the global environment.
subset_selections_server <- function(id,
                                     sobj,
                                     unique_metadata,
                                     tab = c("plots", "dge", "corr"),
                                     metadata_config = config$metadata){
  #Initialize module 
  moduleServer(
    id,
    #Server function for module: uses a separate set of input/output variables 
    function(input,output,session){
      #Server namespace function: used for UI elements rendered in server
      ns <- session$ns
      
      #0. Dynamic UI Components (DGE tab only) ---------------------------------
      dge_selections_ui <- reactive({
        
      })
      
      
      #Render UI
      
      #1. Store all input values from the UI as a reactive list ----------------
      #Use either the module UI (plots and correlation tabs) or the dynamic UI 
      #(DGE Tab)
      selections <- reactive({
        #Store selections for each input in the UI (one menu is created for each
        #metadata category in the config file)
        selections_list <- lapply(
          names(metadata_config),
          function(category){input[[glue("{category}_selection")]]}
          )
        
        #Add categories from metadata file to list names
        names(selections_list) <- names(metadata_config)
        
        return(selections_list)
        })
      
      #2. UI for Filtering Selection Menus -------------------------------------
      #Subset menus will be filtered for 
      #2.1. filters_applied: a boolean that is TRUE when a subset has been 
      #filtered (this may be changed as the filter code is developed)
      filters_applied <- eventReactive(selections(),
                                       ignoreNULL = FALSE,
                                       {
                                         #Check if selections() is shorter than 
                                         #the original list of choices used to 
                                         #generate the menus. 
                                         not_equal <- 
                                           !(setequal(
                                             unlist(unique_metadata),
                                             unlist(selections())
                                             ))
                                         
                                         #If selections is shorter (not_equal==
                                         #TRUE), return TRUE, and vice versa 
                                         if (not_equal==TRUE){
                                           return(TRUE)
                                         } else {
                                           return(FALSE)
                                         }
                                       })
      
      #2.2. Create UI for "Reset Filter button"
      #Button is needed after filtering is applied to reset selections 
      reset_filter_ui <- eventReactive(filters_applied(),
                                       ignoreNULL = FALSE,
                                       {
                                         if (filters_applied() == TRUE){
                                           #Display reset button if filters have been applied 
                                           actionButton(inputId = ns("reset_filter"),
                                                        label = "Reset Filters",
                                                        icon = icon("times-circle")
                                                        )
                                           #Do not display anything otherwise
                                         } else NULL
                                       })
      
      #Render UI for reset button
      output$reset_filter_button <- renderUI({
        reset_filter_ui()
      })
      
      #3. For later: filter menus in UI based on selections --------------------

      #Approach A
      #Create an observer for each menu created in (1.)
      
      #Define metadata categories to loop through (based on names of selections,
      #which stay constant regardless of which options are selected)
      all_categories <- isolate(names(selections()))
      
      #Use lapply to build observers
      lapply(
        X = all_categories,
        FUN = function(current_category){
          observeEvent(input[[glue("{current_category}_selection")]],
                       ignoreNULL = FALSE,
                       ignoreInit = TRUE,
                       {
                         #TEMP: display notification to verify the observer 
                         #is responding correctly
                         showNotification(
                           icon_notification_ui(
                             message=glue("change observed: {current_category}")
                           )
                         )
                         #also print console message
                         print(glue("change observed: {current_category}"))
                         
                         #When the observer is triggered, change all other menus 
                         #based on the selections chosen
                         #Define values of other categories
                         other_categories <- 
                           all_categories[!all_categories %in% current_category] 
                         
                         #For each other category, update the picker inputs based 
                         #on the values selected in the current menu (current category)
                         for (other_category in other_categories){
                           #Generate list of valid choices for the category
                           valid_choices <- sobj@meta.data |> 
                             #Filter object for selections made in current category
                             filter(
                               .data[[current_category]] %in% 
                                 input[[glue("{current_category}_selection")]]
                               ) |> 
                             #Select the column for the *other* category
                             select(.data[[other_category]]) |> 
                             #Fetch unique values
                             unique() |> 
                             #Convert to character vector (must use both unlist() 
                             #and as.character() to get correct values for 
                             #categories that are factors)
                             unlist() |> 
                             as.character()
                           
                           print(glue("Compatible options for {other_category}"))
                           print(valid_choices)
                           
                           #If the category has groups defined in the config file, 
                           #sort the valid values into those groups
                           group_info <- metadata_config[[other_category]]$groups
                           
                           if(!is.null(group_info)){
                             #Use group_metadata_choices() function to do this
                             choices_list <- 
                               group_metadata_choices(group_info,valid_choices)
                             
                             print(glue("List of values for {other_category}"))
                             print(choices_list)
                           } else {
                             #If there is no group info for the category, do not
                             #group choices into a list
                             choices_list <- NULL
                           }
                           
                           #Update picker input with valid choices
                           print(glue("Update menu for {other_category}"))
                           updatePickerInput(session,
                                             inputId = glue("{other_category}_selection"),
                                             #Choices: use the list of group 
                                             #choices if it is defined
                                             choices = 
                                               if (!is.null(choices_list)) {
                                                 choices_list
                                               } else {
                                                   valid_choices
                                                 },
                                             selected = valid_choices,
                                             options = 
                                               list(
                                               "selected-text-format" = "count > 5",
                                               "actions-box"=TRUE
                                               )
                                             )
                         }
                         
                         
                       }) #End observeEvent
        }) #End lapply
      
      #Approach B: one observer, pointed to slections() instead of individual menus
      # observeEvent(selections(),
      #              ignoreNULL = FALSE,
      #              ignoreInit = TRUE,
      #              {
      #                #Temp: console message
      #                print("Change observed")
      #                
      #                #Filter Seurat object for valid options in all of the selections
      #                valid_table <- sobj@meta.data |> 
      #                  #Filter object for selections made in current category
      #                  filter(
      #                    .data[[current_category]] %in% 
      #                      input[[glue("{current_category}_selection")]]
      #                  ) |> 
      #                  #Select the column for the *other* category
      #                  select(.data[[other_category]]) |> 
      #                  #Fetch unique values
      #                  unique() |> 
      #                  #Convert to character vector (must use both unlist() 
      #                  #and as.character() to get correct values for 
      #                  #categories that are factors)
      #                  unlist() |> 
      #                  as.character()
      #                
      #              })
      
      
      
      #Return the reactive list of selections 
      return(selections)
    }
  )
}
