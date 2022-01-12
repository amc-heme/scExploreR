#Subset Selections Module

#subset_selections_ui
#arguments
#id: namespace to use for this module. Reccomended id is "<tab_name>_subset_menus".
#unique_metadata: a list of the unique metadata values for each of the metadata 
#categories listed in the config file. This is generated in the app at startup.
#metadata_config: the metadata section of the config file. This does not need to
#be specified if the config list is stored as "config" in the global environment.
subset_selections_ui <- function(id,
                                 unique_metadata,
                                 metadata_config=config$metadata){
  #Namespace function: prevents conflicts with IDs defined in other modules 
  ns <- NS(id)
  
  #Create a list for storing the Shiny tags from each menu 
  menus<-tagList()
  
  for(category in names(metadata_config)){
    #Create menu for the current category
    menu_tag <- pickerInput(
      #input_prefix: will become unnecessary once the subset menus are placed within a module
      inputId=ns(glue("{category}_selection")),
      #label: uses the label defined for the category in the config file
      label=glue("Restrict by {metadata_config[[category]]$label}"),
      #choices: filled using the unique_metadata list
      #If the metadata category has defined groups, sort choices into a named list 
      #based on the groups. This will show choices divided by group in the pickerInput menu.
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
        "size" = 10, 
        #Add "select all" and "deselect all" buttons
        "actions-box"=TRUE
      )
    )#End pickerInput
    
    #Append tag to list using tagList (append() will modify the HTML of the tag)
    menus <- tagList(menus,menu_tag)
  }
  
  #Return list of menu tags
  return(menus)
}

#Server function
#Arguments
#id: the namespace to use for the module. UI-server function pairs should use the same id.
#metadata_config: the metadata section of the config file. This does not need to
#be specified if the config list is stored as "config" in the global environment.
subset_selections_server <- function(id,
                                     metadata_config=config$metadata){
  #Initialize module 
  moduleServer(
    id,
    #Server function for module: uses a separate set of input/output variables 
    function(input,output,session){
      print("begin subset selections server")
      #1. Store all input values from the UI as a reactive list
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
      
      #2. For later: filter menus in UI based on selections
      
      #Return the reactive list of selections 
      return(selections)
    }
  )
}