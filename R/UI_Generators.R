### Manual_dim_UI ###
#Creates two slider-text box pairs for manual control of the height and width of a plot.
manual_dim_UI <- function(plot_type,
                          initial_width=700,
                          max_width=2000,
                          min_width=200,
                          initial_height=400,
                          max_height=2000,
                          min_height=200){
  #Plot_dimension_input: used within manual_dim_UI. Creates a slider and text box intended to input either the width or height of a plot when manual dimensions are desired
  plot_dimension_input <-function(slider_input_id,
                                  box_input_id,
                                  label=NULL,
                                  slider_min=100,
                                  slider_max=1500,
                                  initial_value=350,
                                  style=NULL){
    div(style=style,
        #Label (if indicated)
        #Additional instructions are printed with label
        if (!is.null(label)){
          tags$p(tags$strong(label),
                 tags$br(),
                 "(Press enter to update text box value)")
        },
        
        
        #Slider (takes up 60% of element width)
        span(style="display: inline-block; vertical-align:top; width: 60%",
             sliderInput(inputId=slider_input_id,
                         label=NULL,
                         min = slider_min,
                         value= initial_value,
                         max= slider_max,
                         ticks=FALSE,
                         post=" px")
        ),
        
        #Text box
        span(style="display: inline-block; width: 60px; margin-bottom:0px; margin-left:5px;",
             searchInput(inputId=box_input_id,
                         value = initial_value,
                         label=NULL)),
        #px suffix after text box
        span(style="display: inline-block;",
             "px"))
  }
  
  #Form inputId values for each UI component created
  checkbox_id <- paste0(plot_type,"_manual_dim")
  width_slider_id <- paste0(plot_type,"_width")
  width_textbox_id <- paste0(plot_type,"_width_text")
  height_slider_id <- paste0(plot_type,"_height")
  height_textbox_id <-paste0(plot_type,"_height_text")
  
  #Create string for the "condition" argument in the conditionalpanel
  manual_dimensions_desired <- paste0("input.",checkbox_id,"==true")
  
  div(#Create checkbox: if checked, manual dimensions for the plot will be used according to inputs below.
    checkboxInput(inputId = checkbox_id,
                  label="Manually Adjust Plot Dimensions",
                  value=FALSE),
    
    #Panel below displays when box is checked.
    conditionalPanel(condition=manual_dimensions_desired,
                     #Slider/text box for specifying width
                     plot_dimension_input(slider_input_id = width_slider_id,
                                          box_input_id = width_textbox_id,
                                          label = paste0("Use slider or text box to adjust plot width"), 
                                          initial_value = initial_width, 
                                          slider_min = min_width,
                                          slider_max=max_width),
                     
                     #Slider/text box for height
                     plot_dimension_input(slider_input_id = height_slider_id,
                                          box_input_id = height_textbox_id,
                                          label = paste0("Use slider or text box to adjust plot height"), 
                                          initial_value = initial_height, 
                                          slider_min = min_height, 
                                          slider_max = max_height)
    )#End conditional panel
  )#End div
}

### Subset Menus UI Function ###
# Generates a series of dropdown menus used to define subsets for an operation

#Arguments
#unique_metadata: a list giving the unique values in the metadata categories 
#defined in the config file
#metadata_config: the metadata section of the config file. Menus will be created 
#for each metadata category in the config file.
#menu_categories: a vector giving the metadata categories to create subset menus for. 
#Use (names(config$metadata)) to create menus for all categories included in the config file.
#input_prefix: a string giving the prefix to add to the input id for each menu. 
subset_menus <- function(unique_metadata,
                         metadata_config,
                         menu_categories,
                         input_prefix=""){
  print("Begin subset_menus")
  #Create a list for storing the Shiny tags from each menu 
  menus <- tagList()
  
  for(category in menu_categories){
    #Create menu for the current category
    print(glue("For loop: {category}"))
    menu_tag <- pickerInput(
      #input_prefix: will become unnecessary once the subset menus are placed within a module
      inputId = glue("{input_prefix}{category}_selection"),
      #label: uses the label defined for the category in the config file
      label = glue("Restrict by {metadata_config[[category]]$label}"),
      #choices: filled using the unique_metadata list
      #If the metadata category has defined groups, sort choices into a named list 
      #based on the groups. This will show choices divided by group in the pickerInput menu.
      choices= 
        if(!is.null(metadata_config[[category]]$groups)){
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
      selected = unique_metadata[[category]],
      multiple = TRUE,
      #Options for pickerInput
      options = list(
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
###

### Icon Notification Function
#Defines the HTML to be printed within a notification box. The function takes the name of a Font Awesome icon and a message as input, and will display the icon and the message inline.
icon_notification_ui <- function(icon_name,message){
  span(
    #Icon (inline and enlarged)
    icon(icon_name, style="display: inline-block; font-size: 1.7em;"),
    #Message (inline with icon, font slightly enlarged)
    span(message,style="font-size: 1.17em;")
  )
}
###