# Manual Plot Dimensions Module
# Creates two textbox/slider pairs to specify the width and height of a plot in pixels.
manual_dimensions_ui <- function(id){
  # Namespace function: prevents conflicts with IDs defined in other modules
  ns <- NS(id)
  
  tagList(
    # Checkbox to enable manual adjustment of dimensions
    checkboxInput(
      inputId = ns("manual_dim"),
      label = "Manually Adjust Plot Dimensions",
      value = FALSE),
    
    # Dynamic UI: appears when the checkbox above is checked
    uiOutput(outputId = ns("manual_dimensions_inputs"))
  )
}

manual_dimensions_server <- function(id,
                                     #Initial values and bounds for sliders
                                     initial_width=700,
                                     max_width=2000,
                                     min_width=200,
                                     initial_height=400,
                                     max_height=2000,
                                     min_height=200
                                     ){
  moduleServer(id,
               function(input,output,session){
                 
                 # Server namespace function: for dynamic UI and modules
                 ns <- session$ns
                 
                 # 1. Dynamic UI ------------------------------------------------
                 # Appears when the user checks the "Manually Adjust Plot 
                 # Dimensions" checkbox
                 manual_dimensions_inputs <- 
                   eventReactive(
                     input$manual_dim,
                     label = glue("{id}: Manual Dimensions UI"),
                     {
                       # Experimental: req() in place of if statement
                       # req() will cause this UI not to be rendered if 
                       # input$manual_dim==FALSE (downstream reactives will also 
                       # not calculate until the input is TRUE)
                       req(input$manual_dim)
                       
                       tagList(
                         # Slider/text box for specifying width
                         plot_dimension_input(
                           slider_input_id = ns("width"),
                           box_input_id = ns("width_text"),
                           label = 
                             "Use slider or text box to adjust plot width", 
                           # Initial, min, and max values: 
                           # use values passed to server function
                           initial_value = initial_width, 
                           slider_min = min_width,
                           slider_max = max_width
                           ),
                         
                         # Slider/text box for height
                         plot_dimension_input(
                           slider_input_id = ns("height"),
                           box_input_id = ns("height_text"),
                           label = 
                             "Use slider or text box to adjust plot height", 
                           # Initial, min, and max values: 
                           # use values passed to server function
                           initial_value = initial_height, 
                           slider_min = min_height, 
                           slider_max = max_height
                           )
                       ) # End tagList
                     })
                 
                 output$manual_dimensions_inputs <- 
                   renderUI({
                     manual_dimensions_inputs()
                     })
                 
                 # 2. Reactively Update Inputs ----------------------------------
                 # Update values between slider/text box pairs when either 
                 # input is changed
                  
                 # Width
                 # Update text box to match slider when the slider is changed
                 observeEvent(
                   input$width,
                   label= glue("{id}: Respond to Change in Width Slider"),
                   {
                     updateSearchInput(
                       session, 
                       inputId = "width_text", 
                       value = input$width, 
                       trigger = TRUE
                       )
                     })
        
                 # Update slider based on text entry (search input waits until 
                 # user presses enter to update)
                 observeEvent(
                   input$width_text,
                   label= glue("{id}: Respond to Change in Width Text Box"),
                   {
                     updateSliderInput(
                       session, 
                       inputId = "width", 
                       value = input$width_text
                       )
                     
                     # Edge case 1: text box is set to a value below the minimum 
                     # value of the slider. The slider will stay at the minimum 
                     # value, but the text box will not be updated
                     # Must convert input$width_text to numeric to perform 
                     # comparison (input is stored as a character vector)
                     if (as.numeric(input$width_text) < min_width){
                       # In this case, set the *text* input to the min value
                       updateSearchInput(
                         session, 
                         inputId = "width_text", 
                         value = min_width, 
                         trigger = TRUE
                         )
                       # Also, display a notification to the user
                       showNotification(
                         ui = 
                           icon_notification_ui_2(
                             icon = "exclamation-triangle",
                             glue("Value lower than minimum 
                                  width ({min_width}px).")
                             ),
                         #Show notification for 3 seconds
                         duration = 3,
                         session=session
                       )
                     }
                     
                     # Edge case 2: text box set to a value above the maximum 
                     # value of the slider)
                     # input$width_text is a character vector. Must convert to
                     # numeric for comparison
                     if (as.numeric(input$width_text) > max_width){
                       # Set text input to max value
                       updateSearchInput(
                         session, 
                         inputId = "width_text", 
                         value = max_width, 
                         trigger = TRUE
                         )
                       
                       # Also, display a notification to the user
                       showNotification(
                         ui = 
                           icon_notification_ui_2(
                             icon = "exclamation-triangle",
                             glue("Value greater than max width 
                                  ({max_width}px).")
                           ),
                         #Show notification for 3 seconds
                         duration = 3,
                         session=session
                       )
                     }
                     
                     })
                 
                 # Height
                 # Update text box to match slider when the slider is changed
                 observeEvent(
                   input$height,
                   label= glue("{id}: Respond to Change in Height Slider"),
                   {
                     updateSearchInput(
                       session, 
                       inputId = "height_text", 
                       value = input$height, 
                       trigger = TRUE
                       )
                     })
                 
                 # Update slider based on text entry (search input waits until 
                 # user presses enter to update)
                 observeEvent(
                   input$height_text,
                   label = glue("{id}: Respond to Change in Height Text Box"),
                   {
                     updateSliderInput(
                       session, 
                       inputId = "height", 
                       value=input$height_text
                       )
                     
                     # Edge case 1: text box is set to a value below the minimum 
                     # value of the slider.
                     # Must convert input$width_text to numeric to perform 
                     # comparison (input is stored as a character vector)
                     if (as.numeric(input$height_text) < min_height){
                       # In this case, set the *text* input to the min value
                       updateSearchInput(
                         session, 
                         inputId = "height_text", 
                         value = min_height, 
                         trigger = TRUE
                       )
                       
                       # Also, display a notification to the user
                       showNotification(
                         ui = 
                           icon_notification_ui_2(
                             icon = "exclamation-triangle",
                             glue("Value lower than minimum 
                                  height ({min_height}px).")
                           ),
                         #Show notification for 3 seconds
                         duration = 3,
                         session=session
                       )
                     }
                     
                     # Edge case 2: text box set to a value above the maximum 
                     # value of the slider
                     # Must convert input$width_text to numeric to perform 
                     # comparison (input is stored as a character vector)
                     if (as.numeric(input$height_text) > max_height){
                       # Set text input to max value
                       updateSearchInput(
                         session, 
                         inputId = "height_text", 
                         value = max_height, 
                         trigger = TRUE
                       )
                       
                       # Also, display a notification to the user
                       showNotification(
                         ui = 
                           icon_notification_ui_2(
                             icon = "exclamation-triangle",
                             glue("Value greater than max height: 
                                  ({max_height}px).")
                           ),
                         #Show notification for 3 seconds
                         duration = 3,
                         session=session
                       )
                     }
                     })
                 
                 # 3. Process Selections for Height and Width -------------------
                 # Listen for changes in either the slider or the text box and 
                 # store the value for the slider (this will be equal to the text 
                 # box after the reactives in 1. run)
                 width <- reactive(
                   label = glue("{id}: Process Width Selection"),
                   {
                     if (input$manual_dim == TRUE){
                       req(input$width)
                       # If the manual dimensions checkbox is checked, store the 
                       # width from input
                       input$width
                       } else {
                         # Otherwise, output NULL for the width argument
                         NULL
                         }
                     })
                 
                 height <- reactive(
                   label = glue("{id}: Process Height Selection"),
                   {
                   if (input$manual_dim == TRUE){
                     req(input$height)
                     # If the manual dimensions checkbox is checked, store the 
                     # height from input
                     input$height
                     } else {
                       # Otherwise, output NULL for the height argument
                       NULL
                       }
                     })
                 
                 # 4. Return Selections for Height and Width -------------------
                 return(
                   list(
                     `width` = reactive({width()}),
                     `height` = reactive({height()})
                     )
                   )
             })
}
