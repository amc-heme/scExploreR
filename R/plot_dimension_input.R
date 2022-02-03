#Plot_dimension_input: creates a slider and text to input either the width or 
#height of a plot when manual dimensions are desired
#Used in the UI of the manual_dimensions module
plot_dimension_input <-function(slider_input_id,
                                box_input_id,
                                label = NULL,
                                slider_min = 200,
                                slider_max = 2000,
                                initial_value = 400
){
  tagList(
    #Label (if indicated)
    #Provides additional instructions to the user 
    if (!is.null(label)){
      tags$p(
        #Print the string passed to label
        tags$strong(label),
        tags$br(),
        #Add additional instructions after label text
        "(Press enter to update text box value)"
      )
    },
    
    #Slider (takes up 60% of element width)
    span(
      style="display: inline-block; vertical-align:top; width: 60%",
      sliderInput(
        inputId = slider_input_id,
        label = NULL,
        min = slider_min,
        value = initial_value,
        max = slider_max,
        ticks = FALSE,
        #Append 'px' to values on slider
        post = " px"
      )
    ),
    
    #Text box
    span(
      style="display: inline-block; width: 60px; margin-bottom:0px; margin-left:5px;",
      #Serach input: takes text as input, but does not update value until 
      #user presses enter
      searchInput(
        inputId = box_input_id,
        value = initial_value,
        label=NULL
      )
    ),
    
    #px suffix after text box
    span(
      style = "display: inline-block;",
      "px"
    )
  )
}