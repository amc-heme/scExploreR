#' plot_dimension_input
#' 
#' A UI function used in the manual_dimensions module. creates a slider and text
#' box combo to input either the width or height of a plot 
#'
#' @param slider_input_id the desired input id for the slider.
#' @param box_input_id the input id for the text box.
#' @param label text to display in bold above the slider and the text box combo.
#' @param slider_min the minimum acceptable value for the slider/text box combo.
#' @param slider_max the maximum acceptable value for the slider/text box combo.
#' @param initial_value the initial value for the combo.
#'
#' @return a list of shiny.tag objects giving a text box and slider combo. Input
#' bindings will be created for the input id's passed to `slider_input_id` and
#' `box_input_id`.
#'
#' @noRd
plot_dimension_input <- 
  function(
    slider_input_id,
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
        style=
          "display: inline-block; width: 60px; margin-bottom:0px; margin-left:5px;",
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