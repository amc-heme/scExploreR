#' Manual Plot Dimensions Module
#'
#' Creates two textbox/slider pairs to specify the width and height of a plot 
#' in pixels.
#' 
#' @param id ID to use for module. All inputs and outputs created will be
#' namespaced using this ID.
#'
#' @return Module UI
#' 
#' @noRd
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

#' Manual Plot Dimensions Module
#'
#' Creates two textbox/slider pairs to specify the width and height of a plot 
#' in pixels.
#' 
#' @param id ID to use for module. All inputs and outputs created will be
#' namespaced using this ID.
#' @param initial_width initial value for width
#' @param max_width maximum value selectable for width
#' @param min_width minimum value
#' @param initial_height initial value for height
#' @param max_height maximum value for height
#' @param min_height minimum value
#'
#' @return module server code
#'
#' @noRd
manual_dimensions_server <- function(id,
                                     #Initial values and bounds for sliders
                                     initial_width=7,
                                     max_width=20,
                                     min_width=2,
                                     initial_height=4,
                                     max_height=20,
                                     min_height=2
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
                         # Slider for specifying width
                         sliderInput(
                           inputId = ns("width"),
                           label =  "Use slider or text box to adjust plot width",
                           min = min_width,
                           value = initial_width,
                           max = max_width,
                           ticks = FALSE,
                           #Append 'inches' to values on slider
                           post = " in"
                         ),
                         # Slider for specifying height
                         sliderInput(
                           inputId = ns("height"),
                           label =  "Use slider or text box to adjust plot height",
                           min = min_height,
                           value = initial_height,
                           max = max_height,
                           ticks = FALSE,
                           #Append 'inches' to values on slider
                           post = " in"
                         ),
                         selectInput(
                           inputId = ns("dpi"),
                           label = "Select resolution in DPI (PNG only):",
                           choices = c(72, 150, 300, 400, 600),
                           selected = 300
                         )
                        ) # End tagList
                     })
                 
                 output$manual_dimensions_inputs <- 
                   renderUI({
                     manual_dimensions_inputs()
                     })

                 # 3. Process Selections for Height and Width ------------------
                 # Listen for changes in either the slider or the text box and
                 # store the value for the slider (this will be equal to
                 # the text box after the reactives in 1. run)
                 width <- reactive(
                   label = glue("{id}: Process Width Selection"),
                   {
                     if (isTruthy(input$manual_dim)){
                       # If the manual dimensions checkbox is checked, store the
                       # width from input
                       
                       #req(input$width)
                       input$width
                       } else {
                         # Otherwise, output NULL for the width argument
                         NULL
                         }
                     })

                 height <- reactive(
                   label = glue("{id}: Process Height Selection"),
                   {
                   if (isTruthy(input$manual_dim)){
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
                     `height` = reactive({height()}),
                     `dpi` = reactive({as.numeric(input$dpi)})
                     )
                   )
              })
}
