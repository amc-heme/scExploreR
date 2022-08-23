#' custom_xlim_ui 
#'
#' @param id ID to use for module elements.
#' @param compact_buttons
#'
custom_xlim_ui <- 
  function(
    id,
    compact = TRUE
    ){
    # Namespace function: prevents conflicts with IDs defined in other modules
    ns <- NS(id)
    
    # UI: Text entry for lower and upper limits, and a set of buttons
    div(
      # Add compact-options-container class if compact is TRUE
      class = 
        if (compact == TRUE){
          "compact-options-container"
          } else {
            ""
            },
      div(
        class = "inline-containers",
        textInput(
          inputId = ns("lower_xlim"),
          label = "Lower Bound:",
          # Default value is filled in the server function
          value = NULL
          ),
        textInput(
          inputId = ns("upper_xlim"),
          label = "Upper Bound:",
          value = NULL
          )
      ),
      div(
        id = ns("buttons"),
        # Restore original x-axis limits
        actionButton(
          inputId = ns("restore_xlim"),
          class = 
            if (compact == TRUE){
              "button-ghost compact-button"
            } else {
              "button-ghost"
            },
          icon = icon("undo-alt"),
          label = "Reset"
          ),
        # Apply button  
        actionButton(
          inputId = ns("apply_xlim"),
          class = 
            if (compact == TRUE){
              "button-primary compact-button"
            } else {
              "button-primary"
            },
          icon = icon("sync"),
          label = "Update"
          )
        )
      )
    }

#' custom_xlim_server
#'
#' @param id ID to use for module server instance.
#'
custom_xlim_server <- 
  function(
    id,
    plot
    ){
    moduleServer(
      id,
      function(input, output, session){
        # x_limits: changes upon pressing the apply or reset buttons and is 
        # returned from the module
        x_limits <- reactiveVal(NULL)
        
        # 1. Current X-axis limits ####
        # Current limits are used to update text inputs
        current_xlim <- 
          reactive(
            label = glue("{id}: Current X-Axis Limits of Plot"),
            {
              req(plot())
              
              # Location of current limits
              # If the scale of the plot has been adjusted, the value of 
              # plot$coordinates$default will be FALSE.
              if (!plot()$coordinates$default){
                # When the plot has been modified using coord_cartesan, the 
                # limits are accessed using the limits element of the 
                # coordinates list stored in the ggplot2 object for the plot. 
                plot()$coordinates$limits$x
              } else {
                # Otherwise, access the default limits using layer_scales.
                layer_scales(plot())$x$range$range
              }
            })
        
        # 2. Update text inputs when current x-axis limits change ####
        observe(
          label = glue("{id}: Update Text Entry of Limits"),
          {
            # Observer will update whenever the plot axes are re-drawn (when 
            # current_xlim changes)
            updateTextInput(
              session = session,
              inputId = "lower_xlim", 
              value = current_xlim()[1]
            )
            
            updateTextInput(
              session = session,
              inputId = "upper_xlim", 
              value = current_xlim()[2]
            )
          })
        
        # 3. Apply button ####
        # Update x-axis limits returned from module
        observeEvent(
          input$apply_xlim,
          # ignoreNULL must be TRUE to avoid updating x_limits when the inputs
          # are not defined, which will cause the plot to be drawn with limits
          # equal to c(NA, NA) instead of the default limits. If this happens,
          # the update statements in 2. will not function properly since the
          # value of current xlim will be based on plot()$coordinates$limits$x
          # since default limits are no longer being used, but this value will
          # be equal to c(NA, NA). The text inputs will initialize as blank in
          # this scenario. 
          ignoreNULL = TRUE,
          ignoreInit = TRUE,
          {
            x_limits(
              c(as.numeric(input$lower_xlim), 
                as.numeric(input$upper_xlim))
              )
          })
        
        # 4. Reset button ####
        # Set the returned limits to NULL.
        observeEvent(
          input$restore_xlim,
          ignoreNULL = FALSE,
          ignoreInit = TRUE,
          {
            x_limits(NULL)
          })
        
        # 5. Return Limits ####
        return(x_limits)
      })
}
