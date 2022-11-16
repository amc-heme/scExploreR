#' threshold_picker_ui 
#'
#' @param id ID to use for module elements.
#' @param plot_width
#' @param plot_height
#' @param buttons_panel
#'
threshold_picker_ui <- 
  function(
    id,
    plot_width = NULL,
    plot_height = NULL,
    buttons_panel = TRUE
    ){
    # Namespace function: prevents conflicts with IDs defined in other modules
    ns <- NS(id)
    
    tagList(
      # Plot is hidden until a feature is passed to the module 
      hidden(
        div(
          id = ns("ridge_plot_ui"),
          # CSS is applied to allow buttons to appear over the plot
          class = "plot-with-buttons",
          # Ridge plot showing histogram of feature expression in current object
          plotOutput(
            outputId = ns("ridge_plot"),
            # Shiny Default width: 100% of parent element
            width = if (!is.null(plot_width)) plot_width else "100%",
            # Shiny Default height: 400px
            height = if (!is.null(plot_height)) plot_height else "400px",
            # Draw vertical line upon user click
            # Adding "plot_hover" will create `input$plot_hover`.
            # A vertical line will be drawn according to the x coordinate 
            # corresponding to the pointer location 
            hover = 
              hoverOpts(
                id = ns("plot_hover"),
                # Hover response will re-compute every 300ms, as long as cursor 
                # is within bounds of plot (throttle behavior)
                delay = 300,
                delayType = "throttle"
              ),
            click =
              clickOpts(
                id = ns("plot_click")
              )
            ),
          div(
            class = "plot-button-panel",
            dropdownButton(
              # status argument adds classes to button
              # Space is intentional. dropdownButton adds "btn-" to the 
              # beginning of the status string
              status = " icon-button plot-button plot-button-dropdown plot-dropDownButton",
              size = "xs",
              tooltip = "Adjust x-axis limits",
              right = FALSE,
              up = TRUE,
              icon = icon("arrows-alt-h"),
              tagList(
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
                actionButton(
                  inputId = ns("apply_xlim"),
                  class = "button-primary float-right",
                  style = "margin-left: 10px;",
                  label = "Apply"
                ),
                actionButton(
                  inputId = ns("restore_xlim"),
                  class = "button-ghost float-right",
                  icon = icon("undo-alt"),
                  label = "Restore Original"
                )
              )
            )
          )
          
          # actionButton(
          #   inputId = "adjust_xlim",
          #   label = NULL,
          #   icon = icon("arrows-alt-h"),
          #   class = "icon-button plot-button"
          #   
          #   )
          )
        ),
      # UI to display statistics based on selected threshold
      # Threshold statistics are hidden until a threshold is selected
      hidden(
        div(
          id = ns("threshold_stats_ui"),
          class = "compact-options-container",
          # Show selected threshold to user
          tags$b(
            class = "center",
            "Chosen threshold"
          ),
          tags$b(
            class ="center half-space-bottom",
            style = 
              "background-color: #FFFFFF; 
               border-radius: 10px;",
            textOutput(
              outputId = ns("chosen_threshold")
              )
            ),
          # Number and percentage of cells above and below threshold
          tags$b(
            "Number of cells above threshold:"
            ),
          textOutput(
            outputId = ns("above_stats")
          ),
          tags$b(
            class = "space-top",
            "Number of cells below threshold:"
            ),
          textOutput(
            outputId = ns("below_stats")
            )
          )
        )
    )
}

#' threshold_picker_server
#'
#' @param id ID to use for module server instance.
#' @param object
#' @param feature
#' @param showhide_animation
#' @param set_threshold A reactive value used to set the selected threshold to
#' a defined value. For example, when editing the threshold value for a feature,
#' this is set to the last value selected for the feature, giving a visual 
#' representation of what the last selection was.
#'
threshold_picker_server <- 
  function(
    id,
    object,
    feature,
    showhide_animation = FALSE,
    set_threshold = NULL
    ){
    moduleServer(
      id,
      function(input, output, session){
        # Server namespacing function
        ns <- session$ns
        
        # Initialize module_data (used to store plots and results, which 
        # undergo state accumulation (some user interactions with plot are 
        # saved, and thus "accumulate")). This type of behavior requires 
        # reactiveValues objects.
        module_data <- reactiveValues()
        
        # When the user hovers over an interactive plot, the x-coordinate
        # returned is on a zero to one scale based on the position on the plot
        # instead of the true x-coordinate on the plot. Furthermore, zero and
        # one do not directly correspond to the lower and upper x-limits, 
        # respectively. The values below are used to convert the coordinates on
        # a 0-1 scale to the true coordinates, and were tuned to yield 
        # the most accurate coordinates possible.
        plot_min_coord <- 0.06
        plot_max_coord <- 0.96
        
        # Spinner that displays over ridge plot during initial computation
        plot_spinner <-
          Waiter$new(
            id = ns("ridge_plot"),
            html = spin_loaders(id = 2, color = "#555588"),
            color = "#B1B1B188",
            # Gives manual control of showing/hiding spinner
            hide_on_render = FALSE
            )
        
        # 1. Show plot when feature() is defined ####
        observe(
          label = glue("{id}: Show/hide Ridge Plot"),
          {
            target_id <- ns("ridge_plot_ui")

            if (isTruthy(feature())){
              showElement(
                id = target_id,
                # If showhide_animation is TRUE, animate plot (will slide 
                # into and out of existence)
                anim = showhide_animation,
                asis = TRUE
                )
            } else {
              # Hide the interactive plot
              hideElement(
                id = target_id,
                anim = showhide_animation,
                asis = TRUE
                )
            }
          })
        
        # 2. Create ridge plot upon feature change ####
        # The plot must be stored in a reactiveValues object (module_data) to
        # avoid reactivity issues when processing hover and click values. 
        # The initial plot is generated below.
        observeEvent(
          feature(),
          label = glue("{id}: Ridge Plot Histogram"),
          ignoreNULL = FALSE,
          # This observer must execute before the observer in 5. to ensure 
          # user-defined thresholds are being applied after the plot is created
          # when switching features (otherwise the threshold will be drawn on
          # the previous plot instead of the current one)
          priority = 2,
          {            
            # When drawing a new plot, clear plots and data associated 
            # with the last plot if present
            module_data$initial_ridge_plot <- NULL
            module_data$ridge_plot_with_threshold <- NULL
            module_data$ridge_plot <- NULL
            module_data$original_xlim <- NULL
            module_data$threshold_x <- NULL
            module_data$threshold_stats <- NULL
            
            # Set module_data$ridge_plot to the plot if a feature is entered
            # in the search bar, otherwise set the plot to NULL
            if (isTruthy(feature())){
              # Prevent unexpected behavior if more than one feature 
              # is specified
              req(length(feature()) == 1)
              
              # Show spinner over plot during computation
              plot_spinner$show()
              
              data <- 
                FetchData(
                  object = object(),
                  vars = feature()
                )
              
              # Get minimum and maximum values of data
              module_data$plot_max <- 
                data |> 
                max()
              
              module_data$plot_min <-
                data |> 
                min()
              
              # The coordinates returned from hover and click events will
              # be multiplied by the range to allow the user to properly select
              # a region on the plot
              # This is a workaround, it would be ideal to find out why Shiny
              # interactive coordinates aren't working for Seurat plots
              module_data$plot_range <- 
                module_data$plot_max - module_data$plot_min
              
              module_data$initial_ridge_plot <-
                shiny_ridge(
                  object = object(), 
                  features_entered = feature(), 
                  group_by = "none", 
                  show_legend = FALSE, 
                  palette = c("#000088"),
                  center_x_axis_title = TRUE
                  ) 
              
              # After drawing the plot, record the original x-axis limits
              module_data$original_xlim <-
                layer_scales(module_data$initial_ridge_plot)$x$range$range
              
              # Also save to module_data$ridge_plot (this plot is the one 
              # printed to the screen)
              module_data$ridge_plot <-
                module_data$initial_ridge_plot
            } else {
              # Do not draw plot if no feature is defined
              module_data$initial_ridge_plot <-
                NULL
            }
            
            plot_spinner$hide()
          })
        
        # 3. Respond to hover event (add vertical line) ####
        # In the event the user hovers over or clicks the plot, add the 
        # corresponding vertical line at the x-coordinate of the click.
        observeEvent(
          input$plot_hover,
          label = glue("{id}: Draw Line on Hover"),
          # IgnoreNULL must be TRUE to avoid the plot computing when
          # input$plot_hover is NULL, which will erase the line from the plot
          # as soon as it is drown
          ignoreNULL = TRUE,
          ignoreInit = TRUE,
          {
            # The saved plot must be different from the initial plot, or a series
            # of vertical lines will be created each time a hover event is
            # registered

            # Transforming X-coordinates for compatibility with hover/click
            # Coordinates are incorrectly being from zero to one, with one
            # being the max value on the plot.
            plot_x_coordinate <-
              interactive_transform(
                x_coord = input$plot_hover$x,
                # Distribution_range: uses the x-axis limits of the plot,
                # as computed in 6.1.
                distribution_range = current_xlim()[2] - current_xlim()[1],
                distribution_minimum = module_data$plot_min,
                plot_min_coord = 0.06,
                plot_max_coord = 0.96
              )

            # Draw vertical line using transformed hover coordinate
            # Hover line is drawn over either the initial plot, or the plot
            # with a defined threshold, depending on whether the user has
            # clicked the plot

            # module_data$click_info is used. module_data$click_info only
            # changes upon a new click, and does not become NULL while the
            # plot is re-drawn
            # if (!is.null(module_data$click_info)){
            #  base_plot <- module_data$initial_ridge_plot
            # } else {
            #   # If module_data$click_info is defined, use the plot with
            #   # the vertical line at the click location
            #   base_plot <- module_data$ridge_plot_with_threshold
            # }

            base_plot <-
              if (!is.null(module_data$ridge_plot_with_threshold)){
                module_data$ridge_plot_with_threshold
              } else{
                module_data$initial_ridge_plot
              }

            module_data$ridge_plot <-
              base_plot +
              geom_vline(
                xintercept = plot_x_coordinate,
                color = "#666666",
                size = 0.75
              )
          })
        
        # 4. Respond to click event ####
        ## 4.1. Draw Vertical line on plot and Compute Statistics ####
        observeEvent(
          input$plot_click,
          label = glue("{id}: Draw Line on Click and Compute Stats"),
          # IgnoreNULL must be TRUE to avoid the plot computing when
          # input$plot_click is NULL, which will erase the line from the plot
          # as soon as it is drown
          ignoreNULL = TRUE,
          ignoreInit = TRUE,
          {
            # Draw a solid, and persistent, line on the x-axis
            # Transfrom x-coordinate of click to match distribution
            plot_x_coordinate <-
              interactive_transform(
                x_coord = input$plot_click$x, 
                # Distribution_range: uses the x-axis limits of the plot, 
                # as computed in 6.1.
                distribution_range = current_xlim()[2] - current_xlim()[1],
                distribution_minimum = module_data$plot_min, 
                plot_min_coord = plot_min_coord, # 0.08, 
                plot_max_coord = plot_max_coord # 0.9
              )
            
            # Record transformed x-coordinate 
            # Round threshold to two decimal places (such that the threshold 
            # displayed will be consistent with the threshold stats computed if
            # it is copied and pasted. To my knowledge, there is no need to use
            # thresholds with greater precision)
            module_data$threshold_x <- 
              plot_x_coordinate |> 
              round(digits = 2)
            
            # Draw vertical line using transformed click coordinate  
            module_data$ridge_plot_with_threshold <-
              module_data$initial_ridge_plot +
              geom_vline(
                xintercept = module_data$threshold_x,
                color = "#000000",
                size = 0.75
              )
            
            # Record threshold statistics
            module_data$threshold_stats <- 
              threshold_stats(
                object = object(), 
                feature = feature(), 
                threshold = module_data$threshold_x
              )
            
            # Record click coordinates
            module_data$click_info <- 
              input$plot_click
            })
        
        ## 4.2 Show stats UI when a threshold is selected ####
        observe(
          label = glue("{id}: Show/hide Threshold Stats"),
          {
            target_id <- "threshold_stats_ui"
            
            if (!is.null(module_data$threshold_x)){
              showElement(
                id = target_id
              )
            } else {
              hideElement(
                id = target_id
              )
            }
          })
        
        # 5. Update a plot with a previously defined threshold ####
        if (is.reactive(set_threshold)){
          # Observer is created only when set_threshold is defined as a 
          # reactive variable (set_threshold is a non-reactive NULL by default,
          # since it is not used on all applications of this module)
          observeEvent(
            set_threshold(),
            ignoreNULL = TRUE,
            ignoreInit = TRUE,
            # Must have a lower priority than the observer in 2. 
            # (see note for that observer)
            priority = 1,
            {
              print("Detected previously set threshold:")
              print(set_threshold())
              
              # When a new value is passed to set_threshold, set the threshold
              # to the new value
              module_data$threshold_x <- set_threshold()
              
              # Draw a vertical line on the plot at the new value, as if a click 
              # event had occurred at the location of the threshold.
              # Draw vertical line using transformed click coordinate  
              module_data$ridge_plot_with_threshold <-
                module_data$initial_ridge_plot +
                geom_vline(
                  xintercept = module_data$threshold_x,
                  color = "#000000",
                  size = 0.75
                )
              
              # Also update module_data$ridge_plot (the plot that is printed)
              module_data$ridge_plot <- module_data$ridge_plot_with_threshold
              
              # Update statistics with new threshold value
              module_data$threshold_stats <- 
                threshold_stats(
                  object = object(), 
                  feature = feature(), 
                  threshold = module_data$threshold_x
                )
            })
          }
        
        # 6. Adjusting X-Axis Limits of plot ####
        ## 6.1. Current X-axis limits ####
        current_xlim <- 
          reactive(
            label = glue("{id}: Current X-Axis Limits of Plot"),
            {
              # Fetch X-axis limits from ggplot object (current plot, whenever
              # *the base plot* is re-drawn (when the feature is changed, or 
              # when x-axis limits are changed, currently))
              req(module_data$initial_ridge_plot)
              
              plot <- module_data$initial_ridge_plot
              
              # If the scale of the plot has been adjusted, the value of 
              # plot$coordinates$default will be FALSE.
              if (!plot$coordinates$default){
                # When the plot has been modified using coord_cartesan, the 
                # limits must be accessed using the limits element of the 
                # coordinates list stored in the ggplot2 object for the plot. 
                plot$coordinates$limits$x
              } else {
                # Otherwise, use the default limits for the data, which are 
                # accessed using layer_scales.
                layer_scales(module_data$initial_ridge_plot)$x$range$range
              }
            })
        
        ## 6.2. Update text entry of limits to reflect current values ####
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
        
        ## 6.3. Respond to apply limits button ####
        # Re-draw Plot With New Limits 
        observeEvent(
          input$apply_xlim,
          ignoreNULL = FALSE,
          ignoreInit = TRUE,
          {
            req(feature())
            
            # Construct plot with new limits
            plot <-
              suppressWarnings(
                shiny_ridge(
                  object = object(), 
                  features_entered = feature(), 
                  group_by = "none", 
                  show_legend = FALSE, 
                  palette = c("#000088"),
                  center_x_axis_title = TRUE,
                  # Pass x-axis limits as a two-element vector
                  xlim = 
                    c(as.numeric(input$lower_xlim), as.numeric(input$upper_xlim))
                )
              )
            
            # Save plot to module_data$initial_ridge_plot
            module_data$initial_ridge_plot <- 
              plot
            
            
            
            # If a threshold is defined, add a vertical line to the plot at
            # the threshold and save to that plot to
            # module_data$ridge_plot_with_threshold (both plots must be updated
            # in this case)
            if (!is.null(module_data$threshold_x)){
              module_data$ridge_plot_with_threshold <-
                plot +
                geom_vline(
                  xintercept = module_data$threshold_x,
                  color = "#000000",
                  size = 0.75
                )
              
              # Update the plot that is printed via the render function in 7.
              # (otherwise the plot will not update on-screen until the user 
              # hovers over it)
              module_data$ridge_plot <- module_data$ridge_plot_with_threshold
            } else {
              # No threshold: update printed plot without a vertical line
              module_data$ridge_plot <- module_data$initial_ridge_plot
            }
          })
        
        ## 6.4. Respond to restore limits button ####
        observeEvent(
          input$restore_xlim,
          ignoreNULL = FALSE,
          ignoreInit = TRUE,
          {
            req(feature())
            
            # Re-draw plot with the original limits 
            plot <-
              suppressWarnings(
                shiny_ridge(
                  object = object(), 
                  features_entered = feature(), 
                  group_by = "none", 
                  show_legend = FALSE, 
                  palette = c("#000088"),
                  center_x_axis_title = TRUE,
                  # Pass NULL to use the default limits for the data
                  xlim = NULL
                  )
                )
            
            # Save plot to module_data$initial_ridge_plot
            module_data$initial_ridge_plot <- 
              plot
            
            # If a threshold is defined, draw the vertical line and then also 
            # save to module_data$ridge_plot_with_threshold
            if (!is.null(module_data$threshold_x)){
              module_data$ridge_plot_with_threshold <-
                plot +
                geom_vline(
                  xintercept = module_data$threshold_x,
                  color = "#000000",
                  size = 0.75
                )
              
              # Update the plot that is printed via the render function in 7.
              # (otherwise the plot will not update on-screen until the user 
              # hovers over it)
              module_data$ridge_plot <- module_data$ridge_plot_with_threshold
            } else {
              # No threshold: update printed plot without a vertical line
              module_data$ridge_plot <- module_data$initial_ridge_plot
              }
            })
        
        
        ## Modal server for changing limits
        # xlim_modal_server(
        #   id = "xlim_modal",
        #   reactive_trigger = reactive({input$adjust_xlim}),
        #   current_xlim = current_xlim,
        #   xlim_orig = original_xlim
        # )
        
        # 7. Render plot and statistics ####
        # Plot
        output$ridge_plot <-
          renderPlot({
            # Must use suppressMessages and print(plot) to suppress a
            # "Picking joint bandwidth of ___" message from ggridges, which
            # overwhelms the console when interactive hovering is used
            # SuppressMessages alone is not enough, print(plot) must also be
            # used. See https://github.com/tidyverse/ggplot2/issues/1101
            suppressMessages(
              print(module_data$ridge_plot)
            )
          })

        # Stats UI
        # output$threshold_stats_ui <-
        #   renderUI({
        #     threshold_stats_ui()
        #   })
        
        # Chosen threshold
        output$chosen_threshold <-
          renderText({
            format(
              module_data$threshold_x,
              # Display at least three sig figs in percentage
              digits = 3,
              # Display at least two digits after decimal point
              nsmall = 2,
              scientific = FALSE
              )
          })
        
        # Number and percentage of cells above and below threshold
        output$above_stats <- 
          renderText({
            glue(
              "{module_data$threshold_stats$n_above}
               ({module_data$threshold_stats$percent_above}%)"
              )
            })
        
        output$below_stats <-
          renderText({
            glue(
              "{module_data$threshold_stats$n_below}
              ({module_data$threshold_stats$percent_below}%)"
              )
            })
        
        # 8. Return chosen threshold from module ####
        # Package into a reactive value for consistency with other modules
        reactive({
          module_data$threshold_x
          })
      })
}
