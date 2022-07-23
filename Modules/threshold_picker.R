#' threshold_picker_ui 
#'
#' @param id ID to use for module elements.
#' @param plot_width
#' @param plot_height
#'
threshold_picker_ui <- 
  function(
    id,
    plot_width = NULL,
    plot_height = NULL
    ){
    # Namespace function: prevents conflicts with IDs defined in other modules
    ns <- NS(id)
    
    tagList(
      # Plot is hidden until a feature is passed to the module 
      hidden(
        div(
          id = ns("ridge_plot_ui"),
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
            )
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
#'
threshold_picker_server <- 
  function(
    id,
    object,
    feature,
    showhide_animation = FALSE
    ){
    moduleServer(
      id,
      function(input,output,session){
        # Server namespacing function
        ns <- session$ns
        
        # Initialize module_data (used to store plots and results, which 
        # undergo state accumulation (some user interactions with plot are 
        # saved, and thus "accumulate")). This type of behavior requires 
        # reactiveValues objects.
        module_data <- reactiveValues()
        
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
            print("Feature processed by threshold server")
            print(feature())
            print("isTruthy test")
            print(isTruthy(feature()))
            
            target_id <- ns("ridge_plot_ui")

            if (isTruthy(feature())){
              print("showElement target")
              print(ns(target_id))
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
        
        # 2. Define Ridge Plot Showing Feature Expression ####
        # The plot must be stored in a reactiveValues object (module_data) to
        # avoid reactivity issues when processing hover and click values. 
        # The initial plot is generated below.
        observeEvent(
          feature(),
          label = glue("{id}: Ridge Plot Histogram"),
          ignoreNULL = FALSE,
          {            
            # When drawing a new plot, clear plots and data associated 
            # with the last plot if present
            module_data$initial_ridge_plot <- NULL
            module_data$ridge_plot_with_threshold <- NULL
            module_data$ridge_plot <- NULL
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
            #   # Extract the object from patchwork format
            #   module_data$initial_ridge_plot <-
            #     module_data$initial_ridge_plot[[1]]
              
            } else {
              # Do not draw plot if no feature is defined
              module_data$initial_ridge_plot <-
                NULL
            }
            
            plot_spinner$hide()
          })
        
        # 3. Add vertical Line Upon Hovering ####
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
            x_original <-
              interactive_transform(
                x_coord = input$plot_hover$x, 
                distribution_range = module_data$plot_range,
                distribution_minimum = module_data$plot_min, 
                plot_min_coord = 0.08, 
                plot_max_coord = 0.9
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
                xintercept = x_original,
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
            x_original <-
              interactive_transform(
                x_coord = input$plot_click$x, 
                distribution_range = module_data$plot_range,
                distribution_minimum = module_data$plot_min, 
                plot_min_coord = 0.08, 
                plot_max_coord = 0.9
              )
            
            # Record transformed x-coordinate 
            # Round threshold to two decimal places (such that the threshold 
            # displayed will be consistent with the threshold stats computed if
            # it is copied and pasted. To my knowledge, there is no need to use
            # thresholds with greater precision)
            module_data$threshold_x <- 
              x_original |> 
              round(digits = 2)
            
            # Draw vertical line using transformed hover coordinate  
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
            
            # print("Cells above")
            # print(module_data$threshold_stats$n_above)
            # print("Cells below")
            # print(module_data$threshold_stats$n_below)
            
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
        
        # 5. Render plot and statistics ####
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
        
        # 6. Return chosen threshold from module ####
        # Package into a reactive value for consistency with other modules
        reactive({
          module_data$threshold_x
          })
      })
}
