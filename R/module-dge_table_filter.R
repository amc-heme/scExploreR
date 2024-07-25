#' dge_table_filtering UI
#'
#' @noRd
dge_table_filtering_ui <- function(id){
  # Namespace function
  ns <- NS(id)
  
  hidden(
    div(
      id = ns("filter_ui"),
      scExploreR:::collapsible_panel(
        inputId = ns("filter_ui_panel"),
        label = "Filter Table",
        active = TRUE,
        
        # Filter by group
        # Use a transparent collapsible panel to cut down on clutter in filter
        # interface
        collapsible_panel(
          inputId = ns("group_header"),
          label = "Filter by Group/Class",
          transparent = TRUE,
          size = "s",
          # VituralSelectInput: like PickerInput, but with a more pleasant 
          # interface
          # https://sa-si-dev.github.io/virtual-select/#/?id=get-started
          # Use zIndex to ensure the dropdown displays above other inputs
          # https://github.com/dreamRs/shinyWidgets/issues/701
          shinyWidgets::virtualSelectInput(
            inputId = ns("group"),
            label = NULL,
            # Choices are populated in the server
            choices = NULL, 
            selected = character(0),
            multiple = TRUE,
            search = TRUE,
            optionsCount = 5,
            zIndex = 4
            )
          ),
        
        # By feature
        collapsible_panel(
          inputId = ns("feature_header"),
          label = "Filter by Feature:",
          transparent = TRUE,
          size = "s",
          shiny::selectizeInput(
            inputId = ns("feature"),
            multiple = TRUE,
            label = NULL,
            choices = NULL,
            selected = NULL,
            # Add remove button to inputs
            options = list(
              'plugins' = list('remove_button'),
              # Do not allow user to input features not
              # in the list of options
              'create'= FALSE
            )
          )
        ),
        
        # Average Expression filter interface
        # Collapsible panel is within a container to show/hide the whole
        # interface based on the presence/absence of the average expression column
        div(
          id = ns("expr_ui"),
          collapsible_panel(
            inputId = ns("expr_header"),
            label = "Filter by Average Expression:",
            transparent = TRUE,
            size = "s",
            # Expression range slider
            shiny::sliderInput(
              inputId = ns("expr"),
              label = NULL,
              # Values are updated in the server function based on the table
              min = 0,
              max = 1,
              value = c(0, 1),
              round = 4
            ),
            
            # Text entry for range
            # Displays if checkbox below is enabled
            shinyWidgets::awesomeCheckbox(
              inputId = ns("expr_manual_enable"),
              label = "Manually Enter Range:",
              status = "primary"
            ),
            
            # Display a min and max checkbox side-by-side
            hidden(
              tags$div(
                id = ns("expr_manual_ui"),
                class = "compact-options-container",
                style = 'display: inline-block;',
                div(
                  # Use column() to display inputs side-by-side
                  # column() uses bootstrap layouts for placing components
                  shiny::column(
                    width = 6,
                    style = "padding: 0px 5px;",
                    shinyWidgets::searchInput(
                      inputId = ns("expr_min_text"),
                      label = "Min Expr"
                    )
                  ),
                  shiny::column(
                    width = 6,
                    style = "padding: 0px 5px;",
                    shinyWidgets::searchInput(
                      inputId = ns("expr_max_text"),
                      label = "Max Expr"
                    )
                  )
                )
              )
            )
          )
        ),
        
        # LFC filter interface
        div(
          id = ns("lfc_ui"),
          collapsible_panel(
            inputId = ns("lfc_header"),
            label = "Filter by Log-2 Fold Change Value:",
            transparent = TRUE,
            size = "s",
            # LFC range slider
            shiny::sliderInput(
              inputId = ns("lfc"),
              label = NULL,
              # Values are updated in the server function based on the table
              min = 0,
              max = 1,
              value = c(0, 1),
              round = 4,
              # Ticks are often unhelpful due to the non-integer range of 
              # the slider (ticks will appear in non-intuitive decimal increments)
              ticks = FALSE
              ),
            
            # Text entry of LFC range
            # Displays if checkbox below is enabled
            shinyWidgets::awesomeCheckbox(
              inputId = ns("lfc_manual_enable"),
              label = "Manually Enter Range:",
              status = "primary"
              ),
            
            # Display a min and max checkbox side-by-side
            hidden(
              tags$div(
                id = ns("lfc_manual_ui"),
                class = "compact-options-container",
                style = 'display: inline-block;',
                div(
                  # Use column() to display inputs side-by-side
                  # column() uses bootstrap layouts for placing components
                  shiny::column(
                    width = 6,
                    style = "padding: 0px 5px;",
                    shinyWidgets::searchInput(
                      inputId = ns("lfc_min_text"),
                      label = "Min LFC"
                    )
                  ),
                  shiny::column(
                    width = 6,
                    style = "padding: 0px 5px;",
                    shinyWidgets::searchInput(
                      inputId = ns("lfc_max_text"),
                      label = "Max LFC"
                      )
                    )
                  )
                )
              )
            )
          ),
        
        # AUC slider
        collapsible_panel(
          inputId = ns("auc_header"),
          label = "Filter by AUC Value:",
          transparent = TRUE,
          size = "s",
          shiny::sliderInput(
            inputId = ns("auc"),
            label = NULL,
            # Values are updated in the server function based on the table
            min = 0,
            max = 1,
            value = c(0,1),
            step = 0.05,
            round = 2
            )
          ),
        
        # Filter by p-value
        collapsible_panel(
          inputId = ns("pval_header"),
          label = "Filter by Adjusted p-value:",
          transparent = TRUE,
          size = "s",
          # Adjusted p-value
          shiny::selectInput(
            inputId = ns("pval_adj"),
            label = "Choose a p-value Threshold:",
            choices = 
              c(1,
                0.05, 
                0.01, 
                0.001, 
                1e-04, 
                1e-05, 
                1e-06,
                1e-07,
                1e-08,
                1e-09,
                1e-10, 
                1e-20, 
                1e-50, 
                1e-100),
            selected = 1
            )
          ),
        
        # Percent in group slider
        collapsible_panel(
          inputId = ns("pct_header"),
          label = "Filter by Percentage Within/Outside Group/Class:",
          transparent = TRUE,
          size = "s",
          shiny::sliderInput(
            inputId = ns("pct_in"),
            label = "Filter by Percentage in Group/Class:",
            min = 0,
            max = 100,
            value = c(0, 100),
            step = 5,
            post = " %"
            ),
          
          shiny::sliderInput(
            inputId = ns("pct_out"),
            label = "Filter by Percentage Outside Group/Class:",
            min = 0,
            max = 100,
            value = c(0, 100),
            step = 5,
            post = " %"
            )
          )
        )
      )
    )
}

#' dge_table_filtering Server
#'
#' @param id Used to match server function to ui function.
#' 
#' @param dge_table the table produced in the DGE tab.
#' 
#' @noRd
dge_table_filtering_server <- 
  function(id,
           dge_table){
    moduleServer(
      id,
      function(input, output, session){
        # 1. Show/hide filter interface ####
        observe({
          # Show/hide based on whether or not the dge table is defined
          target <- "filter_ui"
          
          if (isTruthy(dge_table())){
            shinyjs::showElement(
              id = target,
              anim = TRUE
            )
          } else {
            shinyjs::hideElement(
              id = target,
              anim = TRUE
            )
          }
        })
        
        # 2. Average Expression Filter UI ####
        ## 2.1. Show/hide full interface ####
        # Show only for tables that contain the average expression column
        observe({
          req(dge_table())
          
          target <- "expr_ui"
          
          if ("avgExpr" %in% colnames(dge_table())){
            shinyjs::showElement(
              id = target,
              anim = TRUE
              )
          } else {
            shinyjs::hideElement(
              id = target,
              anim = TRUE
              )
          }
        })
        
        ## 2.2. Manual min/max UI: show/hide, and enable/disable slider ####
        observe({
          manual_interface_id = "expr_manual_ui"
          slider_id = "expr"
          
          if (isTruthy(input$expr_manual_enable)){
            # Show the manual entry interface
            shinyjs::showElement(
              id = manual_interface_id,
              anim = TRUE
              )
            
            # Disable the slider while the text interface is enabled
            shinyjs::disable(
              id = slider_id
              )
          } else {
            # Hide the manual entry interface and re-enable the slider 
            # when the checkbox is unchecked
            shinyjs::hideElement(
              id = manual_interface_id,
              anim = TRUE
            )
            
            shinyjs::enable(
              id = slider_id
            )
          }
        })
        
        # 3. AUC UI ####
        ## 3.1. Show/hide full interface ####
        # Show only for tables that contain the AUC column
        observe({
          req(dge_table())
          
          target <- "auc_ui"
          
          if ("auc" %in% colnames(dge_table())){
            shinyjs::showElement(
              id = target,
              anim = TRUE
            )
          } else {
            shinyjs::hideElement(
              id = target,
              anim = TRUE
            )
          }
        })
        
        ## 3.2. Manual min/max UI: show/hide, and enable/disable slider ####
        observe({
          manual_interface_id = "lfc_manual_ui"
          slider_id = "lfc"
          
          if (isTruthy(input$lfc_manual_enable)){
            # Show the manual min/max entry and disable the slider when 
            # the checkbox is enabled
            shinyjs::showElement(
              id = manual_interface_id,
              anim = TRUE
              )
            
            shinyjs::disable(
              id = slider_id
              )
          } else {
            # When disabled, hide the interface and re-enable the slider
            shinyjs::hideElement(
              id = manual_interface_id,
              anim = TRUE
              )
            
            shinyjs::enable(
              id = slider_id
            )
          }
        })
        
        # 4. Update inputs based on the DGE table returned ####
        observe({
          req(dge_table())
          
          if ("group" %in% colnames(dge_table())){
            # Update filter menu with the group names present in the 
            # original table (the one returned from presto)
            all_groups <- 
              dge_table() %>% 
              dplyr::pull(group) %>% 
              unique()
            
            shinyWidgets::updateVirtualSelect(
              session = session,
              inputId = "group",
              choices = all_groups,
              selected = character(0)
              )
          }
          
          if ("feature" %in% colnames(dge_table())){
            all_features <-
              dge_table() %>% 
              dplyr::pull(feature) %>% 
              unique()
            
            updateSelectizeInput(
              session = session,
              inputId = "feature",
              choices = all_features
              )
          }
          
          # Expression slider: update based on range of avgExpr column
          if ("avgExpr" %in% colnames(dge_table())){
            # Determine min, max L2FC values
            min_expr <- 
              dge_table() %>% 
              pull(avgExpr) %>% 
              min(na.rm = TRUE) %>% 
              signif(digits = 4)
            
            max_expr <- 
              dge_table() %>% 
              pull(avgExpr) %>% 
              max(na.rm = TRUE) %>% 
              signif(digits = 4)
            
            # Update slider with max, min values
            shiny::updateSliderInput(
              session = session,
              inputId = "expr",
              min = min_expr,
              max = max_expr,
              value = c(min_expr, max_expr)
            )
          }
          
          # LFC range slider: update based on range of Log2FC column
          if ("log2FC" %in% colnames(dge_table())){
            # Determine min, max L2FC values
            min_lfc <- 
              dge_table() %>% 
              pull(log2FC) %>% 
              min(na.rm = TRUE) %>% 
              signif(digits = 4)
            
            max_lfc <- 
              dge_table() %>% 
              pull(log2FC) %>% 
              max(na.rm = TRUE) %>% 
              signif(digits = 4)
            
            # Update slider with max, min values
            shiny::updateSliderInput(
              session = session,
              inputId = "lfc",
              min = min_lfc,
              max = max_lfc,
              value = c(min_lfc, max_lfc)
              )
          }
        })
        
        # 5. Conditional processing of inputs ####
        # `expr_value` is Named to avoid namespace collisions with 
        # expr(), expression()
        ## 5.1. Average expression return value ####
        expr_value <- 
          reactive({
            req(dge_table())
            
            print("avgExpr Input processing")
            
            # Conditional values 
            if ("avgExpr" %in% colnames(dge_table())){
              # Return the value of the slider or the text boxes depending on
              # whether manual entry is enabled
              if (isTruthy(input$expr_manual_enable)){
                # Manual (text) entry: return values of max and min, using 
                # NULL for undefined entries (NULL will result in no filtering
                # being applied)
                print("input$expr_min_text")
                print(input$expr_min_text)
                print("input$expr_max_text")
                print(input$expr_max_text)
                
                min <- 
                  if (isTruthy(input$expr_min_text)){
                    as.numeric(input$expr_min_text)
                    } else NULL
                
                max <- 
                  if (isTruthy(input$expr_max_text)){
                    as.numeric(input$expr_max_text)
                    } else NULL
                
                # Explicitly specify `min` and `max` return elements
                # avoids ambiguity that arises if one value is NULL
                # (if c(min, max) were used and max was NULL, the return value 
                # would be of length 1 instead of 2. It would not be clear that
                # this value is the min)
                list(
                  `min` = min,
                  `max` = max
                  )
              } else {
                # Slider entry
                # Return values as-is *unless either bound is equal to the 
                # computed min and max of the slider*. The min and max values 
                # for the slider are rounded, which may result in results equal
                # to the non-rounded values being filtered out incorrectly.
                slider_min <- 
                  dge_table() %>% 
                  pull(avgExpr) %>% 
                  min(na.rm = TRUE) %>% 
                  signif(digits = 4)
                
                slider_max <- 
                  dge_table() %>% 
                  pull(avgExpr) %>% 
                  max(na.rm = TRUE) %>% 
                  signif(digits = 4)
                  
                print("input$expr[1] == slider_min")
                print(input$expr[1] == slider_min)
                
                min <- if (input$expr[1] != slider_min) input$expr[1] else NULL
                max <- if (input$expr[2] != slider_max) input$expr[2] else NULL
                
                # Return values
                list(
                  `min` = min,
                  `max` = max
                  )
              }
            } else {
              # If the average expression column is not in the table, 
              # return NULL for both the min and the max.
              list(
                `min` = NULL,
                `max` = NULL
                )
              }
          })
        
        ## 5.2. LFC return value ####
        lfc_value <- 
          reactive({
            req(dge_table())
            
            # Conditional values 
            if ("log2FC" %in% colnames(dge_table())){
              # Return the value of the slider or the text boxes depending on
              # whether manual entry is enabled
              if (isTruthy(input$lfc_manual_enable)){
                # Manual (text) entry: return values of max and min, using 
                # NULL for undefined entries (NULL will result in no filtering
                # being applied)
                min <- 
                  if (isTruthy(input$lfc_min_text)){
                    as.numeric(input$lfc_min_text)
                  } else NULL
                
                max <- 
                  if (isTruthy(input$lfc_max_text)){
                    as.numeric(input$lfc_max_text)
                  } else NULL
                
                # Explicitly specify `min` and `max` return elements
                # avoids ambiguity that arises if one value is NULL
                # (if c(min, max) were used and max was NULL, the return value 
                # would be of length 1 instead of 2. It would not be clear that
                # this value is the min)
                list(
                  `min` = min,
                  `max` = max
                )
              } else {
                # Slider entry
                # Return values as-is *unless either bound is equal to the 
                # computed min and max of the slider*. The min and max values 
                # for the slider are rounded, which may result in results equal
                # to the non-rounded values being filtered out incorrectly.
                slider_min <- 
                  dge_table() %>% 
                  pull(log2FC) %>% 
                  min(na.rm = TRUE) %>% 
                  signif(digits = 4)
                
                slider_max <- 
                  dge_table() %>% 
                  pull(log2FC) %>% 
                  max(na.rm = TRUE) %>% 
                  signif(digits = 4)
                
                min <- if (input$lfc[1] != slider_min) input$lfc[1] else NULL
                max <- if (input$lfc[2] != slider_max) input$lfc[2] else NULL
                
                # Return values
                list(
                  `min` = min,
                  `max` = max
                )
              }
            } else {
              print("lfc not in table. Returning NULL/NULL for lfc")
              # If the average expression column is not in the table, 
              # return NULL for both the min and the max.
              list(
                `min` = NULL,
                `max` = NULL
              )
            }
          })
        
        # 6. Return filter inputs ####
        return(
          list(
            `group` = reactive({input$group}),
            `feature` = reactive({input$feature}),
            `expression` = expr_value,
            `lfc` = lfc_value,
            `auc` = reactive({input$auc}),
            # pval_adj selection must be converted to numeric before returning
            `pval_adj` = reactive({as.numeric(input$pval_adj)}),
            `pct_in` = reactive({input$pct_in}),
            `pct_out` = reactive({input$pct_out})
            )
        )
      }
  )
}