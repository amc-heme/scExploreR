#Differential Expression Tab Module

#dge_tab_ui
#Arguments
#id: ID to use for module elements. Should be equal to "dge".
#unique_metadata: a list of the unique metadata values for each of the metadata 
#categories listed in the config file. This is generated in the main server 
#function at startup.
#metadata_config: the metadata section of the config file imported in the 
#main server function
dge_tab_ui <- function(id,
                       unique_metadata,
                       metadata_config,
                       meta_categories
                       ){
  # Namespace function: prevents conflicts with 
  # inputs/outputs defined in other modules 
  ns <- NS(id)
  
  # UI for DGE Tab
  fluidPage(
    sidebarLayout(
      # 1. Options Panel
      sidebarPanel(
        # Add a container to display a waiter over when the options are updating
        div(
          id = ns("sidebar"),
          tags$h3("Differential Gene Expression"),
          tags$p(
          "Use the dropdown menus below to select the desired test, the groups 
          to use, and the subset on which to perform the test."
          ),
            
          # Menus to choose test (DGE or marker identification and 
          # classes/groups to include). Uses dge_test_selection module
            dge_test_selection_ui(
              id = ns("test_selections"),
              meta_choices = meta_choices
              ),
          
          # Menus to choose subset (placed within collapsible panel)
          collapsible_panel(
            inputId = ns("subset_selections_collapsible"), 
            label = "Subset Options", 
            active = TRUE, 
            {
              subset_selections_ui(
                id = ns("subset_selections"),
                unique_metadata = unique_metadata,
                metadata_config = metadata_config
                )
              }),
          
          # UMAP options panel (conditional UI)
          uiOutput(
            outputId = ns("umap_options")
            ),
          
          # Checkbox to return positive markers only (shown for both modes)
          checkboxInput(
            inputId = ns("pos"),
            label="Positive Markers Only",
            value = TRUE
            ),
          
          # Submit button
          actionButton(
            inputId = ns("submit"),
            label = "Update"
            ),
            
          # Download Button
          uiOutput(
            outputId = ns("downloads_ui")
            )
          ) # End dge-sidebar div
        ), # End sidebarPanel (1.3.1)
      
      # 2. Main Panel
      mainPanel(
        div(id = ns("main_panel"), 
            class = "spinner-container-main",
            
            # # TEMP: display reactive variables used in this tab while developing
            # "Test Selection Output",
            # verbatimTextOutput(outputId = ns("test_selection_output")),
            # "Subset Selection Output",
            # verbatimTextOutput(outputId = ns("subset_selection_output")),
            # "Subset information:",
            # verbatimTextOutput(outputId = ns("subset_info_output")),
            # "Subset Stats Output:",
            # verbatimTextOutput(outputId = ns("subset_stats_output")),
            
            # Div added to contain Waiter spinner (forces the spinner to cover 
            # the full main panel)
            uiOutput(outputId = ns("main_panel_ui"))
        ) # End dge_main_panel
      ) # End MainPanel
    ) # End sidebarLayout
  ) # End fluidPage
}


# dge_tab_server
#
# Arguments
# id: Used to match server component of module to UI component
# object: The Seurat Object defined in the main server function
# metadata_config: the metadata section of the config file corresponding 
# to the current object.
# meta_categories: metadata categories retrieved from the config file
# unique_metadata: a list of all the unique metadata values in the current 
# object for the categories defined in the config file.
# meta_choices: a named vector with name-value pairs for the display name of 
# the metadata category and the key used to access the category in the Seurat 
# Object. 
# object_trigger: a reactive trigger that invalidates downstream reactives when 
# the object is changed.
dge_tab_server <- function(id,
                           object,
                           metadata_config,
                           # This will replace metadata_config at some point
                           # (It is derived from the config file and is the only
                           # information used)
                           meta_categories,
                           unique_metadata,
                           meta_choices,
                           valid_features,
                           object_trigger,
                           error_list
                           ){
  moduleServer(
    id,
    function(input, output, session){
      # Server namespace function: for dynamic UI and modules
      ns <- session$ns
      
      # Create spinners to display during computation in dge 
      
      # Spinner for options panel
      # Keeps user from being able to press download 
      # buttons before content is ready
      sidebar_spinner <-
        Waiter$new(
          id = ns("sidebar"),
          html = spin_loaders(id = 2, color = "#555588"),
          color = "#B1B1B188",
          # Gives manual control of showing/hiding spinner
          hide_on_render = FALSE
          )
      
      # Spinner for main panel
      # Displays until hidden at the end of computation
      main_spinner <-
        Waiter$new(
          id = ns("main_panel"),
          html = spin_loaders(id = 2,color = "#555588"),
          color = "#FFFFFF",
          # Gives manual control of showing/hiding spinner
          hide_on_render = FALSE
          )
      
      # 1. Process Selections for DGE Test --------------------------
      # 1.1. Selections for DGE Test
      test_selections <-
        dge_test_selection_server(
          id = "test_selections",
          object = object,
          unique_metadata = unique_metadata,
          metadata_config = metadata_config,
          meta_choices = meta_choices
          )

      # 2. Process Subset Selection Options -------------------------
      ## 2.1. Process group_by category from test_selections
      # The metadata chosen as the group by category in the test
      # selections menu must be hidden from the subset selections
      # inputs, since the subset must be equal to the classes or
      # groups chosen, which are already selected by the user in the
      # test selections module.
      group_by_category <-
        reactive(
          label = "DGE Tab: Process Group by Selection",
          {
            test_selections()$group_by
          })

      ## 2.2. Call subset_selections module
      subset_selections <-
        subset_selections_server(
          id = "subset_selections",
          object = object,
          unique_metadata = unique_metadata,
          metadata_config = metadata_config,
          meta_categories = meta_categories,
          valid_features = valid_features,
          hide_menu = group_by_category
          )

      # 3. Calculations ran after submit button is pressed ----------
      # Includes table, stats, and UMAP
      # Subset criteria (3.1) processed first,
      # UMAP (3.6) processed last
      
      ## 3.1. Process Submit button input ####
      # Pass value of action button to nested modules
      # to control reactivity
      submit_button <-
        reactive({
          input$submit
        })
      
      # Show spinner: runs after submit button is pressed
      show_spinner <-
        eventReactive(
          submit_button(),
          label = "DGE: Show Spinner",
          {
            # Hide the main panel UI while calculations are performed
            # print(glue("hiding {ns('main_panel_ui')}"))
            # Do not namespace IDs for hideElement 
            hideElement(id = "main_panel_ui")
            
            # Display spinners
            log_session(session)
            log_info("DGE Tab: Submit button pressed")
            sidebar_spinner$show()
            main_spinner$show()
            log_session(session)
            log_info("DGE Tab: Spinners displayed.")
            
            # Return the value of submit_button()
            # This makes this eventReactive change each time it is
            # ran, triggering the next reactive in the series
            submit_button()
          })
      
      ## 3.2. Determine if Metaclusters are Requested ####
      # This reactive expression saves the need to include the conditional 
      # statements within each time they are needed during the DGE computation
      metaclusters_present <- 
        eventReactive(
          show_spinner(),
          label = "DGE: Determine if Metaclusters are present",
          # All reactives in 3. must run at startup for output to be
          # properly generated (endless spinner results otherwise)
          # TODO: switch ignoreInit to TRUE and try un-suspending the dge table
          ignoreInit = FALSE,
          {
            #print("DGE 3.2: metaclusters_present")
            # Return TRUE when all conditions below are met
            if (test_selections()$dge_mode == "mode_dge"){
              if (!is.null(test_selections()$group_1) &
                  !is.null(test_selections()$group_2)){
                # At least one group must have more than one 
                # member to create metaclusters 
                if (length(test_selections()$group_1) > 1 |
                    length(test_selections()$group_2) > 1){
                  return(TRUE)
                }
              }
            }
            
            FALSE
          })
      
      ## 3.3. Define subset criteria ####
      # Combines outputs from test selection and subset selection
      # modules. The subset criteria are used both in the
      # make_subset() function and the subset stats module (the
      # latter of which is designed to take a reactive variable
      # as input)
      dge_subset_criteria <-
        eventReactive(
          metaclusters_present(),
          label = "DGE: Subset Criteria",
          # All reactives in 3. must run at startup for output to be
          # properly generated (endless spinner results otherwise)
          ignoreInit = FALSE,
          ignoreNULL = TRUE,
          {
            #print("DGE 3.3: subset criteria")
            # Retrieve information from
            # test_selections Category
            group_by_category <- test_selections()$group_by
            # Chosen groups/classes (store in a vector)
            if (test_selections()$dge_mode == "mode_dge"){
              # For DGE mode: set the vector of choices equal to the
              # selections for the two groups
              choices <-
                c(test_selections()$group_1,
                  test_selections()$group_2)
            } else if (test_selections()$dge_mode == "mode_marker"){
              # Marker identification: use vector of selected classes
              choices <- test_selections()$classes_selected
            }
            
            # Retrieve list of subset selections
            # Must unpack from reactive to avoid modifying the
            # reactive with the test_selections data above
            subset_criteria <- subset_selections$selections()
            # Append test_selections information to selections list
            subset_criteria[[group_by_category]] <- choices
            
            return(subset_criteria)
          })
      
      ## 3.4. Form subset ####
      # Initialize variables to control reset of subset when the 
      # object is changed
      
      # object_init: a reactive value set to TRUE when a new object 
      # is loaded. The currently saved subset will be cleared when
      # object_init is set to TRUE
      object_init <- reactiveVal(FALSE)
      
      # Create a reactive trigger (triggers reset of subset when
      # new object is loaded)
      subset_trigger <- makeReactiveTrigger()
      
      # Upon object change: Set object_init to TRUE when an object 
      # is loaded, and trigger the subset eventReactive to run
      observeEvent(
        # Reacts to object_trigger defined in main server function
        object_trigger$depend(),
        label = "DGE: object_init(TRUE)",
        # Must use ignoreNULL = FALSE for reactive triggers
        ignoreNULL = FALSE,
        # Don't want this to run at startup or upon module creation
        ignoreInit = TRUE,
        {
          object_init(TRUE)
          subset_trigger$trigger()
        })
      
      subset <-
        eventReactive(
          c(dge_subset_criteria(), 
            subset_trigger$depend()),
          label = "DGE: Subset",
          ignoreNULL = FALSE,
          {
            #print("DGE 3.4: make subset")
            
            subset <- 
              tryCatch(
                error = function(cnd){
                  # If the user has entered an advanced subsetting
                  # string, log what was entered
                  log_info("Error in dge tab subsetting.")
                  if (
                    !is.null(subset_selections$user_string())
                  ){
                    log_info("Advanced subsetting: TRUE.")
                    log_info("String entered by user:")
                    log_info(subset_selections$user_string())
                  } else {
                    log_info("Advanced subsetting: FALSE.")
                  }
                  
                  # Use error_handler to display notification to user
                  error_handler(
                    session,
                    cnd_message = cnd$message,
                    # Uses a list of
                    # subset-specific errors
                    error_list = error_list$subset_errors
                  )
                  
                  # Hide the spinners
                  main_spinner$hide()
                  sidebar_spinner$hide()
                  
                  # Return NULL for subset to 
                  # discontinue downstream calculations
                  NULL
                },
                # Begin tryCatch code
                {
                  # If object_init == TRUE, return the full object
                  if (object_init() == TRUE){
                    return(object())
                  } else {
                    # Otherwise, create subset from selections
                    subset <- 
                      make_subset(
                        object = object(),
                        criteria_list = dge_subset_criteria(),
                        user_string = subset_selections$user_string()
                        )
                    
                    # Modification of subset
                    # Metacluster Creation (if applicable)
                    if (metaclusters_present()){
                      # Unpack variables from test_selections for simplified code
                      metadata_column <- test_selections()$group_by
                      group_1 <- test_selections()$group_1
                      group_2 <- test_selections()$group_2
                      
                      # Use case_when to create metacluster 
                      subset@meta.data$metacluster <- 
                        case_when(
                          # If the cell has a value for the group by category, 
                          # assign a group name to it based on the members of the
                          # group (for example, a group with "R" and "S" will be
                          # renamed ("R and S"))
                          subset@meta.data[[metadata_column]] %in% group_1 ~
                            vector_to_text(group_1),
                          subset@meta.data[[metadata_column]] %in% group_2 ~
                            vector_to_text(group_2),
                          TRUE ~ "Unspecified"
                          )
                    } # End if (metaclusters_present())
                    
                    # Return subset from tryCatch
                    subset
                  }
                }) # End tryCatch
            
            # Return subset from eventReactive
            subset
          })
      
      ## TEMP: Check Memory usage after making subset
      observeEvent(
        label = "DGE: Subset Memory Query",
        subset(),
        ignoreNULL = FALSE,
        {
          log_session(session)
          log_info(
            glue(
              "Memory used after creating subset in dge tab {to_GB(mem_used())}"
            )
          )
          
        })
      
      ## 3.5. DGE Continuation Conditional ####
      # After subset is computed, downstream computations should only 
      # proceed if the subset has been created as a result of 
      # pressing the submit button, and not as a result of the reset 
      # that ocurrs when switching tabs. 
      
      # Create reactive trigger: proceeds with computation if the 
      # subset is reset due to switching tabs 
      # (when object_init()==TRUE)
      continue <- makeReactiveTrigger()
      
      observeEvent(
        label = "DGE: Continuation Conditional",
        subset(),
        {
          #print("DGE 3.5: Continuation conditional")
          
          if (object_init() == FALSE){
            # If object_init == FALSE, continue with DGE calculations
            continue$trigger()
          } else {
            # If object_init == TRUE, do not continue.
            
            # Set object_init() back to FALSE in this case
            object_init(FALSE)
            
            # In case spinners are displayed, remove them
            sidebar_spinner$hide()
            main_spinner$hide()
          }
          
        })
      
      ## 3.6. Compute subset stats ####
      subset_stats <- 
        subset_stats_server(
          id = "subset_stats",
          tab = "dge",
          subset = subset,
          meta_categories = meta_categories,
          # Responds to continuation conditional 
          event_expr = dge_table_content,
          group_by_category = group_by_category,
          metaclusters_present = metaclusters_present
        )
      
      ## 3.7. Run Presto ####
      dge_table_content <-
        eventReactive(
          # Chose the first reactive variable in the subset stats
          # list (all are updated simultaneously, and it is desired
          # for presto to run after stats are computed)
          continue$depend(),
          label = "DGE: Run Presto",
          ignoreNULL = FALSE,
          #ignoreInit = TRUE,
          {
            #print("DGE 3.7: Run Presto")
            
            log_session(session)
            log_info("DGE Tab: Begin Presto")
            
            dge_table <- 
              tryCatch(
                error = function(cnd){
                  # Use error_handler to display notification to user
                  error_handler(
                    session,
                    cnd_message = cnd$message,
                    # Generic error messages only
                    error_list = list()
                  )
                  
                  # Hide the spinners
                  main_spinner$hide()
                  sidebar_spinner$hide()
                  
                  # Return nothing for the dge table in the event of an error
                  return(NULL)
                  },
                {
                  dge_table <-
                    # Run presto on the subset, using the group by category
                    wilcoxauc(
                      subset(), 
                      # If metaclusters are requested, use 
                      # "metaclusters" for dge groups
                      group_by = 
                        if (!metaclusters_present()){
                          group_by_category()
                        } else {
                          "metacluster"
                        }
                    ) %>%
                    # Explicitly coerce to tibble
                    as_tibble() %>%
                    # remove stat and auc from the output table
                    select(-c(statistic, pval, auc)) %>%
                    # Using magrittr pipes here because the following
                    # statement doesn't work with base R pipes
                    # remove negative logFCs if box is checked
                    {if (input$pos) filter(., logFC > 0) else .} %>%
                    # Arrange in ascending order for padj, pval (lower
                    # values are more "significant"). Ascending order is
                    # used for the log fold-change
                    arrange(padj, desc(abs(logFC)))
                  
                  # Change "logFC" column to log-2 fold change 
                  # (default output uses a natural log)
                  dge_table$logFC <- 
                    to_log2(dge_table$logFC)
                  
                  # Rename column to explicitly specify log-2 fold change
                  dge_table <-
                    dge_table |>
                    dplyr::rename(Log2FC = logFC)
                  
                  log_session(session)
                  log_info("DGE Tab: Completed Presto ")
                  
                  return(dge_table)
                })
            
            dge_table
          })
      
      ## 3.8. DGE table, as DT for viewing ####
      dge_DT_content <-
        eventReactive(
          dge_table_content(),
          label = "DGE: DT Generation",
          {
            #print("DGE 3.8: DGE table")
            
            datatable(
              dge_table_content(),
              # DT classes applied
              # See https://datatables.net/manual/styling/classes
              class = "compact stripe cell-border hover",
              # Disallow selection of rows/cells
              selection = "none",
              # Add filter interface above columns
              filter = "top",
              # Remove rownames
              rownames = FALSE
            ) %>%
              #Use 5 sig figs (3 or more is sufficient)
              formatSignif(3:7, 5)
          })
      
      ## 3.9. UMAP of DE Selected Groups ####
      dge_umap <-
        eventReactive(
          c(dge_DT_content(), 
            input$umap_group_by),
          ignoreNULL = FALSE,
          label = "DGE: UMAP",
          {
            #print("DGE 3.9: UMAP")
            
            # ncol_argument: number of columns
            # Based on number of classes being
            # analyzed in the subset.
            # Access with double brackets returns a dataframe.
            # Slice for the first row (the unique values)
            n_panel <-
              if (!metaclusters_present()){
                # Standard behavior: use group_by_category
                unique(
                  subset()[[group_by_category()]]
                  # Take first column of unique() results
                )[,1] |>
                  length()
              } else {
                # Exception: use "metaclusters" when metaclusters are present
                unique(
                  subset()[["metacluster"]]
                  # Take first column of unique() results
                )[,1] |>
                  length()
              }
              
            
            #Set ncol to number of panels if less than four
            #Panels are created
            if (n_panel < 4){
              ncol = n_panel
            }
            
            #Use three columns for 4-9 panels
            else if (n_panel>=4 & n_panel<9){
              ncol = 3
            }
            
            #Use four columns for 9+ panels
            else if (n_panel>=9){
              ncol = 4
            }
            
            #Create UMAP of subsetted object
            umap <- DimPlot(
              subset(),
              #Split by the group by category (or metaclusters if enabled)
              split.by = 
                if (!metaclusters_present()){
                  group_by_category()
                  } else "metacluster",
              #Group by variable set in UMAP options panel
              group.by = input$umap_group_by,
              ncol = ncol
            )
            
            umap
          })
      
      # 4. Dynamic UI for Main Panel --------------------------------
      dge_ui <-
        eventReactive(
          #UI now renders once all computations are complete
          subset(),
          label = "DGE Main UI (Define Content)",
          #Do not render main UI at startup to avoid errors
          #ignoreInit=TRUE,
          #ignoreNULL = FALSE,
          {
            # User-defined label for group-by variable (for printing
            # in UI below)
            # TODO: make sure this updates when a different group
            # by variable is submitted
            group_by_label <-
              metadata_config()[[test_selections()$group_by]]$label
            
            # UI to display
            ui <- tagList(
              tags$h2(
                glue("Differential Expression/Marker Genes by
                                {group_by_label} in Subset"),
                class="center"
              ),
              tags$h3("Test Summary", class="center"),
              
              # Subset Stats Module for showing summary stats
              subset_stats_ui(
                id = ns("subset_stats"),
                tab = "dge",
                metadata_config = metadata_config,
                # Pass dge_subset_criteria() to this argument instead
                # of subset_selections() (dge_subset_criteria()
                # includes the group by category)
                meta_categories = meta_categories,
                subset_selections = dge_subset_criteria
              ),
              
              # DGE Table (uses DT data table)
              tags$h3(
                "DGE Table",
                class="center"
                ),
              
              # Output container for table
              DTOutput(
                outputId = ns("table"),
                width = "95%"
                ),
              
              # UMAP plot
              # Title for plot
              # Depends on mode, tested through n_classes()
              # Make sure n_classes is a reactive and is defined 
              # to avoid errors
              if (is.reactive(subset_stats$n_classes())){
                if (!is.null(subset_stats$n_classes())){
                  # Use different titles based on the test used
                  if(subset_stats$n_classes() == 2){
                    # Title for differential gene expression
                    tags$h3(
                      "UMAP of groups being compared",
                      class="center"
                    )
                  } else {
                    # Title for marker identification
                    tagList(
                      # Center text
                      tags$h3(
                        "UMAP by class",
                        class="center"
                      ),
                      tags$p(
                        "(Markers are computed for each group shown)",
                        class="center"
                      )
                    )
                  }
                } else NULL
              } else NULL,
              
              
              # UMAP container
              plotOutput(
                outputId = ns("umap"),
                height = "600px"
              )
            ) # End tagList
            
            ui
          })
      
      # 5. Dynamic UI: Download Buttons for Table and Plots ---------
      dge_downloads_ui <-
        eventReactive(
          c(submit_button(), input$table_rows_selected),
          label = "DGE Download Buttons UI",
          ignoreNULL = FALSE,
          {
            # Conditional level one, !hasName(): TRUE before table
            # is created, FALSE after
            if (!hasName(input, "dge_table_rows_selected")) {
              # Display nothing before table is created
              NULL
            } else {
              # !hasName() == FALSE (table created)
              # Display button to download table after table is created
              div(
                downloadButton(
                  outputId = ns("download_table"),
                  label = "Download Table",
                  icon = icon("table")
                )
              )
            }
          }
        )
      
      # 6. Dynamic UI: Options Panel for UMAP -----------------------
      umap_options <-
        eventReactive(
          subset(),
          label = "DGE: UMAP Options Panel",
          #ignoreNULL = FALSE,
          {
            #Display options panel after the umap is created
            #Test: dge_umap is of class 'ggplot'
            if ("ggplot" %in% class(dge_umap())){
              collapsible_panel(
                inputId = ns("umap_options_panel"),
                label = "UMAP Options",
                active = TRUE,
                #group.by selection
                selectInput(
                  inputId = ns("umap_group_by"),
                  label = "Metadata to Group by:",
                  #Remove "none" from selectable
                  #options to group by
                  choices=
                    meta_choices()[!meta_choices() %in% "none"],
                  #First category in meta_choices is selected
                  #by default
                  selected =
                    meta_choices()[!meta_choices() %in% "none"][1]
                )
              )
            } else {
              #Return nothing unless the UMAP is created
              NULL
            }
          }
        )
      
      # 7. Render DGE UI, table, and UMAP ---------------------------
      #Main UI
      output$main_panel_ui <- renderUI({
        dge_ui()
      })
      
      #Options panel for UMAP
      output$umap_options <- renderUI({
        umap_options()
      })
      
      #Download buttons
      output$downloads_ui <- renderUI({
        dge_downloads_ui()
      })
      
      #Table
      output$table <- renderDT({
        dge_DT_content()
      })
      
      #UMAP plot
      output$umap <- suppressGraphics(
        renderPlot({
          dge_umap()
        })
      )
      
      #During development: render outputs of reactive variables in tab
      # output$test_selection_output <- renderPrint({
      #   test_selections()
      # })
      # output$subset_selection_output <- renderPrint({
      #   subset_selections()
      # })
      # output$subset_info_output <- renderPrint({
      #   subset()
      # })
      # output$subset_stats_output<- renderPrint({
      #   subset_stats()
      # })
      
      # 8. Hide Spinners --------------------------------------------
      observeEvent(
        umap_options(),
        label = "DGE: Hide Spinner",
        {
          #Show UI (do not namespace ID for showElement)
          showElement(id = "main_panel_ui")
          #Hide spinners
          sidebar_spinner$hide()
          main_spinner$hide()
          log_session(session)
          log_info("DGE Tab: Spinners removed.")
        }
      )
      
      # 9. Download Handler for DGE Table ---------------------------
      output$dge_download_table <- downloadHandler(
        filename = function() {
          glue("DGE_table_{input$dge_group_by}.csv")
        },
        content = function(file) {
          write.csv(dge_table_content(),
                    file = file,
                    row.names = FALSE)
        },
        contentType = "text/csv"
      )#End downloadHandler
      }
    )
  }
  