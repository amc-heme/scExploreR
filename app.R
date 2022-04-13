# Load Libraries and Data ------------------------------------------------------
## Initialize libraries ####
library(shiny)
library(Seurat, quietly = TRUE, warn.conflicts = FALSE)

# Shiny add-ons 
library(shinyWidgets, quietly = TRUE, warn.conflicts = FALSE)
library(rintrojs, quietly = TRUE, warn.conflicts = FALSE)
library(shinydashboard, quietly = TRUE, warn.conflicts = FALSE)
library(waiter, quietly = TRUE, warn.conflicts = FALSE)
library(shinycssloaders, quietly = TRUE, warn.conflicts = FALSE)
library(shinyjs, quietly = TRUE, warn.conflicts = FALSE)
# library(shinyBS, quietly = TRUE, warn.conflicts = FALSE)


# Reactlog (for debugging)
library(reactlog, quietly = TRUE, warn.conflicts = FALSE)
options(shiny.reactlog=TRUE, warn.conflicts = FALSE)

# Logging and performance monitoring
library(profvis)
library(pryr)
library(rlog)

# Tidyverse packages
library(tidyverse, quietly = TRUE, warn.conflicts = FALSE)
library(stringr, quietly = TRUE, warn.conflicts = FALSE)
library(dplyr, quietly = TRUE, warn.conflicts = FALSE)
library(ggplot2, quietly = TRUE, warn.conflicts = FALSE)
library(glue, quietly = TRUE, warn.conflicts = FALSE)
library(DT, quietly = TRUE, warn.conflicts = FALSE)

# Additional backend packages
library(presto, quietly = TRUE, warn.conflicts = FALSE)
library(R.devices, quietly = TRUE, warn.conflicts = FALSE)

# Load CSS, JavaScript, and R scripts ------------------------------------------
# Load functions in ./R directory
# Get list of files
source_files <- 
  list.files(
    path = "./R",
    # Pattern, any set of characters, followed by ".R"
    # Period is double escaped
    pattern=".*\\.R", 
    full.names=TRUE, 
    ignore.case=TRUE
    )

# Load .R files in modules directory
source_files <- 
  c(source_files, 
    list.files(
      path = "./Modules", 
      # Pattern, any set of characters, followed by ".R"
      # Period is double escaped
      pattern=".*\\.R", 
      full.names=TRUE, 
      ignore.case=TRUE
      )
    )

# Use source() to import files into R
sapply(source_files, source)

# Load CSS files for app: CSS files are defined and each file is converted to a
# <script> tag using includeCSS(). Each tag defined is passed to a list, which 
# is included in the main UI function.
# Get list of .css files in www/ directory
css_files <- 
  list.files(
    path = "./www",
    pattern=".*\\.css", 
    full.names=TRUE, 
    ignore.case=TRUE
    )

# Create list of style tags for each CSS file
css_list <- lapply(css_files,includeCSS)

# Load Javasctipt files for app: find all .js files in www/ directory
# and create a list of script() tags using includeScript().
# Get list of .js files in www/ directory
js_files <- 
  list.files(
    path = "./www", 
    # Regex: uses \\. to select for files ending in ".js".
    # Pattern arguments require double backslashes for eacape 
    # characters to work (R and regex use the same string 
    # character)
    pattern=".*\\.js", 
    full.names=TRUE, 
    ignore.case=TRUE,
    include.dirs = FALSE
    )
# Create list of style tags for each CSS file
js_list <- lapply(js_files, includeScript)

# Non-reactive Global Variables ------------------------------------------------

# Non-zero proportion threshold: if the proportion of cells for a 
# gene is below this threshold, return a warning to the user.
nonzero_threshold <- 0.10

# Error Handling: define possible errors ---------------------------------------
# Errors are defined in a list using the functions in "./R/error_handling.R". 
# The error_handler() function is executed in a tryCatch() statement and checks
# the error message returned against a list of errors.
## List of errors for subset operations ####
error_list <- list(
  add_error_notification(
    message="cannot allocate vector of size",
    notification_ui=icon_notification_ui_2(
      icon_name = "skull-crossbones",
      tagList(
        "Memory Error: RAM is insufficient for analyzing the specified subset. 
        Please narrow down the subset scope using the restriction criteria to 
        the left, and feel free to", 
        github_link(display_text = "let us know"),
        " ", # Space after link
        "if you repeatedly recieve this error.")#End tagList
      ), # End icon_notification_ui
    notification_id = "mem_error"
    ), # End add_error_notification
  
  # Error 2: Vector memory exhausted
  add_error_notification(
    message="vector memory exhausted",
    notification_ui=icon_notification_ui_2(
      icon_name = "skull-crossbones",
      "Error: vector memory exhausted. If this issue persists, please ",
      github_link("contact us"),
      " with a screenshot of the response criteria selected. For now, narrowing 
      down the subset criteria may resolve the error."
      ), # End icon_notification_ui
    notification_id = "vector_mem_error"
),

# Error 3: No Cells in Subset
add_error_notification(
  message = "No cells found",
  icon_notification_ui_2(
    icon_name = "skull-crossbones",
    "No cells were found matching the defined subset criteria. Please check the 
    subset dropdowns for mutually exclusive selections. If you recieve this error 
    for combinations that should be valid, please",
    github_link("let us know"),
    # Period at end of link
    "."
    ), # End icon_notification_ui
  notification_id = "no_cells_found"
  )# End add_error_notification
)# End list of error definitions (subset_errors)

# Datasets: a list of available datasets with paths to object and config files,
# as well as a description
log_info("R process initialization: loading datasets")
datasets <- 
  list(
    `d0_d30` = 
      list(
        `label` = "Longitudinal Data",
        `object` = 
          readRDS("./Seurat_Objects/longitudinal_samples_20211025.rds"),
        `config` = "./Seurat_Objects/d0-d30-config.rds",
        `description` = 
          "Contains 3 normal bone marrow samples, and longitudinal samples from 
          6 patients with the first sample taken at time of diagnosis and the
          second sample taken approximately one month afterward.",
        `plot` = "./www/d0_d30_UMAP.png"
          ),
    `AML_samples` = 
      list(
        `label` = "AML (Pheresis) Dataset",
        `object` = 
          readRDS("./Seurat_Objects/aml_bmmc_totalvi_20211206_slim1000.rds"),
        `config` = "./Seurat_Objects/AML_TotalVI_config.rds",
        `description` = 
          "Contains 3 normal bone marrow samples, and 23 AML samples.",
        `plot` = "./www/aml_UMAP.png"
        )
    # Important:
    # Uncomment entry below when working on NA handling, 
    # then ***comment out before committing***
    # ,
    # `AML_NA_Test` =
    #   list(
    #     `label` = "NA Test Object",
    #     `object` = readRDS("./Seurat_Objects/NA_example.rds"),
    #     `config` = "./Seurat_Objects/AML_TotalVI_config.rds",
    #     `description` =
    #       "Seurat Object used purely to test the handling of NA values. This
    #       object is identical to the pheresis dataset; the only difference is
    #      that clusters that were labeled as 'unknown' are instead labeled NA.",
    #     `plot` = "./www/aml_UMAP.png"
    #     )
    )
log_info("Datasets successfully loaded")

# Table of Contents ------------------------------------------------------------
# TODO: Add module tree here

# Main UI ----------------------------------------------------------------------
# Navigation panel and references to tabs
ui <- tagList(
  # Add CSS from each .css file in the www/ directory
  # Uses a list of style tags defined at startup
  css_list,
  # Introjs UI: for guided tour
  introjsUI(),
  # Waiter UI: spinners
  useWaiter(),
  # Shinyjs: a Shiny JavaScript extension
  useShinyjs(),
  # CSS and JS for collapsible panel
  navbarPage("Shiny scExplorer",
             windowTitle="Shiny scExplorer",
             position="fixed-top",
             tabPanel(
               "Plots",
               uiOutput(
                 outputId = "plots_dynamic_ui"
                 )
               ),
             tabPanel(
               "Differential Expression",
               uiOutput(
                 outputId = "dge_dynamic_ui"
                 )
               ),
             tabPanel(
               "Gene Correlations",
               uiOutput(
                 outputId = "corr_dynamic_ui"
                 )
               )
             ), # End navbarPage()
  
  ## Buttons on upper-right hand corner of app ---------------------------------
  # Help button - Creates a Dropdown menu when clicked
  # Button should appear in the upper right hand corner of the navbar menu
  # This will be achieved with the button_wizzard.js script
  dropdownButton(
    inputId = "help",
    status="info",
    right=TRUE,
    label = "",
    size="sm",
    icon = icon("question"),
    # Dropdown menu content
    # Header
    tagList(
      tags$p("Help and Background",
             style=
               "color: #888888; 
            margin-bottom: 0px;
            font-size: 1.17em;"
      ),
      
      # Interpreting scRNA-seq plots
      tags$a(
        "Interpereting scRNA-seq Plots",
        href="scRNA_Plots_Explained.html",
        class="blue_hover",
        # Opens link in new tab
        target="_blank", 
        # Cybersecurity measure for links that 
        # open in new tab: prevents tabnapping
        rel="noopener noreferrer" 
        ),
      
      # Tutorial Document
      tags$a(
        "Tutorial Vignette",
        href="Shiny_Vignette.html",
        class="blue_hover",
        # Opens link in new tab
        target="_blank", 
        rel="noopener noreferrer" 
        ), # End Detailed Walkthrough link
      
      # File issue on github
      tags$a(
        "Report a Bug",
        href="https://github.com/amc-heme/DataExploreShiny/issues",
        class="blue_hover",
        # Opens link in new tab
        target="_blank", 
        rel="noopener noreferrer"
        )
    )# End tagList
  ), #End Help Button
  
  # Dataset Button ('DNA' icon)
  dropdownButton(
    inputId = "options",
    status="info",
    right=TRUE,
    label = "",
    size="sm",
    icon = icon("ellipsis-h"),
    actionLink(
      inputId = "open_dataset_window",
      label = "Choose Dataset",
      class = "blue_hover"
    )
  ),
  
  # Include list of scripts built from .js files in www/ directory
  js_list
)

# Main Server function ---------------------------------------------------------
server <- function(input, output, session){
  # Define spinner to display over main screen when the object and config files
  # are loading
  app_spinner <-
    Waiter$new(
      # When the ID is null, the waiter is applied to the 
      # <body> element (entire app)
      id = NULL,
      html = 
        tagList(
          spin_loaders(id = 2, color = "#555588"), 
          div(
            class = "spinner_text",
            "Loading dataset, please wait...")
          ),
      color = "#FFFFFF",
      #Gives manual control of showing/hiding spinner
      hide_on_render = FALSE
    )
  
  render_UI_spinner <- 
    Waiter$new(
      # When the ID is null, the waiter is applied to the 
      # <body> element (entire app)
      id = NULL,
      html = 
        tagList(
          spin_loaders(id = 2, color = "#555588"), 
          div(
            class = "spinner_text",
            "Updating menus to match new dataset...")
        ),
      color = "#FFFFFF",
      #Gives manual control of showing/hiding spinner
      hide_on_render = FALSE
    )
  
  # Create a reactive trigger to run the feature text box update after 
  # the UI is created (originally the UI always ran first, but now it does
  # not update until the correlation tab is opened since it is now a 
  # reactive and the output for the UI is not evaluated until the tab is 
  # opened (lazy evaluation))
  update_features <- makeReactiveTrigger()
  
  ###
  # reactiveValues list for main server function
  main_server <- 
    reactiveValues(
      # modules_created: stores the keys of the module server instances created
      modules_created = list(),
      dge_modules_created = list()
    )
  ###
  
  # 1. Reactively load object and config file ----------------------------------
  log_info(
    glue("New Connection \n(session ID: {session$token})")
    )
  log_info(
    glue("Memory usage upon connection: {to_GB(mem_used())}")
    )
  log_session(session)
  
  # Initialize a reactiveVal for storing the key of the last dataset loaded
  dataset_info <- reactiveValues()
  dataset_info$last_object_key <- NULL
  # Initialize reactiveVal for storing the seurat object
  object <- reactiveVal(NULL)
  config <- reactiveVal(NULL)
  
  # Startup: a reactive value created to get eventReactive expression for
  # loading datasets to run at startup, in addition to when the dataset window 
  # is closed
  startup <- reactiveVal(1)
  # A change to startup is made at startup only (below)
  # Value of startup is not important (though it can't be zero or it will be
  # ignored by observers with ignoreNULL arguments set to TRUE)
  startup(2)
  
  ## 1.1. Event observers to open and close modal ####
  observeEvent(
    input$open_dataset_window,
    label = "Open Dataset Modal",
    {
      showModal(
        data_Modal(
          datasets = datasets,
          selected_key = selected_key
          )
        )
      })
  
  ## 1.2. When the "confirm selection" button is selected, close the window ####
  close_dataset_modal <- 
    eventReactive(
      input$confirm_selection,
      label = "Close Dataset Window",
      #ignoreNULL = FALSE,
      {
        removeModal()
        # Record value of input$confirm_selection (code to load the 
        # object and config file will respond to this reactive expression 
        # instead of to the button itself, ensuring the modal is closed before
        # the dataset is computed, and that the window only closes when the
        # "confirm selection" button is pressed)
        input$confirm_selection
      })
  
  ## 1.3. Loading Conditional ####
  # Detects when the selected dataset has changed after the selection window 
  # is closed.
  # Create reactive trigger that will invalidate the "load/update dataset" 
  # reactive when activated in the observer below
  dataset_change <- makeReactiveTrigger()
  
  observeEvent(
    eventExpr = 
      {
        # eventExpr: observer executes when this expression evaluates to TRUE 
        # Observer should execute at startup (when input$confirm_selection is 
        # NULL) and when the window to change datasets is closed.
        if (is.null(input$confirm_selection)){
          # Before the dataset window is created for the first time, 
          # respond to startup()
          isTruthy(startup())
          } else {
            # When the button to close the window is defined for the first time,
            # execute in response to the button 
            isTruthy(close_dataset_modal())
            }
        },
    label = "Loading Conditional",
    ignoreNULL = FALSE,
    {
      # Object and config files should be loaded
      # a. At startup, and
      # b. When the dataset requested is different from the one 
      #    currently loaded (key of dataset selected != key of
      #    currently loaded dataset)
      
      # Fetch key for previously loaded dataset from dataset_info reactive list
      previous_key <- dataset_info$last_object_key
      
      # a. startup (previous_key == NULL)
      if (is.null(previous_key)){
        # Activate Reactive trigger to load dataset
        dataset_change$trigger()
        } else {
          # b. Dataset requested is different
          if (previous_key != selected_key()){
            # Activate Reactive trigger to load dataset
            print("Dataset change trigger")
            dataset_change$trigger()
            } 
          
          # if previous_key == selected_key and selected_key is not NULL, 
          # the dataset is the same. Do not proceed with dataset loading 
          # in this case
          }
      })
  
  # TEMP: Inspect dataset trigger
  observeEvent(
    dataset_change$depend(),
    label = "Dataset Trigger",
    # The depend() function always evaluates to NULL when printed regardless
    # of its true server value. Therefore, ignoreNULL must be used for the 
    # observer to respond to the trigger.
    ignoreNULL = FALSE,
    # Do not want the observer to run upon startup or module creation
    ignoreInit = TRUE,
    {
      print("Dataset trigger in main server ")
      print(dataset_change$depend())
      })
  
  ## 1.4. Load/Update Object ####
  observeEvent(
    # Loads when the reactive trigger in "Loading Conditional" is activated
    dataset_change$depend(),
    label = "Load/Update Object",
    ignoreNULL = FALSE,
    {
      app_spinner$show()
      # Fetch Seurat object from datasets list defined at startup and set 
      # "object" reactiveVal to the result
      object(datasets[[selected_key()]]$object)

      app_spinner$hide()
      })
  
  ## 1.5. Load/Update Config File ####
  # Update config file with the one from the selected dataset, if it has changed
  observeEvent( 
    dataset_change$depend(),
    label = "Load/Update Config File",
    ignoreNULL = FALSE,
    {
      path <- datasets[[selected_key()]]$config
      
      # Load config file using defined path and set reactiveVal object
      config(readRDS(path))
    })
  
  ## 1.6. Save Key of Dataset Selected When Window is Closed ####
  # Save the selected key for the next time the window is opened (the 
  # group of buttons is re-created every time the modal is opened)
  selected_key <-
    eventReactive( 
      # Must run at startup (to load dataset initially) and in response 
      # to the "confirm selection" button 
      eventExpr = 
        {
          # eventExpr: observer executes when this expression evaluates to TRUE 
          # at startup, input$confirm_selection is NULL and 
          # close_dataset_modal() will not run. To get observer to run at 
          # startup, a conditional is used to respond to startup() when 
          # input$confirm_selection is NULL.
          if (is.null(input$confirm_selection)){
            isTruthy(startup())
          } else {
            # When close_dataset_modal is defined, execute in response to the
            # variable
            isTruthy(close_dataset_modal())
          }
        },
      label = "Save Data Key",
      ignoreNULL = FALSE,
      {
        if (!is.null(input$data_key)){
          # Record value of input$data_key
          input$data_key
        } else {
          # If input$data_key is NULL, the key has not yet been defined.
          # in this case, use the default data key (the first one)
          names(datasets)[1]
        }
      })
  
  ## 1.7. Last Key of Last Dataset Loaded ####
  # Save the key of the last dataset loaded into the app.
  # This is updated only when a change in dataset is observed upon closing the 
  # window (as signaled by the loading conditional observer)
  observeEvent(
    dataset_change$depend(),
    label = "Save Key of Last Dataset Loaded",
    ignoreNULL = FALSE,
    {
      # Selected_key() is stored under last_object_key, but only when the 
      # dataset is changed, or at startup (when "Loading Conditional" triggers
      # dataset_change)
      dataset_info$last_object_key <- selected_key()
    })
  
  # 2. Initialize Variables specific to object and config file -----------------
  # Split config file into metadata and assay lists for use downstream
  ## 2.1. Metadata_config ####
  metadata_config <- 
    eventReactive(
      config(),
      label= "metadata_config",
      ignoreNULL = FALSE,
      {
        config()$metadata
        })
  
  ## 2.2. Assay_config ####
  assay_config <-
    eventReactive(
      config(),
      label= "assay_config",
      ignoreNULL = FALSE,
      {
        config()$assays
        })
  
  # TODO: add include_numeric_metadata as an option in the config app 
  include_numeric_metadata <- TRUE
  numeric_metadata_title <- "Metadata Features"
  
  ## 2.3. valid_features ####
  # Create a list of valid features using the assays defined above
  valid_features <-
    eventReactive(
      assay_config(),
      label= "valid_features",
      ignoreNULL = FALSE,
      {
        valid_features <- 
          feature_list_all(
            object = object,
            assay_config = assay_config,
            # include_numeric_metadata: a boolean variable 
            # that is hard-coded for now and will be 
            # defined in the config file
            numeric_metadata = include_numeric_metadata, 
            # The same is true for numeric_metadata_title
            numeric_metadata_title = numeric_metadata_title
            )
        
        valid_features
        })
  
  ## 2.4. meta_categories ####
  # meta_categories: a vector giving the IDs of each of the categories defined
  # in the metadata section of the config file
  meta_categories <- 
    eventReactive(
      metadata_config(),
      label= "meta_categories",
      ignoreNULL = FALSE,
      {
        names(metadata_config())
        })
  
  ## 2.5. category_labels ####
  # Create a list of labels for each metadata category (names are the
  # category IDs and the values are the labels chosen)
  category_labels <- 
    eventReactive(
      metadata_config(),
      label= "category_labels",
      ignoreNULL = FALSE,
      {
        lapply(metadata_config(), function(category){category$label})
        })
  
  ## 2.6 Metadata categories in dropdown menus ####
  # meta_choices: a named vector with name-value pairs for the display name of 
  # the metadata category and the key used to access the category in the Seurat 
  # Object. 
  meta_choices <- 
    eventReactive(
      meta_categories(),
      label = "meta_choices generation",
      ignoreNULL = FALSE,
      {
        # Base vector: contains the "none" option
        meta_choices <- c("None" = "none")
        # Iteratively populate vector using entries in the metadata section 
        # of the config file 
        for (category in meta_categories()){
          # Use setNames from the stats package to add a new name-value 
          # pair to the vector
          meta_choices <- setNames(
            # Add `meta_colname` to vector
            object = c(meta_choices, 
                       metadata_config()[[category]]$meta_colname),
            # Add `label` to the vector as a name
            nm = c(names(meta_choices),
                   metadata_config()[[category]]$label)
            )
        }
        
        # Return meta_choices vector generated above
        meta_choices
    })
  
  ## 2.7. Unique values for each metadata category ####
  unique_metadata <- 
    eventReactive(
      metadata_config(),
      label = "unique_metadata",
      ignoreNULL = FALSE,
      {
        # The unique values for each metadata category listed in the config 
        # file will be stored as vectors in a list 
        unique_metadata <- list()
        
        # Store unique values for each metadata category entered
        for (category in names(metadata_config())){
          # Use sobj@meta.data[[category]] instead of object()[[category]] 
          # to return a vector (sobj[[category]] returns a dataframe)
          unique_metadata[[category]] <- 
            unique(object()@meta.data[[category]])
          # If the metadata category is a factor, convert to a vector with 
          # levels to avoid integers appearing in place of the 
          # unique values themselves
          if (class(unique_metadata[[category]]) == "factor"){
            unique_metadata[[category]] <- 
              levels(unique_metadata[[category]])
          }
        }
        
        unique_metadata
        })
  
  ## 2.8. UMAP Dimensions of full object ####
  # This is used to allow plotting of subsets with original axes scales
  # Plot a UMAP of the full data, store it to memory, and record the
  # x and y limits of the plot
  umap_orig <- 
    reactive({
      req(object())
      DimPlot(
        object()
      )
    })
  
  # Record limits
  xlim_orig <- 
    reactive({
      req(object())
      layer_scales(umap_orig())$x$range$range
    })
    
  ylim_orig <- 
    reactive({
      req(object())
      layer_scales(umap_orig())$y$range$range
    })
  
  # Store number of cells: used to determine if it is a subset
  # TODO: does this apply to non-CITEseq datasets?
  n_cells_original <- 
    reactive({
      req(object())
      ncol(object())
    })
  
  ## 2.9 Reductions in object ####
  reductions <- 
    reactive({
      req(object())
      reductions <- names(object()@reductions)
      
      # Order UMAP reduction first by default, if it exists
      if ("umap" %in% reductions){
        reductions <-
          c(
            reductions[reductions=="umap"],
            reductions[!reductions=="umap"]
            )
        }
      
      reductions
    })
    
  
  # 3. Initialize Modules ------------------------------------------------------
  ## 3.1. Dynamic UI ####
  # All UI for modules is dynamic as it depends on the currently 
  # selected object.
  ### 3.1.1. Plots tab UI
  # Added "dynamic" to end of variable created to prevent collision with the
  # plots_tab_ui reactive function
  plots_tab_ui_dynamic <-
    eventReactive(
      # Reactive event is set as the last initialization variable generated 
      # from the config file (meta_choices reacts to meta_categories, which 
      # reacts to metadata_config, which reacts to config)
      meta_choices(),
      label = "Plots Tab Dynamic UI",
      ignoreNULL = FALSE,
      {
        # Show spinner while new UI is computing
        render_UI_spinner$show()
        
        ui <- 
          plots_tab_ui(
            id = glue("{dataset_info$last_object_key}_plots"),
            meta_choices = meta_choices,
            unique_metadata = unique_metadata,
            category_labels = category_labels,
            metadata_config = metadata_config,
            reductions = reductions
            )
        
        # Hide spinner and return module UI
        render_UI_spinner$hide()
        
        ui
      })
  
  ### 3.1.2. DGE tab UI
  dge_tab_ui_dynamic <-
    eventReactive(
      # UI should only update when the object and config files are switched
      c(object(), config()),
      label = "DGE Tab Dynamic UI",
      ignoreNULL = FALSE,
      {
        dge_tab_ui(
          id = glue("{dataset_info$last_object_key}_dge"),
          unique_metadata = unique_metadata,
          metadata_config = metadata_config,
          meta_categories = meta_categories
          )
      })
  
  ### 3.1.3. Correlations tab UI
  corr_tab_ui_dynamic <-
    eventReactive(
      # UI should only update when the object and config files are switched
      c(object(), config()),
      label = "Correlations Tab Dynamic UI",
      ignoreNULL = FALSE,
      {
        ui <- corr_tab_ui(
          id = glue("{dataset_info$last_object_key}_corr"),
          unique_metadata = unique_metadata,
          metadata_config = metadata_config
          )

        update_features$trigger()
        
        ui
      })
  
  ### 3.1.4. Render Dynamic UI components
  output$plots_dynamic_ui <-
    renderUI({
      plots_tab_ui_dynamic()
      })

  output$dge_dynamic_ui <-
    renderUI({
      dge_tab_ui_dynamic()
      })

  output$corr_dynamic_ui <- 
    renderUI({
      corr_tab_ui_dynamic()
      })
  
  ## 3.2 Server instances ####
  # A separate module instance is needed for each dataset to avoid namespace 
  # collisions, but only one module should be created for each dataset. The 
  # observer below will initialize modules for each tab if they have not already 
  # been created.
  
  ### 3.2.1. Plots tab server ####
  observe(
    label = "Create Module Servers",
    {
      current_key <- dataset_info$last_object_key
      
      # If the current key is not in the list of module servers created, create
      # server instances for each tab.
      if (!current_key %in% main_server$modules_created){
        print(glue("New module for plots tab (key = {current_key})"))
        plots_tab_server(
          id = glue("{current_key}_plots"),
          object = object,
          metadata_config = metadata_config,
          assay_config = assay_config,
          meta_categories = meta_categories,
          category_labels = category_labels,
          unique_metadata = unique_metadata,
          valid_features = valid_features,
          error_list = error_list,
          n_cells_original = n_cells_original,
          xlim_orig = xlim_orig,
          ylim_orig = ylim_orig
          )
        
        # Add current key to list of modules created so module is not re-created
        main_server$modules_created <- 
          c(main_server$modules_created, current_key)
        }
      })

  ### 3.2.2. DGE Tab Server ####
  observe({
    current_key <- dataset_info$last_object_key
    
    if (!current_key %in% main_server$dge_modules_created){
      print(glue("New module for dge tab (key = {current_key})"))
      dge_tab_server(
        id = glue("{current_key}_dge"),
        object = object,
        metadata_config = metadata_config,
        meta_categories = meta_categories,
        unique_metadata = unique_metadata,
        meta_choices = meta_choices,
        object_trigger = dataset_change
      )
      
      # Add current key to list of modules created so module is not re-created
      main_server$dge_modules_created <- 
        c(main_server$dge_modules_created, current_key)
      }
    })
  
  ### 3.2.3. Correlations Tab Server ####
  observe({
    current_key <- dataset_info$last_object_key
    
    if(!current_key %in% main_server$corr_modules_created){
      print(glue("New module for correlations tab (key = {current_key})"))
      corr_tab_server(
        id = glue("{current_key}_corr"),
        object = object,
        metadata_config = metadata_config,
        meta_categories = meta_categories,
        unique_metadata = unique_metadata,
        n_cells_original = n_cells_original,
        nonzero_threshold = nonzero_threshold,
        meta_choices = meta_choices,
        valid_features = valid_features,
        error_list = error_list,
        update_features = update_features,
        object_trigger = dataset_change
      )
      
      # Add current key to list of modules created so module is not re-created
      main_server$corr_modules_created <- 
        c(main_server$corr_modules_created, current_key)
    }
  })
  
  # 4. Dataset Description in modal UI -----------------------------------------
  # Render text for the dataset modal that displays a description of the dataset
  # currently selected
  output$dataset_description <-
    renderText({
      # Fetch description of the dataset selected (key = input$data_key)
      datasets[[input$data_key]]$description
      })
  
  output$dataset_dimplot <-
    renderImage({
      path <- datasets[[input$data_key]]$plot
      list(
        `src` = path,
        `width` = 290,
        `height` = 218
        )
    },
    deleteFile=FALSE
    )
  
  # Observe statement to determine memory usage of all objects in the environment
  # observe({
  #   env <- .GlobalEnv
  #   units <- "MB"
  #   
  #   env_size(env, units)
  # })
  
  observeEvent(
    object(),
    {
      log_info(
        glue("Memory used after loading current object: {to_GB(mem_used())}")
        )
      })
  
  # 5. Callbacks ---------------------------------------------------------------
  ## 5.1. Code to run when the user disconnects ####
  onSessionEnded(
    function(){
      log_info(glue("Session {session$token} disconnected."))
      log_info(glue("Memory usage upon disconnection: {to_GB(mem_used())}"))
    }
    )
  
  # DEBUG: UI to test object and config file are properly rendered
  # output$verify_object <- 
  #   renderUI({
  #     div(
  #       "Object",
  #       verbatimTextOutput(outputId = "object_summary"),
  #       "Config File",
  #       verbatimTextOutput(outputId = "config_summary"),
  #       "Valid Features",
  #       verbatimTextOutput(outputId = "valid_features_summary"),
  #       "Unique Metadata",
  #       verbatimTextOutput(outputId = "unique_metadata_summary"),
  #       "Other Reactives",
  #       verbatimTextOutput(outputId = "other_summary")
  #     )
  #   })
  
  # output$object_summary <- 
  #   renderPrint({
  #     print(object())
  #   })
  # 
  # output$config_summary <-
  #   renderPrint({
  #     print(config())
  #   })
  # 
  # output$valid_features_summary <-
  #   renderPrint({
  #     str(valid_features())
  #   })
  # 
  # output$unique_metadata_summary <-
  #   renderPrint({
  #     print(str(unique_metadata()))
  #   })
  # 
  # output$other_summary <-
  #   renderPrint({
  #     print("meta_categories")
  #     print(meta_categories())
  #     print("category_labels")
  #     print(category_labels())
  #     print("meta_choices")
  #     print(meta_choices())
  #     print("xlim_orig")
  #     print(xlim_orig())
  #     print("ylim_orig")
  #     print(ylim_orig())
  #     print("n_cells_original")
  #     print(n_cells_original())
  #     print("reductions")
  #     print(reductions())
  #   })
}

# Run the application 
shinyApp(ui = ui, server = server)
