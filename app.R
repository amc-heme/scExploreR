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

# Reactlog (for debugging)
library(reactlog, quietly = TRUE, warn.conflicts = FALSE)
options(shiny.reactlog=TRUE, warn.conflicts = FALSE)

# Tidyverse Packages
library(tidyverse, quietly = TRUE, warn.conflicts = FALSE)
library(stringr, quietly = TRUE, warn.conflicts = FALSE)
library(dplyr, quietly = TRUE, warn.conflicts = FALSE)
library(ggplot2, quietly = TRUE, warn.conflicts = FALSE)
library(glue, quietly = TRUE, warn.conflicts = FALSE)
library(DT, quietly = TRUE, warn.conflicts = FALSE)

# Additional Backend Packages
library(presto, quietly = TRUE, warn.conflicts = FALSE)
library(R.devices, quietly = TRUE, warn.conflicts = FALSE)

## Load CSS, JavaScript, and R scripts ####
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

# Load Javasctipt files for app: find all .js files in www/ directory and create
# a list of script() tags using includeScript().
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
js_list <- lapply(js_files,includeScript)

# Load Seurat Object and Config File -------------------------------------------
# currently, D0/D30 data, modified to include gene signature scores
# https://storage.googleapis.com/jv_omics_sandbox/longitudinal_samples_20211025.Rds
sobj <- readRDS("./Seurat_Objects/longitudinal_samples_20211025.rds")

# Load config file
config <- readRDS("./Seurat_Objects/d0-d30-config.rds")

# Split config file into metadata and assay lists for use downstream
metadata_config <- config$metadata
assay_config <- config$assays

# Define Searchable Features ---------------------------------------------------
# TODO: add include_numeric_metadata as an option in the config app 
include_numeric_metadata <- TRUE
numeric_metadata_title <- "Metadata Features"

# Create a list of valid features using the assays defined above
valid_features <- 
  feature_list_all(
    sobj,
    assay_config = assay_config,
    #include_numeric_metadata: a boolean variable 
    #that is hard-coded for now and will be 
    #defined in the config file
    numeric_metadata = include_numeric_metadata, 
    #The same is true for numeric_metadata_title
    numeric_metadata_title = numeric_metadata_title)

# Define Metadata Used in App --------------------------------------------------
# meta_categories: a vector giving the IDs of each of the categories defined
# in the metadata section of the config file
meta_categories <- names(metadata_config)

# category_labels: list of labels for each metadata category (names are the
# category IDs and the values are the labels chosen)
category_labels <- lapply(metadata_config, function(category){category$label})

## Metadata categories in dropdown menua ####
# meta_choices: a named vector with name-value pairs for the display name of 
# the metadata category and the key used to access the category in the Seurat 
# Object. 
# Base vector: contains the "none" option
meta_choices <- c("None"="none")
# Iteratively populate vector using entries in the metadata section 
# of the config file 
for (category in meta_categories){
  # Use setNames from the stats package to add a new name-value 
  # pair to the vector
  meta_choices <- setNames(
    # Add `meta_colname` to vector
    object = c(meta_choices, 
               config$metadata[[category]]$meta_colname),
    # Add `label` to the vector as a name
    nm = c(names(meta_choices),
           config$metadata[[category]]$label)
  )
}

## Unique values for each metadata category ####
# The unique values for each metadata category listed in the config 
# file will be stored as vectors in a list 
unique_metadata <- list()
# Store unique values for each metadata category entered
for (category in names(config$metadata)){
  # Use sobj@meta.data[[category]] instead of sobj[[category]] 
  # to return a vector (sobj[[category]] returns a dataframe)
  unique_metadata[[category]] <- unique(sobj@meta.data[[category]])
  # If the metadata category is a factor, convert to a vector with levels() to 
  # avoid integers appearing in place of the unique values themselves
  if(class(unique_metadata[[category]])=="factor"){
    unique_metadata[[category]] <- levels(unique_metadata[[category]])
  }
}

# Non-reactive Global Variables ------------------------------------------------
# Store UMAP Dimensions of full object
# This is used to allow plotting of subsets with original axes scales
# Plot a UMAP of the full data, store it to memory, and record the
# x and y limits of the plot
umap_orig <- 
  DimPlot(
    sobj,
    group.by = "clusters"
    )

# Record limits
xlim_orig <- layer_scales(umap_orig)$x$range$range
ylim_orig <- layer_scales(umap_orig)$y$range$range

# Store number of cells: used to determine if it is a subset
# TODO: does this apply to non-CITEseq datasets?
n_cells_original <- ncol(sobj)

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
  # CSS style: prevents navbar from appearing on top of content 
  # tags$head(tags$style(HTML("body{
  #                           padding-top: 60px;
  #                           }"))),
  # CSS and JS for collapsible panel
  navbarPage("Shiny scExplorer",
             windowTitle="Shiny scExplorer",
             position="fixed-top",
             tabPanel("Plots",
                      plots_tab_ui(
                        id = "plots",
                        meta_choices = meta_choices,
                        unique_metadata = unique_metadata,
                        category_labels = category_labels,
                        metadata_config = config$metadata
                        )
                      ),
             tabPanel("Differential Expression",
                      dge_tab_ui(
                        id = "dge",
                        unique_metadata = unique_metadata,
                        metadata_config = config$metadata,
                        meta_categories = meta_categories
                        )
                      ),
             tabPanel("Gene Correlations",
                      #Use corr_tab_ui module
                      corr_tab_ui(
                        id = "corr",
                        unique_metadata = unique_metadata,
                        metadata_config = config$metadata
                        )
                      )
             ), # End navbarPage()
  # Help button - Creates a Dropdown menu when clicked
  # Button should appear in the upper right hand corner of the navbar menu
  # This will be achieved with the button_wizzard.js script
  # (Help button is wrapped in two introBoxes)
  introBox(
    data.step=1,
    data.intro=
      'Welcome to the Shiny app.<br><br>Please click the ">" 
      button to continue with the tour, or click "skip" to proceed to the app.',
    data.position = "left",
    # Begin introBox2
    introBox(
      data.step = 2,
      data.intro = 
        'Click this button to view help. For a more 
        detailed explanation of features, select "detailed walkthrough". 
        For more information on interpereting the plots in this app, 
        select "Interpereting scRNA-seq Plots". Click "Guided tour" 
        to view this tour again.',
      data.position="left",
      # Begin Help button
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
            rel="noopener noreferrer")
          )# End tagList
        ) #End Help Button
      ) #End introBox2
    ), #End introBox 1
  # Include list of scripts built from .js files in www/ directory
  js_list
)

# 2. Main Server function ------------------------------------------------------
server <- function(input,output,session){
  ## 2.1. Plots Tab Server Module #####
  plots_tab_server(
    id = "plots",
    sobj = sobj,
    assay_config = assay_config,
    category_labels = category_labels,
    unique_metadata = unique_metadata,
    valid_features = valid_features,
    error_list = error_list,
    n_cells_original = n_cells_original,
    xlim_orig = xlim_orig,
    ylim_orig = ylim_orig,
    metadata_config = metadata_config
    )
  
  ## 2.2. DGE Tab Server Module ####
  dge_tab_server(id = "dge",
                 sobj = sobj,
                 metadata_config = metadata_config,
                 meta_categories = meta_categories,
                 unique_metadata = unique_metadata,
                 meta_choices = meta_choices)
  
  ## 2.3. Correlations Tab Server Module ####
  corr_tab_server(id = "corr",
                  sobj = sobj,
                  metadata_config = metadata_config,
                  meta_categories = meta_categories,
                  unique_metadata = unique_metadata,
                  n_cells_original = n_cells_original, 
                  nonzero_threshold = nonzero_threshold, 
                  meta_choices = meta_choices,
                  valid_features = valid_features,
                  error_list = error_list)
}

# Run the application 
shinyApp(ui = ui, server = server)
