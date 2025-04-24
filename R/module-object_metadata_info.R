#' object_metadata_info UI
#'
#' Displays information on object metadata.
#'
#' @noRd
object_metadata_info_ui <- 
  function(
    id
    ){
    ns <- NS(id)
  
    div(
      id = ns("object_metadata_info_page"),
      # Search bar for metadata variable
      # selectizeInput(
      #   inputId = ns("metadata_var"),
      #   multiple = FALSE,
      #   label = "Enter a metadata variable:",
      #   choices = NULL,
      #   selected = character(0),
      #   # Add remove button to inputs
      #   options = list(
      #     # Do not allow user to input features not
      #     # in the list of options
      #     'create'= FALSE,
      #     "placeholder" = "Loading, please wait..."
      #     )
      #   ),
      shinyWidgets::pickerInput(
        inputId =  ns("metadata_var"),
        label = "Select a metadata variable:",
        choices = NULL,
        selected = character(0),
        multiple = FALSE,
        options =
          pickerOptions(
            placeholder = "Loading, please wait...",
            dropupAuto = FALSE
            )
        ),
      
      hidden(
        div(
          id = ns("metadata_var_info"),
          # Type of metadata variable
          div(
            tags$b("Type: "),
            textOutput(
              outputId = ns("var_type"),
              inline = TRUE
              )
            ),
          # Description of metadata variable (if provided)
          div(
            # This should go in the var_description output
            uiOutput(
              outputId = ns("var_description")
              )
            ),
          # Summary of variable 
          # Either a description of unique values (for categorical variables)
          # or summary statistics (numeric variables)
          uiOutput(
            outputId = ns("var_summary")
            )
          )
        )
      )
    }

#' object_metadata_info Server
#' 
#' Displays information on object metadata.
#'
#' @param id ID to use for module. This must match the id provided to the ui
#' instance of the module for the module to function properly.
#' @param object A single-cell object.
#' @param meta_choices A named vector generated in run_scExploreR with each
#' metadata variable included in the config file. This vector is used when 
#' generating the choices displayed in the metadata information. 
#' @param metadata_config The metadata section of the config file loaded at 
#' startup. This is loaded in the main server function at startup and when the 
#' object is changed.
#' @param include_numeric_metadata The include_numeric_metadata section of the 
#' config file. Used to determine if the app admin has enabled numeric metadata.
#' The description for numeric variables will vary based on this setting. 
#' @param is_seurat Whether an object is a Seurat object. If so, the 
#' descriptions for variables not included in the config app will mention 
#' that advanced code-based subsetting is possible.
#' 
#' @noRd
object_metadata_info_server <- 
  function(
    id,
    object,
    meta_choices,
    metadata_config,
    include_numeric_metadata,
    is_seurat
    ){
    moduleServer(
      id,
      function(input, output, session){
        # Server namespace function 
        ns <- session$ns
        
        # 1. Populate search menu with list of metadata options ####
        observe(
          label = paste0(id, ": Populate metadata list"),
          {
            # Fetch full metadata table 
            meta_table <-
              SCUBA::fetch_metadata(
                object = isolate(object()),
                full_table = TRUE
                )
            
            # Choices: display "featured" metadata from config file first,
            # then all metadata variables left in the object
            remaining_metadata_variables <-
              colnames(meta_table)[!colnames(meta_table) %in% meta_choices()]
            
            # Remaining variables do not have a display name, since they 
            # are not added in the config file. The names of the remaining 
            # variables are set as they appear in the object (downstream code
            # requires )
            names(remaining_metadata_variables) <-
              remaining_metadata_variables
            
            choices <- 
              c(meta_choices(),
                remaining_metadata_variables
                )
            
            # remove first option ("none") from meta_choices
            choices <- choices[2:length(choices)]
            
            # Additional information for each variable
            # Get the class of the variable, and the number of 
            # unique values or levels, if applicable.
            # This will be displayed beneath each variable in the selection 
            # menu
            var_info_text <-
              sapply(
                choices,
                function(meta_var){
                  var_class <- class(meta_table[,meta_var])
                  
                  # If character or factor, also fetch number of 
                  # unique values (character) or levels (factor)
                  additional_info <- ""
                  if (var_class == "character"){
                    n_unique <-
                      meta_table[,meta_var] |> 
                      unique() |> 
                      length()
                    
                    additional_info <-
                      # Use singular "unique value" if only one 
                      # value is present
                      if (n_unique == 1){
                        paste0("1 unique value")
                      } else {
                        paste0(n_unique, " unique values")
                      }
                  } else if (var_class == "factor"){
                    n_levels <-
                      meta_table[,meta_var] |> 
                      levels() |> 
                      length()
                    
                    additional_info <-
                      paste0(n_levels, " levels")
                  }
                  
                  # Text to return: class plus number of unique values/
                  # levels, if applicable
                  paste0(
                    toTitleCase(var_class),
                    # If applicable, add additional info with a space and comma
                    if (isTruthy(additional_info)){
                      paste0(", ", additional_info)
                      }
                    )
                })
            
            # Generate HTML for each choice
            # Variable display name, plus additional 
            # information computed above
            choice_html <-
              sapply(
                1:length(var_info_text),
                function(i){
                  # The names of the variable choices (display names) are 
                  # preserved when computing the info text in the above sapply
                  # operation
                  var_display_name <- names(var_info_text)[i]
                  
                  # doRenderTags converts tags from ShinyTag format to 
                  # character, so they can be properly rendered by the 
                  # pickerInput.
                  htmltools::doRenderTags(
                    tags$div(
                      # First line: display name of variable
                      tags$div(
                        var_display_name,
                        class = "bold"
                        ),
                      # Second line: additional information 
                      tags$div(
                        var_info_text[i],
                        class = "gray"
                        )
                      )
                    )
                  }
                )
            
            # updateSelectizeInput(
            #   inputId = "metadata_var",
            #   choices = choices,
            #   selected = character(0),
            #   server = TRUE,
            #   options = list(
            #     "placeholder" = "Enter a variable"
            #     )
            #   )
            
            updatePickerInput(
              inputId = "metadata_var",
              choices = choices,
              selected = character(0),
              choicesOpt = 
                list(
                  # Display the computed HTML above for variables 
                  # instead of variable names
                  `content` = choice_html
                  ),
              options = pickerOptions(
                placeholder = "Enter a variable",
                liveSearch = TRUE,
                liveSearchPlaceholder = "Search",
                showContent = FALSE
                )
              )
            })
        
        # 2. Show/hide summary stats window ####
        observe(
          label = paste0(id, ": show/hide summary"),
          {
            target_id <- "metadata_var_info"
            
            if (isTruthy(input$metadata_var)){
              shinyjs::showElement(
                id = target_id,
                anim = TRUE
                )
              } else {
                shinyjs::hideElement(
                  id = target_id,
                  anim = TRUE
                  )
                }
            })
        
        # 3. Display information for selected variable ####
        ## 3.1. Fetch metadata for variable ####
        # (separated into a separate reactive expression so calculation 
        # is only done once when the variable is changed)
        metadata_vector <-
          reactive({
            req(input$metadata_var)
            
            SCUBA::fetch_metadata(
              object = isolate(object()),
              vars = input$metadata_var,
              return_class = "vector"
              )
          })
        
        ## 3.2. Type (class) of variable ####
        output$var_type <- 
          renderText({
            toTitleCase(
              class(metadata_vector())
              )
          })
        
        ## 3.3. Variable Description ####
        # Description of metadata variable from config file, if provided
        output$var_description <-
          renderUI({
            if (input$metadata_var %in% names(metadata_config())){
              # Variables added by the browser admin
              # If the metadata variable is in the config file, check if the
              # variable description is defined
              config_var_description <-
                metadata_config()[[input$metadata_var]]$description
              
              if (isTruthy(config_var_description)){
                div(
                  class = "half-space-top",
                  tags$b("Description: "),
                  config_var_description
                  )
              } else {
                # Variables in the config file, but without a description
                # (currently, these are always character, factor, or logical)
                div(
                  class = "half-space-top",
                  tags$b("Description: "),
                  "No description has been provided for this variable."
                  )
              }
            } else {
              # Variables not added by the app admin
              # May be numeric variables, or categorical/logical 
              # variables not added
              if (class(metadata_vector()) %in% c("numeric", "integer")){
                # Numeric variables: information shown depends on whether the
                # user has enabled numeric metadata in the config app
                if (include_numeric_metadata()){
                  # If so, the variable can be used in plotting and subsetting
                  div(
                    class = "half-space-top",
                    tags$b("Description: "),
                    paste0(
                      'This is a numeric metadata variable. You may plot this ',
                      'variable in the plots tab, or use it in creating ',
                      'subsets by selecting "feature expression" in the ',
                      'subset menu.'
                      )
                    )
                } else {
                  # If not enabled, the only possible interaction is via 
                  # advanced, code based subsetting, in Seurat objects
                  div(
                    class = "half-space-top",
                    tags$b("Description: "),
                    paste0(
                      'This is a numeric metadata variable. The app admin ',
                      'has not allowed access to numeric metadata in this ',
                      'scExploreR instance, so it is not possible to plot ',
                      'or subset based on this variable. Please contact ',
                      'the app admin for more information.'#,
                      # If the object is a Seurat object, state that advanced
                      # subsetting can be performed (this is very confusing)
                      # if (is_seurat()){
                      #   paste0(
                      #     ' Since the current dataset is a Seurat object, it is ',
                      #     'possible to subset based on this variable, however, ',
                      #     'using advanced, code based subsetting.'
                      #   )
                      # }
                    )
                  )
                }
              } else {
                # Non-numeric variables not added in the config file
                # explain they can be interacted with via advanced subsetting
                # in Seurat objects, but not elsewhere in the app.
                div(
                  class = "half-space-top",
                  tags$b("Description: "),
                  # Add a note about numeric metadata based on whether the admin
                  # enabled or disabled numeric metadata
                  # Can numeric metadata be used in numeric subsetting? Yes, if 
                  # numeric metadata enabled
                  paste0(
                    'This metadata variable is an "extra" variable not ',
                    "enabled by the app admin. It can't be used for plotting ",
                    "or subsetting. Please contact the app admin to learn ",
                    "more about this variable or request it be included."
                    )
                  )
                }
              }
          })
        
        ## 3.4. Variable Summary ####
        output$var_summary <-
          renderUI({
            if (class(metadata_vector()) == "character"){
              # Character metadata variables: show unique values 
              div(
                class = "half-space-top",
                tags$b("Unique Values: "),
                paste(
                  unique(metadata_vector()), 
                  collapse = ", "
                  )
                )
            } else if (class(metadata_vector()) == "factor"){
              # Factor metadata variables: show levels
              div(
                class = "half-space-top",
                tags$b("Unique Values: "),
                paste(
                  levels(metadata_vector()), 
                  collapse = ", "
                  )
                )
            } else if (class(metadata_vector()) == "logical"){
              # Boolean (logical) vectors: show number of TRUE, FALSE, NA's
              n_true <- sum(metadata_vector() == TRUE, na.rm = TRUE)
              n_false <- sum(metadata_vector() == FALSE, na.rm = TRUE)
              n_na <- sum(is.na(metadata_vector()), na.rm = TRUE)
              
              div(
                class = "half-space-top",
                # Header
                div(
                  tags$b("Number of cells per value:")
                  ),
                # Summary of TRUE/FALSE/NA values
                div(
                  tags$b("TRUE: "),
                  n_true
                  ),
                div(
                  tags$b("FALSE: "),
                  n_false
                  ),
                div(
                  tags$b("NA: "),
                  n_na
                  )
                )
            } else if (class(metadata_vector()) %in% c("numeric", "integer")){
              # Numeric variables: show summary statistics
              # Could also show a ridge plot?
              summary_stats <- summary(metadata_vector())
              
              div(
                class = "half-space-top",
                div(
                  tags$b("Summary statistics")
                  ),
                div(
                  tags$b("Min:"),
                  summary_stats[1]
                  ),
                div(
                  tags$b("Q1:"),
                  summary_stats[2]
                  ),
                div(
                  tags$b("Median:"),
                  summary_stats[3]
                  ),
                div(
                  tags$b("Mean:"),
                  summary_stats[4]
                  ),
                div(
                  tags$b("Q3:"),
                  summary_stats[5]
                  ),
                div(
                  tags$b("Max:"),
                  summary_stats[6]
                  )
                )
              }
            })
      }
    )
  }
