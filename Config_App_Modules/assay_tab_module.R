#' Assays tab in config app
#'
#' @param id ID to use for module elements.
#' @param object The Seurat object being used for the config file. This should
#' be unpacked from its reactive format when used for this function (call with
#' parentheses to unpack).
#'
assays_tab_module_ui <- 
  function(
    id,
    object
    ){
    # Namespace function: prevents conflicts with IDs defined in other modules
    ns <- NS(id)

    sidebarLayout(
      applet_sidebar_panel(
        # input-no-margin class: removes margin of input containers within div
        tagList(
          div(
            class="input-no-margin",
            multiInput(
              inputId = ns("assays_selected"),
              label = "Choose assays to include:",
              width = "100%",
              choices = names(object@assays),
              options = 
                list(
                  enable_search = FALSE,
                  non_selected_header = "Available Assays",
                  selected_header = "Selected Assays",
                  "hide_empty_groups" = TRUE
                )
            )
          ), # End multiInput
          
          # Include numeric metadata for plotting
          awesomeCheckbox(
            inputId = ns("include_numeric_metadata"),
            label = "Include numeric metadata for plotting", 
            value = TRUE,
            # Determines style of checkbox (warning is orange)
            status = "warning"
          )
        )
        
      ),
      
      applet_main_panel(
        # Create an instance of the assay options UI for all possible assays. 
        # Each UI creates a "card"; all are hidden at first and are shown when 
        # their corresponding assay is selected by the user. The "id" argument 
        # in lapply is the name of the assay.
        tagList(
          lapply(
            names(object@assays),
            function(assay){
              print(glue("UI for {assay}"))
              
              options_ui(
                id = ns(assay),
                object = object,
                optcard_type = "assays"
              )
            }
          ),
          
          # Button to activate warning modal (for testing purposes)
          # actionButton(
          #   inputId = "warning_modal", 
          #   label = "", 
          #   icon = icon("exclamation-triangle"),
          #   class = "icon-button x-large"
          #   )
          # End TEMP
        )
      )
    )
    
    
    }

#' Assays tab in config app
#'
#' @param id ID to use for module server instance.
#' @param object Seurat object used for the current config file.
#' @param ADT_tab_selector The jQuery selector for the ADT threshold tab. This
#' is generated in the main server function.
#'
assays_tab_server <- 
  function(
    id,
    object,
    ADT_tab_selector
    ){
    moduleServer(
      id,
      function(input, output, session){
        # 1. Store selected assays as a reactive variable ####
        assays_selected <- 
          eventReactive(
            input$assays_selected,
            ignoreNULL = TRUE,
            ignoreInit = TRUE,
            {
              input$assays_selected
            })
        
        # 2. Create module server instances for each possible assay ####
        # Observe is used to reactively update outputs when inputs in the 
        # module and its sub-modules are changed
        observe({
          req(object())
          print("2. module instance creation")

          # <<- is required for all_assay_options to be accessible
          # to other server code (not sure why)
          all_assay_options <<- list()

          print("begin for loop")
          # Create an assay options module for each assay in the object
          for (id in names(object()@assays)){
            print(glue("assay: {id}"))
            # Must also use <<- here
            all_assay_options[[id]] <<-
              options_server(
                id = id,
                object = object,
                categories_selected = assays_selected,
                options_type = "assays"
              )
            }
          })
        
        # 3. Process list of assay module outputs #### 
        # Filter list of options module outputs and combine into a single 
        # reactive object, which is added to the config_data list. 
        # assay_data <- 
        #   reactive({
        #     print("3. Collection of assay data")
        #     # Options list is only processed when metadata columns 
        #     # have been selected
        #     if (!is.null(input$assays_selected)){
        #       # Extracts each reactive module output and stores them in a list
        #       list <- lapply(all_assay_options, function(x) x())
        #       # Filter list for metadata columns that have been selected 
        #       # by the user
        #       return(list[names(list) %in% input$assays_selected])
        #     } else {
        #       # Return NULL if no columns are selected
        #       return(NULL)
        #     }
        #   })
        # 
        # # 4. Determine designated ADT assay ####
        # ADT_assay <-
        #   reactive({
        #     print("4. Designate ADT assay")
        #     # Get TRUE/FALSE values for whether each assay is an ADT assay
        #     is_adt <-
        #       sapply(
        #         assay_data(),
        #         function(x){
        #           x$designated_adt
        #         }
        #       )
        #     
        #     # Return the name of the assay designated as the ADT assay
        #     if (any(is_adt == TRUE)){
        #       return(
        #         names(
        #           is_adt[is_adt == TRUE]
        #         )
        #       )
        #     } else {
        #       # If none are designated, return NULL.
        #       return(
        #         NULL
        #       )
        #     }
        #   })
        # 
        # # 5. Show/Hide ADT thresholding tab ####
        # observe({
        #   print("5. Show/hide thresholding tab")
        #   # When an assay is designated as the ADT assay, show the 
        #   # corresponding tab
        #   if (!is.null(ADT_assay())){
        #     showElement(
        #       # ADT_tab_selector is defined at the beginning of the server 
        #       # function
        #       selector = ADT_tab_selector
        #     )
        #   } else {
        #     # Hide the tab if there is no designated ADT assay
        #     hideElement(
        #       selector = ADT_tab_selector
        #     )
        #   }
        # })
        # 
        # # 6. Return values from module ####
        # return(
        #   list(
        #     `assay_data` = assay_data,
        #     `ADT_assay` = ADT_assay
        #     )
        #   )
      })
}


# assay_tab_ui_nonmodular <- 
#   function(
    #     object
#   ){
#   sidebarLayout(
#     applet_sidebar_panel(
#       # input-no-margin class: removes margin of input containers within div
#       tagList(
#         div(
#           class="input-no-margin",
#           multiInput(
#             inputId = "assays_selected",
#             label = "Choose assays to include:",
#             width = "100%",
#             choices = names(object()@assays),
#             options = 
#               list(
#                 enable_search = FALSE,
#                 non_selected_header = "Available Assays",
#                 selected_header = "Selected Assays",
#                 "hide_empty_groups" = TRUE
#               )
#           )
#         ), # End multiInput
#         
#         # Include numeric metadata for plotting
#         awesomeCheckbox(
#           inputId = "include_numeric_metadata",
#           label = "Include numeric metadata for plotting", 
#           value = TRUE,
#           # Determines style of checkbox (warning is orange)
#           status = "warning"
#         )
#       )
#       
#     ),
#     
#     applet_main_panel(
#       # Create an instance of the assay options UI for all possible assays. Each 
#       # UI creates a "card"; all are hidden at first and are shown when their 
#       # corresponding assay is selected by the user. The "id" argument in lapply 
#       # is the name of the assay.
#       tagList(
#         lapply(
#           names(object()@assays),
#           function(assay){
#             print("lapply for UI creation")
#             
#             options_ui(
#               id = assay,
#               object = object(),
#               optcard_type = "assays"
#             )
#           }
#         ),
#         
#         # Button to activate warning modal (for testing purposes)
#         # actionButton(
#         #   inputId = "warning_modal", 
#         #   label = "", 
#         #   icon = icon("exclamation-triangle"),
#         #   class = "icon-button x-large"
#         #   )
#         # End TEMP
#       )
#     )
#   )
# }