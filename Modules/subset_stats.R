#Subset stats module

subset_stats_ui <- function(id,
                            tab=c("dge","corr"),
                            metadata_config,
                            gene_selected,
                            subset_selections){
  #Namespace function: prevents conflicts with inputs/outputs defined in other modules
  ns <- NS(id)
  
  #Function
  #subset_output_html: generates an output container to display the unique values 
  #of a given metadata category present in the current subset.
  #Arguments
  #category: the metadata category to generate an output for
  #config: the config file, imported as a list
#  subset_output_html <- function(category, config){
#    #Label for the metadata category used in the current subset selection menu,
#    #defined in config file
#    label<- config$metadata[[category]]$label
#    
#    #Return tagList below
#    tagList(
#      tags$strong(glue("{label}:")),
#      #Text output to report values in selected category found in the subset
#      textOutput(
#        #Output ID: uses the category name 
#        outputId = glue("selected_{category}"),
#        inline=TRUE)
#      )
#    }
  
  if (tab=="dge"){
    
  } else if (tab=="corr"){
    #UI for stats in correlations tab
    ui <- tagList(
      tags$strong("Subset Summary and Quality Statistics", 
                  class="x-large inline-block space-top"),
      #Restriction criteria
      #Create outputs for each subset menu
      lapply(
        #Loop through the category names associated with subset criteria
        X = names(subset_selections()), 
        FUN = function(category, metadata_config){
          #Get label for the metadata category used in the current subset selection 
          #menu, defined in config file
          label<- metadata_config[[category]]$label
        
          #Return HTML
          #Use of div creates a new line between each entry
          div(
            tags$strong(glue("{label}:")),
            #Text output to report values in selected category found in the subset
            textOutput(
              #Output ID: uses the category name 
              outputId = ns(glue("selected_{category}")),
              inline=TRUE)
            )
          },
        #metadata_config must be passed to the function above after it is 
        #defined (see ... argument in ?lapply)
        metadata_config
        ), #End lapply
      
      #Number of cells in subset
      div(
        tags$strong("Number of cells in subset: ",
                      class="space-top inline-block"),
        textOutput(outputId = ns("n_cells"), inline = TRUE)
        ),
      
      #Number and percentage of cells with nonzero reads for the selected gene
      div(
        tags$strong(glue("Cells with non-zero reads for {gene_selected()}:")),
        textOutput(outputId = ns("n_nonzero_and_percent"), inline = TRUE)
        )
      )#End tagList
    
    #Return UI from module to the parent UI 
    return(ui)
  }
  
}

subset_stats_server <- function(id,
                                tab=c("dge","corr"),
                                subset,
                                gene_selected,
                                subset_selections,
                                nonzero_threshold
                                ){
  moduleServer(id, 
               function(input,output,session){
                 #Server namespace function (used for renderUI and JavaScript ID references)
                 ns <- session$ns
                 
                 # 1. Compute stats for subset ---------------------------------
                 #Cells in subset
                 n_cells <- reactive({
                   length(Cells(subset()))
                 })
                 
                 #Cells with nonzero reads
                 n_nonzero <- reactive({
                   sum(subset()@assays$RNA@counts[gene_selected(),] != 0)
                 })
                 
                 #Proportion of nonzero reads
                 prop_nonzero <- reactive({
                   n_nonzero()/n_cells()
                 })
                 
                 #Percentage of nonzero reads
                 percent_nonzero <- reactive({
                   format(
                     prop_nonzero()*100,
                     #Display at least three sig figs in percentage
                     digits=3,
                     #Display at least two digits after decimal point
                     nsmall=2,
                     scientific=FALSE
                     )
                 })
                 
                 # 2. Notifications --------------------------------------------
                 # 2.1. Nonzero proportion is below the defined threshold 
                 #Use observe statement to reactively check prop_nonzero()
                 observe({
                   if (prop_nonzero() < nonzero_threshold){
                     #Define notification UI (warning icon plus text)
                     notification_ui <- span(
                       #Warning icon (inline and enlarged)
                       icon("exclamation-triangle",
                            style="display: inline-block; font-size: 1.7em;"),
                       
                       #Notification text with proportion and number of non-zero cells
                       span(glue("Low gene coverage: the selected feature was detected
                               in {percent_nonzero()}% of cells within the selection
                               restriction criteria ({n_nonzero()}/{n_cells()} cells).
                               Correlation results may be inaccurate."),
                            #Font size of notification text
                            style="font-size: 1.17em;")#End span
                     )#End notification_ui span
                     
                     #Display notification UI
                     showNotification(ui=notification_ui,
                                      #Duration=NULL will make the message persist until dismissed
                                      duration = NULL,
                                      id = ns("high_zero_content"),
                                      session=session)
                   }#End if statement
                 })
                 
                 # 3. Display stats --------------------------------------------
                 ## 3.1. Number of cells
                 output$n_cells <- renderText({
                   #Use isolate to keep values from updating before the submit 
                   #button is pressed
                   isolate(n_cells())
                 })
                 
                 ## 3.2 Cells with nonzero reads (correlations tab only)
                 if (tab=="corr"){
                   output$n_nonzero_and_percent <- renderText({
                     isolate(glue("{n_nonzero()} ({percent_nonzero()}%)"))
                   })
                 }
                 
                 ## 3.3. Summary of unique metadata in subset
                 # output[["selected_response"]] <- renderText({
                 #   vector_to_text(unique(subset()@meta.data[["response"]]))
                 # })
                 
                 #Create an output for each metadata category used in creation 
                 #of the subset
                 lapply(X = names(subset_selections()), 
                        FUN = function(category){
                          output[[glue("selected_{category}")]] <-
                            renderText({
                              #Display unique values appearing in the subset for the category
                              isolate(
                                vector_to_text(
                                  unique(subset()@meta.data[[category]])
                                  )
                                )
                            })
                        })
                 
               })
}
