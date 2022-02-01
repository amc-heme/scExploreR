#subset_stats_metadata

#Returns a container with the user-defined label for the metadata category 
#and the unique values for the category found in the subset
#Arguments
#category: the category to create the output container for 
#metadata_config: metadata section of the config file, loaded in the 
#main server function.
#ns: namespacing function used in the module this function is called from, if
#applicable (called without brackets, name instead of name()).This is optional 
#but must be specified if the function is called from within a module.
subset_stats_metadata_output <- function(category, 
                                  metadata_config, 
                                  ns=NULL){
  #Get label for the metadata category used in the current subset selection 
  #menu, defined in config file
  label<- metadata_config[[category]]$label
  
  #Container UI
  div(
    #Category name 
    tags$strong(glue("{label}:")),
    #Values in subset: rendered by server
    textOutput(
      #Output ID: uses the category name 
      outputId = 
        if(!is.null(ns) && is.function(ns)){
          #Apply namespace function if it is passed to ns argument
          ns(glue("selected_{category}"))
          } else {
            glue("selected_{category}")
            },
      #Category label and values display inline
      inline=TRUE)
    )
}
