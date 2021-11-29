#Compute_subset_stats: used in correlations and dge tab. 
compute_subset_stats <- function(input,output,session,rv,nonzero_threshold){
  #Determine the proportion of cells with nonzero reads for the selected 
  #gene. If it is below the threshold defined at the top of this script,
  #return a warning to the user.
  
  #Cells in subset
  rv$n_cells <- length(Cells(rv$s_sub))
  #Cells with nonzero reads
  rv$n_nonzero <- sum(rv$s_sub@assays$RNA@counts[input$corr_feature_selection,] != 0)
  #Proportion of nonzero reads
  rv$prop_nonzero <- rv$n_nonzero/rv$n_cells
  #Store as a percentage (format to show at least two digits after decimal 
  #point, and at least three sig figs)
  rv$percent_nonzero <- format(rv$prop_nonzero*100, 
                               digits=3, 
                               nsmall=2, 
                               scientific=FALSE)
  
  print(
    paste0("Percent nonzero: ",rv$percent_nonzero,"%")
    )
  
  #Notification if nonzero proportion is too low
  if (rv$prop_nonzero < nonzero_threshold){
    #Define notification UI (warning icon plus text)
    notification_ui <- span(
      #Warning icon (inline and enlarged)
      icon("exclamation-triangle", style="display: inline-block; font-size: 1.7em;"),
      #Notification text with proportion and number of non-zero cells
      span(glue("Low gene coverage: the selected feature was detected in {rv$percent_nonzero}% of cells within the selection restriction criteria ({rv$n_nonzero}/{rv$n_cells} cells). Correlation results may be inaccurate."),
           #Font size of notification text 
           style="font-size: 1.17em;")#End span
    )#End notification_ui span
    
    #Display notification UI
    showNotification(ui=notification_ui, 
                     #Duration=NULL will make the message persist until dismissed
                     duration = NULL,
                     id = "corr_high_zero_content",
                     session=session)  
  
}#End if statement

}

# render_statistics() ####
# prints the results of the subset statistics computed in 
#compute_subset_stats() to screen.
render_statistics <- function(input,output,session,rv){
  #Clusters selected
  output$selected_clusters <- renderText(isolate(vector_to_text(input$clusters_selection)))
  #Response criteria selected
  output$selected_response <- renderText(isolate(vector_to_text(input$response_selection)))
  #Patients selected
  output$selected_htb <- renderText(isolate(vector_to_text(input$htb_selection)))
  
  #Number of cells in subset
  output$print_n_cells <- renderText(isolate(rv$n_cells))
  
  #Percent of cells with non-zero reads
  output$print_nonzero <- renderText(isolate(glue("{rv$n_nonzero} ({rv$percent_nonzero}%)")))
  
}