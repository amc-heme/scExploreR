#Create Subset ####
#Takes a Seurat object and returns a subset based on selection criteria.
#Currently hard-coded to work with specific metadata; will be generalized.
make_subset <- function(input,sobj){
  obj_sub <- subset(sobj, 
                    subset=(clusters %in% input$clusters_selection) & 
                      (response %in% input$response_selection) & 
                      (htb %in% input$htb_selection) &
                      (treatment %in% input$treatment_selection)
                    )
  return(obj_sub)
}

#Compute_subset_stats() ####
#used in correlations and dge tab. 
#Arguments
#session: session in which to display nofitications, if they apply
#rv: list of reactive values created in app.
#gene_selected: a reactive value providing the current gene selected by the user,
#with the "rna_" prefix removed. The prefix is removed in the main app code 
#in section 2.3.1.
#nonzero_threshold: non-reactive numeric value hard-coded in app. Defines the 
#minimum acceptable proportion of cells with nonzero reads for the gene selected. 
#If the proportion is below the threshold, a notification appears warning the 
#user that the results may be inaccurate.
compute_subset_stats <- function(session,
                                 rv,
                                 gene_selected,
                                 nonzero_threshold
                                 ){
  #Determine the proportion of cells with nonzero reads for the selected 
  #gene. If it is below the threshold defined at the top of this script,
  #return a warning to the user.
  
  #Cells in subset
  rv$n_cells <- length(Cells(rv$s_sub))
  #Cells with nonzero reads
  rv$n_nonzero <- sum(rv$s_sub@assays$RNA@counts[gene_selected(),] != 0)
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
  
#Compute_correlation: ####
#Takes the gene selection via input and the Seurat object via object, and 
#computes a correlation table for the selected gene in the Seurat object
#The column names for the table may be specified via the colnames argument

#Arguments
#gene_selected: a reactive value providing the current gene selected by the user,
#with the "rna_" prefix removed.
#object: the Seurat object for which to compute the correlation. May be a 
#subset or the full object.
#colnames: a character vector that should have exactly two elements. The first 
#element will be column name for the feature, and the second will be the column 
#name for the correlation coefficient. The column names will display as entered 
#in the downloaded table (but not in the table in the app since this is defined 
#in the DT table header).
compute_correlation <- function(gene_selected,
                                object,
                                colnames=c("Feature","Correlation_Coefficient")
                                ){
  #Determine the feature and coefficient colnames from the two-element colnames list
  feature_colname <- colnames[1]
  coeff_colname <- colnames[2]
  
  #Form matrix from the Seurat object (either subset or full data)
  mat <- t(as.matrix(object@assays$RNA@data))
  
  #Compute correlation between selected feature and others
  table <- cor(mat[,gene_selected()],mat) |> 
    #Code returns coefficients for each feature in rows (want columns) 
    t() |> 
    #Convert matrix to tibble using enframe()
    #Enframe takes the piped result, which is a single-column matrix with 
    #name/value pairs (genes are the names and the computed coefficients are the 
    #values). The "name" and "value" arguments passed to enframe() will determine
    #the column names of the feature column and the correlation coefficient, 
    #respectively, in the new tibble.
    enframe(name = feature_colname,value = coeff_colname) |> 
    #Filter out selected feature
    filter(.data[[feature_colname]] != gene_selected()) |> 
    #Arrange in descending order by correlation coefficient
    arrange(desc(.data[[coeff_colname]]))
  
  return(table)
}

#hr_name: given the machine-readable name of an input feature, 
#return the human readable name. Used for modifying the titles of plots, which 
#use the machine-readable name by default.
 hr_name <- function(machine_readable_name,assay_info){
  #Check each assay to see which assay's machine-readable prefix corresponds to 
  #the server value of the input
  for (assay in assay_info){
    if (grepl(assay$prefix_machine, machine_readable_name)){
      #When a match is found, remove the assay's machine-readable prefix with sub()
      HR <- sub(assay$prefix_machine,"",machine_readable_name)
      #Add the assay's human-readable suffix to the title
      HR <- paste0(HR,assay$suffix_human)
      break
    }
  }
  
  #Return the human-readable name 
  return(HR)
}

#n_patches: get number of plots in a patchwork object
#Derived from patchwork source code and used to determine number of plots to 
#iterate through in a for loop
#https://github.com/thomasp85/patchwork/blob/master/R/plot_patchwork.R
n_patches <- function(patchwork){
  n_patches <- length(patchwork$patches$plots)
  #length(patchwork$patches$plots) is always one less than the number of plots,
  #except when there are no plots in the object.
  if (!is_empty(patchwork)) {
    n_patches <- n_patches + 1
    }
  return(n_patches)
}

#hr_title: for feature plots with no split.by argument and violin plots, 
#converts the title from the default machine-readable name to a human-readable name.
hr_title <- function(patchwork,n_patches,assay_info){
  for (i in 1:n_patches){
    #Fetch title of plot
    title_machine <- patchwork[[i]]$labels$title
    #Convert title to human-readable format
    title_HR <- hr_name(title_machine,assay_info)
    #Set the plot title to the human readable 
    patchwork[[i]] <- patchwork[[i]] + ggtitle(title_HR)
  }
  #Return corrected patchwork object when complete
  return(patchwork)
}

