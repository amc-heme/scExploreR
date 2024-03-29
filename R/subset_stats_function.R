#' Subset Stats
#'
#' Returns a list of subset statistics for differential gene expressionn or 
#' gene correlation analyses.
#' 
#' @param subset a subsetted Seurat object generated during analysis
#' @param mode the type of analysis being performed (either "dge" or "corr").
#' @param metadata_categories a vector giving the metadata variables 
#' (categories) exposed to the end user in the config app.
#' @param gene_selected for correlations analysis, the gene selected by the user. Must be defined if mode == "corr" but not if mode == "dge".
#' @param nonzero_threshold for correlations analysis, the threshold percentage of cells for which a feature in the correlations tab has non-zero expression, below which the end user is warned. Must be defined if mode == "corr" but not if mode == "dge".
#' @param group_by_category the metadata variable used for comparing dge groups. Must be defined if mode == "dge" but not if mode == "corr".
#'
#' @return a list of statistics for the current subset. 
#' For both modes, the number of cells is given.
#' 
#' For DGE, the following info is returned: 
#'   classes: the groups (classes), 
#'   n_classes: number of groups in the subset
#'   mode_description: the DGE mode
#'   n_by_class: the number of cells by class 
#'   
#' For correlations analysis, the following info is returned;
#'   n_nonzero: the number of cells with non-zero reads
#'   prop_nonzero: the fraction of cells with non-zero reads
#'   percent_nonzero: the percentage of cells with non-zero reads
#'
#' @noRd
subset_stats_function <- 
  function(
    subset,
    mode = c("dge", "corr"),
    metadata_categories, 
    gene_selected = NULL,
    nonzero_threshold = NULL,
    group_by_category = NULL
  ){
    # 1. n_cells: added for both modes
    n_cells <- 
      length(Cells(subset))
    
    # 2-4 Correlations tab-specific variables
    if (mode == "corr"){
      # 2. n_nonzero: number of nonzero reads
      n_nonzero <-
        sum(subset@assays$RNA@counts[gene_selected,] != 0)
      
      # 3. proportion of nonzero reads
      prop_nonzero <-
        n_nonzero / n_cells
      
      # 4. Percentage nonzero reads
      percent_nonzero <- 
        format(
          prop_nonzero * 100,
          # Display at least three sig figs in percentage
          digits = 3,
          # Display at least two digits after decimal point
          nsmall = 2,
          scientific = FALSE
        )
    }
    
    # 5-7 DGE tab-specific variables
    if (mode == "dge"){
      classes <- 
       unique(subset@meta.data[,group_by_category])
      
      # 5. Number of classes of the group_by metadata 
      # category in subset
      n_classes <- 
        length(classes)
      
      # 6. DGE mode
      mode_description <- 
        ifelse(
          # Conditional: TRUE when differential expression 
          # is selected
          n_classes == 2,
          # Differential expression: print the two groups
          # classes() contains the identities of both groups
          glue("Differential Expression ({classes[1]} vs.
               {classes[2]})"),
          # Marker identification: print the number of
          # classes selected
          glue("Marker Identification ({n_classes}  classes)")
          )
      
      # 7. Number of cells by class
      # Number of cells by class (tibble format)
      n_cells_tibble <-
        subset@meta.data |>
        # Group by the specified metadata variable
        group_by(.data[[group_by_category]]) |>
        # Calculate number of cells per group
        summarise(n = n())
      
      # Extract information from tibble
      # Class names in first column of tibble
      class_names <- as.character(n_cells_tibble[[1]])
      # Cell counts are in second column of tibble
      # Convert to vector
      n_cells_vector <- n_cells_tibble[[2]]
      
      # Print list of classes and the number of cells in each
      n_cells_list = list()
      for (i in 1:nrow(n_cells_tibble)){
        n_cells_list[[i]] <-
          glue("{class_names[i]}: {n_cells_vector[i]}")
      }
      
      # Collapse list of class-count pairs into a string
      # \n is the separator (will be read by
      # verbatimTextOutput())
      n_by_class <- paste(n_cells_list, collapse = "\n")
    }
    
    if (mode == "dge"){
      return(
        list(
          `n_cells` = n_cells,
          `classes` = classes,
          `n_classes` = n_classes,
          `mode_description` = mode_description,
          `n_by_class` = n_by_class
        )
      )
    } else if (mode == "corr"){
      return(
        list(
          `n_cells` = n_cells,
          `n_nonzero` = n_nonzero,
          `prop_nonzero` = prop_nonzero,
          `percent_nonzero` = percent_nonzero
        )
      )
    } 
  }