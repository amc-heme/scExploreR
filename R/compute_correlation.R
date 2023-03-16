#' Compute_correlation
#'
#' Takes the gene selection via input and the Seurat object via object, and 
#' computes a correlation table for the selected gene in the Seurat object.
#' The column names for the table may be specified via the colnames argument.
#'
#' @param gene_selected The current gene selected by the user, with the assay 
#' key prefix (i.e. "rna") removed.
#' @param object the Seurat object for which to compute the correlation. May be 
#' a subset or the full object.
#' @param colnames a character vector that should have exactly two elements. 
#' The first element will be column name for the feature, and the second will be 
#' the column name for the correlation coefficient. The column names will display
#' as entered in the downloaded table (but not in the table in the app since this 
#' is defined in the DT table header).
#' @param seurat_assay the name of the assay used to compute correlations. 
#' Defaults to "RNA".
#'
#' @return a table giving the correlation coefficients for all genes in the 
#' object relative to the gene selected. 
#' 
#' @noRd
compute_correlation <- 
  function(
    gene_selected,
    object,
    colnames = c("Feature", "Correlation_Coefficient"),
    seurat_assay = "RNA"
    ){
    # Determine the feature and coefficient colnames from the two-element 
    # colnames list
    feature_colname <- colnames[1]
    coeff_colname <- colnames[2]
    
    # Matrix: fetch expression matrix from the specified assay
    mat <- object@assays[[seurat_assay]]@data
    
    # Compute correlation between selected feature and others
    table <- 
      map2(
       1:nrow(mat),
       rownames(mat),
       ~ tibble::tibble(!!feature_colname := .y, !!coeff_colname := cor(mat[gene_selected, ], mat[.x, ]))
      ) |>
      bind_rows() |>
      # Filter out selected feature
      dplyr::filter(.data[[feature_colname]] != gene_selected) |> 
      # Arrange in descending order by correlation coefficient
      dplyr::arrange(desc(.data[[coeff_colname]]))
  
    return(table)
  }
