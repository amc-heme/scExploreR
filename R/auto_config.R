auto_config <- 
  function(
    object,
    file,
    object_name,
    is_HDF5SummarizedExperiment = FALSE,
    HDF5_prefix = NULL,
    object_description = "",
    include_numeric_metadata = TRUE,
    # Options for metadata_order
    # - "object": as it appears in object
    # - "ascending": ascending alphanumeric order
    # - "descending": descending alphanumeric order
    # metadata_order = "object",
    genes_assay = NULL,
    sample_level_var = NULL
  ){
    # Check inputs
    # genes_assay
    if (is.null(genes_assay)){
      # Set default if NULL (first assay)
      scExploreR:::assay_names(object = object)[[1]]
    } else {
      # Check value provided if not NULL
      # genes_assay must be a character vector
      if (class(genes_assay) != "character"){
        stop("`genes_assay` must be a character vector.")
      }
      
      # genes_assay must be in the assay names 
      if (!genes_assay %in% scExploreR:::assay_names(object = object)){
        stop(
          paste0(
            "The input for `genes_assay` does not match any of the ",
            "assays/modalities/experiments in the object provided."
          )
        )
      }
    }
    
    # General config fields 
    config <-
      list(
        `config_version` = 
          packageVersion("scExploreR") |>
          as.character(),
        `object_class` = 
          # Test for object classes supported by SCUBA and scExploreR
          if (inherits(object, "Seurat")){
            "Seurat"
          } else if (inherits(object, "SingleCellExperiment")){
            "SingleCellExperiment"
          } else if (inherits(object, "AnnDataR6")){
            "AnnDataR6"
          } else {
            stop(
              paste0(
                "Unrecognized object class: ", 
                paste(is(object), collapse = ",")
              )
            )
          },
        # HDF5-specific settings: supplied by the user
        `is_HDF5SummarizedExperiment` =
          is_HDF5SummarizedExperiment,
        `HDF5_prefix` = HDF5_prefix, 
        `label` = object_name,
        `description` = object_description,
        # Preview: a plot that appears in the choose dataset window 
        # to represent the dataset
        # Not supported in this function. Can be set in the config app
        `preview` = list(`type` = "none")
      )
    
    # Assays
    # Iterate through each assay and add using default name
    config$assays <-
      lapply(
        # Loop through each assay as determined by assay_names
        scExploreR:::assay_names(object = object),
        function(assay_name){
          # Construct fields for each assay using defaults
          list(
            # Name of assay
            `assay` = assay_name,
            # "key" for feature access via fetch_data: defined via the
            # make_key method
            `key` = 
              scExploreR:::make_key(
                object,
                assay = assay_name
              ),
            # A label for the assay which appears in parentheses after the assay 
            # name in plots. Default is "", which displays nothing 
            # (no parentheses either)
            `suffix_human` = "",
            # Name of the assay as it appears in dropdown menus
            # equal to the assay name by default and can be replaced with a "label"
            `dropdown_title` = assay_name
          )
        }
      )
    
    # Assign names to each element in config$assays (using assay_names)
    names(config$assays) <- 
      scExploreR:::assay_names(object = object)
    
    config$other_assay_options <-
      list(
        # Genes assay: provided by the user, or the first assay if undefined
        `genes_assay` = genes_assay,
        # ADT assay (used for ADT thresholding)
        # Can't be set in this function, must be done in the config app
        `adt_assay` = "none"
      )
    
    # Metadata
    # Whether to allow numeric metadata in plotting
    config$include_numeric_metadata <- include_numeric_metadata
    
    # Determine categorical metadata (config is only intended for these variable types)
    # meta_table <-
    #   SCUBA::fetch_metadata(
    #     object,
    #     full_table = TRUE
    #     )
    # # Identify variables in table
    # meta_vars <- colnames(meta_table)
    # 
    # # is_numeric: a vector of boolean values used to subset meta_columns
    # # for numeric metadata
    # is_numeric <-
    #   sapply(
    #     meta_vars,
    #     function(x, object){
    #       class(meta_table[[x]]) %in% c("numeric", "integer")
    #     },
    #     object
    #     )
    
    # Iterate through each metadata variable
    config$metadata <-
      lapply(
        # Allow chaos for now 
        SCUBA::meta_varnames(object),
        function(meta_var){
          list(
            # Name of metadata variable as it appears in the object
            `meta_colname` = meta_var,
            # Display name for metadata variable
            # This function will set the labels for each variable equal to the name
            # as it appears in the object.
            `label` = meta_var,
            # Description of metadata variable
            # Used in the object guide in the main app
            # Not supported in this function. This can be modified manually or 
            # in the config app
            `description` = "",
            # "Groups" of values in metadata variable
            # Not supported in this function
            `groups` = NULL
          )
        })
    
    # Apply names to elements in metadata 
    # names are the names of metadata variables as they appear in the object
    names(config$metadata) <- SCUBA::meta_varnames(object)
    
    # Other metadata options
    # Patient_colname: a patient-level metadata variable used for 
    # constructing sample-level metadata pie charts
    config$other_metadata_options <-
      list(
        `patient_colname` = sample_level_var
      )
    
    # Reductions
    # Iterate through each reduction, compile fields
    config$reductions <-
      lapply(
        scExploreR:::reduction_names(object),
        function(reduction_name){
          list(
            # Name of the reduction as it appears on the object
            `reduction` = reduction_name,
            # Display name of reduction: set to the reduction name
            `label` = reduction_name
          )
        })
    
    # Apply names to elements in config$reductions
    # names are the names of reductions as they appear in the object
    names(config$reductions) <- scExploreR:::reduction_names(object)
    
    # ADT thresholds are not supported in this function
    # Must use c() to append `adt_thresholds` with a NULL element 
    # (config$adt_thresholds <- NULL will remove the adt_thresholds element)
    config <-
      c(config, list(`adt_thresholds` = NULL))
    
    # Save config file to `file`
    yaml::write_yaml(
      config,
      file = file
    )
  }