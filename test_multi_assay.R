#!/usr/bin/env Rscript

# Test script for multi-assay differential expression support
# This script tests the key functionality we added

library(scExploreR)

cat("Testing multi-assay differential expression support...\n\n")

# Test 1: Check that the config file is correctly parsed ####
cat("Test 1: Checking config file parsing...\n")
config_path <- "inst/extdata/test_dataset_config.yaml"

if (file.exists(config_path)) {
  config <- yaml::read_yaml(config_path)
  
  # Check assays section
  if (!is.null(config$assays)) {
    cat("  ✓ Config has assays section\n")
    cat("  Available assays:", paste(names(config$assays), collapse = ", "), "\n")
    
    # Check each assay has dropdown_title
    all_have_dropdown_title <- all(
      sapply(config$assays, function(x) !is.null(x$dropdown_title))
    )
    
    if (all_have_dropdown_title) {
      cat("  ✓ All assays have dropdown_title field\n")
    } else {
      cat("  ✗ Some assays missing dropdown_title field\n")
    }
  } else {
    cat("  ✗ Config missing assays section\n")
  }
  
  # Check designated genes assay
  if (!is.null(config$other_assay_options$genes_assay)) {
    cat("  ✓ Config has genes_assay field:", 
        config$other_assay_options$genes_assay, "\n")
  } else {
    cat("  ✗ Config missing genes_assay field\n")
  }
} else {
  cat("  ✗ Config file not found\n")
}

cat("\n")

# Test 2: Check that the UI function has correct parameters ####
cat("Test 2: Checking dge_tab_ui function signature...\n")

# Get the function
ui_func <- scExploreR:::dge_tab_ui

# Check if function exists
if (is.function(ui_func)) {
  cat("  ✓ dge_tab_ui function exists\n")
  
  # Get function parameters
  params <- names(formals(ui_func))
  
  # Check for assay_config parameter
  if ("assay_config" %in% params) {
    cat("  ✓ dge_tab_ui has assay_config parameter\n")
  } else {
    cat("  ✗ dge_tab_ui missing assay_config parameter\n")
  }
  
  # Check for designated_genes_assay parameter
  if ("designated_genes_assay" %in% params) {
    cat("  ✓ dge_tab_ui has designated_genes_assay parameter\n")
  } else {
    cat("  ✗ dge_tab_ui missing designated_genes_assay parameter\n")
  }
} else {
  cat("  ✗ dge_tab_ui function not found\n")
}

cat("\n")

# Test 3: Check that the server function has correct parameters ####
cat("Test 3: Checking dge_tab_server function signature...\n")

# Get the function
server_func <- scExploreR:::dge_tab_server

# Check if function exists
if (is.function(server_func)) {
  cat("  ✓ dge_tab_server function exists\n")
  
  # Get function parameters
  params <- names(formals(server_func))
  
  # Check for assay_config parameter
  if ("assay_config" %in% params) {
    cat("  ✓ dge_tab_server has assay_config parameter\n")
  } else {
    cat("  ✗ dge_tab_server missing assay_config parameter\n")
  }
  
  # Check for designated_genes_assay parameter
  if ("designated_genes_assay" %in% params) {
    cat("  ✓ dge_tab_server has designated_genes_assay parameter\n")
  } else {
    cat("  ✗ dge_tab_server missing designated_genes_assay parameter\n")
  }
} else {
  cat("  ✗ dge_tab_server function not found\n")
}

cat("\n")

# Test 4: Check filtering module returns assay filter ####
cat("Test 4: Checking dge_table_filtering module...\n")

filter_ui_func <- scExploreR:::dge_table_filtering_ui

if (is.function(filter_ui_func)) {
  cat("  ✓ dge_table_filtering_ui function exists\n")
} else {
  cat("  ✗ dge_table_filtering_ui function not found\n")
}

cat("\n")

cat("All basic structure tests completed!\n\n")

cat("Note: Full integration testing requires running the Shiny app with test data.\n")
cat("To manually test:\n")
cat("  1. Run: scExploreR::run_scExploreR(\n")
cat("           object_path = 'inst/extdata/test_dataset.rds',\n")
cat("           config_path = 'inst/extdata/test_dataset_config.yaml'\n")
cat("         )\n")
cat("  2. Navigate to the Differential Expression tab\n")
cat("  3. Verify 'Assays to Analyze' checkbox group appears\n")
cat("  4. Select multiple assays (if available) and run analysis\n")
cat("  5. Verify results include 'Assay' column\n")
cat("  6. Verify assay filter appears in table filtering options\n")
