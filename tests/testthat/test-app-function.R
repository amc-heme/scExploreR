test_that("scExploreR Basic Test", {
  # Don't run these tests on the CRAN build servers
  skip_on_cran()
  
  
  if(Sys.info()['sysname'] == "Darwin"){
    library(shiny)
    library(shinytest2)
    options(shiny.reactlog = TRUE)
    selected_key <- "object"
  }
  
  shiny_app <- scExploreR::run_scExploreR(
    object_path = system.file("extdata", 
                              "test_dataset.rds", 
                              package = "scExploreR"
                              ), 
    config_path = system.file("extdata", 
                              "test_dataset_config.yaml", 
                              package = "scExploreR"
                              ), 
    dev_mode=TRUE
  )
  
  app <- AppDriver$new(shiny_app, name = "scExploreR", load_timeout = 30000)
  #app$stop() 

  # record_test(app,
  #             name = "test1",
  #             seed = NULL,
  #             load_timeout = NULL,
  #             shiny_args = NULL, #list(),
  #             test_file = "test-shinytest2.R",
  #             open_test_file = rlang::is_interactive(),
  #             allow_no_input_binding = NULL,
  #             record_screen_size = TRUE,
  #             run_test = TRUE
  # )
  
  #app$expect_values()
  
  ####
  ####. TESTING AREA
  ####
  
  app$set_window_size(width = 1257, height = 968)
  # Update unbound `input` value
  # Update output value
  app$set_inputs(`object_plots-feature-blend_custom_low` = "#E1E1E1")
  app$set_inputs(`object_plots-feature-blend_custom_1` = "#FF0000")
  app$set_inputs(`object_plots-feature-blend_custom_2` = "#0000FF")
  app$set_inputs(`object_plots-feature-min_color` = "#E1E1E1")
  app$set_inputs(`object_plots-feature-max_color` = "#000000")
  app$set_inputs(`object_plots-subset_selections-numeric_mode` = "greater_than")
  app$set_inputs(`object_plots-which_features` = 0)
  app$set_inputs(`object_plots-palettes` = 0)
  app$set_inputs(`object_plots-feature_stats_collapsible` = 0)
  app$set_inputs(`object_plots-subset_collapsible` = 0)
  app$set_inputs(`object_plots-dimplot_collapsible` = 0)
  app$set_inputs(`object_plots-dimplot-legend_options_panel` = 0)
  app$set_inputs(`object_plots-feature_collapsible` = 0)
  app$set_inputs(`object_plots-vln_collapsible` = 0)
  app$set_inputs(`object_plots-violin-legend_options_panel` = 0)
  app$set_inputs(`object_plots-dot_collapsible` = 0)
  app$set_inputs(`object_plots-scatter_collapsible` = 0)
  app$set_inputs(`object_plots-scatter-legend_options_panel` = 0)
  app$set_inputs(`object_plots-ridge_collapsible` = 0)
  app$set_inputs(`object_plots-ridge-legend_options_panel` = 0)
  app$set_inputs(`object_plots-proportion_collapsible` = 0)
  app$set_inputs(`object_plots-proportion-legend_options_panel` = 0)
  app$set_inputs(`object_plots-categorical_palette` = "default")
  app$set_inputs(`object_plots-continuous_palette` = "default")
  app$set_inputs(`object_plots-subset_selections-categorical_values` = character(0))
  app$set_inputs(`object_plots-feature-blend_palette` = character(0))
  app$click("object_plots-subset_selections-add_filter")
  app$click("object_plots-subset_selections-reset_all_filters")
  app$click("drop532460240")
  app$click("object_plots-subset_selections-filter_threshold-apply_xlim")
  app$click("object_plots-subset_selections-filter_threshold-restore_xlim")
  app$click("object_plots-subset_selections-filter_threshold-range-edit")
  app$click("object_plots-subset_selections-filter_threshold-edit_lower_bound")
  app$click("object_plots-subset_selections-filter_threshold-edit_upper_bound")
  app$click("object_plots-subset_selections-feature_search_dropup")
  app$click("drop382728318")
  app$click("object_plots-subset_selections-feature_stats_interactive-apply_xlim")
  app$click("object_plots-subset_selections-feature_stats_interactive-restore_xlim")
  app$click("object_plots-subset_selections-feature_stats_interactive-range-edit")
  app$click("object_plots-subset_selections-feature_stats_interactive-edit_lower_bound")
  app$click("object_plots-subset_selections-feature_stats_interactive-edit_upper_bound")
  app$click("object_plots-subset_selections-filter_cancel")
  app$click("object_plots-subset_selections-filter_confirm")
  app$click("object_plots-subset_submit")
  app$click("object_plots-dimplot-custom_title_reset")
  app$click("object_plots-dimplot-custom_title_update")
  app$click("object_plots-dimplot-custom_title_multi-reset_all")
  app$click("object_plots-dimplot-custom_title_multi-update")
  app$click("object_plots-dimplot-download")
  app$click("object_plots-feature-custom_title_reset")
  app$click("object_plots-feature-custom_title_update")
  app$click("object_plots-feature-custom_title_multi-reset_all")
  app$click("object_plots-feature-custom_title_multi-update")
  app$click("object_plots-feature-download")
  app$click("object_plots-violin-download")
  app$click("object_plots-dot-rename_dot_x_labels-reset_all")
  app$click("object_plots-dot-rename_dot_x_labels-update")
  app$click("object_plots-dot-download")
  app$click("object_plots-scatter-download")
  app$click("object_plots-ridge-custom_title_reset")
  app$click("object_plots-ridge-custom_title_update")
  app$click("object_plots-ridge-custom_title_multi-reset_all")
  app$click("object_plots-ridge-custom_title_multi-update")
  app$click("object_plots-ridge-custom_xlim-restore_xlim")
  app$click("object_plots-ridge-custom_xlim-apply_xlim")
  app$click("object_plots-ridge-download")
  app$click("object_plots-proportion-custom_title_reset")
  app$click("object_plots-proportion-custom_title_update")
  app$click("object_plots-proportion-custom_title_multi-reset_all")
  app$click("object_plots-proportion-custom_title_multi-update")
  app$click("object_plots-proportion-download")
  app$set_inputs(`object_plots-text_features` = character(0))
  app$set_inputs(`object_plots-subset_selections-filter_type` = "")
  app$set_inputs(`object_plots-subset_selections-categorical_var` = "")
  app$set_inputs(`object_plots-subset_selections-numeric_feature` = "")
  app$set_inputs(`object_plots-subset_selections-search_feature` = "")
  app$set_inputs(`object_plots-dimplot-group_by` = "condensed_cell_type")
  app$set_inputs(`object_plots-dimplot-split_by` = "none")
  app$set_inputs(`object_plots-dimplot-reduction` = "pca")
  app$set_inputs(`object_plots-dimplot-title_settings` = "default")
  app$set_inputs(`object_plots-dimplot-file_type` = "png")
  app$set_inputs(`object_plots-feature-group_by` = "condensed_cell_type")
  app$set_inputs(`object_plots-feature-split_by` = "none")
  app$set_inputs(`object_plots-feature-reduction` = "pca")
  app$set_inputs(`object_plots-feature-title_settings` = "default")
  app$set_inputs(`object_plots-feature-legend_title` = "feature")
  app$set_inputs(`object_plots-feature-blend_layout` = "2col")
  app$set_inputs(`object_plots-feature-file_type` = "png")
  app$set_inputs(`object_plots-violin-group_by` = "condensed_cell_type")
  app$set_inputs(`object_plots-violin-split_by` = "none")
  app$set_inputs(`object_plots-violin-sort_groups` = "ascending")
  app$set_inputs(`object_plots-violin-file_type` = "png")
  app$set_inputs(`object_plots-dot-group_by` = "condensed_cell_type")
  app$set_inputs(`object_plots-dot-sort_groups` = "ascending")
  app$set_inputs(`object_plots-dot-dot_x_labels` = "truncated")
  app$set_inputs(`object_plots-dot-separate_features` = character(0))
  app$set_inputs(`object_plots-dot-file_type` = "png")
  app$set_inputs(`object_plots-scatter-scatter_1` = "")
  app$set_inputs(`object_plots-scatter-scatter_2` = "")
  app$set_inputs(`object_plots-scatter-group_by` = "condensed_cell_type")
  app$set_inputs(`object_plots-scatter-file_type` = "png")
  app$set_inputs(`object_plots-ridge-group_by` = "none")
  app$set_inputs(`object_plots-ridge-title_settings` = "default")
  app$set_inputs(`object_plots-ridge-sort_groups` = "ascending")
  app$set_inputs(`object_plots-ridge-file_type` = "png")
  app$set_inputs(`object_plots-proportion-group_by` = "condensed_cell_type")
  app$set_inputs(`object_plots-proportion-split_by` = "condensed_cell_type")
  app$set_inputs(`object_plots-proportion-title_settings` = "default")
  app$set_inputs(`object_plots-proportion-sort_groups` = "ascending")
  app$set_inputs(`object_plots-proportion-file_type` = "png")
  app$set_inputs(`object_plots-dimplot-legend_ncol` = 1)
  app$set_inputs(`object_plots-dimplot-legend_size` = 6)
  app$set_inputs(`object_plots-violin-legend_ncol` = 1)
  app$set_inputs(`object_plots-violin-legend_size` = 6)
  app$set_inputs(`object_plots-scatter-legend_ncol` = 1)
  app$set_inputs(`object_plots-scatter-legend_size` = 6)
  app$set_inputs(`object_plots-ridge-legend_ncol` = 1)
  app$set_inputs(`object_plots-ridge-legend_size` = 6)
  app$set_inputs(`object_plots-proportion-legend_ncol` = 1)
  app$set_inputs(`object_plots-proportion-legend_size` = 6)
  app$set_inputs(`object_plots-make_dimplot` = TRUE)
  app$set_inputs(`object_plots-make_feature` = FALSE)
  app$set_inputs(`object_plots-make_scatter` = FALSE)
  app$set_inputs(`object_plots-make_proportion` = FALSE)
  app$set_inputs(`object_plots-make_vln` = FALSE)
  app$set_inputs(`object_plots-make_dot` = FALSE)
  app$set_inputs(`object_plots-make_ridge` = FALSE)
  app$set_inputs(`object_plots-raw_feature_names` = FALSE)
  app$set_inputs(`object_plots-dimplot-legend` = TRUE)
  app$set_inputs(`object_plots-dimplot-default_legend_ncol` = TRUE)
  app$set_inputs(`object_plots-dimplot-label` = TRUE)
  app$set_inputs(`object_plots-dimplot-manual_dim-manual_dim` = FALSE)
  app$set_inputs(`object_plots-feature-super_title` = TRUE)
  app$set_inputs(`object_plots-feature-share_scale` = FALSE)
  app$set_inputs(`object_plots-feature-color_by_feature` = FALSE)
  app$set_inputs(`object_plots-feature-blend` = FALSE)
  app$set_inputs(`object_plots-feature-order` = FALSE)
  app$set_inputs(`object_plots-feature-label` = FALSE)
  app$set_inputs(`object_plots-feature-legend` = TRUE)
  app$set_inputs(`object_plots-feature-custom_colors` = FALSE)
  app$set_inputs(`object_plots-feature-manual_dim-manual_dim` = FALSE)
  app$set_inputs(`object_plots-violin-legend` = TRUE)
  app$set_inputs(`object_plots-violin-default_legend_ncol` = TRUE)
  app$set_inputs(`object_plots-violin-manual_dim-manual_dim` = FALSE)
  app$set_inputs(`object_plots-dot-legend` = TRUE)
  app$set_inputs(`object_plots-dot-manual_dim-manual_dim` = FALSE)
  app$set_inputs(`object_plots-dot-use_separate_features` = FALSE)
  app$set_inputs(`object_plots-scatter-legend` = TRUE)
  app$set_inputs(`object_plots-scatter-default_legend_ncol` = TRUE)
  app$set_inputs(`object_plots-scatter-display_coeff` = TRUE)
  app$set_inputs(`object_plots-scatter-manual_dim-manual_dim` = FALSE)
  app$set_inputs(`object_plots-ridge-legend` = TRUE)
  app$set_inputs(`object_plots-ridge-default_legend_ncol` = TRUE)
  app$set_inputs(`object_plots-ridge-use_custom_xlim` = FALSE)
  app$set_inputs(`object_plots-ridge-manual_dim-manual_dim` = FALSE)
  app$set_inputs(`object_plots-proportion-legend` = TRUE)
  app$set_inputs(`object_plots-proportion-default_legend_ncol` = TRUE)
  app$set_inputs(`object_plots-proportion-manual_dim-manual_dim` = FALSE)
  app$set_inputs(`object_plots-subset_selections-adv_filter_code` = "")
  app$set_inputs(`object_plots-subset_selections-filter_threshold-lower_xlim` = "")
  app$set_inputs(`object_plots-subset_selections-filter_threshold-upper_xlim` = "")
  app$set_inputs(`object_plots-subset_selections-feature_stats_interactive-lower_xlim` = "")
  app$set_inputs(`object_plots-subset_selections-feature_stats_interactive-upper_xlim` = "")
  app$set_inputs(`object_plots-dimplot-custom_title` = "")
  app$set_inputs(`object_plots-feature-custom_title` = "")
  app$set_inputs(`object_plots-ridge-custom_title` = "")
  app$set_inputs(`object_plots-ridge-custom_xlim-lower_xlim` = "")
  app$set_inputs(`object_plots-ridge-custom_xlim-upper_xlim` = "")
  app$set_inputs(`object_plots-proportion-custom_title` = "")
  # Update unbound `input` value
  app$set_inputs(`object_plots-subset_selections-feature_search_dropup_state` = FALSE)
  # Update output value
  # Update unbound `input` value
  # Update output value
  app$set_inputs(`object_plots-make_vln` = TRUE)
  # Update output value
  app$set_inputs(`object_plots-text_features` = "rna_ACTG1")
  # Update output value
  app$set_inputs(`object_plots-dot-separate_features` = "rna_ACTG1")
  # Update unbound `input` value
  # Update output value
  app$expect_values()
})
