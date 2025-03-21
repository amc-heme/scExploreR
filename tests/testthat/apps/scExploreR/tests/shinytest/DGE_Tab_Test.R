# Auto-generated script produced using shinyApp::recordTest
app <- ShinyDriver$new("../../", seed = 325)
app$snapshotInit("DGE_Tab")

app$setInputs(waiter_shown = TRUE, allowInputNoBinding_ = TRUE, priority_ = "event")
app$setInputs(waiter_hidden = TRUE, allowInputNoBinding_ = TRUE, priority_ = "event")
app$setInputs(waiter_shown = TRUE, allowInputNoBinding_ = TRUE, priority_ = "event")
app$setInputs(waiter_hidden = TRUE, allowInputNoBinding_ = TRUE, priority_ = "event")
app$setInputs(waiter_shown = TRUE, allowInputNoBinding_ = TRUE, priority_ = "event")
app$setInputs(waiter_shown = TRUE, allowInputNoBinding_ = TRUE, priority_ = "event")
app$setInputs(waiter_shown = TRUE, allowInputNoBinding_ = TRUE, priority_ = "event")
app$setInputs(waiter_shown = TRUE, allowInputNoBinding_ = TRUE, priority_ = "event")
app$setInputs(waiter_shown = TRUE, allowInputNoBinding_ = TRUE, priority_ = "event")
app$setInputs(waiter_shown = TRUE, allowInputNoBinding_ = TRUE, priority_ = "event")
app$setInputs(`object_plots-main_panel_waiter_hidden` = TRUE, allowInputNoBinding_ = TRUE, priority_ = "event")
app$setInputs(`object_plots-subset_panel_waiter_hidden` = TRUE, allowInputNoBinding_ = TRUE, priority_ = "event")
app$setInputs(`object_plots-violin-custom_refactor` = c("B Cells", "BM Monocytes", "CD4+ T Cells", "CD8+ T Cells", "Dendritic cells", "NK Cells", "PBMC Monocytes", "Plasma cells", "Plasmacytoid dendritic cells", "Primitive"), allowInputNoBinding_ = TRUE)
app$setInputs(`object_plots-dot-custom_refactor` = c("B Cells", "BM Monocytes", "CD4+ T Cells", "CD8+ T Cells", "Dendritic cells", "NK Cells", "PBMC Monocytes", "Plasma cells", "Plasmacytoid dendritic cells", "Primitive"), allowInputNoBinding_ = TRUE)
app$setInputs(`object_plots-ridge-custom_refactor` = character(0), allowInputNoBinding_ = TRUE)
app$setInputs(`object_plots-proportion-custom_refactor` = c("B Cells", "BM Monocytes", "CD4+ T Cells", "CD8+ T Cells", "Dendritic cells", "NK Cells", "PBMC Monocytes", "Plasma cells", "Plasmacytoid dendritic cells", "Primitive"), allowInputNoBinding_ = TRUE)

# Swtich to DGE tab: find button in navbar panel, and click the button 
dge_tab_btn <- app$findElement(linkText = "Differential Expression")
dge_tab_btn$click()

app$setInputs(`object_dge-submit` = "click")
app$setInputs(waiter_shown = TRUE, allowInputNoBinding_ = TRUE, priority_ = "event")
app$setInputs(waiter_shown = TRUE, allowInputNoBinding_ = TRUE, priority_ = "event")
app$setInputs(`object_dge-sidebar_waiter_hidden` = TRUE, allowInputNoBinding_ = TRUE, priority_ = "event")
app$setInputs(`object_dge-main_panel_spinner_waiter_hidden` = TRUE, allowInputNoBinding_ = TRUE, priority_ = "event")
app$snapshot()
app$setInputs(`object_dge-test_selections-mode` = "mode_dge")
app$setInputs(`object_dge-test_selections-group_by` = "Batch")
app$setInputs(`object_dge-test_selections-group_1_open` = TRUE, allowInputNoBinding_ = TRUE)
app$setInputs(`object_dge-test_selections-group_1` = "BM_200AB")
app$setInputs(`object_dge-test_selections-group_1_open` = FALSE, allowInputNoBinding_ = TRUE)
app$setInputs(`object_dge-test_selections-group_2_open` = TRUE, allowInputNoBinding_ = TRUE)
app$setInputs(`object_dge-test_selections-group_2` = "PBMC_200AB")
app$setInputs(`object_dge-test_selections-group_2_open` = FALSE, allowInputNoBinding_ = TRUE)
app$setInputs(`object_dge-submit` = "click")
app$setInputs(waiter_shown = TRUE, allowInputNoBinding_ = TRUE, priority_ = "event")
app$setInputs(waiter_shown = TRUE, allowInputNoBinding_ = TRUE, priority_ = "event")
app$setInputs(`object_dge-sidebar_waiter_hidden` = TRUE, allowInputNoBinding_ = TRUE, priority_ = "event")
app$setInputs(`object_dge-main_panel_spinner_waiter_hidden` = TRUE, allowInputNoBinding_ = TRUE, priority_ = "event")
app$snapshot()
app$setInputs(`object_dge-test_selections-group_by` = "condensed_cell_type")
app$setInputs(`object_dge-test_selections-group_1_open` = TRUE, allowInputNoBinding_ = TRUE)
app$setInputs(`object_dge-test_selections-group_1` = "Primitive")
app$setInputs(`object_dge-test_selections-group_1_open` = FALSE, allowInputNoBinding_ = TRUE)
app$setInputs(`object_dge-test_selections-group_2_open` = TRUE, allowInputNoBinding_ = TRUE)
app$setInputs(`object_dge-test_selections-group_2` = "PBMC Monocytes")
app$setInputs(`object_dge-test_selections-group_2` = c("BM Monocytes", "PBMC Monocytes"))
app$setInputs(`object_dge-test_selections-group_2_open` = FALSE, allowInputNoBinding_ = TRUE)
app$setInputs(`object_dge-submit` = "click")
app$setInputs(waiter_shown = TRUE, allowInputNoBinding_ = TRUE, priority_ = "event")
app$setInputs(waiter_shown = TRUE, allowInputNoBinding_ = TRUE, priority_ = "event")
app$setInputs(`object_dge-sidebar_waiter_hidden` = TRUE, allowInputNoBinding_ = TRUE, priority_ = "event")
app$setInputs(`object_dge-main_panel_spinner_waiter_hidden` = TRUE, allowInputNoBinding_ = TRUE, priority_ = "event")
app$snapshot()
