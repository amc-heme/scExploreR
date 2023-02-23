library(shinytest)
library(shiny)

app <- ShinyDriver$new("./apps/scExploreR", seed = 325)

test_that("No errors for Marker ID", {
  # Swtich to DGE tab: find button in navbar panel, and click the button 
  dge_tab_btn <- app$findElement(linkText = "Differential Expression")
  dge_tab_btn$click()
  
  # Must wait for the DGE tab submit button to appear 
  app$waitForShiny()
  
  # No adjustments necessary to run marker identification; just click submit
  app$click("object_dge-submit")
  # Wait for table to compute after pressing submit
  app$waitForShiny()
  
  # The output below will appear only if the DGE test ran without errors
  expect_true(isTruthy(app$getValue("object_dge-subset_stats-n_by_class")))
  # The app should also be able to screenshot the table without issue
  expect_no_error(
    app$takeScreenshot(
      id = "object_dge-table", 
      file = NULL
      )
    )
})
