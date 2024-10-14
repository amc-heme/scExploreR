test_that("scExploreR Basic Test", {
  # Don't run these tests on the CRAN build servers
  skip_on_cran()
  
  
  library(shiny)
  library(shinytest2)
  options(shiny.reactlog = TRUE)
  selected_key <- "object"
  
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

  record_test(app,
              name = "test1",
              seed = NULL,
              load_timeout = NULL,
              shiny_args = NULL, #list(),
              test_file = "test-shinytest2.R",
              open_test_file = rlang::is_interactive(),
              allow_no_input_binding = NULL,
              record_screen_size = TRUE,
              run_test = TRUE
  )
  
  #app$expect_values()
})