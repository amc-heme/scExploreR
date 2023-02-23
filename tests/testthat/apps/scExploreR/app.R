library(scExploreR)

run_scExploreR(
  object_path = system.file("extdata", "test_dataset.rds", package = "scExploreR", mustWork = TRUE),
  config_path = system.file("extdata", "test_dataset_config.yaml", package = "scExploreR", mustWork = TRUE),
  launch.browser = TRUE
)
