library(scExploreR)

run_scExploreR(
  object_path = system.file("extdata", "test_dataset.rds", package = "SCEPlots"),
  config_path = system.file("extdata", "test_dataset_conig.yaml", package = "SCEPlots")
)