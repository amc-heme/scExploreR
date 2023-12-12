# Run this script to knit the dataset setup vignette locally.
# Rmd must be knitted locally since it is not possible to store the associated
# dataset with the package
# /concept frpm https://ropensci.org/blog/2019/12/08/precompute-vignettes/
knitr::knit(
  input = "./vignettes/dataset_setup_walkthrough.Rmd.orig",
  output = "./vignettes/dataset_setup_walkthrough.Rmd"
  )
