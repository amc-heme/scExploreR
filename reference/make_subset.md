# Subset Creation

Subset Creation

Method ran for all classes besides anndata

## Usage

``` r
make_subset(object, filter_list)

# Default S3 method
make_subset(object, filter_list)

# S3 method for class 'AnnDataR6'
make_subset(object, filter_list)
```

## Arguments

- object:

  a single cell object

- filter_list:

  a list of filters constructed by the subset_selections module.

## Value

A Seurat object subsetted for the criteria entered.

## Methods (by class)

- `make_subset(default)`: Seurat, SingleCellExperiment objects

- `make_subset(AnnDataR6)`: Anndata objects
