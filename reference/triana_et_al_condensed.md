# AML Reference Dataset

An AML single cell reference dataset produced by [Triana et al.
2021](https://doi.org/10.1038/s41590-021-01059-0). This is a small
subset of the original data that is intended for use with automated
tests.

The dataset includes a pheresis and a bone marrow sample from a single
young healthy donor. A panel of 462 genes is included in the dataset,
along with 197 surface protein markers. A UMAP projection based on gene
expression was computed from the original Seurat object downloaded from
Figshare (see source below). The cell types provided by Triana et al.
were condensed into 10 generalized cell types, and the object was
randomly downsampled to include 25 cells from each cell type.

## Usage

``` r
triana_et_al_condensed
```

## Format

A Seurat object with 659 features and 250 cells in 2 assays. Included
assays:

- RNA:

  mRNA expression data for 462 genes used as markers for hematopoietic
  stem and progenitor (HSPC) cell types at varying stages of
  differentiation.

- AB:

  Surface protein expression data for 197 surface protein markers
  associated with HSPCs.

## Source

<https://figshare.com/articles/dataset/Expression_of_197_surface_markers_and_462_mRNAs_in_15281_cells_from_blood_and_bone_marrow_from_a_young_healthy_donor/13398065/2>
