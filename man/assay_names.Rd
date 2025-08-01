% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/methods-assay_names.R
\name{assay_names}
\alias{assay_names}
\alias{assay_names.Seurat}
\alias{assay_names.SingleCellExperiment}
\alias{assay_names.AnnDataR6}
\alias{assay_names.md._core.mudata.MuData}
\title{Get names of assays/experiments in object}
\usage{
assay_names(object, ...)

\method{assay_names}{Seurat}(object)

\method{assay_names}{SingleCellExperiment}(object)

\method{assay_names}{AnnDataR6}(object)

\method{assay_names}{md._core.mudata.MuData}(object)
}
\arguments{
\item{object}{a single-cell object. Currently, Seurat and
SingleCellExperiment objects are supported.}

\item{...}{Currently unused.}
}
\description{
Returns the names of all assays in a single-cell object. For Seurat objects,
returns assay names, and for SingleCellExperiment objects "experiments", the
equivalent of assays, are returned.
}
\section{Methods (by class)}{
\itemize{
\item \code{assay_names(Seurat)}: Seurat objects

\item \code{assay_names(SingleCellExperiment)}: SingleCellExperiment objects

\item \code{assay_names(AnnDataR6)}: Anndata objects

Anndata objects do not have an exclusive structure for modalities/assays.
Additional modalities are stored in obsm, but this slot is not specific to
modalities. To use assay_names with Anndata objects, a vector of assays
(python list) must be stored in \code{object.uns["scExploreR_assays"]}. This method
will return an error if this data is not present in the object.

\item \code{assay_names(md._core.mudata.MuData)}: MuData objects

}}
\keyword{internal}
