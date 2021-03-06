% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/network.R
\name{as.igraph.cor_md_tbl}
\alias{as.igraph.cor_md_tbl}
\alias{as.igraph.correlate}
\alias{as.igraph.mantel_tbl}
\alias{as.igraph.easycorrelation}
\alias{as.igraph.rcorr}
\alias{as.igraph.corr.test}
\alias{as_tbl_graph.cor_md_tbl}
\alias{as_tbl_graph.correlate}
\alias{as_tbl_graph.mantel_tbl}
\alias{as_tbl_graph.easycorrelation}
\alias{as_tbl_graph.rcorr}
\alias{as_tbl_graph.corr.test}
\title{Coerce to a Graph}
\usage{
\method{as.igraph}{cor_md_tbl}(
  x,
  ...,
  use_adjacency = FALSE,
  diag = FALSE,
  simplify = TRUE,
  directed = FALSE
)

\method{as.igraph}{correlate}(x, ...)

\method{as.igraph}{mantel_tbl}(x, ...)

\method{as.igraph}{easycorrelation}(x, ...)

\method{as.igraph}{rcorr}(x, ...)

\method{as.igraph}{corr.test}(x, ...)

\method{as_tbl_graph}{cor_md_tbl}(x, ...)

\method{as_tbl_graph}{correlate}(x, ...)

\method{as_tbl_graph}{mantel_tbl}(x, ...)

\method{as_tbl_graph}{easycorrelation}(x, ...)

\method{as_tbl_graph}{rcorr}(x, ...)

\method{as_tbl_graph}{corr.test}(x, ...)
}
\arguments{
\item{x}{any \code{R} object.}

\item{...}{other parameters.}

\item{use_adjacency}{logical, if TRUE will build graph from adjacency matrix.}

\item{diag}{logical, if TRUE will keep the diagonal of adjacency matrix data.}

\item{simplify}{if TRUE, Simple graphs are graphs which do not contain loop
and multiple edges.}

\item{directed}{whether or not to create a directed graph.}
}
\value{
a graph object.
}
\description{
Functions to coerce a object to graph if possible.
}
\author{
Hou Yun
}
