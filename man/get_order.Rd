% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/get-order.R
\name{get_order}
\alias{get_order}
\alias{get_order.numeric}
\alias{get_order.character}
\alias{get_order.hclust}
\alias{get_order.dendrogram}
\alias{get_order.dist}
\alias{get_order.ggtree}
\title{Extract order}
\usage{
get_order(x, ...)

\method{get_order}{numeric}(x, ...)

\method{get_order}{character}(x, name = NULL, ...)

\method{get_order}{hclust}(x, ...)

\method{get_order}{dendrogram}(x, ...)

\method{get_order}{dist}(x, ...)

\method{get_order}{ggtree}(x, name = NULL, ...)
}
\arguments{
\item{x}{any \code{R} object.}

\item{...}{ignore.}

\item{name}{old name.}
}
\value{
a numeric vector.
}
\description{
Extract order
}
\examples{
hc <- hclust(dist(mtcars))
get_order(hc)
}
\author{
Hou Yun
}
