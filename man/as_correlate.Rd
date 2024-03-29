% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/as-correlate.R
\name{as_correlate}
\alias{as_correlate}
\alias{as_correlate.rcorr}
\alias{as_correlate.corr.test}
\alias{as_correlate.matrix}
\alias{as_correlate.data.frame}
\alias{as_correlate.easycorrelation}
\title{Coerce a object to correlate}
\usage{
as_correlate(x, ...)

\method{as_correlate}{rcorr}(x, ...)

\method{as_correlate}{corr.test}(x, ...)

\method{as_correlate}{matrix}(x, is_corr = NULL, p = NULL, ...)

\method{as_correlate}{data.frame}(x, ...)

\method{as_correlate}{easycorrelation}(x, ...)
}
\arguments{
\item{x}{any \code{R} object can convert to correlate.}

\item{...}{others parameters.}

\item{is_corr}{if TRUE (default), the data will be regarded as the correlation
coefficient matrix.}

\item{p}{p value matrix.}
}
\value{
a correlate object.
}
\description{
Coerce a object to correlate.
}
