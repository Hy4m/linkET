% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/geom-corr.R
\docType{data}
\name{geom_corr}
\alias{geom_corr}
\alias{GeomCorr}
\title{Correlation Layer}
\usage{
geom_corr(
  mapping = NULL,
  data = NULL,
  stat = "identity",
  position = "lc",
  ...,
  method = "pearson",
  contain_all = TRUE,
  digits = 2,
  nsmall = 2,
  nudge_x = NULL,
  nudge_y = NULL,
  na.rm = FALSE,
  show.legend = NA,
  inherit.aes = TRUE
)
}
\arguments{
\item{mapping}{Set of aesthetic mappings created by \code{\link[ggplot2:aes]{aes()}}. If specified and
\code{inherit.aes = TRUE} (the default), it is combined with the default mapping
at the top level of the plot. You must supply \code{mapping} if there is no plot
mapping.}

\item{data}{The data to be displayed in this layer. There are three
options:

If \code{NULL}, the default, the data is inherited from the plot
data as specified in the call to \code{\link[ggplot2:ggplot]{ggplot()}}.

A \code{data.frame}, or other object, will override the plot
data. All objects will be fortified to produce a data frame. See
\code{\link[ggplot2:fortify]{fortify()}} for which variables will be created.

A \code{function} will be called with a single argument,
the plot data. The return value must be a \code{data.frame}, and
will be used as the layer data. A \code{function} can be created
from a \code{formula} (e.g. \code{~ head(.x, 10)}).}

\item{stat}{The statistical transformation to use on the data for this
layer, either as a \code{ggproto} \code{Geom} subclass or as a string naming the
stat stripped of the \code{stat_} prefix (e.g. \code{"count"} rather than
\code{"stat_count"})}

\item{position}{position of correlation marker.}

\item{...}{other parameters passed to layer function.}

\item{method}{method of correlation.}

\item{contain_all}{logical, if FALSE will contain each group marker only.}

\item{digits, nsmall}{a positive integer to format correlation.}

\item{nudge_x, nudge_y}{horizontal and vertical adjustment to nudge labels by.}

\item{na.rm}{if FALSE, the default, missing values are removed with a warning,
and if TRUE, missing values are silently removed.}

\item{show.legend}{logical. Should this layer be included in the legends?
\code{NA}, the default, includes if any aesthetics are mapped.
\code{FALSE} never includes, and \code{TRUE} always includes.
It can also be a named logical vector to finely select the aesthetics to
display.}

\item{inherit.aes}{If \code{FALSE}, overrides the default aesthetics,
rather than combining with them. This is most useful for helper functions
that define both data and aesthetics and shouldn't inherit behaviour from
the default plot specification, e.g. \code{\link[ggplot2:borders]{borders()}}.}
}
\value{
a layer object.
}
\description{
This function can be used to add corrlation marker on a pairs plot.
}
\author{
Hou Yun
}
\keyword{datasets}
