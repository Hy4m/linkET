#' @title Coerce to a Graph
#' @description Functions to coerce a object to graph if possible.
#' @param x any \code{R} object.
#' @param simplify if TRUE, Simple graphs are graphs which do not contain loop
#' and multiple edges.
#' @param directed whether or not to create a directed graph.
#' @param ... other parameters.
#' @return  a graph object.
#' @importFrom igraph simplify
#' @importFrom igraph graph_from_data_frame
#' @importFrom igraph as.igraph
#' @importFrom tidygraph as_tbl_graph
#' @author Hou Yun
#' @rdname network
as.igraph.cor_md_tbl <- function(x,
                                 ...,
                                 simplify = TRUE,
                                 directed = FALSE) {
  x <- dplyr::filter(x, ...)

  if(isFALSE(simplify)) {
    nodes <- unique(row_names(x), col_names(x))
  } else {
    nodes <- unique(c(x$.rownames, x$.colnames))
  }
  g <- graph_from_data_frame(x, directed = directed, vertices = nodes)

  if(isTRUE(simplify)) {
    g <- simplify(g)
  }
  g
}

#' @rdname network
as.igraph.correlate <- function(x, ...) {
  as.igraph.cor_md_tbl(as_md_tbl(x), ...)
}

#' @rdname network
as.igraph.mantel_tbl <- function(x, ...) {
  as.igraph.cor_md_tbl(as_md_tbl(x), ...)
}

#' @rdname network
as.igraph.pro_tbl <- function(x, ...) {
  as.igraph.cor_md_tbl(as_md_tbl(x), ...)
}

#' @rdname network
as.igraph.easycorrelation <- function(x, ...) {
  as.igraph.cor_md_tbl(as_md_tbl(x), ...)
}

#' @rdname network
as.igraph.rcorr <- function(x, ...) {
  as.igraph.cor_md_tbl(as_md_tbl(x), ...)
}

#' @rdname network
as.igraph.corr.test <- function(x, ...) {
  as.igraph.cor_md_tbl(as_md_tbl(x), ...)
}

#' @rdname network
as_tbl_graph.cor_md_tbl <- function(x, ...) {
  as_tbl_graph(as.igraph(x, ...))
}

#' @rdname network
as_tbl_graph.correlate <- function(x, ...) {
  as_tbl_graph(as.igraph(x, ...))
}

#' @rdname network
as_tbl_graph.mantel_tbl <- function(x, ...) {
  as_tbl_graph(as.igraph(x, ...))
}

#' @rdname network
as_tbl_graph.pro_tbl <- function(x, ...) {
  as_tbl_graph(as.igraph(x, ...))
}

#' @rdname network
as_tbl_graph.easycorrelation <- function(x, ...) {
  as_tbl_graph(as.igraph(x, ...))
}

#' @rdname network
as_tbl_graph.rcorr <- function(x, ...) {
  as_tbl_graph(as.igraph(x, ...))
}

#' @rdname network
as_tbl_graph.corr.test <- function(x, ...) {
  as_tbl_graph(as.igraph(x, ...))
}
