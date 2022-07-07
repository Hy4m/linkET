#' @title Coerce to a Graph
#' @description Functions to coerce a object to graph if possible.
#' @param x any \code{R} object.
#' @param use_adjacency logical, if TRUE will build graph from adjacency matrix.
#' @param diag logical, if TRUE will keep the diagonal of adjacency matrix data.
#' @param simplify if TRUE, Simple graphs are graphs which do not contain loop
#' and multiple edges.
#' @param directed whether or not to create a directed graph.
#' @param ... other parameters.
#' @return  a graph object.
#' @importFrom igraph as.igraph
#' @importFrom tidygraph as_tbl_graph
#' @author Hou Yun
#' @rdname network
as.igraph.cor_md_tbl <- function(x,
                                 ...,
                                 use_adjacency = FALSE,
                                 diag = FALSE,
                                 simplify = TRUE,
                                 directed = FALSE) {
  rnm <- row_names(x)
  cnm <- col_names(x)
  params <- list(...)
  if (is.null(params)) {
    gparams <- list()
  } else {
    gparams <- params[names(params) != ""]
    params <- unname(params[names(params) == ""])
  }
  x <- do.call(dplyr::filter, c(list(.data = x), params))

  if (isFALSE(use_adjacency)) {
    if(isTRUE(simplify)) {
      if(identical(rnm, cnm) && isFALSE(directed)) {
        x <- extract_upper(x, diag)
      }
      nodes <- unique(c(x$.rownames, x$.colnames))
    } else {
      nodes <- unique(c(rnm, cnm))
    }
    igraph::graph_from_data_frame(x, directed = directed, vertices = nodes)
  } else {
    if(isTRUE(simplify)) {
      if(identical(rnm, cnm) && isFALSE(directed)) {
        x <- extract_upper(x, diag)
      }
    }

    if (isTRUE(directed)) {
      mode <- "directed"
    } else {
      if (isTRUE(simplify)) {
        mode <- "max"
      } else {
        mode <- "undirected"
      }
    }

    do.call(igraph::graph_from_adjacency_matrix,
            c(list(adjmatrix = adjacency_matrix(x),
                   mode = mode, diag = diag, weighted = TRUE), gparams))
  }
}

#' @rdname network
as.igraph.correlate <- function(x, ...) {
  as.igraph(as_md_tbl(x), ...)
}

#' @rdname network
as.igraph.mantel_tbl <- function(x, ...) {
  as.igraph(as_md_tbl(x), ...)
}

#' @rdname network
as.igraph.easycorrelation <- function(x, ...) {
  as.igraph(as_md_tbl(x), ...)
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

#' @noRd
adjacency_matrix <- function(x, ...) {
  UseMethod("adjacency_matrix")
}

#' @method adjacency_matrix cor_md_tbl
adjacency_matrix.cor_md_tbl <- function(x, ...) {
  if (isTRUE(attr(x, "grouped"))) {
    stop("Cannot convert a grouped md_tbl to a adjacency_matrix.", call. = FALSE)
  }
  rnm <- row_names(x)
  cnm <- col_names(x)
  x <- dplyr::filter(x, ...)
  df_to_matrix(x, "r", row_id = ".rownames", col_id = ".colnames",
               row_names = rnm, col_names = cnm, missing = 0)
}

#' @method adjacency_matrix default
adjacency_matrix.default <- function(x, ...) {
  adjacency_matrix(as_md_tbl(x, ...))
}

#' @noRd
df_to_matrix <- function(x,
                         value,
                         row_id = NULL,
                         col_id = NULL,
                         row_names = NULL,
                         col_names = NULL,
                         missing = NA) {
  row_id <- row_id %||% names(x)[1]
  col_id <- col_id %||% names(x)[2]
  rnm <- row_names %||% unique(x[[row_id]])
  cnm <- col_names %||% unique(x[[col_id]])
  ID <- paste(rep(rnm, length(cnm)), rep(cnm, each = length(rnm)), sep = "--")
  vv <- rep(missing, length(ID))
  ii <- match(paste(x[[row_id]], x[[col_id]], sep = "--"), ID)
  vv[ii] <- x[[value]]
  matrix(vv, nrow = length(rnm), ncol = length(cnm),
         dimnames = list(rnm, cnm))
}
