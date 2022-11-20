## ## Notes: all functions about network will be moved to netET packages
## #' @title Coerce to a Graph
## #' @description Functions to coerce a object to graph if possible.
## #' @param x any \code{R} object.
## #' @param from_adjacency logical, if TRUE will build graph from adjacency matrix.
## #' @param diag logical, if TRUE will keep the diagonal of adjacency matrix data.
## #' @param simplify if TRUE, Simple graphs are graphs which do not contain loop
## #' and multiple edges.
## #' @param directed whether or not to create a directed graph.
## #' @param ... other parameters.
## #' @return  a graph object.
## #' @importFrom igraph as.igraph
## #' @importFrom tidygraph as_tbl_graph
## #' @author Hou Yun
## #' @rdname network
## as.igraph.cor_md_tbl <- function(x,
##                                  ...,
##                                  from_adjacency = FALSE,
##                                  diag = FALSE,
##                                  simplify = TRUE,
##                                  directed = FALSE) {
##   rnm <- row_names(x)
##   cnm <- col_names(x)
##   x <- dplyr::filter(x, ...)
##   if (isTRUE(simplify)) {
##     x <- trim_duplicate(x, diag = !diag)
##     nodes <- unique(c(x$.rownames, x$.colnames))
##   } else {
##     nodes <- unique(c(rnm, cnm))
##   }
##
##   if (isFALSE(from_adjacency)) {
##     igraph::graph_from_data_frame(x, directed = directed, vertices = nodes)
##   } else {
##     if (isTRUE(directed)) {
##       mode <- "directed"
##     } else {
##       if (isTRUE(simplify)) {
##         mode <- "max"
##       } else {
##         mode <- "undirected"
##       }
##     }
##     igraph::graph_from_adjacency_matrix(adjmatrix = adjacency_matrix(x),
##                                         mode = mode,
##                                         weighted = TRUE,
##                                         diag = diag)
##   }
## }
##
## #' @rdname network
## as.igraph.correlate <- function(x,
##                                 ...,
##                                 from_adjacency = FALSE,
##                                 diag = FALSE,
##                                 simplify = TRUE,
##                                 directed = FALSE) {
##   if (isTRUE(from_adjacency)) {
##     if (isTRUE(directed)) {
##       mode <- "directed"
##     } else {
##       if (isTRUE(simplify)) {
##         mode <- "max"
##       } else {
##         mode <- "undirected"
##       }
##     }
##     igraph::graph_from_adjacency_matrix(adjmatrix = adjacency_matrix(x, ...),
##                                         mode = mode,
##                                         weighted = TRUE,
##                                         diag = diag)
##   } else {
##     as.igraph(x = as_md_tbl(x),
##               ...,
##               from_adjacency = FALSE,
##               diag = diag,
##               simplify = simplify,
##               directed = directed)
##   }
## }
##
## #' @rdname network
## as.igraph.mantel_tbl <- function(x, ...) {
##   as.igraph(as_md_tbl(x), ...)
## }
##
## #' @rdname network
## as.igraph.easycorrelation <- function(x, ...) {
##   as.igraph(as_correlate(x), ...)
## }
##
## #' @rdname network
## as.igraph.rcorr <- function(x, ...) {
##   as.igraph(as_correlate(x), ...)
## }
##
## #' @rdname network
## as.igraph.corr.test <- function(x, ...) {
##   as.igraph(as_correlate(x), ...)
## }
##
## #' @rdname network
## as_tbl_graph.cor_md_tbl <- function(x, ...) {
##   as_tbl_graph(as.igraph(x, ...))
## }
##
## #' @rdname network
## as_tbl_graph.correlate <- function(x, ...) {
##   as_tbl_graph(as.igraph(x, ...))
## }
##
## #' @rdname network
## as_tbl_graph.mantel_tbl <- function(x, ...) {
##   as_tbl_graph(as.igraph(x, ...))
## }
##
## #' @rdname network
## as_tbl_graph.easycorrelation <- function(x, ...) {
##   as_tbl_graph(as.igraph(x, ...))
## }
##
## #' @rdname network
## as_tbl_graph.rcorr <- function(x, ...) {
##   as_tbl_graph(as.igraph(x, ...))
## }
##
## #' @rdname network
## as_tbl_graph.corr.test <- function(x, ...) {
##   as_tbl_graph(as.igraph(x, ...))
## }
##
## #' @title Generate adjacency matrix
## #' @description Can be used to convert a correlation object to adjacency matrix.
## #' @param x a correlation object (\code{correlate}, \code{md_tbl}, and so on).
## #' @param ... expressions used to subset data.
## #' @param weighted if TRUE (default) will return filtered correlation
## #' coefficient matrix, and if (FALSE) will return adjacency matrix.
## #' @param type one of "full", "upper" or "lower", used to extract upper or lower
## #' triangular part of a adjacency matrix.
## #' @param diag if FALSE (default),  self-edges will be removed.
## #' @return a matrix object.
## #' @author Hou Yun
## #' @rdname adjacency_matrix
## #' @export
## adjacency_matrix <- function(x,
##                              ...,
##                              weighted = TRUE,
##                              type = "full",
##                              diag = FALSE) {
##   type <- match.arg(type, c("full", "upper", "lower"))
##   if (!inherits(x, "cor_md_tbl") && !inherits(x, "correlate")) {
##     clss <- class(x)[1]
##     x <- as_md_tbl(x)
##     if (!inherits(x, "cor_md_tbl")) {
##       stop("Cannot convert a", clss, "to adjacency matrix.", call. = FALSE)
##     }
##   }
##   if (inherits(x, "cor_md_tbl")) {
##     if (isTRUE(attr(x, "grouped"))) {
##       stop("Cannot convert a grouped md_tbl to a adjacency_matrix.", call. = FALSE)
##     }
##     rnm <- row_names(x)
##     cnm <- col_names(x)
##     if (isFALSE(diag) && identical(rnm, cnm)) {
##       x <- trim_diag(x)
##     }
##     if (!identical(type, "full") && identical(rnm, cnm)) {
##       x <- do.call(paste0("extract_", type), list(md = x))
##     }
##     x <- dplyr::filter(x, ...)
##     out <- df_to_matrix(x, "r", row_id = ".rownames", col_id = ".colnames",
##                         row_names = rnm, col_names = cnm, missing = NA)
##   } else {
##     out <- x$r
##     params <- rlang::enquos(...)
##     if (length(params) >= 1) {
##       for (ii in seq_along(params)) {
##         if (ii == 1) {
##           id <- rlang::eval_tidy(params[[ii]], x)
##         } else {
##           id <- id & rlang::eval_tidy(params[[ii]], x)
##         }
##       }
##       out[!id] <- NA
##     }
##
##     if (isFALSE(diag) && identical(rownames(out), colnames(out))) {
##       diag(out) <- NA
##     }
##     if (!identical(type, "full") && identical(rownames(out), colnames(out))) {
##       .f <- switch (type,
##         "upper" = "lower.tri",
##         "lower" = "upper.tri"
##       )
##       out[do.call(.f, list(x = out))] <- NA
##     }
##   }
##
##   if (isTRUE(weighted)) {
##     out[is.na(out)] <- 0
##   } else {
##     out[!is.na(out)] <- 1
##     out[is.na(out)] <- 0
##   }
##
##   out
## }
