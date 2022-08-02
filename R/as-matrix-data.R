## TODO: add corr attributes instead of sub-class
#' Coerce to a matrix_data
#' @title as_matrix_data
#' @param x any \code{R} object.
#' @param name variable name.
#' @param group a character vector used to split the matrix data.
#' @param extra_mat a list of matrix.
#' @param ... ignore.
#' @return a matrix_data object.
#' @rdname as_matrix_data
#' @author Hou Yun
#' @export
as_matrix_data <- function(x, ...)
{
  UseMethod("as_matrix_data")
}

#' @rdname as_matrix_data
#' @method as_matrix_data matrix
as_matrix_data.matrix <- function(x,
                                  name = NULL,
                                  group = NULL,
                                  ...)
{
  if (is.null(name)) {
    name <- deparse(substitute(x))
  }

  if (!is.null(group)) {
    row_tree <- col_tree <- NULL
  } else {
    row_tree <- attr(x, "row_tree")
    col_tree <- attr(x, "col_tree")
  }

  x <- matrix_data(rlang::set_names(list(x), name),
                   group = group, ...)
  attr(x, "row_tree") <- row_tree
  attr(x, "col_tree") <- col_tree
  x
}

#' @param include one of "numeric" (default), "character" or "factor".
#' @rdname as_matrix_data
#' @method as_matrix_data data.frame
as_matrix_data.data.frame <- function(x,
                                      name = NULL,
                                      include = "numeric",
                                      group = NULL,
                                      ...)
{
  if (is.null(name)) {
    name <- deparse(substitute(x))
  }

  include <- match.arg(include, c("numeric", "character", "factor"))
  Fun <- paste0("is.", include)
  id <- vapply(x, Fun, logical(1))

  x <- rlang::set_names(list(x[id]), name)
  matrix_data(x, group = group, ...)
}

#' @rdname as_matrix_data
#' @method as_matrix_data correlate
as_matrix_data.correlate <- function(x,
                                     extra_mat = list(),
                                     ...) {
  row_tree <- attr(x, "row_tree")
  col_tree <- attr(x, "col_tree")
  id <- vapply(x, is.null, logical(1))
  x <- x[!id]
  if(length(extra_mat) > 0) {
    if(any(names(extra_mat) %in% names(x))) {
      stop("`extra_mat` contains invalid name.", call. = FALSE)
    }
    x <- c(x, extra_mat)
  }
  x <- matrix_data(x, group = NULL, ...)
  class(x) <- c("cor_matrix_data", class(x))
  attr(x, "row_tree") <- row_tree
  attr(x, "col_tree") <- col_tree
  x
}

#' @rdname as_matrix_data
#' @method as_matrix_data grouped_correlate
as_matrix_data.grouped_correlate <- function(x,
                                             extra_mat = list(),
                                             ...) {
  nm <- names(x)
  if (length(extra_mat) > 0) {
    if (all(names(extra_mat) %in% nm)) {
      extra_mat <- extra_mat[nm]
    } else {
      extra_mat <- rep_len(list(extra_mat), length(x))
      names(extra_mat) <- nm
    }
  }
  x <- lapply(nm, function(.nm) {
    as_matrix_data(x = x[[.nm]], extra_mat = extra_mat[[.nm]], ...)
  })
  names(x) <- nm
  class(x) <- "grouped_matrix_data"
  x
}

#' @rdname as_matrix_data
#' @method as_matrix_data rcorr
as_matrix_data.rcorr <- function(x,
                                 extra_mat = list(),
                                 ...) {
  x <- as_correlate(x)
  as_matrix_data(x, extra_mat = extra_mat, ...)
}

#' @rdname as_matrix_data
#' @method as_matrix_data corr.test
as_matrix_data.corr.test <- function(x,
                                     extra_mat = list(),
                                     ...) {
  x <- as_correlate(x)
  as_matrix_data(x, extra_mat = extra_mat, ...)
}

#' @rdname as_matrix_data
#' @method as_matrix_data default
as_matrix_data.default <- function(x, ...)
{
  stop("Unknown data type.", call. = FALSE)
}
