#' @title Reorder Matrix Data
#' @description reorder a \code{matrix_data} or \code{md_tbl} object.
#' @param x a \code{matrix_data} or \code{md_tbl} object.
#' @param by any \code{R} object.
#' @param ... when \code{x} is a \code{matrix_data} object and \code{by} is NULL,
#' these are passed to the \code{gdist()} function.
#' @return a modified \code{matrix_data} or \code{md_tbl} object.
#' @rdname reorder
#' @author Hou Yun
#' @export
#' @examples
#' x <- as_matrix_data(mtcars)
#' reorder_rows(x)

reorder_rows <- function(x, by = NULL, ...) {
  stopifnot(is_matrix_data(x) || is_md_tbl(x))

  if(is_md_tbl(x) && is.null(by)) {
    return(x)
  }

  if(is_matrix_data(x)) {
    if(is.null(by)) {
      by <- gdist(x[[1]], ...)
    }
  }

  clss <- class(by)[1]
  ord <- switch (clss,
                 character = get_order(by, row_names(x)),
                 get_order(by))
  if(is_matrix_data(x)) {
    for(i in seq_along(x)) {
      x[[i]] <- x[[i]][ord, , drop = FALSE]
      attr(x, "row_names") <- row_names(x)[ord]
    }
  } else {
    row_names(x) <- row_names(x)[ord]
  }

  if(!identical(row_names(x), col_names(x))) {
    if(attr(x, "type") != "full" || isFALSE(attr(x, "diag"))) {
      warning("After reordering, the data becomes asymmetric matrix.",
              call. = FALSE)
      attr(x, "type") <- "full"
      attr(x, "diag") <- TRUE
    }
  }
  x
}

#' @rdname reorder
#' @export
reorder_cols <- function(x, by = NULL, ...) {
  stopifnot(is_matrix_data(x) || is_md_tbl(x))

  if(is_md_tbl(x) && is.null(by)) {
    return(x)
  }

  if(is_matrix_data(x)) {
    if(is.null(by)) {
      by <- gdist(t(x[[1]]), ...)
    }
  }

  clss <- class(by)[1]
  ord <- switch (clss,
                 character = get_order(by, col_names(x)),
                 get_order(by))
  if(is_matrix_data(x)) {
    for(i in seq_along(x)) {
      x[[i]] <- x[[i]][, ord, drop = FALSE]
      attr(x, "col_names") <- col_names(x)[ord]
    }
  } else {
    col_names(x) <- col_names(x)[ord]
  }

  if(!identical(row_names(x), col_names(x))) {
    if(attr(x, "type") != "full" || isFALSE(attr(x, "diag"))) {
      warning("After reordering, the data becomes asymmetric matrix.",
              call. = FALSE)
      attr(x, "type") <- "full"
      attr(x, "diag") <- TRUE
    }
  }
  x
}
