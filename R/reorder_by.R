#' @title Re-sorting Matrix Data
#' @description Resort matrix data by "hclust", "dendrogram", and so on.
#' @param x matrix data.
#' @param by_rows,by_cols method of reorder, default is "hclust".
#' @param dist_fun a function to calculute distance matrix.
#' @param order the index of the matrix to be reordered.
#' @param ... other parameters.
#' @return same object with `x`.
#' @author Hou Yun
#' @rdname reorder_by
#' @export
reorder_by <- function(x, ...) {
  UseMethod("reorder_by")
}

#' @rdname reorder_by
#' @export
reorder_by.matrix <- function(x,
                              by_rows = "hclust",
                              by_cols = "hclust",
                              dist_fun = dist_func(),
                              ...) {
  if (identical(by_rows, "none") && identical(by_cols, "none")) {
    return(x)
  }

  if (identical(by_rows, "none")) {
    by_rows <- seq_len(nrow(x))
  }
  if (identical(by_cols, "none")) {
    by_cols <- seq_len(ncol(x))
  }

  ## get row order
  if (identical(by_rows, "hclust")) {
    by_rows <- stats::hclust(dist_fun(x))
  }
  if (inherits(by_rows, "dist")) {
    by_rows <- hclust(by_rows, ...)
  }
  if (inherits(by_rows, "dendrogram")) {
    by_rows <- stats::as.hclust(by_rows)
  }

  if (inherits(by_rows, "numeric") || inherits(by_rows, "character")) {
    row_ord <- by_rows
  } else {
    row_ord <- get_order(by_rows)
  }

  ## get col order
  if (identical(by_cols, "hclust")) {
    by_cols <- stats::hclust(dist_fun(t(x)))
  }
  if (inherits(by_cols, "dist")) {
    by_cols <- hclust(by_cols, ...)
  }
  if (inherits(by_cols, "dendrogram")) {
    by_cols <- stats::as.hclust(by_cols)
  }

  if (inherits(by_cols, "numeric") || inherits(by_cols, "character")) {
    col_ord <- by_cols
  } else {
    col_ord <- get_order(by_cols)
  }
  x <- x[row_ord, col_ord, drop = FALSE]

  if (inherits(by_rows, "hclust") || inherits(by_rows, "ggtree")) {
    attr(x, "row_tree") <- by_rows
  }
  if (inherits(by_cols, "hclust") || inherits(by_cols, "ggtree")) {
    attr(x, "col_tree") <- by_cols
  }
  x
}

#' @rdname reorder_by
#' @export
reorder_by.data.frame <- function(x,
                                  by_rows = "hclust",
                                  by_cols = "hclust",
                                  dist_fun = dist_func(),
                                  ...) {
  x <- reorder_by(x = as.matrix(x),
                  by_rows = by_rows,
                  by_cols = by_cols,
                  dist_fun = dist_fun,
                  ...)
  as.data.frame(x)
}

#' @rdname reorder_by
#' @export
reorder_by.correlate <- function(x,
                                 by_rows = "hclust",
                                 by_cols = "hclust",
                                 dist_fun = NULL,
                                 ...) {
  r <- x$r
  is_symmet <- identical(rownames(r), colnames(r))

  if (identical(by_rows, "hclust")) {
    if (is_symmet) {
      if (is.null(dist_fun)) {
        by_rows <- stats::as.dist(1 - r)
      } else {
        by_rows <- dist_fun(r)
      }
    } else {
      dist_fun <- dist_fun %||% dist_func()
      by_rows <- dist_fun(r)
    }
  }

  if (identical(by_cols, "hclust")) {
    if (is_symmet) {
      if (is.null(dist_fun)) {
        by_cols <- stats::as.dist(1 - t(r))
      } else {
        by_cols <- dist_fun(t(r))
      }
    } else {
      dist_fun <- dist_fun %||% dist_func()
      by_cols <- dist_fun(t(r))
    }
  }

  r <- reorder_by(x = r,
                  by_rows = by_rows,
                  by_cols = by_cols,
                  dist_fun = dist_fun,
                  ...)
  rnm <- rownames(r)
  cnm <- colnames(r)
  for (ii in names(x)) {
    if (ii == "r") {
      x[[ii]] <- r
    } else {
      if (is.null(x[[ii]])) next
      x[[ii]] <- x[[ii]][rnm, cnm, drop = FALSE]
    }
  }
  attr(x, "row_tree") <- attr(r, "row_tree")
  attr(x, "col_tree") <- attr(r, "col_tree")
  x
}

#' @rdname reorder_by
#' @export
reorder_by.matrix_data <- function(x,
                                   by_rows = "hclust",
                                   by_cols = "hclust",
                                   dist_fun = dist_func(),
                                   order = 1,
                                   ...) {
  mat <- reorder_by(x = x[[order]],
                    by_rows = by_rows,
                    by_cols = by_cols,
                    dist_fun = dist_fun,
                    ...)
  rnm <- rownames(mat)
  cnm <- colnames(mat)
  for (ii in seq_along(x)) {
    if (ii == order) {
      x[[ii]] <- mat
    } else {
      if (is.null(x[[ii]])) next
      x[[ii]] <- x[[ii]][rnm, cnm, drop = FALSE]
    }
  }
  attr(x, "row_tree") <- attr(mat, "row_tree")
  attr(x, "col_tree") <- attr(mat, "col_tree")
  attr(x, "row_names") <- rnm
  attr(x, "col_names") <- cnm
  x
}

#' @rdname reorder_by
#' @export
reorder_by.md_tbl <- function(x,
                              by_rows = NULL,
                              by_cols = NULL,
                              ...) {
  if (inherits(by_rows, "dist")) {
    by_rows <- hclust(by_rows, ...)
  }
  if (inherits(by_rows, "dendrogram")) {
    by_rows <- stats::as.hclust(by_rows, ...)
  }
  if (inherits(by_rows, "md_tbl")) {
    by_rows <- row_names(by_rows)
  }
  if (!is.null(by_rows)) {
    if (inherits(by_rows, "hclust") || inherits(by_rows, "ggtree")) {
      attr(x, "row_tree") <- by_rows
    }
    row_ord <- get_order(by_rows, name = row_names(x))
    attr(x, "row_names") <- row_names(x)[row_ord]
  }

  if (inherits(by_cols, "dist")) {
    by_cols <- hclust(by_cols, ...)
  }
  if (inherits(by_cols, "dendrogram")) {
    by_cols <- stats::as.hclust(by_cols, ...)
  }
  if (inherits(by_cols, "md_tbl")) {
    by_cols <- col_names(by_cols)
  }
  if (!is.null(by_cols)) {
    if (inherits(by_cols, "hclust") || inherits(by_cols, "ggtree")) {
      attr(x, "col_tree") <- by_cols
    }
    col_ord <- get_order(by_cols, name = col_names(x))
    attr(x, "col_names") <- col_names(x)[col_ord]
  }
  x
}
