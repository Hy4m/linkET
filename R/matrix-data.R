#' Matrix data
#' @title Matrix data
#' @param x a list of matrix-like data.
#' @param type character, "full" (default), "upper" or "lower", display full
#' matrix, lower triangular or upper triangular matrix.
#' @param diag logical, if TRUE (default) will keep the diagonal of matrix data.
#' @param row_names,col_names the name of rows and columns.
#' @param ... passing to \code{\link{make_cluster}}.
#' @return a object of matrix_data
#' @rdname matrix_data
#' @author Hou Yun
#' @export
matrix_data <- function(x,
                        type = "full",
                        diag = TRUE,
                        row_names = NULL,
                        col_names = NULL,
                        ...)
{
  stopifnot(is.list(x))
  type <- match.arg(type, c("full", "upper", "lower"))
  nm <- names(x)
  if (length(x) != length(unique(nm))) {
    stop("matrix_data error: each element of `x` must have a unique name.", call. = FALSE)
  }
  mat <- lapply(x, as.matrix)
  mat <- check_matrix(mat, row_names, col_names)

  first <- mat[[1]]
  row_names <- rownames(first)
  col_names <- colnames(first)
  if(!identical(row_names, col_names)) {
    if(type != "full") {
      warning("'type = ", type, "' just support for symmetric matrices.")
      type <- "full"
    }
    if(!isTRUE(diag)) {
      diag <- TRUE
    }
  }
  md <- structure(.Data = mat,
                  type = type,
                  diag = diag,
                  class = "matrix_data")
  make_cluster(md, ...)
}

#' @method print matrix_data
#' @export
print.matrix_data <- function(x, ...) {
  cat("A matrix data object:\n")
  cat("Number: ", length(x), "\n")
  cat("Names: ", glue::glue_collapse(names(x), ", ", 60), "\n")
  cat("Dimensions: ", paste0(nrows(x), " rows, ", ncols(x), " columns\n"))
  cat("Row names: ", glue::glue_collapse(row_names(x), ", ", 60), "\n")
  cat("Column names: ", glue::glue_collapse(col_names(x), ", ", 60), "\n")
}

check_matrix <- function(mat,
                         row_names = NULL,
                         col_names = NULL)
{
  n <- length(mat)
  if (n == 0L || (nrow(mat[[1]]) == 0L) || (ncol(mat[[1]]) == 0L)) {
    stop("The input data is empty.", call. = FALSE)
  }

  first <- mat[[1]]
  if (length(unique(unlist(lapply(mat, nrow)))) != 1L) {
    stop("The rows of input matrix should be same.", call. = FALSE)
  }
  if (length(unique(unlist(lapply(mat, ncol)))) != 1L) {
    stop("The rows of input matrix should be same.", call. = FALSE)
  }

  row_names <- row_names %||% rownames(first) %||% paste0("Row", seq_len(nrow(first)))
  col_names <- col_names %||% colnames(first) %||% paste0("Col", seq_len(ncol(first)))
  if(length(unique(row_names)) != nrow(first)) {
    stop("`row_names` contains duplicate values.", call. = FALSE)
  }
  if(length(unique(col_names)) != ncol(first)) {
    stop("`col_names` contains duplicate values.", call. = FALSE)
  }

  mat <- lapply(mat, function(x) {
    if (!is.null(row_names)) {
      rownames(x) <- row_names
    } else {
      if (is.null(rownames(x))) {
        rownames(x) <- rownames(first) %||% paste0("Row", seq_len(nrow(first)))
      }
    }
    if (!is.null(col_names)) {
      colnames(x) <- col_names
    } else {
      if (is.null(colnames(x))) {
        colnames(x) <- colnames(first) %||% paste0("Col", seq_len(ncol(first)))
      }
    }
    x
  })

  l_row_names <- lapply(mat, rownames)
  l_col_names <- lapply(mat, colnames)
  if (!all(vapply(l_row_names, identical, logical(1), y = l_row_names[[1]]))) {
    stop("Invalid rownames.", call. = FALSE)
  }

  if (!all(vapply(l_col_names, identical, logical(1), y = l_col_names[[1]]))) {
    stop("Invalid colnames.", call. = FALSE)
  }

  mat
}

