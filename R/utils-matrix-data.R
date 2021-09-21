#' Helper function for matrix_data object
#' @title Helper function
#' @param md a matrix_data object.
#' @param value a valid value for dimension names.
#' @param diag logical, if TRUE (default) will keep the diagonal of matrix data.
#' @rdname Helper_function
#' @author Hou Yun
#' @export
row_names <- function(md)
{
  if (is_matrix_data(md)) {
    rownames(md[[1]])
  } else if (is_md_tbl(md)) {
    attr(md, "row_names")
  } else {
    stop("Unknown data type.", call. = FALSE)
  }
}

#' @rdname Helper_function
#' @export
`row_names<-` <- function(md, value)
{
  if (is_matrix_data(md)) {
    md <- lapply(md, function(.md) {
      rownames(.md) <- value
      .md
    })
    structure(.Data = md, class = "matrix_data")
  } else if (is_md_tbl(md)) {
    attr(md, "row_names") <- value
    md
  } else {
    stop("Unknown data type.", call. = FALSE)
  }
}

#' @rdname Helper_function
#' @export
col_names <- function(md)
{
  if (is_matrix_data(md)) {
    colnames(md[[1]])
  } else if (is_md_tbl(md)) {
    attr(md, "col_names")
  } else {
    stop("Unknown data type.", call. = FALSE)
  }
}

#' @rdname Helper_function
#' @export
`col_names<-` <- function(md, value)
{
  if (is_matrix_data(md)) {
    md <- lapply(md, function(.md) {
      colnames(.md) <- value
      .md
    })
    structure(.Data = md, class = "matrix_data")
  } else if (is_md_tbl(md)) {
    attr(md, "col_names") <- value
    md
  } else {
    stop("Unknown data type.", call. = FALSE)
  }
}

#' @rdname Helper_function
#' @export
nrows <- function(md) {
  if (is_matrix_data(md)) {
    nrow(md[[1]])
  } else if (is_md_tbl(md)) {
    length(attr(md, "row_names"))
  } else {
    stop("Unknown data type.", call. = FALSE)
  }
}

#' @rdname Helper_function
#' @export
ncols <- function(md) {
  if (is_matrix_data(md)) {
    ncol(md[[1]])
  } else if (is_md_tbl(md)) {
    length(attr(md, "col_names"))
  } else {
    stop("Unknown data type.", call. = FALSE)
  }
}

#' @rdname Helper_function
#' @export
is_matrix_data <- function(md)
{
  inherits(md, "matrix_data")
}

#' @rdname Helper_function
#' @export
is_md_tbl <- function(md)
{
  inherits(md, "md_tbl")
}

#' @rdname Helper_function
#' @export
extract_upper <- function(md, diag = TRUE)
{
  stopifnot(is_matrix_data(md) || is_md_tbl(md))
  row_names <- row_names(md)
  col_names <- col_names(md)
  if(!identical(row_names, col_names)) {
    stop("`extract_upper()` just support for symmetric matrices.", call. = FALSE)
  }
  if(is_matrix_data(md)) {
    attr(md, "type") <- "upper"
    attr(md, "diag") <- diag
  } else {
    n <- nrows(md)
    x <- as.integer(factor(md$.rownames, levels = rev(row_names)))
    y <- as.integer(factor(md$.colnames, levels = col_names))
    if(isTRUE(diag)) {
      row_id <- ((x + y) >= (n + 1))
    } else {
      row_id <- ((x + y) > (n + 1))
    }
    md <- dplyr::filter(md, row_id)
  }
  attr(md, "type") <- "upper"
  attr(md, "diag") <- diag
  md
}

#' @rdname Helper_function
#' @export
extract_lower <- function(md, diag = TRUE)
{
  stopifnot(is_matrix_data(md) || is_md_tbl(md))
  row_names <- row_names(md)
  col_names <- col_names(md)
  if(!identical(row_names, col_names)) {
    stop("`extract_lower()` just support for symmetric matrices.", call. = FALSE)
  }
  if(is_matrix_data(md)) {
    attr(md, "type") <- "lower"
    attr(md, "diag") <- diag
  } else {
    n <- nrows(md)
    x <- as.integer(factor(md$.rownames, levels = rev(row_names)))
    y <- as.integer(factor(md$.colnames, levels = col_names))
    if(isTRUE(diag)) {
      row_id <- ((x + y) <= (n + 1))
    } else {
      row_id <- ((x + y) < (n + 1))
    }
    md <- dplyr::filter(md, row_id)
  }
  attr(md, "type") <- "lower"
  attr(md, "diag") <- diag
  md
}

#' @rdname Helper_function
#' @export
extract_diag <- function(md)
{
  stopifnot(is_md_tbl(md))
  row_names <- row_names(md)
  col_names <- col_names(md)
  if(!identical(row_names, col_names)) {
    stop("`extract_diag()` just support for symmetric matrices.", call. = FALSE)
  }
  n <- nrows(md)
  x <- as.integer(factor(md$.rownames, levels = rev(row_names)))
  y <- as.integer(factor(md$.colnames, levels = col_names))
  row_id <- ((x + y) == (n + 1))
  md <- dplyr::filter(md, row_id)
  md
}

#' @rdname Helper_function
#' @export
trim_diag <- function(md)
{
  stopifnot(is_matrix_data(md) || is_md_tbl(md))
  row_names <- row_names(md)
  col_names <- col_names(md)
  if(!identical(row_names, col_names)) {
    stop("`trim_diag()` just support for symmetric matrices.", call. = FALSE)
  }
  if(is_matrix_data(md)) {
    attr(md, "diag") <- FALSE
  } else {
    n <- nrows(md)
    x <- as.integer(factor(md$.rownames, levels = rev(row_names)))
    y <- as.integer(factor(md$.colnames, levels = col_names))
    row_id <- ((x + y) != (n + 1))
    md <- dplyr::filter(md, row_id)
  }
  md
}
