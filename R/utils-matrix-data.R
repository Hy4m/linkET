#' Helper function for matrix_data object
#' @title Helper function
#' @param md a matrix_data object.
#' @param value a valid value for dimension names.
#' @param type character, "full" (default), "upper", "lower" or "diag".
#' @param diag logical, if TRUE (default) will keep the diagonal of matrix data.
#' @param ... other parameters.
#' @rdname Helper_function
#' @author Hou Yun
#' @export
row_names <- function(md)
{
  if (is_matrix_data(md)) {
    rownames(md[[1]])
  } else if (is_grouped_matrix_data(md)){
    unique(unlist(lapply(md, attr, "row_names")))
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
  } else if (is_grouped_matrix_data(md)) {
    md <- lapply(md, function(.md) {
      e <- lapply(.md, function(e) {
        rownames(e) <- value
        e
      })
      class(e) <- "matrix_data"
      e
    })
    structure(.Data = md, class = "grouped_matrix_data")
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
  } else if (is_grouped_matrix_data(md)){
    unique(unlist(lapply(md, attr, "col_names")))
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
  } else if (is_grouped_matrix_data(md)) {
    md <- lapply(md, function(.md) {
      e <- lapply(.md, function(e) {
        colnames(e) <- value
        e
      })
      class(e) <- "matrix_data"
      e
    })
    structure(.Data = md, class = "grouped_matrix_data")
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
  } else if (is_grouped_matrix_data(md)) {
    max(vapply(md, nrows, numeric(1)))
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
  } else if (is_grouped_matrix_data(md)) {
    max(vapply(md, ncols, numeric(1)))
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
is_grouped_matrix_data <- function(md)
{
  inherits(md, "grouped_matrix_data")
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
  stopifnot(is_matrix_data(md) || is_grouped_matrix_data(md) || is_md_tbl(md))
  row_names <- row_names(md)
  col_names <- col_names(md)
  if(!identical(row_names, col_names)) {
    stop("`extract_upper()` just support for symmetric matrices.", call. = FALSE)
  }
  if(is_matrix_data(md) || is_grouped_matrix_data(md)) {
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
  stopifnot(is_matrix_data(md) || is_grouped_matrix_data(md) || is_md_tbl(md))
  row_names <- row_names(md)
  col_names <- col_names(md)
  if(!identical(row_names, col_names)) {
    stop("`extract_lower()` just support for symmetric matrices.", call. = FALSE)
  }
  if(is_matrix_data(md) || is_grouped_matrix_data(md)) {
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
  stopifnot(is_matrix_data(md) || is_grouped_matrix_data(md) || is_md_tbl(md))
  row_names <- row_names(md)
  col_names <- col_names(md)
  if(!identical(row_names, col_names)) {
    stop("`trim_diag()` just support for symmetric matrices.", call. = FALSE)
  }
  if(is_matrix_data(md) || is_grouped_matrix_data(md)) {
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

#' @rdname Helper_function
#' @export
filter_func <- function(..., type = "full", diag = FALSE) {
  type <- match.arg(type, c("full", "upper", "lower", "diag"))
  function(data) {
    data <- switch(type,
                   full = if(isTRUE(diag)) data else trim_diag(data),
                   upper = extract_upper(data, diag),
                   lower = extract_lower(data, diag),
                   diag = extract_diag(data))
    dplyr::filter(data, ...)
  }
}

#' @rdname Helper_function
#' @export
trim_duplicate <- function(md, diag = TRUE) {
  if (empty(md)) return(md)

  if (identical(row_names(md), col_names(md)) && isTRUE(diag)) {
    md <- trim_diag(md)
  }

  id <- purrr::map2_chr(md$.rownames, md$.colnames, function(id, id2) {
    paste0(sort(c(id, id2)), collapse = "-")
  })

  md[!duplicated(id), ]
}

#' @param keep logical, the output matrix remains symmetric if
#' the input matrix is symmetric.
#' @rdname Helper_function
#' @export
simplify <- function(md, keep = TRUE) {
  stopifnot(is_md_tbl(md))
  if (empty(md)) {
    stop("Empty data input.", call. = FALSE)
  }

  rnm <- attr(md, "row_names")
  cnm <- attr(md, "col_names")
  if (!identical(rnm, cnm)) {
    keep <- FALSE
  }

  if (isTRUE(keep)) {
    row_and_cols <- unique(c(md$.rownames, md$.conames))
    attr(md, "row_names") <- rnm[rnm %in% row_and_cols]
    attr(md, "col_names") <- cnm[cnm %in% row_and_cols]
  } else {
    rows <- unique(md$.rownames)
    cols <- unique(md$.colnames)
    attr(md, "row_names") <- rows
    attr(md, "col_names") <- cols
    if (!identical(rows, cols)) {
      attr(md, "type") <- "full"
      attr(md, "diag") <- TRUE
    }
  }
  md
}

#' @export
as.data.frame.md_tbl <- function(x, ...) {
  if (empty(x)) {
    return(as.data.frame(list()))
  }
  x <- x[names(x)]
  class(x) <- "data.frame"
  x
}

#' @importFrom tibble as_tibble
#' @export
as_tibble.md_tbl <- function(x, ...) {
  x <- as.data.frame(x)
  class(x) <- c("tbl_df", "tbl", "data.frame")
  x
}
