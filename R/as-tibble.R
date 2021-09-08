#' Coerce matrix_data to data frames
#' @title Coerce matrix_data to data frames
#' @param x any \code{R} can be converted to md_tbl.
#' @param byrow a logical value indicating whether arrange the 'spec' columns
#' on y axis.
#' @param type character, "full" (default), "upper" or "lower", display full
#' matrix, lower triangular or upper triangular matrix.
#' @param diag logical, if TRUE (default) will keep the diagonal of matrix data.
#' @param name variable name.
#' @param ... other parameters.
#' @return a tibble object.
#' @importFrom dplyr bind_cols
#' @importFrom dplyr bind_rows
#' @importFrom tibble tibble
#' @rdname as_md_tbl
#' @author Hou Yun
#' @export
as_md_tbl <- function(x) {
  UseMethod("as_md_tbl")
}

#' @method as_md_tbl matrix_data
#' @rdname as_md_tbl
#' @export
as_md_tbl.matrix_data <- function(x, ...)
{
  type <- attr(x, "type")
  diag <- attr(x, "diag")
  value <- new_data_frame(lapply(x, as.vector))
  id <- new_data_frame(list(.rownames = rep(row_names(x), ncols(x)),
                            .colnames = rep(col_names(x), each = nrows(x))))
  md_tbl <- structure(.Data = bind_cols(id, value),
                      row_names = row_names(x),
                      col_names = col_names(x),
                      type = type,
                      diag = diag,
                      class = c("md_tbl", "tbl_df", "tbl", "data.frame"))
  if(type == "upper") {
    extract_upper(md_tbl, diag = diag)
  } else if(type == "lower") {
    extract_lower(md_tbl, diag = diag)
  } else {
    if(isFALSE(diag)) {
      trim_diag(md_tbl)
    } else {
      md_tbl
    }
  }
}

#' @method as_md_tbl pro_tbl
#' @rdname as_md_tbl
#' @export
as_md_tbl.pro_tbl <- function(x, byrow = TRUE, ...) {
  env_nm <- unique(x$env)
  spec_nm <- unique(x$spec)
  spec <- env <- NULL
  if(byrow) {
    col_names <- env_nm
    row_names <- spec_nm
    x <- dplyr::rename(.rownames = spec,
                       .colnames = env)
  } else {
    col_names <- spec_nm
    row_names <- env_nm
    x <- dplyr::rename(.rownames = env,
                       .colnames = spec)
  }

  structure(
    .Data = x,
    row_names = row_names,
    col_names = col_names,
    type = "full",
    diag = TRUE,
    class = c("cor_md_tbl", "md_tbl", "tbl_df", "tbl", "data.frame"))
}

#' @method as_md_tbl mantel_tbl
#' @rdname as_md_tbl
#' @export
as_md_tbl.mantel_tbl <- function(x, byrow = TRUE, ...) {
  env_nm <- unique(x$env)
  spec_nm <- unique(x$spec)
  spec <- env <- NULL
  if(byrow) {
    col_names <- env_nm
    row_names <- spec_nm
    x <- dplyr::rename(.rownames = spec,
                       .colnames = env)
  } else {
    col_names <- spec_nm
    row_names <- env_nm
    x <- dplyr::rename(.rownames = env,
                       .colnames = spec)
  }

  structure(
    .Data = x,
    row_names = row_names,
    col_names = col_names,
    type = "full",
    diag = TRUE,
    class = c("cor_md_tbl", "md_tbl", "tbl_df", "tbl", "data.frame"))
}

#' @method as_md_tbl easycorrelation
#' @rdname as_md_tbl
#' @export
as_md_tbl.easycorrelation <- function(x,
                                      type = "full",
                                      diag = TRUE,
                                      ...) {
  if(inherits(x, "grouped_easycorrelation")) {
    stop("`grouped_easycorrelation` is not implemented yet.", call. = FALSE)
  }
  if(nrow(x) < 1) {
    stop("Empty data.", call. = FALSE)
  }

  type <- match.arg(type, c("full", "upper", "lower"))
  is_null_data2 <- is.null(attr(x, "data2"))
  row_names <- unique(x$Parameter1)
  col_names <- unique(x$Parameter2)

  if(!isTRUE(is_null_data2)) {
    if(type %in% c("upper", "lower")) {
      warning("'type = ", type, "' just support for symmetric matrices.")
    }
    type <- "full"
    diag <- TRUE
    out <- tibble(.rownames = x$Parameter1,
                  .colnames = x$Parameter2,
                  r = x$r,
                  p = x$p)
  } else {
    row_names <- col_names <- c(row_names, col_names[length(col_names)])
    if(type == "full") {
      out <- tibble(.rownames = c(x$Parameter1, x$Parameter2),
                    .colnames = c(x$Parameter2, x$Parameter1),
                    r = c(x$r, x$r),
                    p = c(x$p, x$p))

    } else if(type == "upper") {
      out <- tibble(.rownames = x$Parameter1,
                    .colnames = x$Parameter2,
                    r = x$r,
                    p = x$p)
    } else {
      out <- tibble(.rownames = x$Parameter2,
                    .colnames = x$Parameter1,
                    r = x$r,
                    p = x$p)
    }
    if(isTRUE(diag)) {
      diag_tbl <- tibble(.rownames = row_names,
                         .colnames = col_names,
                         r = 1,
                         p = 0)
      out <- bind_rows(out, diag_tbl)
    }
  }

  structure(.Data = out,
            row_names = row_names,
            col_names = col_names,
            type = type,
            diag = diag,
            class = c("cor_md_tbl", "md_tbl", "tbl_df", "tbl", "data.frame"))
}

#' @method as_md_tbl correlate
#' @rdname as_md_tbl
#' @export
as_md_tbl.correlate <- function(x, ...) {
  x <- as_md_tbl(as_matrix_data(x, ...))
  class(x) <- c("cor_md_tbl", class(x))
  x
}

#' @method as_md_tbl rcorr
#' @rdname as_md_tbl
#' @export
as_md_tbl.rcorr <- function(x, ...) {
  as_md_tbl(as_correlate(x), ...)
}

#' @method as_md_tbl corr.test
#' @rdname as_md_tbl
#' @export
as_md_tbl.corr.test <- function(x, ...) {
  as_md_tbl(as_correlate(x), ...)
}

#' @method as_md_tbl data.frame
#' @rdname as_md_tbl
#' @export
as_md_tbl.data.frame <- function(x, name = NULL, ...) {
  if (is.null(name)) {
    name <- deparse(substitute(x))
  }
  as_md_tbl(as_matrix_data(x, name = name), ...)
}

#' @method as_md_tbl matrix
#' @rdname as_md_tbl
#' @export
as_md_tbl.matrix <- function(x, name = NULL, ...) {
  if (is.null(name)) {
    name <- deparse(substitute(x))
  }
  as_md_tbl(as_matrix_data(x, name = name), ...)
}
