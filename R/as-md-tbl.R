## TODO: add corr attributes instead of sub-class
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
as_md_tbl <- function(x, ...) {
  UseMethod("as_md_tbl")
}

#' @method as_md_tbl matrix_data
#' @rdname as_md_tbl
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
                      row_tree = attr(x, "row_tree"),
                      col_tree = attr(x, "col_tree"),
                      type = type,
                      diag = diag,
                      class = c("md_tbl", "tbl_df", "tbl", "data.frame"))
  if (inherits(x, "cor_matrix_data")) {
    class(md_tbl) <- c("cor_md_tbl", class(md_tbl))
  }
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

#' @method as_md_tbl grouped_matrix_data
#' @rdname as_md_tbl
as_md_tbl.grouped_matrix_data <- function(x, ...)
{
  type <- vapply(x, attr, character(1), "type")
  diag <- vapply(x, attr, logical(1), "diag")
  row_names <- unique(unlist(lapply(x, attr, "row_names")))
  col_names <- unique(unlist(lapply(x, attr, "col_names")))

  if (length(unique(type)) != 1L) {
    type <- "full"
  } else {
    type <- unique(type)
  }

  if (length(unique(diag)) != 1L) {
    diag <- TRUE
  } else {
    diag <- unique(diag)
  }

  x <- lapply(names(x), function(.nm) {
    as_md_tbl(x[[.nm]], ...) %>%
      mutate(.group = .nm)
  })

  clss <- c("grouped_md_tbl", class(x[[1]]))

  structure(do.call(dplyr::bind_rows, x),
            type = type,
            diag = diag,
            row_names = row_names,
            col_names = col_names,
            class = clss)
}

#' @method as_md_tbl mantel_tbl
#' @rdname as_md_tbl
as_md_tbl.mantel_tbl <- function(x, byrow = TRUE, ...) {
  class(x) <- class(x)[-1L]
  grouped <- attr(x, "grouped")
  spec <- env <- NULL
  if(isTRUE(byrow)) {
    x <- as_md_tbl(x,
                   row_vars = spec,
                   col_vars = env,
                   is_corr = TRUE)
  } else {
    x <- as_md_tbl(x,
                   row_vars = env,
                   col_vars = spec,
                   is_corr = TRUE)
  }
  if (isTRUE(grouped)) {
    class(x) <- c("cor_md_tbl", "grouped_md_tbl", "md_tbl", "tbl_df", "tbl", "data.frame")
  }
  x
}

#' @method as_md_tbl easycorrelation
#' @rdname as_md_tbl
as_md_tbl.easycorrelation <- function(x,
                                      type = "full",
                                      diag = TRUE,
                                      ...) {
  if(nrow(x) < 1) {
    stop("Empty data.", call. = FALSE)
  }

  grouped <- inherits(x, "grouped_easycorrelation")
  type <- match.arg(type, c("full", "upper", "lower"))
  is_null_data2 <- is.null(attr(x, "data2"))
  row_names <- unique(x$Parameter1)
  col_names <- unique(x$Parameter2)

  if(isFALSE(is_null_data2)) {
    if(type %in% c("upper", "lower")) {
      warning("'type = ", type, "' just support for symmetric matrices.")
    }
    type <- "full"
    diag <- TRUE
    out <- tibble(.rownames = x$Parameter1,
                  .colnames = x$Parameter2,
                  r = x$r,
                  p = x$p)
    if (isTRUE(grouped)) {
      out$.group <- x$Group
    }
  } else {
    row_names <- col_names <- c(row_names, col_names[length(col_names)])
    if(type == "full") {
      out <- tibble(.rownames = c(x$Parameter1, x$Parameter2),
                    .colnames = c(x$Parameter2, x$Parameter1),
                    r = c(x$r, x$r),
                    p = c(x$p, x$p))
      if (isTRUE(grouped)) {
        out$.group <- c(x$Group, x$Group)
      }
    } else if(type == "upper") {
      out <- tibble(.rownames = x$Parameter1,
                    .colnames = x$Parameter2,
                    r = x$r,
                    p = x$p)
      if (isTRUE(grouped)) {
        out$.group <- x$Group
      }
    } else {
      out <- tibble(.rownames = x$Parameter2,
                    .colnames = x$Parameter1,
                    r = x$r,
                    p = x$p)
      if (isTRUE(grouped)) {
        out$.group <- x$Group
      }
    }
    if(isTRUE(diag)) {
      diag_tbl <- tibble(.rownames = row_names,
                         .colnames = col_names,
                         r = 1,
                         p = 0)
      if (isTRUE(grouped)) {
        n <- length(unique(x$Group))
        id <- rep(seq_along(row_names), each = n)
        diag_tbl <- diag_tbl[id, ]
        diag_tbl$.group <- rep(unique(x$Group), length(row_names))
      }
      out <- bind_rows(out, diag_tbl)
    }
  }

  clss <- if (isTRUE(grouped)) {
    c("cor_md_tbl", "grouped_md_tbl", "md_tbl", "tbl_df", "tbl", "data.frame")
  } else {
    c("cor_md_tbl", "md_tbl", "tbl_df", "tbl", "data.frame")
  }

  structure(.Data = out,
            row_names = row_names,
            col_names = col_names,
            type = type,
            diag = diag,
            class = clss)
}

#' @method as_md_tbl correlate
#' @rdname as_md_tbl
as_md_tbl.correlate <- function(x, ...) {
  as_md_tbl(as_matrix_data(x, ...))
}

#' @method as_md_tbl grouped_correlate
#' @rdname as_md_tbl
as_md_tbl.grouped_correlate <- function(x, ...) {
  as_md_tbl(as_matrix_data(x, ...))
}

#' @method as_md_tbl rcorr
#' @rdname as_md_tbl
as_md_tbl.rcorr <- function(x, ...) {
  as_md_tbl(as_correlate(x), ...)
}

#' @method as_md_tbl corr.test
#' @rdname as_md_tbl
#' @export
as_md_tbl.corr.test <- function(x, ...) {
  as_md_tbl(as_correlate(x), ...)
}

#' @param row_vars,col_vars,group_vars variable name of row/column/group id.
#' @param row_names,col_names character, row/column names of matrix.
#' @param is_corr if TRUE, the data will be regarded as the correlation
#' coefficient.
#' @param r_vars variable name of correlation coeffient column.
#' @param p_vars variable name of p value column.
#' @method as_md_tbl data.frame
#' @rdname as_md_tbl
as_md_tbl.data.frame <- function(x,
                                 name = NULL,
                                 row_vars = NULL,
                                 col_vars = NULL,
                                 group_vars = NULL,
                                 row_names = NULL,
                                 col_names = NULL,
                                 is_corr = FALSE,
                                 r_vars = NULL,
                                 p_vars = NULL,
                                 ...) {
  row_vars <- rlang::enquo(row_vars)
  col_vars <- rlang::enquo(col_vars)
  group_vars <- rlang::enquo(group_vars)
  if(rlang::quo_is_null(row_vars) || rlang::quo_is_null(col_vars)) {
    if(isTRUE(is_corr)) {
      x <- as_md_tbl(as_correlate(x, is_corr = TRUE), ...)
    } else {
      if (is.null(name)) {
        name <- deparse(substitute(x))
      }
      if (!rlang::quo_is_null(group_vars)) {
        group <- rlang::eval_tidy(group_vars, x)
      } else {
        group <- NULL
      }
      x <- as_md_tbl(as_matrix_data(x, name = name, group = group), ...)
    }
  } else {
    row_vars <- rlang::as_name(row_vars)
    col_vars <- rlang::as_name(col_vars)
    x <- rename(x, .rownames = row_vars, .colnames = col_vars)
    if (!rlang::quo_is_null(group_vars)) {
      x <- rename(x, .group = rlang::as_name(group_vars))
      class(x) <- c("grouped_md_tbl", "md_tbl", "tbl_df", "tbl", "data.frame")
    } else {
      class(x) <- c("md_tbl", "tbl_df", "tbl", "data.frame")
    }

    if(isTRUE(is_corr)) {
      class(x) <- c("cor_md_tbl", class(x))

      r_vars <- rlang::enquo(r_vars)
      p_vars <- rlang::enquo(p_vars)
      if(!rlang::quo_is_null(r_vars)) {
        x <- rename(x, r = rlang::as_name(r_vars))
      }
      if(!rlang::quo_is_null(p_vars)) {
        x <- rename(x, p = rlang::as_name(p_vars))
      }
      if(!"r" %in% names(x)) {
        stop("Did you forget to set the 'r_vars' param?", call. = FALSE)
      }
    }

    attr(x, "row_names") <- row_names %||% unique(x$.rownames)
    attr(x, "col_names") <- col_names %||% unique(x$.colnames)
    attr(x, "type") <- "full"
    attr(x, "diag") <- TRUE
  }
  x
}

#' @method as_md_tbl matrix
#' @rdname as_md_tbl
as_md_tbl.matrix <- function(x, ...) {
  x <- as.data.frame(x)
  as_md_tbl(x, ...)
}

#' @method as_md_tbl md_tbl
#' @rdname as_md_tbl
as_md_tbl.md_tbl <- function(x, ...) {
  x
}

#' @method as_md_tbl md_tbl
#' @rdname as_md_tbl
as_md_tbl.default <- function(x, ...) {
  msg <- paste("Cannot convert a", class(x)[1L], "object to md_tbl.")
  stop(msg, call. = FALSE)
}

