#' Coerce a object to correlate.
#' @title Coerce a object to correlate
#' @param x any \code{R} object can convert to correlate.
#' @param is_corr if TRUE (default), the data will be regarded as the correlation
#' coefficient matrix.
#' @param p p value matrix.
#' @param ... others parameters.
#' @return a correlate object.
#' @rdname as_correlate
#' @export
as_correlate <- function(x, ...) {
  UseMethod("as_correlate")
}

#' @method as_correlate rcorr
#' @rdname as_correlate
as_correlate.rcorr <- function(x, ...) {
  p <- x$P
  diag(p) <- 0
  structure(.Data = list(r = x$r, p = p), class = "correlate")
}

#' @method as_correlate corr.test
#' @rdname as_correlate
as_correlate.corr.test <- function(x, ...) {
  structure(.Data = list(r = x$r, p = x$p), class = "correlate")
}

#' @method as_correlate matrix
#' @rdname as_correlate
as_correlate.matrix <- function(x,
                                is_corr = NULL,
                                p = NULL,
                                ...) {
  is_corr <- is_corr %||% check_corr(x)

  if(isFALSE(is_corr)) {
    x <- correlate(x, ...)
  } else {
    if(is.null(p)) {
      x <- structure(.Data = list(r = x), class = "correlate")
    } else {
      p <- as.matrix(p)
      if(!identical(dim(x), dim(p))) {
        stop("'p' should have same dimension as 'x'.", call. = FALSE)
      }
      x <- structure(.Data = list(r = x, p = p), class = "correlate")
    }
  }
  x
}

#' @method as_correlate data.frame
#' @rdname as_correlate
as_correlate.data.frame <- function(x, ...) {
  as_correlate(as.matrix(x), ...)
}

#' @method as_correlate easycorrelation
#' @rdname as_correlate
as_correlate.easycorrelation <- function(x, ...) {
  if(nrow(x) < 1) {
    stop("Empty data.", call. = FALSE)
  }

  grouped <- inherits(x, "grouped_easycorrelation")
  is_null_data2 <- is.null(attr(x, "data2"))
  row_names <- colnames(attr(x, "data"))
  col_names <- if (is_null_data2) row_names else colnames(attr(x, "data2"))

  if(isFALSE(is_null_data2)) {
    corr <- tibble(.rownames = x$Parameter1,
                   .colnames = x$Parameter2,
                   r = x$r,
                   p = x$p)
    if (isTRUE(grouped)) {
      corr$.group <- x$Group
    }
  } else {
    corr <- tibble(.rownames = c(x$Parameter1, x$Parameter2),
                   .colnames = c(x$Parameter2, x$Parameter1),
                   r = c(x$r, x$r),
                   p = c(x$p, x$p))
    if (isTRUE(grouped)) {
      corr$.group <- c(x$Group, x$Group)
      diag <- tibble(.rownames = rep(row_names, length(unique(x$Group))),
                     .colnames = rep(col_names, length(unique(x$Group))),
                     r = 1,
                     p = 0,
                     .group = rep(unique(x$Group), each = length(row_names)))
      corr <- dplyr::bind_rows(corr, diag)
    } else {
      diag <- tibble(.rownames = row_names,
                     .colnames = col_names,
                     r = 1,
                     p = 0)
      corr <- dplyr::bind_rows(corr, diag)
    }
  }

  if (isTRUE(grouped)) {
    out <- lapply(split(corr, corr$.group), function(.corr) {
      r <- df_to_matrix(.corr, "r", row_id = ".rownames", col_id = ".colnames",
                        row_names = row_names, col_names = col_names)
      p <- df_to_matrix(.corr, "p", row_id = ".rownames", col_id = ".colnames",
                        row_names = row_names, col_names = col_names)
      as_correlate(x = r, p = p, is_corr = TRUE)
    })
    class(out) <- "grouped_correlate"
  } else {
    r <- df_to_matrix(corr, "r", row_id = ".rownames", col_id = ".colnames",
                      row_names = row_names, col_names = col_names)
    p <- df_to_matrix(corr, "p", row_id = ".rownames", col_id = ".colnames",
                      row_names = row_names, col_names = col_names)
    out <- as_correlate(x = r, p = p, is_corr = TRUE)
  }
  out
}


#' @noRd
check_corr <- function(x) {
  if(!is.numeric(x)) {
    stop("Not support non-numeric data.", call. = FALSE)
  }
  rnm <- rownames(x)
  cnm <- colnames(x)
  out <- TRUE
  if(!is.null(cnm) && is.null(rnm)) {
    out <- FALSE
  }

  if(is.null(cnm) && !is.null(rnm)) {
    out <- FALSE
  }

  if(any(x > 1) || any(x < -1)) {
    out <- FALSE
  }

  if(identical(rnm, cnm) && !is.null(rnm)) {
    if(isSymmetric(x)) {
      out <- TRUE
    } else {
      out <- FALSE
    }
  }
  if(isTRUE(out)) {
    message("The input data is a correlation matrix,\n",
            "you can override this behavior by setting the `is_corr` parameter.")
  } else {
    message("The input data is not a correlation matrix,\n",
            "you can override this behavior by setting the `is_corr` parameter.")
  }
  out
}
