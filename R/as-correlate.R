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
#' @export
as_correlate.rcorr <- function(x, ...) {
  p <- x$P
  diag(p) <- 0
  structure(.Data = list(r = x$r, p = p), class = "correlate")
}

#' @method as_correlate corr.test
#' @rdname as_correlate
#' @export
as_correlate.corr.test <- function(x, ...) {
  structure(.Data = list(r = x$r, p = x$p), class = "correlate")
}

#' @method as_correlate matrix
#' @rdname as_correlate
#' @export
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
#' @export
as_correlate.data.frame <- function(x, ...) {
  as_correlate(as.matrix(x), ...)
}


#' @noRd
check_corr <- function(x) {
  if(!is.numeric(x)) {
    stop("Not support non-numeric data.", call. = FALSE)
  }
  rnm <- rownames(x)
  cnm <- colnames(x)
  if(!is.null(cnm) && is.null(rnm)) {
    return(FALSE)
  }

  if(is.null(cnm) && !is.null(rnm)) {
    return(FALSE)
  }

  if(any(x > 1) || any(x < -1)) {
    return(FALSE)
  }

  if(identical(rnm, cnm) && !is.null(rnm)) {
    if(isSymmetric(x)) {
      return(TRUE)
    } else {
      return(FALSE)
    }
  }
  TRUE
}
