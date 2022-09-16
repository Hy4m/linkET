#' @title Adjust p values
#' @description Adjust correlation p values based on user-specified method.
#' @param x a correlate or md_tbl object.
#' @param .FUN adjust function, such as \code{stats::p.adjust()},
#' \code{multtest::mt.rawp2adjp()}.
#' @param ... other parameters passing on to adjust function.
#' @return a object same as x.
#' @author Hou Yun
#' @rdname adjust_pvalue
#' @export
adjust_pvalue <- function(x, .FUN = "p.adjust", ...) {
  if (!inherits(x, "cor_md_tbl") && !inherits(x, "correlate")) {
    stop("Can only adjust p-value for 'md_tbl' and 'correlate' object.",
         call. = FALSE)
  }
  if (!"p" %in% names(x)) {
    return(x)
  }

  .FUN <- match.fun(.FUN)
  if (inherits(x, "cor_md_tbl")) {
    x$p <- .FUN(x$p, ...)
  } else {
    x$p[] <- .FUN(x$p, ...)
  }
  x
}

