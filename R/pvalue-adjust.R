#' @title Adjust p values
#' @description Adjust correlation p values based on user-specified method.
#' @param x a correlate or md_tbl object.
#' @param .FUN adjust function, such as \code{stats::p.adjust()},
#' \code{multtest::mt.rawp2adjp()}.
#' @param method adjust method when .FUN is "p.adjust".
#' @param proc adjust method when .FUN is "mt.rawp2adjp".
#' @param ... other parameters passing on to adjust function.
#' @return a object same as x.
#' @author Hou Yun
#' @rdname adjust_pvalue
#' @export
adjust_pvalue <- function(x,
                          .FUN = "p.adjust",
                          method = "holm",
                          proc = "Holm",
                          ...) {
  if (!inherits(x, "cor_md_tbl") && !inherits(x, "correlate")) {
    stop("Can only adjust p-value for 'md_tbl' and 'correlate' object.",
         call. = FALSE)
  }
  if (!"p" %in% names(x)) {
    return(x)
  }

  .FUN <- match.arg(.FUN, c("p.adjust", "mt.rawp2adjp"))

  if (.FUN == "p.adjust") {
    .FUN <- get_function("stats", "p.adjust")
    if (inherits(x, "cor_md_tbl")) {
      adj_p <- stats::p.adjust(x$p, method = method, n = nrows(x) * ncols(x))
    } else {
      adj_p <- stats::p.adjust(x$p, method = method)
    }
  } else {
    .FUN <- get_function("multtest", "mt.rawp2adjp")
    proc <- match.arg(proc, c("Bonferroni", "Holm", "Hochberg", "SidakSS",
                              "SidakSD", "BH", "BY", "ABH", "TSBH"))
    if (inherits(x, "cor_md_tbl")) {
      if (length(x$p) != nrows(x) * ncols(x)) {
        warning("Please adjust p-value before filter.", call. = FALSE)
      }
    }
    adj_p <- .FUN(rawp = x$p, proc = proc, ...)$adjp[, 2]
  }

  if (inherits(x, "cor_md_tbl")) {
    x$p <- adj_p
  } else {
    x$p[] <- adj_p
  }
  x
}

