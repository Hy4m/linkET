#' @title Adjust p values
#' @description Adjust correlation p values based on user-specified method.
#' @param x a correlate or md_tbl object.
#' @param method adjust method.
#' @param ... other parameters passing on to adjust function.
#' @return a object same as x.
#' @author Hou Yun
#' @rdname adjust_pvalue
#' @export
adjust_pvalue <- function(x, method = "holm", ...) {
  if (!inherits(x, "cor_md_tbl") && !inherits(x, "correlate")) {
    stop("Can only adjust p-value for 'md_tbl' and 'correlate' object.",
         call. = FALSE)
  }
  if (!"p" %in% names(x)) {
    return(x)
  }

  method <- match.arg(method, c("Bonferroni", "Holm", "Hochberg", "SidakSS",
                                "SidakSD", "BH", "BY", "ABH", "TSBH", "holm",
                                "hochberg", "hommel", "bonferroni", "fdr", "none"))
  if (method == "none") {
    return(x)
  }

  if (method %in% stats::p.adjust.methods) {
    if (inherits(x, "cor_md_tbl")) {
      adj_p <- stats::p.adjust(x$p, method = method, n = nrows(x) * ncols(x))
    } else {
      adj_p <- stats::p.adjust(x$p, method = method)
    }
  } else {
    .FUN <- get_function("multtest", "mt.rawp2adjp")
    if (inherits(x, "cor_md_tbl")) {
      if (length(x$p) != nrows(x) * ncols(x)) {
        warning("Please adjust p-value before filter.", call. = FALSE)
      }
    }
    adj_p <- .FUN(rawp = x$p, proc = method, ...)$adjp[, 2]
  }

  if (inherits(x, "cor_md_tbl")) {
    x$p <- adj_p
  } else {
    x$p[] <- adj_p
  }
  x
}

