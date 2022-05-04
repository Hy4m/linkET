#' Matrix of Correlations, P-values and confidence intervals
#' @title Correlate
#' @param x,y a matrix object or NULL.
#' @param group NULL or a character vector.
#' @param method a character string indicating which correlation coefficient
#' is to be used
#' for the test. One of "pearson", "kendall", or "spearman".
#' @param use an optional character string giving a method for computing
#' covariances in the presence of missing values.
#' @param adjust logical, if TRUE (default) will adjust p value for multiple
#' comparisons.
#' @param adjust_method correction method.
#' @param engine one of "default", "WGCNA", "picante", "Hmisc" or "psych",
#' indicating which package is used to calculate the correlation.
#' @param ... extra params, see Details.
#' @details The columns of 'x' will be tested for each pair when y is NULL(the default),
#' otherwise each column in 'x' and each column in 'y' is tested for each pair.
#' @return a list with correlation matrix, P values matrix, confidence intervals matrix.
#' @importFrom stats cor cor.test p.adjust p.adjust.methods
#' @importFrom purrr walk2
#' @rdname correlate
#' @examples
#' correlate(mtcars)
#'
#' m1 <- matrix(rnorm(100), nrow = 10)
#' m2 <- matrix(rnorm(60), nrow = 10)
#' correlate(m1, m2)
#'
#' ## adjust p value
#' correlate(m1, m2, cor.test = TRUE, adjust = TRUE)
#'
#' ## fast compute correlation
#' \dontrun{
#' require(WGCNA)
#' fast_correlate(m1, m2)
#'
#' require(picante)
#'   fast_correlate2(m1)
#' }
#' @seealso \code{\link[stats]{cor}}, \code{\link[stats]{cor.test}}.
#' @author Hou Yun
#' @export
correlate <- function(x,
                      y = NULL,
                      group = NULL,
                      method = "pearson",
                      use = "everything",
                      adjust = FALSE,
                      adjust_method = "holm",
                      engine = "default",
                      ...) {
  engine <- match.arg(engine, c("default", "WGCNA", "picante", "Hmisc", "psych"))
  if (engine == "picante" && !is.null(y)) {
    warning("`y` will be abandoned when 'engine = picante'.", call. = FALSE)
    y <- NULL
  }



  if (is.null(group)) {
    if (engine == "default") {
      out <- .correlate(x = x,
                        y = y,
                        method = method,
                        use = use,
                        adjust = adjust,
                        adjust_method = adjust_method,
                        ...)
    } else if (engine == "WGCNA") {
      corAndPvalue <- get_function("WGCNA", "corAndPvalue")
      corr <- corAndPvalue(x = x,
                           y = y,
                           method = method,
                           use = use,
                           ...)
      if(isTRUE(adjust)) {
        adjust_method <- match.arg(adjust_method, p.adjust.methods)
        corr$p <- p.adjust(corr$p, adjust_method)
      }
      out <- structure(.Data = list(r = corr$cor, p = corr$p),
                       class = "correlate")
    } else if (engine == "picante") {
      cor.table <- get_function("picante", "cor.table")
      corr <- cor.table(x = x, cor.method = method, ...)
      if(isTRUE(adjust)) {
        adjust_method <- match.arg(adjust_method, p.adjust.methods)
        corr$p <- p.adjust(corr$p, adjust_method)
      }
      out <- structure(.Data = list(r = corr$r, p = corr$P),
                       class = "correlate")
    } else if (engine == "Hmisc") {
      rcorr <- get_function("Hmisc", "rcorr")
      out <- as_correlate(rcorr(x = as.matrix(x),
                                y = if (!is.null(y)) as.matrix(y) else NULL,
                                type = method,
                                ...))
      if (isTRUE(adjust)) {
        adjust_method <- match.arg(adjust_method, p.adjust.methods)
        out$p <- p.adjust(out$p, adjust_method)
      }
    } else {
      corr.test <- get_function("psych", "corr.test")
      out <- as_correlate(corr.test(x = x,
                                    y = y,
                                    use = if (use == "everything") "complete" else "pairwise",
                                    method = method,
                                    adjust = if (isTRUE(adjust)) adjust_method else "none"))
    }
  } else {
    if (length(group) != nrow(x)) {
      stop("group should have same length as rows of x.", call. = FALSE)
    }
    x <- tryCatch(as.data.frame(x), error = function(e) as.data.frame(as.matrix(x)))
    x <- split(x, group)
    if (!is.null(y)) {
      y <- tryCatch(as.data.frame(y), error = function(e) as.data.frame(as.matrix(y)))
      y <- split(y, group)
    } else {
      y <- rep_len(list(NULL), length(x))
    }

    out <- purrr::map2(x, y, function(.x, .y) {
      correlate(x = .x,
                y = .y,
                group = NULL,
                engine = engine,
                method = method,
                use = use,
                adjust = adjust,
                adjust_method = adjust_method,
                ...)
    })
    names(out) <- names(x)
    class(out) <- "grouped_correlate"
  }
  out
}

#' @noRd
.correlate <- function(x,
                       y = NULL,
                       method = "pearson",
                       use = "everything",
                       adjust = FALSE,
                       adjust_method = "holm",
                       ...)
{
  y_is_null <- is.null(y)
  y <- y %||% x
  if(!is.matrix(x))
    x <- as.matrix(x)
  if(!is.matrix(y))
    y <- as.matrix(y)
  n <- ncol(x)
  m <- ncol(y)
  r <- cor(x, y, use = use, method = method)

  p <- lower_ci <- upper_ci <- matrix(NA, ncol = m, nrow = n)
  id <- expand.grid(1:n, 1:m)
  if (y_is_null) {
    id <- id[id$Var1 > id$Var2, , drop = FALSE]
    purrr::walk2(id$Var1, id$Var2, function(.idx, .idy) {
      tmp <- cor.test(x = x[ , .idx], y = y[ , .idy], method = method, ...)
      p[c(.idx, .idy), c(.idy, .idx)] <<- tmp$p.value
      if (method == "pearson") {
        if (nrow(x) > 3) {
          lower_ci[c(.idx, .idy), c(.idy, .idx)] <<- tmp$conf.int[1]
          upper_ci[c(.idx, .idy), c(.idy, .idx)] <<- tmp$conf.int[2]
        } else {
          warning("correlation test interval needs 4 observations at least.", call. = FALSE)
        }
      }
    })
    diag(p) <- 0
    if (method == "pearson") {
      diag(lower_ci) <- diag(upper_ci) <- 1
    }
  } else {
    purrr::walk2(id$Var1, id$Var2, function(.idx, .idy) {
      tmp <- cor.test(x = x[ , .idx], y = y[ , .idy], method = method, ...)
      p[.idx, .idy] <<- tmp$p.value
      if (method == "pearson") {
        if (nrow(x) > 3) {
          lower_ci[.idx, .idy] <<- tmp$conf.int[1]
          upper_ci[.idx, .idy] <<- tmp$conf.int[2]
        } else {
          warning("correlation test interval needs 4 observations at least.", call. = FALSE)
        }
      }
    })
  }

  lower_ci <- if (method == "pearson") lower_ci else NULL
  upper_ci <- if (method == "pearson") upper_ci else NULL
  if (isTRUE(adjust)) {
    adjust_method <- match.arg(adjust_method, p.adjust.methods)
    p[] <- p.adjust(p, adjust_method)
  }

  structure(
    .Data = list(
      r = r,
      p = p,
      lower_ci = lower_ci,
      upper_ci = upper_ci
    ), class = "correlate"
  )
}

#' @rdname correlate
#' @export
fast_correlate <- function(...)
{
  warning("fast_correlate() is soft deprecated,\n",
          "please use correlate() instead.", call. = FALSE)
  correlate(..., engine = "WGCNA")
}

#' @rdname correlate
#' @export
fast_correlate2 <- function (...)
{
  warning("fast_correlate2() is soft deprecated,\n",
          "please use correlate() instead.", call. = FALSE)
  correlate(..., engine = "picante")
}

#' Print correlate object.
#' @param all if FALSE (default) just print correlation matrix, else will
#'     print all values.
#' @param ... extra params passing to \code{print}.
#' @method print correlate
#' @rdname correlate
#' @examples
#' m <- correlate(mtcars, cor.test = TRUE)
#' print(m)
#' print(m, TRUE)
#' @author Hou Yun
#' @export
print.correlate <- function(x, all = FALSE, ...) {
  if (all) print(x, ...) else print(x$r, ...)
}

#' @method print grouped_correlate
#' @rdname correlate
#' @export
print.grouped_correlate <- function(x, all = FALSE, ...) {
  if (all) {
    print(x, ...)
  } else {
    for (i in names(x)) {
      cat("Group:", i, "\n")
      cat("\tA", nrow(x[[1]]$r), "rows, and", ncol(x[[1]]$r), "columns correlate object.\n")
    }
  }
}
