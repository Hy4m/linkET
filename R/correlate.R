#' Matrix of Correlations, P-values and confidence intervals
#' @title Correlate
#' @param x,y a matrix object or NULL.
#' @param method a character string indicating which correlation coefficient is to be used
#' for the test. One of "pearson", "kendall", or "spearman".
#' @param use an optional character string giving a method for computing covariances in the presence of missing values.
#' @param adjust logical, if TRUE (default) will adjust p value for multiple comparisons.
#' @param adjust_method correction method.
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
fast_correlate <- function(x,
                           y = NULL,
                           adjust = FALSE,
                           adjust_method = "holm",
                           use = "everything",
                           ...)
{
  if(!requireNamespace("WGCNA", quietly = TRUE)) {
    stop("'fast_correlate' needs 'WGCNA' package.", call. = FALSE)
  }
  corr <- WGCNA::corAndPvalue(x, y, use, ...)
  if(isTRUE(adjust)) {
    adjust_method <- match.arg(adjust_method, p.adjust.methods)
    corr$p <- p.adjust(corr$p, adjust_method)
  }
  structure(.Data = list(r = corr$cor, p = corr$p),
            class = "correlate")
}

#' @rdname correlate
#' @export
fast_correlate2 <- function (x,
                             method = "pearson",
                             adjust = FALSE,
                             adjust_method = "holm",
                             ...)
{
  if(!requireNamespace("picante", quietly = TRUE)) {
    stop("'fast_correlate2' needs 'picante' package.", call. = FALSE)
  }
  corr <- picante::cor.table(x, method, ...)
  if(isTRUE(adjust)) {
    adjust_method <- match.arg(adjust_method, p.adjust.methods)
    corr$p <- p.adjust(corr$p, adjust_method)
  }
  structure(.Data = list(r = corr$r, p = corr$P),
            class = "correlate")
}

#' Print correlate object.
#' @param x a correlate object.
#' @param all if FALSE (default) just print correlation matrix, else will
#'     print all values.
#' @param ... extra params passing to \code{print}.
#' @examples
#' m <- correlate(mtcars, cor.test = TRUE)
#' print(m)
#' print(m, TRUE)
#' @author Hou Yun
#' @export
print.correlate <- function(x, all = FALSE, ...) {
  if(all) print(x, ...) else print(x$r, ...)
}
