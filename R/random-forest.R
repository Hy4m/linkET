## TODO: rand_forest and calc_relimp will be merged in feature importance function.
#' Random forests
#' @title Random forests
#' @param spec,env a data.frame object.
#' @param byrow a logical value, if TRUE, the 'spec' on the rows.
#' @param seed a integer value.
#' @param x a rand_forest object.
#' @param ... extra parameters.
#' @return a rand_forest object.
#' @rdname random_forest
#' @examples \dontrun{
#' spec <- mtcars[c(1, 3, 4, 5)]
#' env <- mtcars[6:11]
#' random_forest(spec, env)
#' }
#' @author Hou Yun
#' @export
random_forest <- function(spec,
                          env,
                          byrow = TRUE,
                          seed = 123,
                          ...)
{
  if(!is.data.frame(spec))
    spec <- as.data.frame(spec)
  if(!is.data.frame(env))
    env <- as.data.frame(env)

  n <- length(spec)
  m <- length(env)
  if(any(n < 1, m < 1)) {
    stop("Zero length data.", call. = FALSE)
  }

  if(nrow(spec) != nrow(env)) {
    stop("'env' shold have the same rows as 'spec'.", call. = FALSE)
  }

  rfPermute <- get_function("rfPermute", "rfPermute")
  importance <- get_function("rfPermute", "importance")
  set.seed(seed)
  seeds <- as.integer(stats::runif(n) * 10000)

  explained <- vector(length = n)
  imp <- matrix(NA, nrow = n, ncol = m, dimnames = list(names(spec), names(env)))
  p <- matrix(NA, nrow = n, ncol = m, dimnames = list(names(spec), names(env)))

  for (i in seq_len(n)) {
    set.seed(seeds[i])
    rf <- rfPermute(spec[[i]] ~ ., data = env, ...)
    type <- rf$rf$type
    imp_mat <- importance(rf, scale = TRUE)[names(env), , drop = FALSE]
    if(type == "classification") {
      explained[i] <- 100 - 100 * rf$rf$err.rate[rf$rf$ntree, "OOB"]
      imp[i, ] <- imp_mat[, "MeanDecreaseAccuracy"]
      p[i, ] <- imp_mat[, "MeanDecreaseAccuracy.pval"]
    } else {
      explained[i] <- 100 * rf$rf$rsq[length(rf$rf$rsq)]
      imp[i, ] <- imp_mat[, "%IncMSE"]
      p[i, ] <- imp_mat[, "%IncMSE.pval"]
    }
  }

  if(isFALSE(byrow)) {
    imp <- t(imp)
    p <- t(p)
  }
  structure(.Data = list(explained = data.frame(name = names(spec),
                                                explained = explained,
                                                stringsAsFactors = FALSE),
                         importance = as.data.frame(imp),
                         p = as.data.frame(p)),
            byrow = byrow,
            class = "random_forest")
}

#' @method print random_forest
#' @rdname random_forest
#' @export
print.random_forest <- function(x, ...) {
  cat("Var explained (%):\n")
  print(x$explained)
  cat("\n")
  cat("Var importance:\n")
  print(x$importance)
  cat("\n")
  cat("Var importance p value:\n")
  print(x$p)
}
