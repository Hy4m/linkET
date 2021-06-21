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
  rp.importance <- get_function("rfPermute", "rp.importance")
  set.seed(seed)
  seeds <- as.integer(stats::runif(n) * 10000)

  explained <- vector(length = n)
  importance <- matrix(NA, nrow = n, ncol = m, dimnames = list(names(spec), names(env)))
  p <- matrix(NA, nrow = n, ncol = m, dimnames = list(names(spec), names(env)))

  for (i in seq_len(n)) {
    set.seed(seeds[i])
    rf <- rfPermute(spec[[i]] ~ ., data = env, importance = TRUE, ...)

    type <- rf$type
    imp <- rp.importance(rf, scale = TRUE)[names(env), , drop = FALSE]
    if(type == "classification") {
      explained[i] <- 100 - 100 * rf$err.rate[rf$ntree, "OOB"]
    } else {
      explained[i] <- 100 * rf$rsq[length(rf$rsq)]
    }
    if(type == "classification") {
      importance[i, ] <- imp[, "MeanDecreaseAccuracy"]
      p[i, ] <- imp[, "MeanDecreaseAccuracy.pval"]
    } else {
      importance[i, ] <- imp[, "%IncMSE"]
      p[i, ] <- imp[, "%IncMSE.pval"]
    }
  }

  if(isFALSE(byrow)) {
    importance <- t(importance)
    p <- t(p)
  }
  structure(.Data = list(explained = data.frame(name = names(spec),
                                                explained = explained,
                                                stringsAsFactors = FALSE),
                         importance = as.data.frame(importance),
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
