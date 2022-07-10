## TODO: rand_forest and calc_relimp will be merged in feature importance function.
#' Relative importance
#' @title Relative importance
#' @param spec,env a data.frame object.
#' @param type one of "lmg", "last", "first", "betasq", "pratt", "genizi" or "car".
#' @param family 	a link function to be used in the model, see \code{\link[stats]{family}}.
#' @param na.action a function which indicates what should happen when the data contain NAs.
#' @param byrow a logical value, if TRUE, the 'spec' on the rows.
#' @param x a calc_relimp object.
#' @param ... extra parameters.
#' @return a calc_relimp object.
#' @importFrom stats glm gaussian na.exclude
#' @rdname calc_relimp
#' @examples \dontrun{
#' spec <- mtcars[c(1, 3, 4, 5)]
#' env <- mtcars[6:11]
#' calc_relimp(spec, env)
#' }
#' @author Hou Yun
#' @export
calc_relimp <- function(spec,
                        env,
                        type = "lmg",
                        family = gaussian,
                        na.action = na.exclude,
                        byrow = TRUE,
                        ...)
{
  type <- match.arg(type, c("lmg", "last", "first", "betasq", "pratt", "genizi", "car"))
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

  calc.relimp <- get_function("relaimpo", "calc.relimp")
  explained <- vector(length = n)
  importance <- p <- NULL

  for (i in seq_len(n)) {
    lm <- stats::glm(spec[[i]] ~ ., data = env, na.action = na.action, family = family)
    sm <- summary(lm)
    cr <- calc.relimp(lm, type = type, ...)
    explained[i] <- extract_s4(cr, "R2") * 100
    if(i == 1) {
      importance <- extract_s4(cr, type)
      p <- sm$coefficients[, "Pr(>|t|)"][-1]
    } else {
      importance <- rbind(importance, extract_s4(cr, type))
      p <- rbind(p, sm$coefficients[, "Pr(>|t|)"][-1])
    }
  }
  rownames(importance) <- rownames(p) <- names(spec)

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
            class = "calc_relimp")
}

#' @method print calc_relimp
#' @rdname calc_relimp
#' @export
print.calc_relimp <- function(x, ...) {
  cat("Var explained (%):\n")
  print(x$explained)
  cat("\n")
  cat("Var importance:\n")
  print(x$importance)
  cat("\n")
  cat("Var importance p value:\n")
  print(x$p)
}

#' @noRd
extract_s4 <- function(x, e) {
  do.call("@", list(x, e))
}
