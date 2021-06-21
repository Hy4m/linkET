#' Make rand dataset
#' @title Make dataset
#' @param rows,cols number of rows and columns.
#' @param vars number of variables.
#' @param obs number of observation.
#' @param reorder reorder the dataset at random.
#' @param row_names,col_names row/column names.
#' @param type one of "perlin", "cubic", "simplex", "value", "worley" or "white".
#' @param frequency determines the granularity of the features in the noise.
#' @param seed an integer value or NULL.
#' @param ... extra parameters.
#' @return a correlate object or data.frame.
#' @rdname rand_correlate
#' @author Hou Yun
#' @export
rand_correlate <- function(rows = 12,
                           cols = NULL,
                           obs = 100,
                           row_names = NULL,
                           col_names = NULL,
                           type = "cubic",
                           frequency = 0.15,
                           reorder = TRUE,
                           seed = NULL,
                           ...)
{
  if(is.null(cols)) {
    row_names <- row_names %||% col_names %||% paste0("row", 1:rows)
    col_names <- col_names %||% row_names
  } else {
    row_names <- row_names %||% paste0("row", 1:rows)
    col_names <- col_names %||% paste0("col", 1:cols)
  }
  d1 <- rand_dataset(vars = rows, obs = obs, reorder = reorder,
                     col_names = row_names, type = type,
                     frequency = frequency, seed = seed)
  if(is.null(cols)) {
    d2 <- d1
  } else {
    d2 <- rand_dataset(vars = cols, obs = obs, reorder = reorder,
                       col_names = col_names, type = type,
                       frequency = frequency, seed = seed)
  }
  correlate(x = d1, y = d2, ...)
}

#' @rdname rand_correlate
#' @export
rand_dataset <- function(vars = 12,
                         obs = 100,
                         reorder = TRUE,
                         row_names = NULL,
                         col_names = NULL,
                         type = "cubic",
                         frequency = 0.15,
                         seed = NULL,
                         ...) {
  if(!requireNamespace("ambient", quietly = TRUE)) {
    stop("'rand_dataset()' needs 'ambient' package.", call. = TRUE)
  }
  type <- match.arg(type, c("perlin", "cubic", "simplex", "value", "worley", "white"))
  f <- get_function("ambient", paste0("noise_", type))

  row_names <- row_names %||% paste0("sample", 1:obs)
  col_names <- col_names %||% paste0("var", 1:vars)

  if(is.null(seed)) {
    seed <- as.numeric(Sys.time())
  }
  set.seed(seed)
  m <- do.call(f, modifyList(list(dim = c(obs, vars), frequency = frequency),
                              list(...))) * 100
  if(isTRUE(reorder)) {
    set.seed(seed)
    row.ord <- sample(obs)
    set.seed(seed)
    col.ord <- sample(vars)
    m <- m[row.ord, col.ord, drop = FALSE]
  }
  n <- max(floor(frequency * vars), floor(0.4 * vars))
  if(n >= 1) {
    set.seed(seed)
    id <- sample(vars, n)
    m[, id] <- -m[, id]
  }

  rownames(m) <- row_names
  colnames(m) <- col_names
  as.data.frame(m)
}
