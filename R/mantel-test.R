#' Mantel and partial mantel test for dissimilarity matrices
#' @title Mantel test
#' @param spec,env data frame object.
#' @param group vector for grouping the rows.
#' @param env_ctrl NULL (default), data frame.
#' @param mantel_fun string, function of mantel test.
#'    \itemize{
#'      \item{\code{"mantel"} will use \code{vegan::mantel()} (default).}
#'      \item{\code{"mantel.randtest"} will use \code{ade4::mantel.randtest()}.}
#'      \item{\code{"mantel.rtest"} will use \code{ade4::mantel.rtest()}.}
#'      \item{\code{"mantel.partial"} will use \code{vegan::mantel.partial()} (default).}
#'   }
#' @param spec_select,env_select NULL (default), numeric or character vector index of columns.
#' @param use one of "everything", "complete" or "pairwise".
#' @param spec_dist NULL (default) or \code{dist_func()}.
#' @param env_dist NULL (default) or \code{dist_func()}.
#' @param env_ctrl_dist NULL (default) or \code{dist_func()}.
#' @param env_dist_method has been droped.
#' @param seed a integer value.
#' @param spec_dist_method has been droped.
#' @param env_dist_method has been droped.
#' @param ... extra params passing to \code{mantel_fun}.
#' @return a data.frame.
#' @importFrom dplyr mutate
#' @importFrom purrr pmap pmap_dfr
#' @importFrom stats complete.cases runif
#' @rdname mantel_test
#' @examples \dontrun{
#' library(vegan)
#' data("varespec")
#' data("varechem")
#' mantel_test(varespec, varechem,
#'   spec_select = list(spec01 = 1:5, spec02 = 6:12))
#' mantel_test(varespec, varechem,
#'   spec_select = list(spec01 = 1:5, spec02 = 6:12),
#'   env_select = list(env01 = 1:5, env02 = 6:10, env03 = 11:14))
#' set.seed(20191224)
#' sam_grp <- sample(paste0("sample", 1:3), 24, replace = TRUE)
#' mantel_test(varespec, varechem, group = sam_grp)
#' }
#' @seealso \code{\link{mantel_test}}.
#' @author Hou Yun
#' @export
mantel_test <- function(spec,
                        env,
                        group = NULL,
                        env_ctrl = NULL, # named list if grouped
                        mantel_fun = "mantel",
                        spec_select = NULL, # a list of index vector
                        env_select = NULL,
                        use = "everything",
                        spec_dist = NULL,
                        env_dist = NULL,
                        env_ctrl_dist = NULL,
                        seed = 123,
                        spec_dist_method,
                        env_dist_method,
                        ...)
{
  if(!is.data.frame(spec))
    spec <- as.data.frame(spec)
  if(!is.data.frame(env))
    env <- as.data.frame(env)
  if(nrow(spec) != nrow(env)) {
    stop("'spec' must have the same rows as 'env'.", call. = FALSE)
  }
  if(mantel_fun == "mantel.partial") {
    if(is.null(env_ctrl))
      stop("Did you forget to set the 'env_ctrl' param?", call. = FALSE)
    if(!is.data.frame(env_ctrl) && !is.list(env_ctrl))
      stop("'env_ctrl' needs a list or data.frame.", call. = FALSE)
  }
  if(!is.null(group)) {
    if(length(group) != nrow(spec))
      stop("Length of 'group' and rows of 'spec' must be same.", call. = FALSE)
    spec <- split(spec, group, drop = FALSE)
    env <- split(env, group, drop = FALSE)
    if(mantel_fun == "mantel.partial") {
      if(is.data.frame(env_ctrl)) {
        env_ctrl <- rep_len(list(env_ctrl), length(spec))
      } else {
        env_ctrl <- env_ctrl[names(spec)]
      }
    } else {
      env_ctrl <- as.list(rep(NA, length(spec)))
    }
    df <- purrr::pmap_dfr(
      list(spec, env, env_ctrl, as.list(names(spec))),
      function(.spec, .env, .env_ctrl, .group) {
        .mantel_test(spec = .spec,
                     env = .env,
                     env_ctrl = .env_ctrl,
                     mantel_fun = mantel_fun,
                     spec_select = spec_select,
                     env_select = env_select,
                     spec_dist = spec_dist,
                     env_dist = env_dist,
                     env_ctrl_dist = env_ctrl_dist,
                     use = use,
                     seed = seed,
                     spec_dist_method = spec_dist_method,
                     env_dist_method = env_dist_method,
                     ...) %>%
          dplyr::mutate(.group = .group)
      })
  } else {
    df <- .mantel_test(spec = spec,
                       env = env,
                       env_ctrl = env_ctrl,
                       mantel_fun = mantel_fun,
                       spec_select = spec_select,
                       env_select = env_select,
                       spec_dist = spec_dist,
                       env_dist = env_dist,
                       env_ctrl_dist = env_ctrl_dist,
                       use = use,
                       seed = seed,
                       spec_dist_method = spec_dist_method,
                       env_dist_method = env_dist_method,
                       ...)
  }
  grouped <- if(!is.null(group)) TRUE else FALSE
  attr(df, "grouped") <- grouped
  df
}

#' @noRd
.mantel_test <- function(spec,
                         env,
                         env_ctrl = NULL, # named list if grouped
                         mantel_fun = "mantel",
                         spec_select = NULL, # a list of index vector
                         env_select = NULL,
                         use = "everything",
                         spec_dist = NULL,
                         env_dist = NULL,
                         env_ctrl_dist = NULL,
                         seed = 123,
                         spec_dist_method,
                         env_dist_method,
                         ...)
{
  .FUN <- switch (mantel_fun,
                  mantel = get_function("vegan", "mantel"),
                  mantel.partial = get_function("vegan", "mantel.partial"),
                  mantel.randtest = get_function("ade4", "mantel.randtest"),
                  mantel.rtest = get_function("ade4", "mantel.rtest"),
                  stop("Invalid 'mantel_fun' parameter.", call. = FALSE)
  )

  use <- match.arg(use, c("everything", "complete", "pairwise"))

  if(!is.data.frame(spec))
    spec <- as.data.frame(spec)
  if(!is.data.frame(env))
    env <- as.data.frame(env)
  if(nrow(spec) != nrow(env)) {
    stop("'spec' must have the same rows as 'env'.", call. = FALSE)
  }
  if(mantel_fun == "mantel.partial") {
    if(is.null(env_ctrl))
      stop("Did you forget to set the 'env_ctrl' param?", call. = FALSE)
    if(!is.data.frame(env_ctrl))
      env_ctrl <- as.data.frame(env_ctrl)
  }
  if(!is.list(spec_select) && !is.null(spec_select))
    stop("'spec_select' needs a list or NULL.", call. = FALSE)
  if(!is.list(env_select) && !is.null(env_select))
    stop("'env_select' needs a list or NULL.", call. = FALSE)
  if(is.null(spec_select)) {
    spec_select <- list(spec = 1:ncol(spec))
  }
  if(is.null(env_select)) {
    env_select <- as.list(setNames(1:ncol(env), names(env)))
  }

  if(use == "complete") {
    non_na <- complete.cases(spec) & complete.cases(env)
    if(mantel_fun == "mantel.partial") {
      non_na <- non_na & complete.cases(env_ctrl)
    }
    spec <- spec[non_na, , drop = FALSE]
    env <- env[non_na, , drop = FALSE]
    if(mantel_fun == "mantel.partial") {
      env_ctrl <- env_ctrl[non_na, , drop = FALSE]
    }
  }

  if(is.null(spec_dist)) {
    if(all(vapply(spec, is.numeric, logical(1)))) {
      message("`mantel_test()` using 'bray' dist method for 'spec'.")
      spec_dist <- dist_func(.FUN = "vegdist", method = "bray")
    } else {
      message("`mantel_test()` using 'gower' dist method for 'spec'.")
      spec_dist <- dist_func(.FUN = "gowdis")
    }
  }

  if(is.null(env_dist)) {
    if(all(vapply(env, is.numeric, logical(1)))) {
      message("`mantel_test()` using 'euclidean' dist method for 'env'.")
      env_dist <- dist_func(.FUN = "vegdist", method = "euclidean")
    } else {
      message("`mantel_test()` using 'gower' dist method for 'env'.")
      env_dist <- dist_func(.FUN = "gowdis")
    }
  }

  spec_select <- make_list_names(spec_select, "spec")
  env_select <- make_list_names(env_select, "env")
  spec_name <- rep(names(spec_select), each = length(env_select))
  env_name <- rep(names(env_select), length(spec_select))

  set.seed(seed)
  seeds <- 10000 * round(runif(length(spec_name)))

  spec <- purrr::map(spec_select, function(.x) {
    subset(spec, select = .x, drop = FALSE)})
  env <- purrr::map(env_select, function(.x) {
    subset(env, select = .x, drop = FALSE)})

  rp <- purrr::pmap(list(spec_name, env_name, seeds), function(.x, .y, .seed) {
    .spec <- spec[[.x]]
    .env <- env[[.y]]
    if(use == "pairwise") {
      non_na <- complete.cases(.spec) & complete.cases(.env)
      if(mantel_fun == "mantel.partial") {
        non_na <- non_na & complete.cases(env_ctrl)
      }
      .spec <- .spec[non_na, , drop = FALSE]
      .env <- .env[non_na, , drop = FALSE]
      if(mantel_fun == "mantel.partial") {
        env_ctrl <- env_ctrl[non_na, , drop = FALSE]
      }
    }

    spec_dist <- spec_dist(.spec)
    env_dist <- env_dist(.env)

    set.seed(.seed)
    if(mantel_fun == "mantel.partial") {
      if(is.null(env_ctrl_dist)) {
        if(all(vapply(env_ctrl, is.numeric, logical(1)))) {
          message("`mantel_test()` using 'euclidean' dist method for 'env_ctrl'.")
          env_ctrl_dist <- dist_func(.FUN = "vegdist", method = "euclidean")
        } else {
          message("`mantel_test()` using 'gower' dist method for 'env_ctrl'.")
          env_ctrl_dist <- dist_func(.FUN = "gowdis")
        }
      }

      env_ctrl_dist <- env_ctrl_dist(env_ctrl)
      .FUN(spec_dist, env_dist, env_ctrl_dist, ...)
    } else {
      .FUN(spec_dist, env_dist, ...)
    }
  }) %>% extract_mantel(mantel_fun)

  structure(.Data = tibble::tibble(spec = spec_name,
                                   env = env_name,
                                   r = rp$r,
                                   p = rp$p),
            grouped = FALSE,
            class = c("mantel_tbl", "tbl_df", "tbl", "data.frame"))
}

#' @importFrom purrr map_dbl
#' @noRd
extract_mantel <- function(x, .f = "mantel") {
  .f <- match.arg(.f, c("mantel", "mantel.partial",
                        "mantel.randtest", "mantel.rtest"))
  if(.f %in% c("mantel", "mantel.partial")) {
    r <- purrr::map_dbl(x, `[[`, "statistic")
    p <- purrr::map_dbl(x, `[[`, "signif")
  } else {
    r <- purrr::map_dbl(x, `[[`, "obs")
    p <- purrr::map_dbl(x, `[[`, "pvalue")
  }
  list(r = r, p = p)
}
