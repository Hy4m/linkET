#' Mantel and partial mantel test for dissimilarity matrices
#' @title Mantel test
#' @param spec,env data frame object.
#' @param group vector for grouping the rows.
#' @param env_ctrl NULL (default), TRUE or a data frame.
#' @param mantel_fun string, function of mantel test.
#'    \itemize{
#'      \item{\code{"mantel"} will use \code{vegan::mantel()} (default).}
#'      \item{\code{"mantel.randtest"} will use \code{ade4::mantel.randtest()}.}
#'      \item{\code{"mantel.rtest"} will use \code{ade4::mantel.rtest()}.}
#'      \item{\code{"mantel.partial"} will use \code{vegan::mantel.partial()}.}
#'   }
#' @param spec_select,env_select NULL (default), numeric or character vector index of columns.
#' @param na_omit if TRUE (default), the incomplete cases will be removed.
#' @param spec_dist NULL (default) or \code{dist_func()}.
#' @param env_dist NULL (default) or \code{dist_func()}.
#' @param env_ctrl_dist NULL (default) or \code{dist_func()}.
#' @param seed a integer value.
## @param spec_all_dist_method has been droped.
## @param env_all_dist_method has been droped.
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
#'
#' ## set group id of `spec` on columns by `spec_select`
#' mantel_test(varespec, varechem,
#'   spec_select = list(spec01 = 1:5, spec02 = 6:12))
#'
#' ## set group id of `spec` on columns by `spec_select`
#' ## set group id of `env` on columns by `env_select`
#' mantel_test(varespec, varechem,
#'   spec_select = list(spec01 = 1:5, spec02 = 6:12),
#'   env_select = list(env01 = 1:5, env02 = 6:10, env03 = 11:14))
#'
#' ## set group id of all data on rows by `group`
#' set.seed(20211123)
#' sam_grp <- sample(paste0("sample", 1:3), 24, replace = TRUE)
#' mantel_test(varespec, varechem, group = sam_grp)
#'
#' ## partial mantel test
#' m <- matrix(rnorm(24*5), nrow = 24)
#' mantel_test(varespec, varechem, env_ctrl = m,
#' mantel_fun = "mantel.partial")
#' }
#' @author Hou Yun
#' @export
mantel_test <- function(spec,
                        env,
                        group = NULL,
                        env_ctrl = NULL, # named list if grouped
                        mantel_fun = "mantel",
                        spec_select = NULL, # a list of index vector
                        env_select = NULL,
                        na_omit = TRUE,
                        spec_dist = NULL,
                        env_dist = NULL,
                        env_ctrl_dist = NULL,
                        seed = 123,
                        # spec_all_dist_method,
                        # env_all_dist_method,
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
    if(is.null(env_ctrl)) {
      stop("Did you forget to set the 'env_ctrl' param?", call. = FALSE)
    }

    if(!is.data.frame(env_ctrl) && !is.list(env_ctrl) && !isTRUE(env_ctrl)) {
      env_ctrl <- as.data.frame(env_ctrl)
    }

    if(is.data.frame(env_ctrl)) {
      if(nrow(env_ctrl) != nrow(spec)) {
        stop("'env_ctrl' must have the same rows as 'spec'.", call. = FALSE)
      }
    }
  }

  # if(!missing(spec_all_dist_method)) {
  #   warning("'spec_all_dist_method' parameter has been deprecated,\n",
  #           "please use 'spec_dist' parameter instead.", call. = FALSE)
  # }
  # if(!missing(env_all_dist_method)) {
  #   warning("'env_all_dist_method' parameter has been deprecated,\n",
  #           "please use 'env_dist' parameter instead.", call. = FALSE)
  # }

  if(!is.null(group)) {
    # ## TODO: set group by regex
    # if(length(group) != nrow(spec)) {
    #   stop("Length of 'group' and rows of 'spec' must be same.", call. = FALSE)
    # }

    spec <- split_by_group(spec, group)
    env <- split_by_group(env, group)

    if(mantel_fun == "mantel.partial") {
      if(is.data.frame(env_ctrl)) {
        env_ctrl <- split_by_group(env_ctrl, group)
      }
      if(is.list(env_ctrl)) {
        env_ctrl <- env_ctrl[names(spec)]
      } else {
        env_ctrl <- rep_len(env_ctrl, length(spec))
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
                     na_omit = na_omit,
                     seed = seed,
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
                       na_omit = na_omit,
                       seed = seed,
                       ...)
  }
  attr(df, "grouped") <- if(!is.null(group)) TRUE else FALSE
  df
}

#' @noRd
.mantel_test <- function(spec,
                         env,
                         env_ctrl = NULL, # named list if grouped
                         mantel_fun = "mantel",
                         spec_select = NULL, # a list of index vector
                         env_select = NULL,
                         na_omit = TRUE,
                         spec_dist = NULL,
                         env_dist = NULL,
                         env_ctrl_dist = NULL,
                         seed = 123,
                         ...)
{
  .FUN <- switch (mantel_fun,
                  mantel = get_function("vegan", "mantel"),
                  mantel.partial = get_function("vegan", "mantel.partial"),
                  mantel.randtest = get_function("ade4", "mantel.randtest"),
                  mantel.rtest = get_function("ade4", "mantel.rtest"),
                  stop("Invalid 'mantel_fun' parameter.", call. = FALSE)
  )

  if(!is.list(spec_select) && !is.null(spec_select))
    stop("'spec_select' needs a list or NULL.", call. = FALSE)
  if(!is.list(env_select) && !is.null(env_select))
    stop("'env_select' needs a list or NULL.", call. = FALSE)
  if(is.null(spec_select)) {
    spec_select <- list(spec = 1:ncol(spec))
  }
  if(is.null(env_select)) {
    env_select <- as.list(stats::setNames(1:ncol(env), names(env)))
  }

  if(mantel_fun == "mantel.partial") {
    env_ctrl <- check_env_ctrl(env, env_ctrl, env_select)
  }

  spec_select <- make_list_names(spec_select, "spec")
  env_select <- make_list_names(env_select, "env")
  spec_name <- rep(names(spec_select), each = length(env_select))
  env_name <- rep(names(env_select), length(spec_select))

  set.seed(seed)
  seeds <- 10000 * round(runif(length(spec_name)))

  spec <- purrr::map(spec_select, function(.x) {
    if (is.function(.x)) {
      .x(spec)
    } else {
      subset(spec, select = .x, drop = FALSE)
    }})
  env <- purrr::map(env_select, function(.x) {
    if (is.function(.x)) {
      .x(env)
    } else {
      subset(env, select = .x, drop = FALSE)
    }})

  if (!is.function(spec_dist)) {
    if(is.null(spec_dist)) {

      all_spec_is_numeric <- all(vapply(spec, function(x) {
        all(vapply(x, is.numeric, logical(1)))
      }, logical(1)))

      if(all_spec_is_numeric) {
        ## detect rowsum is zero?
        rowsum_is_zero <- vapply(spec, function(x) {
          any(rowSums(x) == 0)
        }, logical(1))
        if (any(rowsum_is_zero)) {
          message("`mantel_test()` using 'euclidean' dist method for 'spec'.")
          spec_dist <- dist_func(.FUN = "vegdist", method = "euclidean")
        } else {
          message("`mantel_test()` using 'bray' dist method for 'spec'.")
          spec_dist <- dist_func(.FUN = "vegdist", method = "bray")
        }
      } else {
        message("`mantel_test()` using 'gower' dist method for 'spec'.")
        spec_dist <- dist_func(.FUN = "gowdis")
      }
    } else {
      dist_of_spec <- match.arg(spec_dist, all_dist_method)
      if (spec_dist == "gower") {
        spec_dist <- dist_func(.FUN = "gowdis")
      } else if (spec_dist %in% c("manhattan", "euclidean", "canberra",
                                  "maximum", "binary" ,"minkowski")) {
        spec_dist <- dist_func(.FUN = "dist", method = dist_of_spec)
      } else {
        spec_dist <- dist_func(.FUN = "vegdist", method = dist_of_spec)
      }
    }
  }

  if (!is.function(env_dist)) {
    if(is.null(env_dist)) {
      all_env_is_numeric <- all(vapply(env, function(x) {
        all(vapply(x, is.numeric, logical(1)))
      }, logical(1)))

      if(all_env_is_numeric) {
        message("`mantel_test()` using 'euclidean' dist method for 'env'.")
        env_dist <- dist_func(.FUN = "vegdist", method = "euclidean")
      } else {
        message("`mantel_test()` using 'gower' dist method for 'env'.")
        env_dist <- dist_func(.FUN = "gowdis")
      }
    } else {
      dist_of_env <- match.arg(env_dist, all_dist_method)
      if (env_dist == "gower") {
        env_dist <- dist_func(.FUN = "gowdis")
      } else if (env_dist %in% c("manhattan", "euclidean", "canberra",
                                 "maximum", "binary" ,"minkowski")) {
        env_dist <- dist_func(.FUN = "dist", method = dist_of_env)
      } else {
        env_dist <- dist_func(.FUN = "vegdist", method = dist_of_env)
      }
    }
  }

  rp <- purrr::pmap(list(spec_name, env_name, seeds), function(.x, .y, .seed) {
    .spec <- spec[[.x]]
    .env <- env[[.y]]
    if(mantel_fun == "mantel.partial") {
      .env_ctrl <- env_ctrl[[.y]]
      if (!is.function(env_ctrl_dist)) {
        if(is.null(env_ctrl_dist)) {
          if(all(vapply(.env_ctrl, is.numeric, logical(1)))) {
            message("`mantel_test()` using 'euclidean' dist method for 'env_ctrl'.")
            env_ctrl_dist <- dist_func(.FUN = "vegdist", method = "euclidean")
          } else {
            message("`mantel_test()` using 'gowdis' dist method for 'env_ctrl'.")
            env_ctrl_dist <- dist_func(.FUN = "gowdis")
          }
        } else {
          dist_of_ctrl <- match.arg(env_ctrl_dist, all_dist_method)
          if (env_ctrl_dist == "gower") {
            env_ctrl_dist <- dist_func(.FUN = "gowdis")
          } else if (env_ctrl_dist %in% c("manhattan", "euclidean", "canberra",
                                          "maximum", "binary" ,"minkowski")) {
            env_ctrl_dist <- dist_func(.FUN = "dist", method = dist_of_ctrl)
          } else {
            env_ctrl_dist <- dist_func(.FUN = "vegdist", method = dist_of_ctrl)
          }
        }
      }
    }

    if(isTRUE(na_omit)) {
      non_na <- complete.cases(.spec) & complete.cases(.env)
      if(mantel_fun == "mantel.partial") {
        non_na <- non_na & complete.cases(.env_ctrl)
        .env_ctrl <- .env_ctrl[non_na, , drop = FALSE]
      }
      .spec <- .spec[non_na, , drop = FALSE]
      .env <- .env[non_na, , drop = FALSE]
    }

    spec_dist <- spec_dist(.spec)
    env_dist <- env_dist(.env)

    set.seed(.seed)
    if(mantel_fun == "mantel.partial") {
      env_ctrl_dist <- env_ctrl_dist(.env_ctrl)
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

#' @noRd
check_env_ctrl <- function(env, env_ctrl, env_select) {
  if(is.null(env_ctrl)) {
    stop("Did you forget to set the 'env_ctrl' param?", call. = FALSE)
  }

  if(isTRUE(env_ctrl)) {
    env_select <- make_list_names(env_select, "env")
    env_list <- purrr::map(env_select, function(.x) {
      if (is.function(.x)) {
        .x(env)
      } else {
        subset(env, select = .x, drop = FALSE)
      }})
    name_list <- purrr::map(env_list, function(.env) {
      setdiff(names(env), names(.env))
    })
    if(any(purrr::map_dbl(name_list, length) < 1)) {
      stop("Zero length error: `env_ctrl` is empty.", call. = FALSE)
    }
    env_ctrl <- purrr::map(name_list, function(.name) {
      subset(env, select = .name, drop = FALSE)})
  }

  if(!is.data.frame(env_ctrl) && !is.list(env_ctrl)) {
    env_ctrl <- as.data.frame(env_ctrl)
  }

  if(is.data.frame(env_ctrl)) {
    if(nrow(env_ctrl) != nrow(env)) {
      stop("'env_ctrl' must have the same rows as 'env'.", call. = FALSE)
    }
    env_ctrl <- rlang::set_names(rep_len(list(env_ctrl), length(env_select)), names(env_select))
  }

  env_ctrl
}

#' @noRd
all_dist_method <- c(
  "manhattan", "euclidean", "canberra", "clark", "bray", "kulczynski",
  "jaccard", "gower", "altGower", "morisita", "horn", "mountford", "raup",
  "binomial", "chao", "cao", "mahalanobis", "chisq", "chord", "hellinger",
  "aitchison", "robust.aitchison", "maximum", "binary", "minkowski"
)

#' @title Subset columns using regular expression
#' Subset columns from a data frame if column names match the regular pattern.
#' @param data a data frame.
#' @param chr a character used to match the column names.
#' @param prefix prefix of column names.
#' @param suffix suffix of column names.
#' @param regex other string (or regular expression) in column names.
#' @param ... other parameters passing to \code{grepl}.
#' @param byrow logical, if TRUE will select rows based on regex.
#' @return a data frame.
#' @rdname regex_select
#' @author Hou yun
#' @examples
#' prefix_with(mtcars, "m")
#' @export
regex_select <- function(prefix = NULL,
                         suffix = NULL,
                         regex = NULL,
                         ...,
                         byrow = FALSE) {
  if (is.null(prefix %||% suffix %||% regex)) {
    stop("At least one pattern is not null.", call. = FALSE)
  }
  if (!is.null(prefix)) {
    regex <- paste0("^", prefix)
  } else if (!is.null(suffix)) {
    regex <- paste0(suffix, "$")
  } else {
    regex <- regex
  }

  function(data) {
    if (!is.data.frame(data)) {
      data <- as.data.frame(data)
    }
    if (empty(data)) {
      stop("Empty data.", call. = FALSE)
    }

    if (isTRUE(byrow)) {
      id <- rownames(data)[Reduce("|", lapply(regex, function(.regex) {
        grepl(pattern = .regex, x = rownames(data), ...)
      }))]
      data[id, , drop = FALSE]
    } else {
      id <- colnames(data)[Reduce("|", lapply(regex, function(.regex) {
        grepl(pattern = .regex, x = colnames(data), ...)
      }))]
      data[, id, drop = FALSE]
    }
  }
}

#' @rdname regex_select
#' @export
prefix_with <- function(data, chr = "", ..., byrow = FALSE) {
  FUN <- regex_select(prefix = chr, ..., byrow = byrow)
  FUN(data)
}

#' @rdname regex_select
#' @export
suffix_with <- function(data, chr = "", ..., byrow = FALSE) {
  FUN <- regex_select(suffix = chr, ..., byrow = byrow)
  FUN(data)
}

#' @rdname regex_select
#' @export
contain_with <- function(data, chr = "", ..., byrow = FALSE) {
  FUN <- regex_select(regex = chr, ..., byrow = byrow)
  FUN(data)
}
