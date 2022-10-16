#' @noRd
get_function <- function (pkg, fun)
{
  if (!requireNamespace(pkg, quietly = TRUE)) {
    stop(pkg, " package has not been installed", call. = FALSE)
  }
  eval(parse(text = paste0(pkg, "::", fun)))
}

#' @noRd
aes_modify <- function (aes1, aes2)
{
  aes <- modifyList(as.list(aes1), as.list(aes2))
  class(aes) <- "uneval"
  aes
}

# Note: copy from ggplot2 (3.3.6)
#' @noRd
new_data_frame <- function (x = list(), n = NULL)
  {
    if (length(x) != 0 && is.null(names(x))) {
      stop("Elements must be named", call. = FALSE)
    }
    lengths <- vapply(x, length, integer(1))
    if (is.null(n)) {
      n <- if (length(x) == 0 || min(lengths) == 0)
        0
      else max(lengths)
    }
    for (i in seq_along(x)) {
      if (lengths[i] == n)
        next
      if (lengths[i] != 1) {
        stop("Elements must equal the number of rows or 1", call. = FALSE)
      }
      x[[i]] <- rep(x[[i]], n)
    }
    class(x) <- "data.frame"
    attr(x, "row.names") <- .set_row_names(n)
    x
  }

#' @noRd
aes_vars <- function(mapping, vars) {
  if(vars %in% names(mapping)) {
    rlang::as_name(mapping[[vars]])
  } else {
    NULL
  }
}

#' @noRd
`%||%` <- function(a, b) {
  if(is.null(a)) b else a
}

#' @noRd
#' @noRd
make_list_names <- function(x, pre = "X", sep = "")
{
  stopifnot(is.list(x))
  n <- length(x)
  name <- names(x)
  if(!is.null(name) && all(name != "" & !is.na(name)))
    return(x)
  if(is.null(x)) {
    names(x) <- paste0(pre, sep, seq_len(n))
  }
  if(all(name == "" | is.na(name))) {
    names(x) <- paste0(pre, sep, seq_len(n))
  } else {
    idx <- name == "" | is.na(name)
    name[idx] <- paste0(pre, sep, sum(idx))
    names(x) <- make.unique(name)
  }
  return(x)
}

#' @noRd
empty <- function (df)
{
  if (inherits(df, "data.frame") || inherits(df, "matrix")) {
    is.null(df) || nrow(df) == 0 || ncol(df) == 0
  } else {
    is.null(df) || length(df) == 0
  }
}

#' @noRd
ggname <- function (prefix, grob)
{
  grob$name <- grid::grobName(grob, prefix)
  grob
}

#' @noRd
rename <- function(data, ...) {
  ll <- list(...)

  if(length(ll) == 0) {
    data
  } else {
    old <- unname(unlist(ll))
    new <- names(ll)
    names(data)[names(data) %in% old] <- new
  }
  data
}

#' @noRd
r_version <- function() {
  strsplit(R.version.string, " ")[[1]][3]
}

#' @noRd
split_by_group <- function(data, group) {
  n <- nrow(data)
  if (is.list(group)) {
    nm <- names(group) %||% rep("", length(group))
    out <- vector("list", length = length(group))

    for (ii in seq_along(group)) {
      g <- group[[ii]]
      if (is.function(g)) {
        if (nm[ii] == "") {
          stop("Invalid group parameters: should be a named list.", call. = FALSE)
        } else {
          out[[ii]] <- g(data)
        }
      } else {
        if (!is.atomic(g)) {
          stop("Invalid group parameters: should be a named list.", call. = FALSE)
        }
        if (length(g) == 1L) {
          if (nm[ii] == "") {
            nm[ii] <- g
          }
          out[[ii]] <- regex_select(regex = g, byrow = TRUE)(data)
        } else {
          if (nm[ii] == "") {
            nm[ii] <- paste_with_na(g, collapse = "-")
          }
          out[[ii]] <- regex_select(regex = g, byrow = TRUE)(data)
        }
      }
    }
    names(out) <- nm
  } else {
    out <- split(data, group)
  }
  out
}

#' @noRd
df_to_matrix <- function(x,
                         value,
                         row_id = NULL,
                         col_id = NULL,
                         row_names = NULL,
                         col_names = NULL,
                         missing = NA) {
  row_id <- row_id %||% names(x)[1]
  col_id <- col_id %||% names(x)[2]
  rnm <- row_names %||% unique(x[[row_id]])
  cnm <- col_names %||% unique(x[[col_id]])
  ID <- paste(rep(rnm, length(cnm)), rep(cnm, each = length(rnm)), sep = "--")
  vv <- rep(missing, length(ID))
  ii <- match(paste(x[[row_id]], x[[col_id]], sep = "--"), ID)
  vv[ii] <- x[[value]]
  matrix(vv, nrow = length(rnm), ncol = length(cnm),
         dimnames = list(rnm, cnm))
}

#' @noRd
get_facet_vars <- function(plot) {
  if (inherits(plot$facet, "FacetNull")) {
    return(NULL)
  }
  facet <- plot$facet$params$facets
  names(facet)
}
