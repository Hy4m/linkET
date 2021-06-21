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

#' @noRd
set_attrs <- function(x, ...) {
  attrs <- list(...)
  if(length(attrs) == 0L) {
    return(x)
  }
  nm <- names(attrs)
  for(i in nm) {
    attr(x, i) <- attrs[[i]]
  }
  x
}

#' @noRd
new_data_frame <- getFromNamespace("new_data_frame", "ggplot2")

#' @noRd
na_or_value <- function(x, value = 0.2) {
  if(is.null(x)) {
    x <- 0.2
  }
  if(is.na(x)) value else x
}

#' @noRd
get_mapping_vars <- function(mapping, ...) {
  purrr::map_chr(list(...), function(.x) {
    rlang::as_name(mapping[[.x]])
  })
}

#' @noRd
mapping_data <- function(data, mapping) {
  nm <- names(mapping)
  n <- nrow(data)
  if(length(nm) == 0L) return(NULL)
  dd <- purrr::map(mapping, function(.m) {
    if(is.atomic(.m)) {
      .m
    } else {
      rlang::eval_tidy(.m, data)
    }
  })
  names(dd) <- nm
  dd <- tibble::as_tibble(dd)
  if(nrow(dd) != n) {
    dd[rep_len(1L, n), ]
  } else {
    dd
  }
}

#' @noRd
xlimits <- function(.plot) {
  ggplot2::layer_scales(.plot)[["x"]]$limits
}

#' @noRd
ylimits <- function(.plot) {
  ggplot2::layer_scales(.plot)[["y"]]$limits
}

#' @noRd
xrange <- function (.plot)
{
  ggplot2::ggplot_build(.plot)$layout$panel_params[[1]]$x.range
}

#' @noRd
yrange <- function (.plot)
{
  ggplot2::ggplot_build(.plot)$layout$panel_params[[1]]$y.range
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
  is.null(df) || nrow(df) == 0 || ncol(df) == 0
}

#' @noRd
ggname <- function (prefix, grob)
{
  grob$name <- grid::grobName(grob, prefix)
  grob
}
