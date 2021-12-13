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
  is.null(df) || nrow(df) == 0 || ncol(df) == 0
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
is_richtext <- function(x, pattern = NULL) {
  if(is.null(pattern)) {
    pattern <- c("<sub>", "<sup>", "<br>", "<span")
  }
  if(length(pattern) > 1) {
    pattern <- paste(pattern, collapse = "|")
  }
  x <- gsub("\\s+", "", x)
  any(grepl(pattern, x))
}
