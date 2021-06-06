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
extract_data <- function(data, mapping) {
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
  tibble::as_tibble(dd)
}

#' @noRd
xrange <- function(.plot) {
  ggplot2::layer_scales(.plot)[["x"]]$range$range
}

#' @noRd
yrange <- function(.plot) {
  ggplot2::layer_scales(.plot)[["y"]]$range$range
}

#' @noRd
`%||%` <- function(a, b) {
  if(is.null(a)) b else a
}
