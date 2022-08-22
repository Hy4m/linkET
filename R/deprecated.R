#' @rdname correlate
#' @export
fast_correlate <- function(...)
{
  stop("fast_correlate() is soft deprecated,\n",
       "please use correlate(..., engine = \"WGCNA\") instead.",
       call. = FALSE)
}

#' @rdname correlate
#' @export
fast_correlate2 <- function (...)
{
  ## Note: will be removed in next version
  stop("fast_correlate2() is soft deprecated,\n",
       "please use correlate(..., engine = \"picante\") instead.",
       call. = FALSE)
}

#' @rdname geom_couple
#' @export
anno_link <- function(...) {
  ## Note: will be removed in next version
  stop("`anno_link()` has been deprecated,\n",
       "please use `geom_couple()` instead.",
       call. = FALSE)
}
