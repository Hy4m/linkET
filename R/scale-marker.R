#' @title Discrete and continuous marker scales
#' @description marker scales provide discrete and continuous marker scales.
#' @return a Scale object.
#' @family scale_marker_*
#' @name scale_marker
#' @rdname scale_marker_discrete
NULL

#' @rdname scale_marker_discrete
#' @inheritParams ggplot2::scale_shape_discrete
#' @importFrom ggplot2 discrete_scale
#' @export
scale_marker_discrete <- function(...) {
  discrete_scale("marker", "marker_d", marker_pal(), ...)
}

#' @rdname scale_marker_discrete
#' @inheritParams ggplot2::scale_shape_continuous
#' @export
scale_marker_continuous <- function (...)
{
  stop("A continuous variable can not be mapped to marker.", call. = FALSE)
}

#' @rdname scale_marker_discrete
#' @inheritParams ggplot2::scale_shape_identity
#' @importFrom ggplot2 ScaleContinuousIdentity
#' @export
scale_marker_identity <- function(..., guide = "none") {
  discrete_scale("marker", "identity", scales::identity_pal(),
                 ..., guide = guide, super = ScaleContinuousIdentity)
}

#' @rdname scale_marker_discrete
#' @inheritParams ggplot2::scale_shape_manual
#' @export
scale_marker_manual <- function(..., values, breaks = waiver(), na.value = NA) {
  manual_scale <- utils::getFromNamespace("manual_scale", "ggplot2")
  manual_scale("marker", values, breaks, ..., na.value = na.value)
}

#' @rdname scale_marker_discrete
#' @inheritParams ggplot2::scale_shape_binned
#' @importFrom ggplot2 binned_scale
#' @export
scale_marker_binned <- function(...) {
  binned_pal <- utils::getFromNamespace("binned_pal", "ggplot2")
  binned_scale("marker", "marker_b", binned_pal(marker_pal()), ...)
}

#' @noRd
marker_pal <- function ()
{
  function(n) {
    if (n > length(all_type)) {
      warning(paste0("The marker palette can deal with a maximum of ",
                     length(all_type), " discrete."), call. = FALSE)
    }
    all_type[seq_len(n)]
  }
}


