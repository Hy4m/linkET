#' @title Secondary and Nested Axis
#' @description functions to add secondary and nested axis.
#' @param ... other parameters passing to \code{ggh4x::guide_axis_manual()} or
#' \code{ggh4x::guide_axis_nested()}.
#' @param position where this guide should be drawn: one of top, bottom, left, or right.
#' @param position_aes one of "x" or "y".
#' @return a guide object.
#' @rdname axis
#' @importFrom ggplot2 guides
#' @importFrom ggplot2 waiver
#' @author Hou Yun
#' @export
#' @examples \dontrun{
#' as_md_tbl(mtcars) %>%
#' hyplot() +
#'   geom_point() +
#'   secondary_x_axis()
#' }
secondary_x_axis <- function(...) {
  set_secondary_axis(..., position_aes = "x")
}

#' @rdname axis
#' @export
secondary_y_axis <- function(...) {
  set_secondary_axis(..., position_aes = "y")
}

#' @rdname axis
#' @export
set_secondary_axis <- function(...,
                               position = waiver(),
                               position_aes = "x"
                               ) {
  structure(list(position = position,
                 position_aes = position_aes,
                 ...), class = "secondary_axis")
}

#' @method ggplot_add secondary_axis
#' @export
ggplot_add.secondary_axis <- function(object, plot, object_name) {
  stopifnot(is_hyplot(plot))
  guide_axis_manual <- get_function("ggh4x", "guide_axis_manual")
  md <- plot$data
  type <- attr(md, "type")
  position_aes <- object$position_aes
  object <- object[setdiff(names(object), "position_aes")]
  if(position_aes == "x") {
    object$position <- switch(type,
                              full = "top",
                              upper = "bottom",
                              lower = "top")
    object <- do.call(guide_axis_manual, object)
    object <- guides(x.sec = object)
  } else {
    object$position <- switch(type,
                              full = "right",
                              upper = "left",
                              lower = "right")
    object <- do.call(guide_axis_manual, object)
    object <- guides(y.sec = object)
  }

  ggplot_add(object, plot, object_name)
}
