#' @title Annotate Layer
#' @description A wrapper funtion to add some annotate elements on ggplot.
#' @param annotate a grob object, or other object can be converted to a grob.
#' @param ... other parameters passing to convert-function.
#' @inheritParams ggplot2::layer
#' @param position string, position of annotate:
#'    \itemize{
#'      \item{\code{"rt"}: right-top corner.}
#'      \item{\code{"rb"}: right-bottom corner.}
#'      \item{\code{"rc"}: right-center.}
#'      \item{\code{"lt"}: left-top corner.}
#'      \item{\code{"lb"}: left-bottom corner.}
#'      \item{\code{"lc"}: left-center.}
#'      \item{\code{"cc"}: center-center.}
#'      \item{\code{"ct"}: center-top.}
#'      \item{\code{"cb"}: center-bottom.}
#'   }
#' @param width,height width/height of annotate.
#' @param nudge_x,nudge_y a minor shift of position, should be a grid::unit object.
#' @return a layer object.
#' @rdname geom_annotate
#' @export
geom_annotate <- function(annotate = NULL,
                          ...,
                          mapping = NULL,
                          data = NULL,
                          stat = "identity",
                          position = "rt",
                          inherit.aes = TRUE,
                          show.legend = FALSE,
                          width = NULL,
                          height = NULL,
                          nudge_x = NULL,
                          nudge_y = NULL,
                          na.rm = FALSE)
{
  if (is.null(mapping)) {
    required_aes <- NULL
  } else {
    required_aes <- names(mapping)
  }
  if (packageVersion("ggplot2") <= "3.3.5") {
    default_aes = aes(xmin = -Inf, xmax = -Inf)
  } else {
    default_aes = aes()
  }

  GeomAnnotate <- ggproto(
    "GeomAnnotate", Geom,
    required_aes = required_aes,
    default_aes = default_aes,
    draw_panel = function(data,
                          panel_params,
                          coord,
                          annotate = NULL,
                          position = "rt",
                          width = NULL,
                          height = NULL,
                          nudge_x = NULL,
                          nudge_y = NULL,
                          params = list(),
                          na.rm = FALSE) {
      params <- c(list(annotate = annotate,
                       position = position,
                       width = width,
                       height = height,
                       nudge_x = nudge_x,
                       nudge_y = nudge_y), params)
      do.call(annotateGrob, params)
    },
    draw_key = ggplot2::draw_key_blank
  )

  layer(
    data = data,
    mapping = mapping,
    stat = stat,
    geom = GeomAnnotate,
    position = "identity",
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      annotate = annotate,
      position = position,
      width = width,
      height = height,
      nudge_x = nudge_x,
      nudge_y = nudge_y,
      params = list(...),
      na.rm = na.rm
    )
  )
}

#' @noRd
annotateGrob <- function(annotate, ...) {
  UseMethod("annotateGrob")
}

#' @noRd
annotateGrob.grob <- function(annotate,
                              position = "rt",
                              width = NULL,
                              height = NULL,
                              nudge_x = 0,
                              nudge_y = 0,
                              default.units = "npc",
                              ...) {
  if (is.null(position)) {
    position <- "rt"
  }
  if (is.list(position)) {
    x_pos <- "l"
    y_pos <- "c"
    x <- position$x
    y <- position$y
    just_x <- 0
    just_y <- 0.5
  } else {
    position <- match.arg(position,
                          as.vector(outer(c("l", "r", "c"),
                                          c("t", "b", "c"),
                                          paste0)))
    x_pos <- unlist(strsplit(position, ""))[1]
    y_pos <- unlist(strsplit(position, ""))[2]
    x <- just_x <- switch(x_pos, "l" = 0, "r" = 1, "c" = 0.5)
    y <- just_y <- switch(y_pos, "t" = 1, "b" = 0, "c" = 0.5)
  }

  if (!grid::is.unit(x)) {
    x <- grid::unit(x, "npc")
  }
  if (!grid::is.unit(y)) {
    y <- grid::unit(y, "npc")
  }

  nudge_x <- abs(nudge_x %||% 0)
  nudge_y <- abs(nudge_y %||% 0)

  grid::gTree(annotate = annotate,
              x = x,
              y = y,
              width = width,
              height = height,
              position = c(x_pos, y_pos),
              just = c(just_x, just_y),
              nudge_x = nudge_x,
              nudge_y = nudge_y,
              default.units = default.units,
              cl = "annotateGrob")
}

#' @noRd
annotateGrob.character <- function(annotate,
                                   position = "rt",
                                   width = NULL,
                                   height = NULL,
                                   nudge_x = 0,
                                   nudge_y = 0,
                                   default.units = "npc",
                                   ...) {
  annotate <- paste_with_na(annotate, collapse = "\n")
  annotate <- grid::textGrob(annotate,
                             x = 0.5,
                             y = 0.5,
                             just = "center",
                             ...)
  annotateGrob(annotate = annotate,
               position = position,
               width = width,
               height = height,
               nudge_x = nudge_x,
               nudge_y = nudge_y,
               default.units = default.units)
}

#' @noRd
annotateGrob.raster <- function(annotate,
                                position = "rt",
                                width = 0.5,
                                height = 0.5,
                                nudge_x = 0,
                                nudge_y = 0,
                                default.units = "npc",
                                ...) {
  annotate <- grid::rasterGrob(annotate,
                               x = 0.5,
                               y = 0.5,
                               just = "center",
                               ...)
  annotateGrob(annotate = annotate,
               position = position,
               width = width,
               height = height,
               nudge_x = nudge_x,
               nudge_y = nudge_y,
               default.units = default.units)
}

#' @noRd
`annotateGrob.magick-image` <- function(annotate,
                                        position = "rt",
                                        width = 0.5,
                                        height = 0.5,
                                        nudge_x = 0,
                                        nudge_y = 0,
                                        default.units = "npc",
                                        ...) {
  annotate <- grDevices::as.raster(annotate)
  annotateGrob(annotate = annotate,
               position = position,
               width = width,
               height = height,
               nudge_x = nudge_x,
               nudge_y = nudge_y,
               default.units = default.units,
               ...)
}

#' @noRd
annotateGrob.ggplot <- function(annotate,
                                position = "rt",
                                width = 0.5,
                                height = 0.5,
                                nudge_x = 0,
                                nudge_y = 0,
                                default.units = "npc",
                                ...) {
  annotate <- ggplot2::ggplotGrob(annotate)
  annotateGrob(annotate = annotate,
               position = position,
               width = width,
               height = height,
               nudge_x = nudge_x,
               nudge_y = nudge_y,
               default.units = default.units,
               ...)
}

#' @noRd
annotateGrob.NULL <- function(annotate, ...) {
  annotate <- grid::nullGrob()
  annotateGrob(annotate = annotate,
               position = position,
               width = width,
               height = height,
               nudge_x = nudge_x,
               nudge_y = nudge_y,
               default.units = default.units,
               ...)
}

#' @noRd
makeContent.annotateGrob <- function(x) {
  annotate <- x$annotate
  xx <- x$x
  yy <- x$y
  nudge_x <- x$nudge_x
  nudge_y <- x$nudge_y
  default.units <- x$default.units

  if (!grid::is.unit(xx)) {
    xx <- grid::unit(xx, default.units)
  }
  if (!grid::is.unit(yy)) {
    yy <- grid::unit(yy, default.units)
  }

  if (!grid::is.unit(nudge_x)) {
    nudge_x <- grid::unit(nudge_x, "mm")
  }
  if (!grid::is.unit(nudge_y)) {
    nudge_y <- grid::unit(nudge_y, "mm")
  }
  xx <- switch(x$position[1],
               "l" = xx + nudge_x,
               "r" = xx - nudge_x,
               "c" = xx)
  yy <- switch(x$position[2],
               "t" = yy - nudge_y,
               "b" = yy + nudge_y,
               "c" = yy)
  width <- x$width %||% grobWidth(annotate)
  height <- x$height %||% grobHeight(annotate)
  if (!grid::is.unit(width)) {
    width <- grid::unit(width, default.units)
  }
  if (!grid::is.unit(height)) {
    height <- grid::unit(height, default.units)
  }
  width <- grid::convertWidth(width, "in")
  height <- grid::convertHeight(height, "in")
  vp <- grid::viewport(x = xx,
                       y = yy,
                       width = width,
                       height = height,
                       just = x$just,
                       default.units = default.units)
  annotate <- grid::editGrob(annotate, vp = vp)

  grid::setChildren(x, do.call(grid::gList, list(annotate)))
}
