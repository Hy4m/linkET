#' Curve Layer
#'
#' @inheritParams ggplot2::layer
#' @inheritParams ggplot2::geom_curve
#' @section Aesthetics:
#' \code{geom_curve2()} understands the following aesthetics (required aesthetics are in bold):
#'     \itemize{
#'       \item \strong{\code{x}}
#'       \item \strong{\code{y}}
#'       \item \strong{\code{xend}}
#'       \item \strong{\code{yend}}
#'       \item \code{alpha}
#'       \item \code{colour}
#'       \item \code{linetype}
#'       \item \code{size}
#'       \item \code{curvature}
#'   }
#' @importFrom ggplot2 GeomCurve
#' @importFrom ggplot2 GeomPoint
#' @importFrom ggplot2 draw_key_path
#' @importFrom grid gTree
#' @rdname geom_curve2
#' @author Hou Yun
geom_curve2 <- function(mapping = NULL,
                        data = NULL,
                        stat = "identity",
                        position = "identity",
                        ...,
                        angle = 90,
                        ncp = 5,
                        arrow = NULL,
                        arrow.fill = NULL,
                        lineend = "butt",
                        na.rm = FALSE,
                        show.legend = NA,
                        inherit.aes = TRUE) {
  layer(
    data = data,
    mapping = mapping,
    stat = stat,
    geom = GeomCurve2,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      angle = angle,
      ncp = ncp,
      arrow = arrow,
      arrow.fill = arrow.fill,
      lineend = lineend,
      na.rm = na.rm,
      ...
    )
  )
}

#' @rdname linkET-extensions
#' @format NULL
#' @usage NULL
GeomCurve2 <- ggproto(
  "GeomCurve2", GeomCurve,
  default_aes = aes(colour = "grey35", size = 0.5, linetype = 1,
                    alpha = NA, curvature = 0),
  required_aes = c("x", "y", "xend", "yend"),

  draw_panel = function(self, data, panel_params, coord, drop = TRUE,
                        node.shape = 21, node.colour = "blue", node.fill = "red",
                        node.size = 2, angle = 90, ncp = 5, arrow = NULL,
                        arrow.fill = NULL, lineend = "butt", node.color = NULL,
                        na.rm = FALSE) {
    if(empty(data)) {
      return(ggplot2::zeroGrob())
    }

    if (!coord$is_linear()) {
      warning("`geom_curve2` is not implemented for non-linear coordinates.",
              call. = FALSE)
    }

    trans <- coord$transform(data, panel_params)

    arrow.fill <- arrow.fill %||% trans$colour

    grobs <- curve2Grob(x1 = trans$x,
                        y1 = trans$y,
                        x2 = trans$xend,
                        y2 = trans$yend,
                        default.units = "native",
                        curvature = trans$curvature,
                        col = scales::alpha(trans$colour, trans$alpha),
                        fill = scales::alpha(arrow.fill, trans$alpha),
                        lwd = trans$size * ggplot2::.pt,
                        lty = trans$linetype,
                        lineend = lineend,
                        angle = angle,
                        ncp = ncp,
                        square = FALSE,
                        squareShape = 1,
                        inflect = FALSE,
                        open = TRUE,
                        arrow = arrow)

    aesthetics <- setdiff(names(data), c("x", "y", "xend", "yend", "colour",
                                         "fill", "size", "linetype"))
    if(!is.null(node.color)) {
      node.colour <- node.color
    }
    start.colour <- node.colour[1]
    end.colour <- if(length(node.colour) > 1) node.colour[2] else node.colour[1]
    start.fill <- node.fill[1]
    end.fill <- if(length(node.fill) > 1) node.fill[2] else node.fill[1]
    start.shape <- node.shape[1]
    end.shape <- if(length(node.shape) > 1) node.shape[2] else node.shape[1]
    start.size <- node.size[1]
    end.size <- if(length(node.size) > 1) node.size[2] else node.size[1]
    start.data <- new_data_frame(
      list(x = data$x,
           y = data$y,
           colour = start.colour,
           fill = start.fill,
           shape = start.shape,
           size = start.size,
           stroke = 0.5))
    end.data <- new_data_frame(
      list(x = data$xend,
           y = data$yend,
           colour = end.colour,
           fill = end.fill,
           shape = end.shape,
           size = end.size,
           stroke = 0.5))
    if(isTRUE(drop)) {
      start.data <- cbind(start.data, data[aesthetics])[!duplicated(start.data), , drop = FALSE]
      end.data <- cbind(end.data, data[aesthetics])[!duplicated(end.data), , drop = FALSE]
    } else {
      start.data <- cbind(start.data, data[aesthetics])
      end.data <- cbind(end.data, data[aesthetics])
    }
    ggname(
      "geom_curve2",
      grid::gTree(
        children = grid::gList(
          grobs,
          GeomPoint$draw_panel(start.data, panel_params, coord),
          GeomPoint$draw_panel(end.data, panel_params, coord)
        )
      )
    )
  },
  draw_key = draw_key_path
)

#' @importFrom grid grobTree
#' @noRd
curve2Grob <- function(x1, y1, x2, y2,
                       curvature = 1,
                       col = "black",
                       fill = "grey50",
                       lwd = 0.5,
                       lty = 1,
                       lineend = "butt",
                       ...) {
  n <- max(length(x1), length(y1), length(x2), length(y2))

  x1 <- rep_len(x1, n)
  y1 <- rep_len(y1, n)
  x2 <- rep_len(x2, n)
  y2 <- rep_len(y2, n)
  curvature <- rep_len(curvature, n)
  col <- rep_len(col, n)
  fill <- rep_len(fill, n)
  lwd <- rep_len(lwd, n)
  lty <- rep_len(lty, n)

  grobs <- lapply(seq_len(n), function(.n) {
    grid::curveGrob(x1 = x1[.n],
                    y1 = y1[.n],
                    x2 = x2[.n],
                    y2 = y2[.n],
                    curvature = curvature[.n],
                    gp = gpar(col = col[.n],
                              fill = fill[.n],
                              lwd = lwd[.n],
                              lty = lty[.n],
                              lineend = lineend),
                    ...)
  })
  do.call("grobTree", grobs)
}
