#' Halfcircle Layer
#'
#' @inheritParams ggplot2::layer
#' @inheritParams geomtextpath::geom_textpath
#' @param n the larger the x, the smoother the curve.
#' @section Aesthetics:
#' \code{geom_square()} understands the following aesthetics (required aesthetics are in bold):
#'     \itemize{
#'       \item \strong{\code{x}}
#'       \item \strong{\code{y}}
#'       \item \strong{\code{xend}}
#'       \item \strong{\code{yend}}
#'       \item \code{alpha}
#'       \item \code{colour}
#'       \item \code{label}
#'       \item \code{angle}
#'       \item \code{family}
#'       \item \code{fontface}
#'       \item \code{hjust}
#'       \item \code{linecolour}
#'       \item \code{lineheight}
#'       \item \code{linetype}
#'       \item \code{linewidth}
#'       \item \code{size}
#'       \item \code{spacing}
#'       \item \code{textcolour}
#'    }
#' @importFrom ggplot2 draw_key_path
#' @importFrom geomtextpath GeomTextpath
#' @rdname geom_halfcircle
#' @author Hou Yun
#' @export
geom_halfcircle <- function(mapping = NULL,
                            data = NULL,
                            stat = "identity",
                            position = "identity",
                            na.rm = FALSE,
                            show.legend = NA,
                            inherit.aes = TRUE,
                            ...,
                            n = 100) {
  layer(
    data = data,
    mapping = mapping,
    stat = stat,
    geom = GeomHalfcircle,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      n = n,
      na.rm = na.rm,
      ...
    )
  )
}

#' @rdname geom_halfcircle
#' @format NULL
#' @usage NULL
#' @export
GeomHalfcircle <- ggproto(
  "GeomHalfcircle", geomtextpath::GeomTextpath,
  default_aes = aes(colour = "black", size = 3.88, alpha = NA, side = "left",
                    label = "", vjust = -0.3, hjust = 0.5, angle = 0,
                    family = "", fontface = 1, linecolour = NULL,
                    lineheight = 1.2, linetype = 1, linewidth = 0.5,
                    spacing = 0, textcolour = NULL),
  required_aes = c("x", "y", "xend", "yend"),
  draw_panel = function(self, data, panel_params, coord, n = 100) {
    if (empty(data)) {
      return(ggplot2::zeroGrob())
    }

    data$group <- seq_len(nrow(data))
    pos <- data[c("x", "y", "xend", "yend", "side", "group")]
    aesthetics <- data[rep(seq_len(nrow(data)), each = n),
                       setdiff(names(data), c("x", "y", "xend", "yend", "side",
                                              "group")), drop = FALSE]
    pos <- purrr::pmap_dfr(pos, function(x, y, xend, yend, side, group) {
      half_circle(x = x,
                  y = y,
                  xend = xend,
                  yend = yend,
                  side = side,
                  n = n) %>%
        mutate(group = group)
    })
    data <- cbind(pos, aesthetics)
    geomtextpath::GeomTextpath$draw_panel(data, panel_params, coord)
  },
  draw_key = draw_key_path
)

#' @noRd
half_circle <- function(x, y, xend, yend,
                        side = "left",
                        directed = FALSE,
                        n = 100) {
  side <- match.arg(side, c("left", "right"))
  if (isFALSE(directed)) {
    if (x > x1) {
      temp_x <- x
      temp_y <- y
      x <- x1
      x1 <- temp_x
      y <- y1
      y1 <- temp_y
    }
    if (identical(x, x1)) {
      if (y > y1) {
        temp_x <- x
        temp_y <- y
        x <- x1
        x1 <- temp_x
        y <- y1
        y1 <- temp_y
      }
    }
  }

  r <- sqrt((xend - x)^2 + (yend - y)^2) / 2
  cx <- (x + xend) / 2
  cy <- (y + yend) / 2
  if (identical(x, xend)) {
    if (y > yend) {
      tt <- switch (side,
        left = seq(2.5 * pi, 1.5 * pi, length.out = n),
        right = seq(0.5 * pi, 1.5 * pi, length.out = n)
      )
    } else {
      tt <- switch (side,
                    left = seq(1.5 * pi, 0.5 * pi, length.out = n),
                    right = seq(1.5 * pi, 2.5 * pi, length.out = n)
      )
    }
  } else {
    rot <- atan((yend - y) / (xend - x))

    if ( x < xend) {
      if (rot < 0) {
        tt <- switch (side,
                      left = seq(rot + 3 * pi, rot + 2 * pi, length.out = n),
                      right = seq(rot + pi, rot + 2 * pi, length.out = n))
      } else {
        tt <- switch (side,
                      left = seq(rot + pi, rot, length.out = n),
                      right = seq(rot + pi, rot + 2 * pi, length.out = n))
      }
    } else {
      if (rot < 0) {
        tt <- switch (side,
                      left = seq(rot + 2 * pi, rot + pi, length.out = n),
                      right = seq(rot + 2 * pi, rot + 3 * pi, length.out = n))
      } else {
        tt <- switch (side,
                      left = seq(rot + 2 * pi, rot + pi, length.out = n),
                      right = seq(rot, rot + pi, length.out = n))
      }
    }
  }
  tibble::tibble(x = r * cos(tt) + cx,
                 y = r * sin(tt) + cy)
}
