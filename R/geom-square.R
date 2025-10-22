#' Square Layer
#'
#' @inheritParams ggplot2::layer
#' @inheritParams ggplot2::geom_rect
#' @section Aesthetics:
#' \code{geom_square()} understands the following aesthetics (required aesthetics are in bold):
#'     \itemize{
#'       \item \strong{\code{x}}
#'       \item \strong{\code{y}}
#'       \item \code{r0}
#'       \item \code{alpha}
#'       \item \code{colour}
#'       \item \code{fill}
#'       \item \code{linetype}
#'       \item \code{size}
#'    }
#' @importFrom ggplot2 layer
#' @importFrom ggplot2 ggproto
#' @importFrom ggplot2 GeomRect
#' @importFrom ggplot2 aes
#' @importFrom ggplot2 draw_key_polygon
#' @rdname geom_square
#' @export
geom_square <- function(mapping = NULL,
                        data = NULL,
                        stat = "identity",
                        position = "identity",
                        ...,
                        na.rm = FALSE,
                        show.legend = NA,
                        inherit.aes = TRUE) {
  layer(
    data = data,
    mapping = mapping,
    stat = stat,
    geom = GeomSquare,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      na.rm = na.rm,
      ...
    )
  )
}

#' @rdname linkET-extensions
#' @format NULL
#' @usage NULL
#' @export
GeomSquare <- ggproto(
  "GeomSquare", GeomRect,
  default_aes = aes(
    r0 = 0.5, colour = "grey35", fill = NA,
    size = 0.25, linetype = 1, alpha = NA
  ),
  required_aes = c("x", "y"),

  # Generate rectangular boundaries during the setup data phase
  setup_data = function(data, params) {
    if (is.null(data$r0)) data$r0 <- 0.5
    dd <- point_to_square(data$x, data$y, data$r0)
    cbind(dd, data)
  },

  draw_panel = function(self, data, panel_params, coord, linejoin = "mitre") {
    GeomRect$draw_panel(data, panel_params, coord)
  },

  draw_key = draw_key_polygon
)


#' @noRd
point_to_square <- function(x, y, r0) {
  r0 <- 0.5 * sign(r0) * sqrt(abs(r0))
  new_data_frame(list(
    xmin = -r0 + x,
    xmax = r0 + x,
    ymin = -r0 + y,
    ymax = r0 + y
  ))
}

