#' Doughnut Layer
#' @description layer function to draw doughnut.
#' @param units unit of the piechart size.
#' @inheritParams ggplot2::layer
#' @inheritParams ggplot2::geom_polygon
#' @section Aesthetics:
#' \code{geom_doughnut()} understands the following aesthetics (required aesthetics are in bold):
#'     \itemize{
#'       \item \strong{\code{x}}
#'       \item \strong{\code{y}}
#'       \item \code{r0}
#'       \item \code{r1}
#'       \item \code{value}
#'       \item \code{colour}
#'       \item \code{fill}
#'       \item \code{alpha}
#'       \item \code{size}
#'       \item \code{linetype}
#'       \item \code{label}
#'   }
#' @importFrom ggplot2 GeomPolygon
#' @importFrom ggplot2 draw_key_polygon
#' @importFrom grid grobTree
#' @importFrom scales alpha
#' @rdname geom_doughnut
#' @author Hou Yun
#' @export
geom_doughnut_temp <- function(mapping = NULL,
                               data = NULL,
                               stat = "identity",
                               position = "identity",
                               ...,
                               units = "mm",
                               label_size = 7.5,
                               label_col = "black",
                               na.rm = FALSE,
                               show.legend = NA,
                               inherit.aes = TRUE) {
  layer(
    data = data,
    mapping = mapping,
    stat = stat,
    geom = GeomDoughnut,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      units = units,
      label_size = label_size,
      label_col = label_col,
      na.rm = na.rm,
      ...
    )
  )
}

#' @rdname geom_doughnut
#' @format NULL
#' @usage NULL
#' @export
GeomDoughnut <- ggproto(
  "GeomDoughnut", GeomPolygon,
  default_aes = aes(r0 = 4, r1 = 5, value = 1, colour = "grey35", size = 0.5,
                    linetype = 1, fill = "grey50", alpha = NA, label = NA),
  required_aes = c("x", "y"),

  draw_panel = function(self, data, panel_params, coord, units = "mm",
                        label_size = 7.5, label_col = "black", na.rm = FALSE) {
    if(empty(data)) {
      return(ggplot2::zeroGrob())
    }

    data <- coord$transform(data, panel_params)
    grobs <- lapply(split(data, data$group), function(.data) {
      first_row <- .data[1, , drop = FALSE]
      DoughnutGrob(x = first_row$x,
                   y = first_row$y,
                   r0 = first_row$r0,
                   r1 = first_row$r1,
                   value = .data$value,
                   label = .data$label,
                   label_size = label_size,
                   label_col = label_col,
                   units = units,
                   default.units = "native",
                   gp = gpar(col  = scales::alpha(.data$colour,
                                                  .data$alpha),
                             fill = scales::alpha(.data$fill,
                                                  .data$alpha),
                             lty  = .data$linetype,
                             size = .data$size * ggplot2::.pt))
    })

    ggname("geom_doughnut", do.call("grobTree", grobs))
  },
  draw_key = draw_key_polygon
)
