#' @title Key glyphs for marker
#' @description Designed to customize marker legends.
#' @param data a data frame.
#' @param params a list of additional parameters.
#' @param size width/height of key in mm.
#' @return a grid grob object.
#' @author Hou Yun
#' @rdname draw_key_marker
#' @export
draw_key_marker <- function(data, params, size) {
  marker <- marker(x = data$marker %||% "square",
                   width = data$width %||% 1,
                   height = data$height %||% 1,
                   width_unit = params$width_unit,
                   height_unit = params$height_unit,
                   r = data$r %||% 0,
                   n = data$n %||% 5,
                   ratio = data$ratio %||% 0.618)
  markerGrob(marker = marker,
             x = 0.5,
             y = 0.5,
             angle = 0,
             hjust = 0.5,
             vjust = 0.5,
             default.units = "npc",
             gp = gpar(col  = data$colour %||% "grey20",
                       fill = scales::alpha(data$fill %||% "white", data$alpha),
                       lty  = data$linetype %||% 1,
                       size = (data$size %||% 0.5) * ggplot2::.pt
             ))
}

#' @title Shaping Layer
#' @description It can be used to draw custom marker on ggplot.
#' @inheritParams ggplot2::layer
#' @inheritParams ggplot2::geom_polygon
#' @param width_unit,height_unit units of width or height.
#' @param rasterize logical, whether to convert raster image before drawing.
#' @param res positive numeric, used to set the resolution of raster.
#' @section Aesthetics:
#' \code{geom_shaping()} understands the following aesthetics (required aesthetics are in bold):
#'     \itemize{
#'       \item \strong{\code{x}}
#'       \item \strong{\code{y}}
#'       \item \code{marker}
#'       \item \code{width}
#'       \item \code{height}
#'       \item \code{angle}
#'       \item \code{hjust}
#'       \item \code{vjust}
#'       \item \code{n}
#'       \item \code{r}
#'       \item \code{ratio}
#'       \item \code{colour}
#'       \item \code{fill}
#'       \item \code{alpha}
#'       \item \code{size}
#'       \item \code{linetype}
#'   }
#' @rdname geom_shaping
#' @author Hou Yun
#' @importFrom ggplot2 Geom
#' @export
#' @examples \dontrun{
#' set_corrplot_style()
#'
#' ## heart marker
#' qcorrplot(mtcars) + geom_shaping(marker = marker("heart"))
#'
#' ## square marker
#' qcorrplot(mtcars) + geom_shaping(marker = marker("square"))
#'
#' ## ellipse marker
#' qcorrplot(mtcars) + geom_shaping(marker = marker("ellipse"))
#'
#' ## square and shade marker
#' qcorrplot(mtcars) +
#'   geom_shaping(marker = marker("square")) +
#'   geom_shaping(aes(angle = ifelse(r > 0, 0, 180)), marker = marker("shade"),
#'                colour = "white", size = 0.3)
#' }
geom_shaping <- function(mapping = NULL,
                         data = NULL,
                         stat = "identity",
                         position = "identity",
                         ...,
                         width_unit = "native",
                         height_unit = width_unit,
                         rasterize = FALSE,
                         res = 100,
                         na.rm = FALSE,
                         show.legend = NA,
                         inherit.aes = TRUE) {
  params <- rename(list(width_unit = width_unit,
                        height_unit = height_unit,
                        rasterize = rasterize,
                        res = res,
                        na.rm = na.rm,
                        ...), "shaping" = "marker")
  if ("shaping" %in% names(params)) {
    params$shaping <- marker(params$shaping)
  }

  layer(
    data = data,
    mapping = mapping,
    stat = stat,
    geom = Geomshaping,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = params
  )
}

#' @rdname linkET-extensions
#' @format NULL
#' @usage NULL
#' @export
Geomshaping <- ggproto(
  "Geomshaping", Geom,
  default_aes = aes(marker = "square", colour = "grey35", fill = NA, size = 0.25,
                    linetype = 1, alpha = NA, width = 1, height = 1, n = 5, r = 0,
                    angle = 0, hjust = 0.5, vjust = 0.5, ratio = 0.618),
  required_aes = c("x", "y"),
  optional_aes = c("n", "r", "ration"),

  draw_panel = function(self, data, panel_params, coord, shaping = NULL,
                        width_unit = NULL, height_unit = NULL, rasterize = FALSE,
                        res = 100) {
    if (empty(data)) {
      return(nullGrob())
    }

    ## handle native unit
    if (width_unit == "native") {
      data$width <- data$width / diff(panel_params$x.range)
    }
    if (height_unit == "native") {
      data$height <- data$height / diff(panel_params$y.range)
    }

    data <- coord$transform(data, panel_params)
    # if (!tibble::is_tibble(data)) {
    #   data <- as_tibble(data)
    # }
    n <- nrow(data)
    if (!is.null(shaping)) {
      shaping <- rep_len(shaping, n)
      shaping$width_unit <- rep_len(width_unit %||% shaping$width_unit, n)
      shaping$height_unit <- rep_len(height_unit %||% width_unit %||% shaping$height_unit, n)

      shaping$width <- ifelse(shaping$width_unit == "native",
                              shaping$width / diff(panel_params$x.range),
                              shaping$width)
      shaping$height <- ifelse(shaping$height_unit == "native",
                               shaping$height / diff(panel_params$y.range),
                               shaping$height)

    } else {
      shaping <- marker(x = data$marker,
                        width = data$width,
                        height = data$height,
                        width_unit = width_unit %||% "native",
                        height_unit = height_unit %||% "native",
                        r = data$r %||% 0,
                        n = data$n %||% 5,
                        ratio = data$ratio %||% 0.618)
    }

    markerGrob(marker = shaping,
               x = data$x,
               y = data$y,
               angle = data$angle,
               hjust = data$hjust,
               vjust = data$vjust,
               rasterize = rasterize,
               res = res,
               default.units = "native",
               gp = gpar(col  = scales::alpha(data$colour, data$alpha),
                         fill = scales::alpha(data$fill, data$alpha),
                         lty  = data$linetype,
                         size = data$size * ggplot2::.pt
               ))
  },

  draw_key = draw_key_marker
)
