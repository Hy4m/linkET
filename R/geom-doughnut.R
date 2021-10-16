#' Doughnut Layer
#' @description layer function to draw doughnut.
#' @param units unit of the piechart size.
#' @param label_size size of labels.
#' @param label_col color of labels.
#' @param percent logical. If FALSE (the default) the value will be treated as
#' source value.
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
#' @examples
#' library(ggplot2)
#' dd <- tibble::tibble(x = runif(20, -180, 180),
#'                      y = runif(20, -90, 90),
#'                      value = purrr::map(sample(2:10, 20, TRUE), runif)
#'                      )
#'
#' ggplot(dd, aes(x, y)) +
#'   geom_doughnut(aes(value = value))
#' @export
geom_doughnut <- function(mapping = NULL,
                          data = NULL,
                          stat = "identity",
                          position = "identity",
                          ...,
                          percent = FALSE,
                          units = "mm",
                          label_size = 7.5,
                          label_col = "black",
                          na.rm = FALSE,
                          show.legend = NA,
                          inherit.aes = TRUE) {
  structure(list(mapping = mapping,
                 data = data,
                 stat = stat,
                 position = position,
                 percent = percent,
                 units = units,
                 label_size = label_size,
                 label_col = label_col,
                 na.rm = na.rm,
                 show.legend = show.legend,
                 inherit.aes = inherit.aes,
                 ...), class = "doughnut"
            )
}

#' @rdname geom_doughnut
#' @export
geom_node_doughnut <- function(mapping = NULL,
                               data = NULL,
                               position = "identity",
                               show.legend = NA,
                               ...) {
  StatFilter <- get_function("ggraph", "StatFilter")
  aes_intersect <- utils::getFromNamespace("aes_intersect", "ggraph")
  mapping <- aes_intersect(mapping, aes_(x = ~x, y = ~y))
  layer(data = data,
        mapping = mapping,
        stat = StatFilter,
        geom = "doughnut",
        position = position,
        show.legend = show.legend,
        inherit.aes = FALSE,
        params = list(
          na.rm = FALSE,
          ...)
        )
}

#' @method ggplot_add doughnut
#' @export
ggplot_add.doughnut <- function(object, plot, object_name) {
  data <- object$data %||% plot$data
  if(isTRUE(object$inherit.aes)) {
    mapping <- aes_modify(plot$mapping, object$mapping)
  } else {
    mapping <- object$mapping
  }

  nm <- names(data)
  nn <- nrow(data)

  if(empty(data)) {
    return(plot)
  }

  vv <- aes_vars(mapping, "value")
  lv <- aes_vars(mapping, "label")
  gv <- aes_vars(mapping, "group")
  fv <- aes_vars(mapping, "fill")

  value <- NULL
  if(!is.null(vv) && is.list(data[[vv]])) {
    value <- unlist(data[[vv]])
    ll <- vapply(data[[vv]], length, numeric(1))
    if(!is.null(lv)) {
      label <- if(is.list(data[[lv]])) unlist(data[[lv]]) else rep(data[[lv]], ll)
    } else {
      label <- NULL
    }
    if(!is.null(fv)) {
      fill <- if(is.list(data[[fv]])) unlist(data[[fv]]) else rep(data[[fv]], ll)
    } else {
      fill <- NULL
    }

    if(is.null(object$fill)) {
      if(is.null(fill)) {
        col <- colorspace::qualitative_hcl(max(ll), "Set 2")
        object$fill <- unlist(lapply(ll, function(.n) rep_len(col, .n)))
      } else {
        data[[fv]] <- fill
      }
    } else {
      if(length(object$fill) == nn) {
        object$fill <- rep(object$fill, ll)
      } else {
        if(object$fill != sum(ll)) {
          object$fill <- unlist(lapply(ll, function(.n) {
            rep_len(object$fill, .n)
            }))
        }
      }
    }
    data <- data[rep(seq_len(nn), ll), , drop = FALSE]
    if(!is.null(value)) data[[vv]] <- value
    if(!is.null(label)) data[[lv]] <- label
    data[["group"]] <- rep(seq_len(nn), ll)
    object$mapping <- aes_modify(object$mapping, aes_(group = ~group))

  }
  object$data <- data
  object <- do.call(geom_doughnut_temp, object)
  ggplot_add(object, plot, object_name)
}

#' @noRd
geom_doughnut_temp <- function(mapping = NULL,
                               data = NULL,
                               stat = "identity",
                               position = "identity",
                               ...,
                               units = "mm",
                               percent = FALSE,
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
      percent = percent,
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
  default_aes = aes(r0 = 0, r1 = 5, value = 1, colour = "grey35", size = 0.5,
                    linetype = 1, fill = "grey50", alpha = NA, label = NA),
  required_aes = c("x", "y"),

  draw_panel = function(self, data, panel_params, coord, units = "mm",
                        percent = FALSE, label_size = 7.5, label_col = "black",
                        na.rm = FALSE) {
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
                   percent = percent,
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
