#' Doughnut Layer
#' @description layer function to draw doughnut.
#' @param units unit of the piechart size.
#' @param percent logical. If FALSE (the default) the value will be treated as
#' source value.
#' @param rfill the fill colour of other area.
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
                          rfill = "grey90",
                          units = "mm",
                          na.rm = FALSE,
                          show.legend = NA,
                          inherit.aes = TRUE) {
  structure(list(mapping = mapping,
                 data = data,
                 stat = stat,
                 position = position,
                 percent = percent,
                 rfill = rfill,
                 units = units,
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
  if(!tibble::is_tibble(data)) {
    data <- tibble::as_tibble(data)
  }
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
  fv <- aes_vars(mapping, "fill")

  if(is.null(vv)) {
    data$value <- 1
    mapping <- aes_modify(mapping, aes_(value = ~.value))
    vv <- ".value"
  }
  if(!is.list(data[[vv]])) {
    data[[vv]] <- as.list(data[[vv]])
  }
  if(all(vapply(data[[vv]], is.character, logical(1))) ||
     all(vapply(data[[vv]], is.factor, logical(1)))) {
    value <- lapply(data[[vv]], function(.value) as.numeric(table(.value)))

    if(is.null(object$fill) && is.null(fv)) {
      fill <- lapply(data[[vv]], function(.value) names(table(.value)))
      data$.fill <- fill
      mapping <- aes_modify(mapping, aes_(fill = ~.fill))
      fv <- ".fill"
    }
    object$percent <- FALSE
  }

  data$.group <- seq_len(nn)
  value <- unlist(data[[vv]])
  ll <- vapply(data[[vv]], length, numeric(1))

  if(is.null(fv)) {
    if(is.null(object$fill)) {
      object$fill <- colorspace::qualitative_hcl(max(ll), "Set 2")
    }
    if(is.list(object$fill)) {
      object$fill <- rep_len(unlist(object$fill), sum(ll))
    } else {
      if(length(object$fill) <= max(ll)) {
        object$fill <- unlist(lapply(ll, function(.ll) rep_len(object$fill, .ll)))
      } else {
        object$fill <- rep_len(object$fill, sum(ll))
      }
    }
  } else {
    if(!is.list(data[[fv]])) {
      data[[fv]] <- as.list(data[[fv]])
    }
    data[[fv]] <- purrr::map2(data[[fv]], ll, rep_len)
    fill <- unlist(data[[fv]])
  }

  robj <- NULL
  if(isTRUE(object$percent)) {
    data2 <- data
    object2 <- object
    data2$.value <- vapply(value, function(.value) {
      s <- sum(.value, na.rm = TRUE)
      if(s >= 0) {
        s - 1
      } else {
        1 - s
      }
    }, numeric(1))

    data2 <- data2[!identical(data2$.value, 0), ]
    if(nrow(data2) >= 1) {
      athestic <- c("x", "y", "colour", "color", "linetype", "size", "group",
                    "alpha", "r0", "r1")
      mapping2 <- aes_modify(mapping[athestic],
                             aes_(value = ~.value, group = ~.group))
      object2$fill <- object2$rfill
      object2$data <- data2
      object2$mapping <- mapping2
      object2 <- object2[setdiff(names(object2), "rfill")]
      robj <- do.call(geom_doughnut_temp, object2)
    }
  }

  ids <- rep(data$.group, ll)
  data <- data[ids, ]
  data[[vv]] <- value
  if(!is.null(fv)) data[[fv]] <- fill
  mapping <- aes_modify(mapping, aes_(group = ~.group))
  object$data <- data
  object$mapping <- mapping
  object <- do.call(geom_doughnut_temp, object[setdiff(names(object), "rfill")])

  ggplot_add(list(robj, object), plot, object_name)
}

#' @noRd
geom_doughnut_temp <- function(mapping = NULL,
                               data = NULL,
                               stat = "identity",
                               position = "identity",
                               ...,
                               units = "mm",
                               percent = FALSE,
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
                        percent = FALSE, na.rm = FALSE) {
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
