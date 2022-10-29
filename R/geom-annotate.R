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
#' @param hjust,vjust a numeric vector specifying horizontal/vertical justification.
#' @param width,height width/height of annotate.
#' @param recycle if TRUE indicating annotate will repeated to the same length
#' as rows of data.
#' @param nudge_x,nudge_y a minor shift of position, should be a grid::unit object.
#' @param na.rm not used.
#' @param digits integer indicating the number of decimal places (round)
#' to be used, the default value is 2.
#' @param nsmall the minimum number of digits to the right of the decimal point,
#'  the default value is 2.
#' @return a layer object.
#' @rdname geom_annotate
#' @author Hou Yun
#' @export
geom_annotate <- function(mapping = NULL,
                          data = NULL,
                          stat = "identity",
                          position = "rt",
                          show.legend = FALSE,
                          ...,
                          annotate = NULL,
                          width = NULL,
                          height = NULL,
                          recycle = FALSE,
                          na.rm = FALSE)
{

  params <- list(...)
  others <- params[setdiff(names(params), c("v", "h", "nudge_x", "nudge_y"))]
  params <- params[intersect(names(params), c("v", "h", "nudge_x", "nudge_y"))]
  if (inherits(annotate, "ggplot") || inherits(annotate, "raster") ||
      inherits(annotate, "magick-image")) {
    annotate <- list(annotate)
    width <- width %||% 0.5
    height <- height %||% 0.5
  } else {
    annotate <- as.list(annotate)
  }

  layer(
    data = data,
    mapping = mapping,
    stat = stat,
    geom = GeomAnnotate,
    position = "identity",
    show.legend = show.legend,
    inherit.aes = FALSE,
    params = c(
      list(
        annotate = annotate,
        position = position,
        width = width,
        height = height,
        recycle = recycle,
        params = others,
        na.rm = na.rm
      ),
      params
    )
  )
}

#' @rdname linkET-extensions
#' @format NULL
#' @usage NULL
#' @export
GeomAnnotate <- ggproto(
  "GeomAnnotate", Geom,
  required_aes = NULL,
  default_aes = aes(h = "r", v = "t", nudge_x = 0, nudge_y = 0),
  setup_data = function(data, params) {
    if (empty(data)) {
      return(data)
    }
    annotate <- params$annotate
    recycle <- params$recycle
    data <- tibble::as_tibble(data)
    if (isTRUE(recycle)) {
      if (length(annotate) > nrow(data)) {
        annotate <- rep_len(annotate, length(annotate))
      } else {
        data <- data[rep_len(seq_len(nrow(data)), length(annotate)), ]
      }
      data$annotate <- annotate
    } else {
      data <- data[seq_len(length(annotate)), ]
      data$annotate <- annotate
    }
    data
  },
  draw_panel = function(data,
                        panel_params,
                        coord,
                        annotate = list(),
                        position = NULL,
                        width = NULL,
                        height = NULL,
                        recycle = FALSE,
                        params = list(),
                        na.rm = FALSE) {
    if (empty(data)) {
      return(grid::nullGrob())
    }

    grobs <- lapply(split(data, seq_len(nrow(data))), function(rows) {
      pp <- c(list(annotate = rows$annotate[[1]],
                   position = position %||% paste0(rows$h, rows$v),
                   width = width,
                   height = height,
                   nudge_x = rows$nudge_x,
                   nudge_y = rows$nudge_y), params)
      do.call(annotateGrob, pp)
    })
    ggname("geom_annotate", do.call("grobTree", grobs))
  },
  draw_key = ggplot2::draw_key_blank
)

#' @rdname geom_annotate
#' @param default.units A string indicating the default units to use.
#' @export
annotateGrob <- function(annotate, ...) {
  UseMethod("annotateGrob")
}

#' @rdname geom_annotate
#' @method annotateGrob grob
#' @export
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

#' @param parse if TRUE (default) will convert text to richtext.
#' @rdname geom_annotate
#' @method annotateGrob character
#' @export
annotateGrob.character <- function(annotate,
                                   position = "rt",
                                   hjust = 0.5,
                                   vjust = 0.5,
                                   width = NULL,
                                   height = NULL,
                                   nudge_x = 0,
                                   nudge_y = 0,
                                   default.units = "npc",
                                   parse = FALSE,
                                   ...) {
  if (length(annotate) < 1) {
    return(grid::nullGrob())
  }

  parse_fun <- parse
  if (isTRUE(parse_fun)) {
    if (length(annotate) > 1 || any(grepl("\n", annotate, fixed = TRUE))) {
      parse_fun <- parse_func(output = "richtext")
    } else {
      parse_fun <- parse_func()
    }
  }
  if (is.function(parse_fun)) {
    annotate <- parse_fun(annotate)
    if ( is.character(annotate) && !is_richtext(annotate)) {
      annotate <- parse_safe(annotate)
    }
  }
  if (is_richtext(annotate) && length(annotate) > 1) {
    annotate <- paste_with_na(annotate, collapse = "<br>")
  }

  if (length(annotate) > 1 && !is.expression(annotate)) {
    warning("R expression not supports multiline formula,\n",
            "please use richtext instead.", call. = FALSE)
    if (is.character(annotate)) {
      annotate <- paste_with_na(annotate, collapse = "\n")
    }
  }

  hjust <- get_hjust(hjust)
  vjust <- 0.5
  if (identical(hjust, 1)) {
    xx <- 1
  } else if (identical(hjust, 0)) {
    xx <- 0
  } else {
    xx <- 0.5
  }

  if (is_richtext(annotate)) {
    richtext_grob <- get_function("gridtext", "richtext_grob")
    annotate <- richtext_grob(text = annotate,
                              x = xx,
                              y = 0.5,
                              hjust = hjust,
                              vjust = 0.5,
                              ...)
  } else {
    annotate <- grid::textGrob(label = annotate,
                               x = xx,
                               y = 0.5,
                               hjust = hjust,
                               vjust = 0.5,
                               ...)
  }

  annotateGrob(annotate = annotate,
               position = position,
               width = width,
               height = height,
               nudge_x = nudge_x,
               nudge_y = nudge_y,
               default.units = default.units)
}

#' @rdname geom_annotate
#' @method annotateGrob raster
#' @export
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

#' @rdname geom_annotate
#' @method annotateGrob magick-image
#' @export
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

#' @rdname geom_annotate
#' @method annotateGrob ggplot
#' @export
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

#' @rdname geom_annotate
#' @method annotateGrob NULL
#' @export
annotateGrob.NULL <- function(annotate, ...) {
  annotate <- grid::nullGrob()
  annotateGrob(annotate = annotate, ...)
}

#' @rdname geom_annotate
#' @method annotateGrob numeric
#' @export
annotateGrob.numeric <- function(annotate,
                                 digits = 2,
                                 nsmall = 2,
                                 ...) {
  annotate <- format(annotate, digits = digits, nsmall = nsmall)
  annotateGrob(annotate = annotate, ...)
}

#' @export
makeContent.annotateGrob <- function(x) {
  annotate <- x$annotate
  is_label <- inherits(annotate, "text") || inherits(annotate, "richtext_grob")
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
                       clip = "off",
                       default.units = default.units)
  annotate <- grid::editGrob(annotate, vp = vp)

  grid::setChildren(x, do.call(grid::gList, list(annotate)))
}

#' @noRd
is_nested_annotate_list <- function(annotate) {
  id <- vapply(annotate, function(x) {
    is.list(x) && !inherits(x, "ggplot") && !inherits(x, "grob") &&
      !inherits(x, "raster") && !inherits(x, "magick-image") &&
      !inherits(x, "NULL")
  }, logical(1))
  if (any(id)) TRUE else FALSE
}

#' @noRd
get_hjust <- function(hjust) {
  if (is.character(hjust)) {
    hjust <- tolower(hjust)
    hjust <- match.arg(hjust, c("left", "right", "center", "l", "r", "c"))
    hjust <- switch (hjust,
                     "left" = 0,
                     "right" = 1,
                     "center" = 0.5,
                     "l" = 0,
                     "r" = 1,
                     "c" = 0.5
    )
  } else {
    if (is.null(hjust) || !is.numeric(hjust)) {
      hjust <- 0.5
    }
    if (length(hjust) < 1) {
      hjust <- 0.5
    } else {
      hjust <- hjust[1]
    }
    if (hjust > 0.5) {
      hjust <- 1
    } else if (hjust < 0.5) {
      hjust <- 0
    } else {
      hjust <- 0.5
    }
  }
  hjust
}
