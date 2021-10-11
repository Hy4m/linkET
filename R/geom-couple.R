#' @title Link Layer
#' @description  Function to draw mantel test plot on a correlation matrix heatmap.
#' @param data a data frame.
#' @param mapping aesthetic mappings parameters.
#' @param label.size,label.colour,label.family,label.fontface parameters for label.
#' @param nudge_x horizonal justification of label.
#' @param ... extra parameters passing to layer function.
#' @return a ggplot layer.
#' @note `anno_link()` has been soft deprecated, please use `geom_couple()`
#' instead.
#' @importFrom ggplot2 geom_segment
#' @importFrom ggplot2 geom_text
#' @importFrom rlang eval_tidy
#' @rdname geom_couple
#' @author Hou Yun
#' @export
geom_couple <- function(data,
                        mapping = NULL,
                        label.size = 3.88,
                        label.colour = "black",
                        label.family = "",
                        label.fontface = 1,
                        nudge_x = 0.1,
                        ...)
{
  structure(.Data = list(mapping = mapping,
                         data = data,
                         label.size = label.size,
                         label.colour = label.colour,
                         label.family = label.family,
                         label.fontface = label.fontface,
                         nudge_x = nudge_x,
                         params = list(...)),
            class = "geom_couple")
}

#' @rdname geom_couple
#' @export
anno_link <- function(...) {
  warning("`anno_link()` has been deprecated,\n",
          "please use `geom_couple()` instead.", call. = FALSE)
  geom_couple(...)
}

#' @importFrom ggplot2 aes_
#' @importFrom ggplot2 ggplot_add
#' @importFrom ggplot2 expand_limits
#' @importFrom rlang set_names
#' @export
ggplot_add.geom_couple <- function(object, plot, object_name) {
  stopifnot(is_hyplot(plot))
  md <- plot$data
  type <- attr(md, "type")
  if(type == "full") {
    stop("`geom_couple()` just support for triangle corrplot.")
  }

  diag <- attr(md, "diag")
  n <- ncols(md)
  m <- nrows(md)
  from <- object$mapping$from
  to <- object$mapping$to
  edge <- link_tbl(data = object$data,
                   md = md,
                   row_names = rev(row_names(md)),
                   col_names = col_names(md),
                   type = type,
                   diag = diag,
                   from = from,
                   to = to)
  node <- attr(edge, "node")
  mapping <- aes_modify(aes_(x = ~.x, y = ~.y, xend = ~.xend, yend = ~.yend),
                        object$mapping)
  params <- modifyList(list(data = edge, mapping = mapping, inherit.aes = FALSE),
                       object$params)

  xmin <- min(edge$.x, na.rm = TRUE)
  xmax <- max(edge$.x, na.rm = TRUE)
  if(type == "upper") {
    if(isTRUE(diag)) {
      xrange <- c(min(-0.5, xmin - 0.2 * n), n + 0.5)
      yrange <- c(0.5, n + 0.5)
    } else {
      xrange <- c(min(-0.5, xmin - 0.2 * n), n - 0.5)
      yrange <- c(-0.5, n - 0.5)
    }

  } else if (type == "lower") {
    if(isTRUE(diag)) {
      xrange <- c(0.5, max(xmax + 0.2 * n, n + 1.5))
      yrange <- c(0.5, n + 0.5)
    } else {
      xrange <- c(0.5, max(xmax + 0.2 * n, n + 0.5))
      yrange <- c(0.5, n + 0.5)
    }
  }
  nudge_x <- object$nudge_x
  if(type == "upper") {
    nudge_x <- - nudge_x
  }

  hjust <- switch (type,
                   lower = 0,
                   upper = 1
  )

  obj <- list(
    do.call(geom_curve2, params),
    do.call(geom_text,
            list(mapping = aes_(x = ~.x, y = ~.y, label = ~.label),
                 data = node, hjust = hjust, size = object$label.size,
                 colour = object$label.colour, family = object$label.family,
                 fontface = object$label.fontface, nudge_x = nudge_x,
                 inherit.aes = FALSE))
  )
  plot <- plot + expand_limits(x = xrange, y = yrange)

  ggplot_add(object = obj, plot = plot, object_name = object_name)
}

#' @noRd
link_tbl <- function(data,
                     md,
                     row_names,
                     col_names,
                     type,
                     diag,
                     from = NULL,
                     to = NULL)
{
  if(!is.data.frame(data))
    data <- as.data.frame(data)
  if(!is_md_tbl(md))
    stop("Need a md_tbl.", call. = FALSE)

  from <- if(is.null(from)) {
    data[[1]]
  } else {
    eval_tidy(from, data)
  }
  to <- if(is.null(to)) {
    data[[2]]
  } else {
    eval_tidy(to, data)
  }
  if(!is.character(from))
    from <- as.character(from)
  if(!is.character(to))
    to <- as.character(to)
  unique_from <- unique(from[!is.na(from)])
  n <- length(row_names)
  m <- length(unique_from)
  if(type == "upper") {
    if(m == 1) {
      x <- 0.5 + 0.18 * n
      y <- 0.5 + 0.3 * n
    } else if(m == 2) {
      x <- c(0.5 - 0.02 * n, 0.5 + 0.2 * n)
      y <- c(0.5 + 0.46 * n, 0.5 + 0.2 * n)
    } else {
      y <- seq(0.5 + n * (1 - 0.3), 0.5 + n * 0.1, length.out = m)
      x <- seq(0.5 - 0.25 * n, 0.5 + 0.3 * n, length.out = m)
    }
  } else if(type == "lower") {
    if(m == 1) {
      x <- 0.5 + 0.82 * n
      y <- 0.5 + 0.7 * n
    } else if(m == 2) {
      x <- c(0.5 + 0.8 * n, 0.5 + 1.02 * n)
      y <- c(0.5 + 0.8 * n, 0.5 + 0.54 * n)
    } else {
      y <- seq(0.5 + n * (1 - 0.1), 0.5 + n * 0.3, length.out = m)
      x <- seq(0.5 + 0.75 * n, 0.5 + 1.3 * n, length.out = m)
    }
  }
  x <- set_names(x, unique_from)
  y <- set_names(y, unique_from)

  ## get position of env point
  xend <- n:1
  yend <- 1:n
  if(type == "upper") {
    if(isTRUE(diag)) {
      xend <- xend - 1
    }
  } else {
    if(isTRUE(diag)) {
      xend <- xend + 1
    }
  }
  xend <- set_names(xend, row_names)
  yend <- set_names(yend, row_names)

  ## bind postion end data
  edge <- tibble::tibble(.x = x[from],
                         .y = y[from],
                         .xend = xend[to],
                         .yend = yend[to])
  node <- tibble::tibble(.x = x[unique_from],
                         .y = y[unique_from],
                         .label = unique_from)

  structure(.Data = dplyr::bind_cols(edge, data), node = node,
            class = class(edge))
}
