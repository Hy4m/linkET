#' @title Link Layer
#' @description  Function to draw mantel test plot on a correlation matrix heatmap.
#' @param data a data frame.
#' @param mapping aesthetic mappings parameters.
#' @param drop logical. If TRUE (default), the unused levels will be droped.
#' @param label.size,label.colour,label.family,label.fontface parameters for label.
#' @param nudge_x horizonal justification of label.
#' @param offset_x,offset_y NULL or named-list, add a little offset to the node points.
#' @param ... extra parameters passing to layer function.
#' @return a ggplot layer.
#' @note `anno_link()` has been deprecated in 0.0.4, please use `geom_couple()`
#' instead.
#' @importFrom ggplot2 geom_segment
#' @importFrom ggplot2 geom_text
#' @importFrom rlang eval_tidy
#' @rdname geom_couple
#' @author Hou Yun
#' @export
geom_couple <- function(data,
                        mapping = NULL,
                        drop = TRUE,
                        label.size = 3.88,
                        label.colour = "black",
                        label.family = "",
                        label.fontface = 1,
                        nudge_x = 0.5,
                        offset_x = NULL,
                        offset_y = NULL,
                        ...)
{
  structure(.Data = list(mapping = mapping,
                         data = data,
                         drop = drop,
                         label.size = label.size,
                         label.colour = label.colour,
                         label.family = label.family,
                         label.fontface = label.fontface,
                         nudge_x = nudge_x,
                         offset_x = offset_x,
                         offset_y = offset_y,
                         params = list(...)),
            class = "geom_couple")
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
  diag <- attr(md, "diag")
  n <- ncols(md)
  m <- nrows(md)
  from <- object$mapping$from
  to <- object$mapping$to

  if("curvature" %in% names(object$params)) {
    if(inherits(object$params$curvature, "nice_curvature")) {
      .f <- object$params$curvature
      object$data <- .f(object$data, md, to)
      object$mapping <- aes_modify(object$mapping, aes_(curvature = ~.curvature))
      object$params <- object$params[setdiff(names(object$params), "curvature")]
    }
  }

  facets <- get_facet_vars(plot)
  if (length(facets) > 1L) {
    stop("Multi facets has not been implemented.", call. = FALSE)
  }
  if (!is.null(facets) && !facets %in% names(object$data)) {
    stop("Not contain facets vars in data of geom_couple().", call. = FALSE)
  }
  link_data <- link_tbl(data = object$data,
                        row_names = rev(row_names(md)),
                        col_names = col_names(md),
                        type = type,
                        diag = diag,
                        from = from,
                        to = to,
                        drop = object$drop,
                        offset_x = object$offset_x,
                        offset_y = object$offset_y,
                        facets = facets,
                        facets_order = attr(plot, "facets_order"))

  .isEdge <- NULL
  edge <- dplyr::filter(link_data, .isEdge)
  node <- dplyr::filter(link_data, !.isEdge)

  mapping <- aes_modify(aes_(x = ~.x, y = ~.y, xend = ~.xend, yend = ~.yend),
                        object$mapping[setdiff(names(object$mapping), c("from", "to"))])
  mapping2 <- aes_modify(aes_(x = ~.x, y = ~.y, label = ~.label),
                         object$mapping[c("x", "y")])
  params <- modifyList(list(data = edge, mapping = mapping, inherit.aes = FALSE),
                       object$params)

  xmin <- min(edge$.x, na.rm = TRUE)
  xmax <- max(edge$.x, na.rm = TRUE)
  if(type == "upper") {
    xrange <- c(min(-0.5, xmin - 0.2 * n), n + 0.5)
    yrange <- c(0.5, n + 0.5)
  } else if (type == "lower") {
    xrange <- c(0.5, max(xmax + 0.2 * n, n + 1.5))
    yrange <- c(0.5, n + 0.5)
  } else {
    xrange <- c(0.5, max(xmax + 0.2 * n, n + 1.5))
    yrange <- c(0.5, n + 0.5)
  }

  nudge_x <- object$nudge_x
  if(type == "upper") {
    nudge_x <- - nudge_x
  }

  hjust <- switch (type, full = 0, lower = 0, upper = 1)

  obj <- list(
    do.call(geom_curve2, params),
    do.call(geom_text,
            list(mapping = mapping2,
                 data = node, hjust = hjust, size = object$label.size,
                 colour = object$label.colour, family = object$label.family,
                 fontface = object$label.fontface, nudge_x = nudge_x,
                 inherit.aes = FALSE))
  )
  plot <- plot + expand_limits(x = xrange, y = yrange)

  ggplot_add(object = obj, plot = plot, object_name = object_name)
}

#' @param curvature a numeric value giving the amount of curvature.
#' @param by one of "from" or "to". Compute the best curvature based on
#' start or end points.
#' @rdname geom_couple
#' @export
nice_curvature <- function(curvature, by = "to") {
  by <- match.arg(by, c("from", "to"))

  if(missing(curvature)) {
    curvature <- 0.1
  }

  if(curvature < 0) {
    curvature <- -curvature
  }

  f <- function(.data, .md, from = NULL, to = NULL) {
    if(by == "from") {
      from <- if(is.null(from)) .data[[1]] else rlang::eval_tidy(from, .data)
      nm <- unique(from)
      half <- length(nm) / 2
      id <- rlang::set_names(seq_along(nm), rev(nm))
      .data$.curvature <- ifelse(id[from] >= half, curvature, -curvature)
      if(length(nm) %% 2 == 1) {
        to <- if(is.null(to)) .data[[2]] else rlang::eval_tidy(to, .data)
        rnm <- row_names(.md)
        half2 <- length(rnm) / 2
        id2 <- rlang::set_names(seq_along(rnm), rev(rnm))
        .data$.curvature <- ifelse((id[from] == (half + 0.5)) & id2[to] >= half2,
                                   .data$.curvature, -.data$.curvature)
      }
    } else {
      to <- if(is.null(to)) .data[[2]] else rlang::eval_tidy(to, .data)
      rnm <- row_names(.md)
      half <- length(rnm) / 2
      id <- rlang::set_names(seq_along(rnm), rev(rnm))
      .data$.curvature <- ifelse(id[to] >= half, curvature, -curvature)
    }
    .data
  }
  class(f) <- c("nice_curvature", class(f))
  f
}

#' @noRd
link_tbl <- function(data,
                     row_names,
                     col_names,
                     type,
                     diag,
                     from = NULL,
                     to = NULL,
                     drop = TRUE,
                     offset_x = NULL,
                     offset_y = NULL,
                     facets = NULL,
                     facets_order = NULL)
{
  if (is.null(facets)) {
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
    if(is.factor(from)) {
      unique_from <- levels(from)
      from <- as.character(from)
      if(isTRUE(drop)) {
        unique_from <- unique_from[unique_from %in% from]
      }
    } else {
      from <- as.character(from)
      unique_from <- unique(from[!is.na(from)])
    }

    if(!is.character(to)) {
      to <- as.character(to)
    }

    n <- length(row_names)
    m <- length(unique_from)
    if(type == "full") {
      l <- length(col_names)
      x <- rep(l + 1 + max(l, n) * 0.382, m)
      y <- seq(n + 0.5, 0.5, length.out = m + 2)[-c(1, m + 2)]
      xend <- rep_len(l + 1, n)
      yend <- 1:n
    } else {
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
      } else {
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
    }
    x <- set_names(x, unique_from)
    y <- set_names(y, unique_from)
    xend <- set_names(xend, row_names)
    yend <- set_names(yend, row_names)

    if (!is.null(offset_x)) {
      offset_x <- as.list(offset_x)
      offset_x[["..x.."]] <- x
      x <- do.call(offset, offset_x)
      offset_x[["..x.."]] <- xend
      xend <- do.call(offset, offset_x)
    }
    if (!is.null(offset_y)) {
      offset_y <- as.list(offset_y)
      offset_y[["..x.."]] <- y
      y <- do.call(offset, offset_y)
      offset_y[["..x.."]] <- yend
      yend <- do.call(offset, offset_y)
    }

    edge <- tibble::tibble(.x = x[from],
                           .y = y[from],
                           .xend = xend[to],
                           .yend = yend[to],
                           .isEdge = TRUE)
    node <- tibble::tibble(.x = x[unique_from],
                           .y = y[unique_from],
                           .label = unique_from,
                           .isEdge = FALSE)
    dplyr::bind_rows(dplyr::bind_cols(edge, data), node)
  } else {
    data <- split(data, data[[facets]])
    out <- purrr::map2_dfr(data, names(data), function(.data, .name) {
      df <- link_tbl(data = .data,
                     row_names = row_names,
                     col_names = col_names,
                     type = type,
                     diag = diag,
                     from = from,
                     to = to,
                     drop = drop,
                     offset_x = offset_x,
                     offset_y = offset_y,
                     facets = NULL)
      df[[facets]][!df$.isEdge] <- .name
      df
    })
    if (!is.null(facets_order)) {
      out[[facets]] <- factor(out[[facets]], levels = facets_order)
    }
    out
  }
}

offset <- function(..x.., ...) {
  ll <- list(...)
  if (length(ll) < 1) {
    return(..x..)
  }

  nm <- names(..x..)
  nm2 <- names(ll)
  nm2 <- intersect(nm, nm2)
  if (length(nm2) < 1) {
    return(..x..)
  }

  ll <- ll[nm2]
  all_one_length_num <- any(vapply(ll, length, numeric(1)) == 1L) &&
    any(vapply(ll, function(.x) class(.x) %in% c("numeric", "integer"), logical(1)))
  if (!all_one_length_num) {
    stop("offset should be one-length numeric value.", call. = FALSE)
  }

  for (ii in nm2) {
    ..x..[nm == ii] <- ..x..[nm == ii] + ll[[ii]]
  }
  ..x..
}

