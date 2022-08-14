#' @title Draw link-curves
#' @description This function can be used to draw a special network, which nodes
#' should be matched width margin plot.
#' @param graph a igraph, tbl_graph, or other can be converted to igraph object.
#' @param mapping default list of aesthetic mappings to use for plot.
#' @param ... margin plot, should be named with "r", "l", "t" or "b".
#' @param geom no used.
#' @param widths width of margin plot.
#' @param heights height of margin plot.
#' @param guides a string specifying how guides should be treated in the layout.
#' @return a ggplot object.
#' @author Hou Yun
#' @importFrom ggplot2 geom_path
#' @importFrom ggplot2 geom_segment
#' @export
qlink <- function(graph,
                  mapping = NULL,
                  ...,
                  geom = "auto",
                  widths = NULL,
                  heights = NULL,
                  guides = "collect") {
  if (!inherits(graph, "igraph")) {
    graph <- as.igraph(graph)
  }
  graph <- igraph::as_data_frame(graph)

  ll <- list(...)
  params <- ll[setdiff(names(ll), c("r", "l", "t", "b"))]
  ll <- ll[intersect(names(ll), c("r", "l", "t", "b"))]
  n <- length(ll)
  if (n < 1) {
    stop("Empty annotation plot.", call. = FALSE)
  }
  nm <- names(ll)
  limits <- list()
  rngs <- list()
  for (ii in nm) {
    if (inherits(ll[[ii]], "ggplot")) {
      ll[[ii]] <- list(ll[[ii]])
    }
    if (length(ll[[ii]]) > 1) {
      for (jj in seq_along(length(ll[[ii]]))) {
        if (jj == 1L) next
        if (ii %in% c("r", "l")) {
          ll[[ii]][[jj]] <- ll[[ii]][[jj]] + aplot::ylim2(ll[[ii]][[1]])
        } else {
          ll[[ii]][[jj]] <- ll[[ii]][[jj]] + aplot::xlim2(ll[[ii]][[1]])
        }
      }
    }
    if (ii %in% c("r", "l")) {
      limits[[ii]] <- aplot::yrange(ll[[ii]][[1]], "limit", "plot")
      rngs[[ii]] <- aplot::yrange(ll[[ii]][[1]], "limit")
    } else {
      limits[[ii]] <- aplot::xrange(ll[[ii]][[1]], "limit", "plot")
      rngs[[ii]] <- aplot::xrange(ll[[ii]][[1]], "limit")
    }
  }

  if (is.null(mapping)) {
    mapping <- ggplot2::aes()
  }
  if (all(c("from_id", "to_id") %in% names(mapping))) {
    from_id <- rlang::eval_tidy(mapping$from_id, graph)
    to_id <- rlang::eval_tidy(mapping$to_id, graph)
    if (is.numeric(from_id)) {
      from_id <- c("b", "l", "t", "r")[from_id]
    }
    if (is.numeric(to_id)) {
      to_id <- c("b", "l", "t", "r")[to_id]
    }
    from_id <- switch_side(from_id)
    to_id <- switch_side(to_id)

    if (!all(from_id %in% nm)) {
      stop("All ID of positions should be matched with annotation.",
           call. = FALSE)
    }
    if (!all(to_id %in% nm)) {
      stop("All ID of positions should be matched with annotation.",
           call. = FALSE)
    }
    graph$from <- paste(from_id, graph$from, sep = ".-.")
    graph$to <- paste(to_id, graph$to, sep = ".-.")
    limits <- lapply(nm, function(.nm) {
      paste(.nm, limits[[.nm]], sep = ".-.")
    })
  }

  if (length(ll) == 1) {
    if (nm == "r") {
      xx <- rep(0, length(limits$r))
      yy <- seq_along(limits$r)
      pos_x <- rlang::set_names(xx, limits$r)
      pos_y <- rlang::set_names(yy, limits$r)
    } else if (nm == "l") {
      xx <- rep(1, length(limits$l))
      yy <- seq_along(limits$l)
      pos_x <- rlang::set_names(xx, limits$l)
      pos_y <- rlang::set_names(yy, limits$l)
    } else if (nm == "t") {
      xx <- seq_along(limits$t)
      yy <- rep(1, length(limits$t))
      pos_x <- rlang::set_names(xx, limits$t)
      pos_y <- rlang::set_names(yy, limits$t)
    } else {
      xx <- seq_along(limits$b)
      yy <- rep(0, length(limits$b))
      pos_x <- rlang::set_names(xx, limits$b)
      pos_y <- rlang::set_names(yy, limits$b)
    }
    graph$.y <- pos_y[graph$from]
    graph$.yend <- pos_y[graph$to]
    graph$.x <- pos_x[graph$from]
    graph$.xend <- pos_x[graph$to]

    graph <- gen_half_circle(graph, side = nm)
    mapping <- aes_modify(mapping, aes_string(x = ".x", y = ".y", group = ".group"))
    p <- ggplot(graph, mapping) + do.call("geom_path", params)

    if (nm %in% c("r", "l")) {
      p <- p + ggplot2::scale_y_continuous(limits = rngs[[nm]],
                                           expand = c(0, 0))
    } else {
      p <- p + ggplot2::scale_x_continuous(limits = rngs[[nm]],
                                           expand = c(0, 0))
    }
    p <- p + ggplot2::theme_void()

  } else {
    if (identical(sort(nm), c("l", "r"))) {
      xx <- c(rep(0, length(limits$l)), rep(1, length(limits$r)))
      yy <- c(seq_along(limits$l),
              scales::rescale(seq_along(limits$r), rngs$r, rngs$l))
      pos_x <- rlang::set_names(xx, c(limits$l, limits$r))
      pos_y <- rlang::set_names(yy, c(limits$l, limits$r))
    } else if (identical(sort(nm), c("l", "t"))) {
      xx <- c(seq_along(limits$t), rep_len(length(limits$t), length(limits$l)))
      yy <- c(rep_len(length(limits$l), length(limits$t)), seq_along(limits$l))
      pos_x <- rlang::set_names(xx, c(limits$t, limits$l))
      pos_y <- rlang::set_names(yy, c(limits$t, limits$l))
    } else if (identical(sort(nm), c("b", "l"))) {
      xx <- c(seq_along(limits$b), rep_len(length(limits$b), length(limits$l)))
      yy <- c(rep_len(length(limits$l), length(limits$b)), seq_along(limits$l))
      pos_x <- rlang::set_names(xx, c(limits$b, limits$l))
      pos_y <- rlang::set_names(yy, c(limits$b, limits$l))
    } else if (identical(sort(nm), c("b", "t"))) {
      xx <- c(seq_along(limits$b),
              scales::rescale(seq_along(limits$t), rngs$t, limits$b))
      yy <- c(rep(0, length(limits$b)), rep(1, length(limits$t)))
      pos_x <- rlang::set_names(xx, c(limits$b, limits$t))
      pos_y <- rlang::set_names(yy, c(limits$b, limits$t))
    } else if (identical(sort(nm), c("b", "r"))) {
      xx <- c(seq_along(limits$b), rep_len(length(limits$b), length(limits$r)))
      yy <- c(rep_len(length(limits$r), length(limits$b)), seq_along(limits$r))
      pos_x <- rlang::set_names(xx, c(limits$b, limits$r))
      pos_y <- rlang::set_names(yy, c(limits$b, limits$r))
    } else if (identical(sort(nm), c("r", "t"))) {
      xx <- c(seq_along(limits$t), rep_len(length(limits$t), length(limits$r)))
      yy <- c(rep_len(length(limits$r), length(limits$t)), seq_along(limits$r))
      pos_x <- rlang::set_names(xx, c(limits$t, limits$r))
      pos_y <- rlang::set_names(yy, c(limits$t, limits$r))
    } else if (identical(sort(nm), c("l", "r", "t"))) {
      xx <- c(seq_along(limits$t), rep_len(1, length(limits$r)),
              rep_len(length(limits$t), length(rngs$t)))
      yy <- c(rep(length(limits$t), length(limits$t)),
              scales::rescale(seq_along(limits$r), rngs$r, rngs$l),
              seq_along(limits$l))
      pos_x <- rlang::set_names(xx, c(limits$t, limits$r, limits$l))
      pos_y <- rlang::set_names(yy, c(limits$t, limits$r, limits$l))
    } else if (identical(sort(nm), c("b", "l", "t"))) {
      xx <- c(seq_along(limits$b),
              seq_along(limits$l),
              scales::rescale(seq_along(limits$t), rngs$t, rngs$b))
      yy <- c(rep(rngs$l[1], length(limits$b)),
              seq_along(limits$l),
              rep(rngs$l[2], length(limits$t)))
      pos_x <- rlang::set_names(xx, c(limits$b, limits$l, limits$t))
      pos_y <- rlang::set_names(yy, c(limits$b, limits$l, limits$t))
    } else if (identical(sort(nm), c("b", "r", "t"))) {
      xx <- c(seq_along(limits$b),
              seq_along(limits$r),
              scales::rescale(seq_along(limits$t), rngs$t, rngs$b))
      yy <- c(rep(rngs$r[1], length(limits$b)),
              seq_along(limits$r),
              rep(rngs$r[2], length(limits$t)))
      pos_x <- rlang::set_names(xx, c(limits$b, limits$r, limits$t))
      pos_y <- rlang::set_names(yy, c(limits$b, limits$r, limits$t))
    } else if (identical(sort(nm), c("b", "l", "r"))) {
      xx <- c(seq_along(limits$b),
              rep(rngs$b[1], length(limits$l)),
              rep(rngs$b[2], length(limits$r)))
      yy <- c(rep(rngs$l[1], length(limits$b)),
              seq_along(limits$l),
              scales::rescale(limits$r, rngs$r, rngs$l))
      pos_x <- rlang::set_names(xx, c(limits$b, limits$l, limits$r))
      pos_y <- rlang::set_names(yy, c(limits$b, limits$l, limits$r))
    } else {
      xx <- c(seq_along(limits$b),
              scales::rescale(seq_along(limits$t), rngs$t, rngs$b),
              rep(rngs$b[1], length(limits$l)),
              rep(rngs$b[2], length(limits$r)))
      yy <- c(rep(rngs$l[1], length(limits$b)),
              rep(rngs$l[2], length(limits$t)),
              seq_along(limits$l),
              scales::rescale(seq_along(limits$r), rngs$r, rngs$l))
      pos_x <- rlang::set_names(xx, c(limits$b, limits$t, limits$l, limits$r))
      pos_y <- rlang::set_names(yy, c(limits$b, limits$t, limits$l, limits$r))
    }
    graph$.y <- pos_y[graph$from]
    graph$.yend <- pos_y[graph$to]
    graph$.x <- pos_x[graph$from]
    graph$.xend <- pos_x[graph$to]
    mapping <- aes_modify(mapping, aes_string(x = ".x", y = ".y",
                                              xend = ".xend", yend = ".yend"))
    p <- ggplot(graph, mapping) + do.call("geom_segment", params)
    p <- p + ggplot2::scale_x_continuous(limits = rngs[["b"]] %||% rngs[["t"]],
                                         expand = c(0, 0))
    p <- p + ggplot2::scale_y_continuous(limits = rngs[["l"]] %||% rngs[["r"]],
                                         expand = c(0, 0))
    p <- p + ggplot2::theme_void()
  }

  structure(.Data = p,
            anno = ll,
            widths = widths,
            heights = heights,
            guides = guides,
            class = c("qlink", class(p)))
}

#' @noRd
as_patchwork <- function(plot, ...) {
  anno <- attr(plot, "anno")
  widths <- as.list(attr(plot, "widths"))
  heights <- as.list(attr(plot, "heights"))
  guides <- attr(plot, "guides")
  class(plot) <- setdiff(class(plot), "qlink")
  nm <- names(anno)
  if ("l" %in% nm) {
    width_l <- rev(widths$l) %||% 1
    width_l <- rep_len(width_l, length(anno$l))
  } else {
    width_l <- NULL
  }

  if ("r" %in% nm) {
    width_r <- widths$r %||% 1
    width_r <- rep_len(width_r, length(anno$r))
  } else {
    width_r <- NULL
  }

  if ("t" %in% nm) {
    height_t <- rev(heights$t) %||% 1
    height_t <- rep_len(height_t, length(anno$t))
  } else {
    height_t <- NULL
  }

  if ("b" %in% nm) {
    height_b <- heights$b %||% 1
    height_b <- rep_len(height_b, length(anno$b))
  } else {
    height_b <- NULL
  }

  widths <- c(width_l, 1, width_r)
  heights <- c(height_t, 1, height_b)

  n <- length(widths)
  m <- length(heights)
  plotlist <- rep_len(list(patchwork::plot_spacer()), n * m)
  id_row <- seq(n * length(anno$t) + 1, n * (length(anno$t) + 1))
  id_col <- seq(length(anno$l) + 1, n * m, by = n)
  plotlist[id_row] <- c(rev(anno$l), list(plot), anno$r)
  plotlist[id_col] <- c(rev(anno$t), list(plot), anno$b)

  plotlist <- lapply(plotlist, function(p) {
    p + ggplot2::theme(plot.margin = ggplot2::margin())
  })

  Reduce("+", plotlist) + patchwork::plot_layout(byrow = TRUE,
                                                 widths = widths,
                                                 heights = heights,
                                                 guides = guides)
}

#' @export
print.qlink <- function(x, ...) {
  x <- as_patchwork(x)
  print(x)
}

#' @noRd
gen_half_circle <- function(data, side) {
  nm <- setdiff(names(data), c(".x", ".y", ".xend", ".yend"))
  purrr::map_dfr(seq_len(nrow(data)), function(id) {
    row <- data[id, , drop = FALSE]

    if (side %in% c("l", "r")) {
      r <- 0.5 * abs(row$.yend - row$.y)
      cx <- 0.5 * (row$.x + row$.xend)
      cy <- 0.5 * (row$.y + row$.yend)
      if (row$.yend > row$.y) {
        if (side == "l") {
          tt <- seq(-pi/2, pi/2, length.out = 100)
        } else {
          tt <- seq(pi * 1.5, pi/2, length.out = 100)
        }
      } else {
        if (side == "l") {
          tt <- seq(pi/2, -pi/2, length.out = 100)
        } else {
          tt <- seq(pi/2, pi * 1.5, length.out = 100)
        }
      }
      pos <- tibble::tibble(.x = r * cos(tt) + cx,
                            .y = r * sin(tt) + cy,
                            .group = id)
      dplyr::bind_cols(pos, row[rep_len(1, 100), nm, drop = FALSE])
    } else {
      r <- 0.5 * abs(row$.xend - row$.x)
      cx <- 0.5 * (row$.x + row$.xend)
      cy <- 0.5 * (row$.y + row$.yend)
      if (row$.xend > row$.x) {
        if (side == "b") {
          tt <- seq(pi, 0, length.out = 100)
        } else {
          tt <- seq(-pi, 0, length.out = 100)
        }
      } else {
        if (side == "b") {
          tt <- seq(0, pi, length.out = 100)
        } else {
          tt <- seq(0, -pi, length.out = 100)
        }
      }
      pos <- tibble(.x = r * cos(tt) + cx,
                    .y = r * sin(tt) + cy,
                    .group = id)
      dplyr::bind_cols(pos, row[rep_len(1, 100), nm, drop = FALSE])
    }
  })
}

#' @noRd
switch_side <- function(x) {
  if (!is.character(x)) {
    x <- as.character(x)
  }
  x <- tolower(x)
  x[x == "right"] <- "r"
  x[x == "left"] <- "l"
  x[x == "top"] <- "t"
  x[x == "bottom"] <- "b"
  x
}

