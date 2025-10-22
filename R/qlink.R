## TODO: can draw link-curve from continuous axis?
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
#' @param nsteps integer, the value determines the smoothness of the curve.
#' @param hratio range 0 to 1, height ratio of half-circle link.
#' @return a ggplot object.
#' @author Hou Yun
#' @importFrom ggplot2 geom_path
#' @importFrom ggplot2 geom_segment
#' @export
qlink <- function(graph,
                  mapping = NULL,
                  ...,
                  geom = "path",
                  widths = NULL,
                  heights = NULL,
                  guides = "collect",
                  nsteps = 100,
                  hratio = 0.33) {

  ## check input data, split annotation plot and params
  if (!inherits(graph, "igraph")) {
    graph <- tryCatch(igraph::as.igraph(graph),
                      error = function(e) tidygraph::as_tbl_graph(graph))
    if (inherits(graph, "tbl_graph")) {
      graph <- igraph::as.igraph(graph)
    }
  }
  nodes <- igraph::as_data_frame(graph, "vertices")
  graph <- igraph::as_data_frame(graph)
  nodes$name <- long_to_short(nodes$name)
  graph$from <- long_to_short(graph$from)
  graph$to <- long_to_short(graph$to)

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

  for (ii in names(limits)) {
    limits[[ii]] <- paste(ii, limits[[ii]], sep = ".-.")
  }
  if (anyDuplicated(unlist(limits))) {
    stop("Contain duplicate id,\n",
         "maybe not set `from` and `to` id?", call. = FALSE)
  }

  graph <- inner_link_side(graph, limits = limits)

  if (identical(nm, "r")) {
    xx <- rep(0, length(limits$r))
    yy <- seq_along(limits$r)
    pos_x <- get_xy_pos(xx, limits$r)
    pos_y <- get_xy_pos(yy, limits$r)
  } else if (identical(nm, "l")) {
    xx <- rep(1, length(limits$l))
    yy <- seq_along(limits$l)
    pos_x <- get_xy_pos(xx, limits$l)
    pos_y <- get_xy_pos(yy, limits$l)
  } else if (identical(nm, "t")) {
    xx <- seq_along(limits$t)
    yy <- rep(1, length(limits$t))
    pos_x <- get_xy_pos(xx, limits$t)
    pos_y <- get_xy_pos(yy, limits$t)

  } else if (identical(nm, "b")) {
    xx <- seq_along(limits$b)
    yy <- rep(0, length(limits$b))
    pos_x <- get_xy_pos(xx, limits$b)
    pos_y <- get_xy_pos(yy, limits$b)
  } else if (identical(sort(nm), c("l", "r"))) {
    xx <- c(rep(0, length(limits$l)), rep(1, length(limits$r)))
    yy <- c(seq_along(limits$l),
            scales::rescale(seq_along(limits$r), rngs$l, rngs$r))
    pos_x <- get_xy_pos(xx, c(limits$l, limits$r))
    pos_y <- get_xy_pos(yy, c(limits$l, limits$r))
  } else if (identical(sort(nm), c("l", "t"))) {
    xx <- c(seq_along(limits$t), rep_len(rngs$t[1], length(limits$l)))
    yy <- c(rep_len(rngs$l[2], length(limits$t)), seq_along(limits$l))
    pos_x <- get_xy_pos(xx, c(limits$t, limits$l))
    pos_y <- get_xy_pos(yy, c(limits$t, limits$l))
  } else if (identical(sort(nm), c("b", "l"))) {
    xx <- c(seq_along(limits$b), rep_len(rngs$b[1], length(limits$l)))
    yy <- c(rep_len(rngs$l[1], length(limits$b)), seq_along(limits$l))
    pos_x <- get_xy_pos(xx, c(limits$b, limits$l))
    pos_y <- get_xy_pos(yy, c(limits$b, limits$l))
  } else if (identical(sort(nm), c("b", "t"))) {
    xx <- c(seq_along(limits$b),
            scales::rescale(seq_along(limits$t), rngs$b, rngs$t))
    yy <- c(rep(0, length(limits$b)), rep(1, length(limits$t)))
    pos_x <- get_xy_pos(xx, c(limits$b, limits$t))
    pos_y <- get_xy_pos(yy, c(limits$b, limits$t))
  } else if (identical(sort(nm), c("b", "r"))) {
    xx <- c(seq_along(limits$b), rep_len(rngs$b[2], length(limits$r)))
    yy <- c(rep_len(rngs$r[1], length(limits$b)), seq_along(limits$r))
    pos_x <- get_xy_pos(xx, c(limits$b, limits$r))
    pos_y <- get_xy_pos(yy, c(limits$b, limits$r))
  } else if (identical(sort(nm), c("r", "t"))) {
    xx <- c(seq_along(limits$t), rep_len(rngs$t[2], length(limits$r)))
    yy <- c(rep_len(rngs$r[2], length(limits$t)), seq_along(limits$r))
    pos_x <- get_xy_pos(xx, c(limits$t, limits$r))
    pos_y <- get_xy_pos(yy, c(limits$t, limits$r))
  } else if (identical(sort(nm), c("l", "r", "t"))) {
    xx <- c(seq_along(limits$t), rep_len(rngs$t[2], length(limits$r)),
            rep_len(rngs$t[1], length(limits$l)))
    yy <- c(rep(rngs$l[2], length(limits$t)),
            scales::rescale(seq_along(limits$r), rngs$l, rngs$r),
            seq_along(limits$l))
    pos_x <- get_xy_pos(xx, c(limits$t, limits$r, limits$l))
    pos_y <- get_xy_pos(yy, c(limits$t, limits$r, limits$l))
  } else if (identical(sort(nm), c("b", "l", "t"))) {
    xx <- c(seq_along(limits$b),
            rep_len(rngs$b[1], length(limits$l)),
            scales::rescale(seq_along(limits$t), rngs$b, rngs$t))
    yy <- c(rep(rngs$l[1], length(limits$b)),
            seq_along(limits$l),
            rep(rngs$l[2], length(limits$t)))
    pos_x <- get_xy_pos(xx, c(limits$b, limits$l, limits$t))
    pos_y <- get_xy_pos(yy, c(limits$b, limits$l, limits$t))
  } else if (identical(sort(nm), c("b", "r", "t"))) {
    xx <- c(seq_along(limits$b),
            rep_len(rngs$b[2], length(limits$r)),
            scales::rescale(seq_along(limits$t), rngs$b, rngs$t))
    yy <- c(rep(rngs$r[1], length(limits$b)),
            seq_along(limits$r),
            rep(rngs$r[2], length(limits$t)))
    pos_x <- get_xy_pos(xx, c(limits$b, limits$r, limits$t))
    pos_y <- get_xy_pos(yy, c(limits$b, limits$r, limits$t))
  } else if (identical(sort(nm), c("b", "l", "r"))) {
    xx <- c(seq_along(limits$b),
            rep(rngs$b[1], length(limits$l)),
            rep(rngs$b[2], length(limits$r)))
    yy <- c(rep(rngs$l[1], length(limits$b)),
            seq_along(limits$l),
            scales::rescale(seq_along(limits$r), rngs$l, rngs$r))
    pos_x <- get_xy_pos(xx, c(limits$b, limits$l, limits$r))
    pos_y <- get_xy_pos(yy, c(limits$b, limits$l, limits$r))
  } else {
    xx <- c(seq_along(limits$b),
            scales::rescale(seq_along(limits$t), rngs$b, rngs$t),
            rep(rngs$b[1], length(limits$l)),
            rep(rngs$b[2], length(limits$r)))
    yy <- c(rep(rngs$l[1], length(limits$b)),
            rep(rngs$l[2], length(limits$t)),
            seq_along(limits$l),
            scales::rescale(seq_along(limits$r), rngs$l, rngs$r))
    pos_x <- get_xy_pos(xx, c(limits$b, limits$t, limits$l, limits$r))
    pos_y <- get_xy_pos(yy, c(limits$b, limits$t, limits$l, limits$r))
  }

  graph$.y <- pos_y[graph$from]
  graph$.yend <- pos_y[graph$to]
  graph$.x <- pos_x[graph$from]
  graph$.xend <- pos_x[graph$to]
  graph$.isEdges <- TRUE
  .x <- .y <- .xend <- .yend <- NULL
  graph <- dplyr::filter(graph, !is.na(.x), !is.na(.y), !is.na(.xend), !is.na(.yend))
  empty_edges <- empty(graph)
  nodes$.x <- pos_x[nodes$name]
  nodes$.y <- pos_y[nodes$name]
  nodes$.isEdges <- FALSE
  graph <- dplyr::bind_rows(graph, nodes)

  if (empty_edges) {
    mapping <- aes_modify(mapping, aes(x = `.x`, y = `.y`,
                                              xend = `.xend`, yend = `.yend`))
    p <- ggplot(graph, mapping) + ggplot2::theme_void()
  } else {
    graph <- gen_half_circle(graph,
                             ranges = rngs,
                             limits = limits,
                             nsteps = nsteps,
                             hratio = hratio)
    if (!grepl("geom_", geom, fixed = TRUE)) {
      geom <- paste0("geom_", geom)
    }
    mapping <- aes_modify(mapping, aes(x = `.x`,
                                              y = `.y`,
                                              group = `.group`))
    data <- function(data) data[data$.isEdges, , drop = FALSE]
    p <- ggplot(graph, mapping) + do.call(geom, c(params, list(data = data)))
    p <- p + ggplot2::scale_x_continuous(limits = rngs$b %||% rngs$t,
                                         expand = c(0, 0))
    p <- p + ggplot2::scale_y_continuous(limits = rngs$l %||% rngs$r,
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
    width_l <- widths$l %||% 1
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
    height_t <- heights$t %||% 1
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
  plotlist[id_row] <- c(anno$l, list(plot), anno$r)
  plotlist[id_col] <- c(anno$t, list(plot), anno$b)

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
gen_half_circle <- function(data, ranges, limits,
                            nsteps = 100, hratio = 0.33) {
  edges <- data[data$.isEdges, , drop = FALSE]
  nodes <- data[!data$.isEdges, , drop = FALSE]
  nm <- setdiff(names(edges), c(".x", ".y", ".xend", ".yend", ".inner"))
  edges <- purrr::map2_dfr(seq_len(nrow(edges)), edges$.inner, function(id, side) {
    row <- edges[id, , drop = FALSE]

    if (side %in% c("l", "r")) {
      rngs <- ranges$b %||% ranges$t %||% c(0, 1)
      ry <- 0.5 * abs(row$.yend - row$.y)
      cx <- 0.5 * (row$.x + row$.xend)
      cy <- 0.5 * (row$.y + row$.yend)
      ratio <- ry / (length(limits[[side]]) - 1) * 2
      rx <- hratio*ratio*diff(rngs)

      if (row$.yend > row$.y) {
        if (side == "l") {
          tt <- seq(-pi/2, pi/2, length.out = nsteps)
        } else {
          tt <- seq(pi * 1.5, pi/2, length.out = nsteps)
        }
      } else {
        if (side == "l") {
          tt <- seq(pi/2, -pi/2, length.out = nsteps)
        } else {
          tt <- seq(pi/2, pi * 1.5, length.out = nsteps)
        }
      }
      pos <- tibble::tibble(.x = rx * cos(tt) + cx,
                            .y = ry * sin(tt) + cy,
                            .group = id,
                            .index = seq_len(nsteps))
      dplyr::bind_cols(pos, row[rep_len(1, nsteps), nm, drop = FALSE])
    } else if (side %in% c("b", "t")) {
      rngs <- ranges$l %||% ranges$r %||% c(0, 1)
      rx <- 0.5 * abs(row$.xend - row$.x)
      cx <- 0.5 * (row$.x + row$.xend)
      cy <- 0.5 * (row$.y + row$.yend)
      ratio <- rx / (length(limits[[side]]) - 1) * 2
      ry <- hratio*ratio*diff(rngs)

      if (row$.xend > row$.x) {
        if (side == "b") {
          tt <- seq(pi, 0, length.out = nsteps)
        } else {
          tt <- seq(-pi, 0, length.out = nsteps)
        }
      } else {
        if (side == "b") {
          tt <- seq(0, pi, length.out = nsteps)
        } else {
          tt <- seq(0, -pi, length.out = nsteps)
        }
      }
      pos <- tibble(.x = rx * cos(tt) + cx,
                    .y = ry * sin(tt) + cy,
                    .group = id,
                    .index = seq_len(nsteps))
      dplyr::bind_cols(pos, row[rep_len(1, nsteps), nm, drop = FALSE])
    } else {
      pos <- tibble(.x = c(row$.x, row$.xend),
                    .y = c(row$.y, row$.yend),
                    .group = id,
                    .index = seq_len(2))
      dplyr::bind_cols(pos, row[rep_len(1, 2), nm, drop = FALSE])
    }
  })
  dplyr::bind_rows(edges, nodes[setdiff(names(nodes), c(".xend", ".yend", ".inner"))])
}

#' @noRd
long_to_short <- function(x) {
  if (is.null(x)) {
    return(x)
  }

  if (!is.character(x)) {
    x <- as.character(x)
  }

  contain_side <- grepl(".-.", x, fixed = TRUE)
  if (!any(contain_side)) {
    return(x)
  }

  temp <- unlist(strsplit(x[contain_side], ".-.", fixed = TRUE))
  side <- tolower(temp[rep(c(TRUE, FALSE), sum(contain_side))])
  name <- temp[rep(c(FALSE, TRUE), sum(contain_side))]

  side[side == "right"] <- "r"
  side[side == "left"] <- "l"
  side[side == "top"] <- "t"
  side[side == "bottom"] <- "b"
  side[!side %in% c("r", "l", "b", "t")] <- NA

  x[contain_side] <- paste_with_na(side, name, sep = ".-.")
  x
}

#' @noRd
inner_link_side <- function(data, limits) {
  if (length(limits) == 1) {
    data$.inner <- names(limits)
  } else {
    nm <- names(limits)
    n <- length(nm)
    data$.inner <- purrr::map2_chr(data$from, data$to, function(from, to) {
       inner_from <- inner_to <- NA_character_
      for (ii in seq_along(nm)) {
        if (grepl(".-.", from, fixed = TRUE)) {
          if (from %in% limits[[ii]]) {
            inner_from <- nm[ii]
          }
        } else {
          lim <- gsub("^[rltb]\\.\\-\\.", "", limits[[ii]])
          if (from %in% lim) {
            inner_from <- nm[ii]
          }
        }

        if (grepl(".-.", to, fixed = TRUE)) {
          if (to %in% limits[[ii]]) {
            inner_to <- nm[ii]
          }
        } else {
          lim <- gsub("^[rltb]\\.\\-\\.", "", limits[[ii]])
          if (to %in% lim) {
            inner_to <- nm[ii]
          }
        }

        if (anyNA(c(inner_from, inner_to)) || !identical(inner_from, inner_to)) {
          inner <- NA_character_
        } else {
          inner <- inner_from
        }
      }
       inner
    })
  }
  data
}

#' @noRd
get_xy_pos <- function(x, nm) {
  short <- gsub("^[rltb]\\.\\-\\.", "", nm)
  if (identical(short, nm)) {
    rlang::set_names(x, nm)
  } else {
    rlang::set_names(c(x, x), c(nm, short))
  }
}

#' @rdname qlink
#' @export
extract_nodes <- function() {
  function(data) {
    data <- data[!data$.isEdges, , drop = FALSE]
    data$name <- gsub("^[rltb]\\.\\-\\.", "", data$name)
    data
  }
}
