#' @export
qpairs <- function(data,
                   mapping = NULL,
                   axis_child = TRUE,
                   data2 = NULL,
                   type = "full",
                   diag = TRUE,
                   rasterize = TRUE,
                   res = NULL,
                   grid_col = "grey50",
                   grid_size = 0.25,
                   ...) {
  df <- .pairs_tbl(data = data,
                   data2 = data2,
                   type = type,
                   diag = diag,
                   mapping = mapping,
                   ...)

  ## init and add panel grid
  p <- hyplot(df) +
    geom_panel_grid(colour = grid_col, size = grid_size) +
    ggplot2::coord_fixed(expand = FALSE) +
    theme(panel.background = element_blank())

  ## add child axis
  if (isTRUE(axis_child)) {
    p <- p
  }
  class(p) <- c("qpairs", class(p))
  p
}

#' Pairs Later
#' @description This function can be used to add plot on a scatter matrix plot.
#' @inheritParams geom_ggplot
#' @param ptype plot type.
#' @param ID character, used to add elements based on ID.
#' @param theme a ggplot theme object.
#' @rdname geom_pairs
#' @author Hou Yun
#' @export
geom_pairs <- function(mapping = NULL,
                       data = NULL,
                       stat = "identity",
                       position = "identity",
                       ...,
                       ptype = plot_type(),
                       ID = NULL,
                       rasterize = FALSE,
                       res = 100,
                       na.rm = FALSE,
                       show.legend = "collect",
                       inherit.aes = TRUE) {
  structure(list(mapping = mapping,
                 data = data,
                 stat = stat,
                 position = position,
                 ptype = ptype,
                 ID = ID,
                 rasterize = rasterize,
                 res = res,
                 na.rm = na.rm,
                 show.legend = show.legend,
                 inherit.aes = inherit.aes,
                 ...), class = "geom_pairs")
}

#' @method ggplot_add geom_pairs
#' @export
ggplot_add.geom_pairs <- function(object, plot, object_name) {
  data <- object$data %||% plot$data
  if (is.function(data)) {
    data <- data(plot$data)
  }
  if (!is.null(object$ID)) {
    id <- grepl(object$ID, data$ID)
    data <- data[id, , drop = FALSE]
  }

  if (empty(data)) {
    return(plot)
  }

  gglist <- lapply(seq_len(nrow(data)), function(ii) {
    .build_plot(plot = data$.plot[[ii]],
                type = data$.type[ii],
                pos = data$.pos[ii],
                ptype = object$ptype)
  })
  object <- object[setdiff(names(object), c("ID", "ptype"))]
  object <- modifyList(object,
                       list(data = data,
                            mapping = plot$mapping,
                            gglist = gglist,
                            inherit.aes = FALSE,
                            width = 1,
                            height = 1,
                            width_unit = "native",
                            height_unit = "native"))
  object <- do.call(geom_ggplot, object)
  ggplot_add(object, plot, object_name)
}

#' @noRd
.build_plot <- function(plot, type, pos, ptype) {
  ptype <- .get_plot_type(type, pos, ptype)
  layers <- lapply(ptype, function(pt) {
    if (pt == "density") {
      if (pos == "diag") {
        ggplot2::geom_density(aes_string(y = "..density.."))
      } else {
        ggplot2::geom_density2d()
      }
    } else if (pt == "lm") {
      ggplot2::geom_smooth(method = "lm")
    } else if (pt == "bar") {
      if (is_binary(rlang::eval_tidy(plot$mapping$x, plot$data))) {
        mp <- plot$mapping[setdiff(names(plot$mapping), "y")]
        ggplot2::geom_bar(mapping = mp, inherit.aes = FALSE)
      } else {
        mp <- plot$mapping[setdiff(names(plot$mapping), "x")]
        ggplot2::geom_bar(mapping = mp, inherit.aes = FALSE)
      }
    } else {
      .FUN <- match.fun(paste0("geom_", pt))
      do.call(.FUN, list())
    }
  })
  p <- plot + layers
  p
}

#' Set Plot Type
#' @description This function can be used to set plot type of scatter matrix plot.
#' @param ... arguments in \code{tag = value} form.
#' @return a list of plot type.
#' @rdname plot_type
#' @author Hou Yun
#' @export
plot_type <- function(...) {
  params <- list(...)
  if (length(params) >= 1) {
    nm <- names(params)
    if (!all(nm %in% c("cd", "dc", "cc", "dd", "diag", "upper", "lower"))) {
      stop("Invalid plot type params in `plot_type()`.", call. = FALSE)
    }
    vv <- vapply(params, function(p) {
      is.atomic(p) || is.function(p) || inherits(p, "plot_type")
    }, logical(1))
    if (!all(vv)) {
      stop("All elements of plot_type should a atomic vecter, a function\n",
           "or a plot_type object.", call. = FALSE)
    }

    not_fun <- !vapply(params, is.function, logical(1))
    if (!all(unlist(params[not_fun]) %in% c("point", "histogram", "bar", "boxplot", "violin",
                                            "density", "lm", "smooth", "path", "line"))) {
      stop("Invalid plot type values in plot_type().", call. = FALSE)
    }
  }
  class(params) <- "plot_type"
  params
}

#' @noRd
.get_plot_type <- function(type, pos, ptype) {

  if (pos == "diag") {
    diag <- ptype[["diag"]] %||% list()
    out <- diag[[type]] %||% ptype[[type]] %||% .default_plot_type[["diag"]][[type]]
  }
  if (pos == "upper") {
    upper <- ptype[["upper"]] %||% list()
    out <- upper[[type]] %||% ptype[[type]] %||% .default_plot_type[["upper"]][[type]]
  }
  if (pos == "lower") {
    lower <- ptype[["lower"]] %||% list()
    out <- lower[[type]] %||% ptype[[type]] %||% .default_plot_type[["lower"]][[type]]
  }
  if (pos == "full") {
    full <- ptype[["full"]]
    out <- full[[type]] %||% ptype[[type]] %||% .default_plot_type[["full"]][[type]]
  }
  out
}

#' @noRd
.default_plot_type <- list(diag = plot_type(dd = "bar",
                                            cc = "density"),
                           full = plot_type(dd = "bar",
                                            cc = "point",
                                            cd = "boxplot",
                                            dc = "boxplot"),
                           lower = plot_type(dd = "bar",
                                             cc = "point",
                                             cd = "boxplot",
                                             dc = "boxplot"),
                           upper = plot_type(dd = "bar",
                                             cc = "point",
                                             cd = "boxplot",
                                             dc = "boxplot"),
                           dd = "point",
                           cc = "point",
                           cd = "boxplot",
                           dc = "boxplot")

#' @noRd
.pairs_tbl <- function(data,
                       data2 = NULL,
                       type = "full",
                       diag = TRUE,
                       mapping = NULL,
                       ...) {
  data <- as.data.frame(data)
  if (is.null(data2)) {
    data2 <- data
  } else {
    data2 <- as.data.frame(data2)
  }
  if (any(empty(data), empty(data2))) {
    stop("Input data is empty.", call. = FALSE)
  }
  if (nrow(data) != nrow(data2)) {
    stop("data2 should have same rows as data.", call. = FALSE)
  }

  d_type <- ifelse(vapply(data, is_binary, logical(1)), "d", "c")
  d2_type <- ifelse(vapply(data2, is_binary, logical(1)), "d", "c")

  rnm <- rep(names(data), each = ncol(data2))
  cnm <- rep(names(data2), ncol(data))
  df <- tibble::tibble(ID = paste(rnm, cnm, sep = "-"),
                       .rownames = rnm,
                       .colnames = cnm,
                       .type = paste0(rep(d_type, each = ncol(data2)),
                                      rep(d2_type, ncol(data)))
  )

  if (identical(data, data2)) {
    source_data <- data
  } else {
    source_data <- cbind(data, data2[setdiff(names(data2), names(data))])
  }

  df <- structure(df,
                  row_names = names(data),
                  col_names = names(data2),
                  type = type,
                  diag = diag,
                  class = c("scattermatrix", "md_tbl", class(df)))
  if (!identical(data, data2)) {
    type <- "full"
    diag <- TRUE
  }
  if(type == "upper") {
    df <- extract_upper(df, diag = diag)
  } else if(type == "lower") {
    df <- extract_lower(df, diag = diag)
  } else {
    if(isFALSE(diag)) {
      df <- trim_diag(df)
    }
  }
  df <- .set_position(df)
  params <- list(...)
  nm <- names(params)
  df$.plot <- lapply(seq_len(nrow(df)), function(ii) {
    mapping2 <- aes_string(x = df$.colnames[ii], y = df$.rownames[ii])
    mapping <- aes_modify(mapping2, mapping)
    p <- ggplot(data = source_data, mapping = mapping) + ggplot2::theme_void()
    if (length(params) >= 1) {
      if (is.null(nm)) {
        p <- p + params
      } else {
        if (!is.null(params[[df$.type[ii]]])) {
          p <- p + params[[df$.type[ii]]]
        }
        if (!is.null(params[[df$.pos[ii]]])) {
          p <- p + params[[df$.pos[ii]]]
        }
        p <- p + params[nm == ""]
      }
    }
    p
  })
  df
}

#' @noRd
.set_position <- function(md) {
  if (!identical(row_names(md), col_names(md))) {
    pos <- rep_len("full", nrow(md))
  } else {
    x <- as.integer(factor(md$.rownames, levels = rev(row_names(md))))
    y <- as.integer(factor(md$.colnames, levels = col_names(md)))
    pos <- rep_len("upper", nrow(md))
    pos <- ifelse(x + y < nrows(md) + 1, "lower", pos)
    pos <- ifelse(x + y == nrows(md) + 1, "diag", pos)
  }
  md$.pos <- pos
  md
}

#' @noRd
is_binary <- function(x) {
  is.factor(x) || is.character(x) || is.logical(x)
}


## STOLEN: ggh4x::guide_dendro
#' @title Children axis guide
#' @description This function can be used to add children axis on a ggplot.
#' @inheritParams ggplot2::guide_axis
#' @param child a tibble of child axis information, see examples.
#' @param only_child if TRUE, will remove main axis elements.
#' @param child_size size of child axis label.
#' @rdname axis_child
#' @export
guide_axis_child <- function(title = waiver(),
                             check.overlap = FALSE,
                             angle = NULL,
                             n.dodge = 1,
                             order = 0,
                             position = waiver(),
                             child = NULL,
                             only_child = FALSE,
                             child_size = 7) {

  structure(
    list(title = title,
         check.overlap = check.overlap,
         angle = angle,
         n.dodge = n.dodge,
         order = order,
         position = position,
         child = child,
         only_child = only_child,
         child_size = child_size,
         available_aes = c("x", "y"),
         name = "axis"),
    class = c("guide", "guide_child", "axis")
  )
}

#' @method guide_train guide_child
#' @importFrom ggplot2 guide_train
#' @export
guide_train.guide_child <- function(guide, scale, aesthetic = NULL) {
  guide <- NextMethod()
  if (empty(guide$child)) {
    return(guide)
  }
  if (scale$is_discrete()) {
    id <- guide$child$label %in% guide$key$.label & (!duplicated(guide$child$label))
    guide$child <- guide$child[id, , drop = FALSE]
    guide$is_discrete <- TRUE
  } else {
    guide$is_discrete <- FALSE
  }
  guide
}

#' @method guide_transform guide_child
#' @importFrom ggplot2 guide_transform
#' @importFrom rlang :=
#' @importFrom grid unit.c gList
#' @importFrom gtable gtable gtable_height gtable_width
#' @export
guide_transform.guide_child <- function(guide, coord, panel_params) {
  if (is.null(guide$position) || nrow(guide$key) == 0) {
    return(guide)
  }
  key <- guide$key
  child <- guide$child
  aesthetics <- names(guide$key)[!grepl("^\\.", names(guide$key))]
  other_aesthetic <- setdiff(c("x", "y"), aesthetics)
  override_value <- if (guide$position %in% c("bottom", "left")) -Inf else Inf
  guide$key[[other_aesthetic]] <- override_value
  guide$key <- coord$transform(guide$key, panel_params)

  if (empty(child)) {
    return(guide)
  }

  ch <- tibble(!!aesthetics := numeric(0),
               .value = character(0),
               .label = character(0))

  if (isTRUE(guide$is_discrete)) {
    key[[aesthetics]] <- unclass(key[[aesthetics]])
    id <- match(child$label, key$.label)
    child[[aesthetics]] <- key[[aesthetics]][id]

    for (row in split(child, seq_len(nrow(child)))) {
      limits <- unlist(row$limits)
      breaks <- unlist(row$breaks)
      if (is.factor(breaks)) {
        breaks <- levels(breaks)
      }
      if (is.character(breaks)) {
        labels <- breaks
        breaks <- seq_along(breaks)
      } else {
        if (all(is.na(breaks))) {
          breaks <- pretty_in_range(limits)
        }
        labels <- breaks
      }

      pos <- scales::rescale(breaks, c(-0.5, 0.5), limits)
      ch <- dplyr::bind_rows(ch, tibble(!!aesthetics := pos + row[[aesthetics]],
                                        .value = labels,
                                        .label = labels))
    }
  } else {
    MIN <- if (aesthetics == "x") panel_params$x.range[1] else panel_params$y.range[1]
    MAX <- if (aesthetics == "x") panel_params$x.range[2] else panel_params$y.range[2]
    child$from <- ifelse(child$from < MIN, MIN, child$from)
    child$to <- ifelse(child$to > MAX, MAX, child$to)
    child <- child[child$from < child$to, , drop = FALSE]
    if (empty(child)) {
      return(guide)
    }

    for (row in split(child, seq_len(nrow(child)))) {
      limits <- unlist(row$limits)
      breaks <- unlist(row$breaks)
      if (is.factor(breaks)) {
        breaks <- levels(breaks)
      }
      if (is.character(breaks)) {
        labels <- breaks
        breaks <- seq_along(breaks)
      } else {
        if (all(is.na(breaks))) {
          breaks <- pretty_in_range(limits)
        }
        labels <- breaks
      }

      pos <- scales::rescale(breaks, c(row$from, row$to), limits)
      ch <- dplyr::bind_rows(ch, tibble(!!aesthetics := pos,
                                        .value = breaks,
                                        .label = labels))
    }
  }
  ch[[other_aesthetic]] <- override_value
  guide$child <- coord$transform(ch, panel_params)
  guide
}

#' @noRd
pretty_in_range <- function(x) {
  rng <- range(x, na.rm = TRUE)
  x <- pretty(x)
  x[x > rng[1] & x < rng[2]]
}

#' @method guide_gengrob guide_child
#' @importFrom ggplot2 guide_gengrob
#' @export
guide_gengrob.guide_child <- function(guide, theme) {
  if (empty(guide$child)) {
    guide <- NextMethod()
    return(guide)
  }

  draw_axis <- utils::getFromNamespace("draw_axis", "ggplot2")
  axis_position <- match.arg(guide$position, c("top", "bottom", "right", "left"))
  aesthetic <- if (axis_position %in% c("bottom", "top")) "x" else "y"

  ## child theme not equal theme
  child_theme <- theme
  line_element_name <- paste0("axis.line.", aesthetic, ".", axis_position)
  tick_element_name <- paste0("axis.ticks.", aesthetic, ".", axis_position)
  label_element_name <- paste0("axis.text.", aesthetic, ".", axis_position)

  child_label_element <- ggplot2::calc_element(label_element_name, child_theme)
  child_label_element <- child_label_element %||% ggplot2::element_text()
  child_label_element$size <- guide$child_size
  child_theme[[label_element_name]] <- child_label_element
  child_grobs <- draw_axis(break_positions = guide$child[[aesthetic]],
                           break_labels = guide$child$.label,
                           axis_position = guide$position,
                           theme = child_theme,
                           check.overlap = guide$check.overlap,
                           angle = guide$angle,
                           n.dodge = guide$n.dodge)
  if (isTRUE(guide$only_child)) {
    return(child_grobs)
  }

  theme[[tick_element_name]] <- ggplot2::element_blank()
  theme[[line_element_name]] <- ggplot2::element_blank()
  main_grobs <- draw_axis(break_positions = guide$key[[aesthetic]],
                          break_labels = guide$key$.label,
                          axis_position = guide$position,
                          theme = theme,
                          check.overlap = guide$check.overlap,
                          angle = guide$angle,
                          n.dodge = guide$n.dodge)


  ## unit main and child axis
  if (axis_position %in% c("left", "right")) {
    width <- if (axis_position == "left") {
      unit.c(grobWidth(main_grobs), grobWidth(child_grobs))
    } else {
      unit.c(grobWidth(child_grobs), grobWidth(main_grobs))
    }
    height <- unit(1, "null")
    gt <- gtable(widths = width, heights = height)
    if (axis_position == "left") {
      gt <- gtable_add_grob(gt, grobs = list(main_grobs, child_grobs),
                            t = c(1, 1), l = c(1, 2))
    } else {
      gt <- gtable_add_grob(gt, grobs = list(main_grobs, child_grobs),
                            t = c(1, 1), l = c(2, 1))
    }
  } else {
    height <- if (axis_position == "top") {
      unit.c(grobHeight(main_grobs), grobHeight(child_grobs))
    } else {
      unit.c(grobHeight(child_grobs), grobHeight(main_grobs))
    }
    width <- unit(1, "null")
    gt <- gtable(widths = width, heights = height)
    if (axis_position == "top") {
      gt <- gtable_add_grob(gt, grobs = list(main_grobs, child_grobs),
                            t = c(1, 2), l = c(1, 1))
    } else {
      gt <- gtable_add_grob(gt, grobs = list(main_grobs, child_grobs),
                            t = c(2, 1), l = c(1, 1))
    }
  }
  gTree(children = gList(gt), width = gtable_width(gt), height = gtable_height(gt),
        cl = "absoluteGrob")
}

#' @importFrom grid grobHeight
#' @noRd
grobHeight.absoluteGrob <- function(x) {
  grobs <- x$children
  hl <- lapply(grobs, function(g) {
    if (inherits(g, "gtable")) {
      gtable::gtable_height(g)
    } else {
      grid::grobHeight(g)
    }
  })
  Reduce("sum", hl)
}

#' @importFrom grid grobWidth
#' @noRd
grobWidth.absoluteGrob <- function(x) {
  grobs <- x$children
  wl <- lapply(grobs, function(g) {
    if (inherits(g, "gtable")) {
      gtable::gtable_width(g)
    } else {
      grid::grobWidth(g)
    }
  })
  Reduce("sum", wl)
}

#' @title Draw ggplot on ggplot
#' @description This function convert a ggplot object to marker, and then draw it
#' on plot.
#' @inheritParams ggplot2::layer
#' @inheritParams geom_shaping
#' @param gglist a list of ggplot object.
#' @param width_unit,height_unit units of width or height.
#' @param rasterize logical, whether to convert raster image before drawing.
#' @param res positive numeric, used to set the resolution of raster.
#' @section Aesthetics:
#' \code{geom_ggplot()} understands the following aesthetics (required aesthetics are in bold):
#'     \itemize{
#'       \item \strong{\code{x}}
#'       \item \strong{\code{y}}
#'       \item \code{width}
#'       \item \code{height}
#'       \item \code{angle}
#'       \item \code{hjust}
#'       \item \code{vjust}
#'   }
#' @rdname geom_ggplot
#' @author Hou Yun
#' @export
geom_ggplot <- function(mapping = NULL,
                        data = NULL,
                        stat = "identity",
                        position = "identity",
                        gglist = NULL,
                        width_unit = "cm",
                        height_unit = width_unit,
                        rasterize = FALSE,
                        res = 100,
                        ...,
                        na.rm = FALSE,
                        show.legend = "collect",
                        inherit.aes = TRUE) {
  structure(list(mapping = mapping,
                 data = data,
                 stat = stat,
                 position = position,
                 gglist = gglist,
                 width_unit = width_unit,
                 height_unit = height_unit,
                 rasterize = rasterize,
                 res = res,
                 params = list(...),
                 na.rm = na.rm,
                 show.legend = show.legend,
                 inherit.aes = inherit.aes), class = "geom_ggplot")
}

#' @method ggplot_add geom_ggplot
#' @export
ggplot_add.geom_ggplot <- function(object, plot, object_name) {
  data <-  object$data %||% plot$data
  if (is.function(object$data)) {
    data <- object$data(plot$data)
  }
  gglist <- object$gglist
  if (is.ggplot(gglist)) {
    gglist <- list(gglist)
  }
  if (empty(data) || is.null(gglist) || length(gglist) == 0) {
    return(plot)
  }

  mapping <- object$mapping
  if (isTRUE(object$inherit.aes)) {
    mapping <- aes_modify(plot$mapping, mapping)
  }
  if (!all(c("x", "y") %in% names(mapping))) {
    stop("`geom_ggplot` requires the `x` and `y` aesthetics.", call. = FALSE)
  }
  mapping <- mapping[intersect(names(mapping),
                               c("x", "y", "hjust", "vjust", "width", "height", "angle"))]

  if (!all(vapply(gglist, is.ggplot, logical(1)))) {
    stop("all elements of `gglist` should be a ggplot object.", call. = FALSE)
  }

  thm <- .get_theme(plot)
  show.legend <- object$show.legend
  if (identical(thm$legend.position, "none")) {
    show.legend <- FALSE
  }
  if (length(thm$legend.position) == 2 || isTRUE(show.legend) ||
      is.na(show.legend)) {
    show.legend <- "keep"
  }
  if (identical(show.legend, FALSE)) {
    gglist <- lapply(gglist, function(gg) gg + theme(legend.position = "none"))
  }
  if (identical(show.legend, "collect")) {
    gdefs <- NULL
    for (ii in seq_along(gglist)) {
      gdefs <- .merge_guide_grobs(gdefs, .get_guides(gglist[[ii]]))
      gglist[[ii]] <- gglist[[ii]] + theme(legend.position = "none")
    }
    attr(plot, "guides") <- .merge_guide_grobs(attr(plot, "guides"), gdefs)
    class(plot) <- c("gggplot", class(plot))
  }

  mk <- rename_grobs(x = lapply(gglist, ggplotGrob), force = TRUE)
  params <- c(list(mapping = mapping,
                   data = data,
                   marker = mk,
                   show.legend = FALSE,
                   width_unit = object$width_unit,
                   height_unit = object$height_unit,
                   rasterize = object$rasterize,
                   res = object$res), object$params)

  object <- do.call(geom_shaping, params)
  ggplot_add(object, plot, object_name)
}

#' @importFrom ggplot2 ggplot_build
#' @export
ggplot_build.gggplot <- function(plot) {
  gdefs <- attr(plot, "guides")
  if (is.null(gdefs) || length(gdefs) < 1) {
    plot <- NextMethod()
    plot <- ggplot2::ggplot_gtable(plot)
  } else {
    plot <- .rebuild_guides(plot)
  }
  plot
}

#' @export
print.gggplot <- function(x, ...) {
  x <- ggplot_build(x)
  grid::grid.newpage()
  grid.draw(x)
}

#' @export
plot.gggplot <- print.gggplot

#' @noRd
.get_theme <- function(plot) {
  ggplot2::theme_get() + plot$theme
}

#' @noRd
.get_guides <- function(gg) {
  if (!inherits(gg, "gtable")) {
    if (inherits(gg, "gggplot")) {
      class(gg) <- setdiff(class(gg), "gggplot")
    }
    gg <- ggplot2::ggplotGrob(gg)
  }

  guides <- gtable::gtable_filter(gg, "guide-box")
  if (nrow(guides) < 1) {
    return(NULL)
  }
  guides <- guides$grobs[[1]]
  nm <- gsub("\\d+_", "", rownames(guides))
  guides <- gtable::gtable_filter(guides, "guides")$grobs
  rlang::set_names(guides, nm)
}

#' @noRd
.merge_guide_grobs <- function(gdefs, gdefs2) {
  if (is.null(gdefs)) {
    return(gdefs2)
  }

  if (is.null(gdefs2)) {
    return(gdefs)
  }

  id <- !names(gdefs2) %in% names(gdefs)
  c(gdefs, gdefs2[id])
}

#' @noRd
.get_data <- function(plot) {
  params <- ggplot2::ggplot_build(plot)$layout$panel_params[[1]]
  tibble(x = min(params$x.range),
         y = min(params$y.range),
         colour = "A")
}

## STOLEN: ggplot2:::guides_build
## TODO: rebuild axis
#' @importFrom ggplot2 ggplotGrob is.ggplot
#' @importFrom gtable gtable_row gtable_add_row_space gtable_add_cols gtable_add_rows
#' @importFrom gtable gtable_add_row_space gtable_col gtable_add_col_space
#' @importFrom gtable gtable_add_grob
#' @noRd
.rebuild_guides <- function(plot) {
  gdefs <- attr(plot, "guides")
  class(plot) <- setdiff(class(plot), "gggplot")
  if (is.null(gdefs)) {
    return(plot)
  }

  theme <- .get_theme(plot)
  plot_guides <- .get_guides(plot)
  is_null_plot_guides <- is.null(plot_guides)

  gdefs <- .merge_guide_grobs(plot_guides, gdefs)

  theme$legend.spacing <- theme$legend.spacing %||% grid::unit(0.5, "lines")
  theme$legend.spacing.y <- theme$legend.spacing.y  %||% theme$legend.spacing
  theme$legend.spacing.x <- theme$legend.spacing.x  %||% theme$legend.spacing

  widths <- do.call(grid::unit.c, lapply(gdefs, function(g) sum(g$widths)))
  heights <- do.call(grid::unit.c, lapply(gdefs, function(g) sum(g$heights)))



  # setting that is different for vertical and horizontal guide-boxes.
  if (identical(theme$legend.box, "horizontal")) {
    guides <- gtable_row(name = "guides",
                         grobs = gdefs,
                         widths = widths, height = max(heights))

    # add space between the guide-boxes
    guides <- gtable_add_col_space(guides, theme$legend.spacing.x)

  } else { # theme$legend.box == "vertical"
    guides <- gtable_col(name = "guides",
                         grobs = gdefs,
                         width = max(widths), heights = heights)

    # add space between the guide-boxes
    guides <- gtable_add_row_space(guides, theme$legend.spacing.y)
  }

  # Add margins around the guide-boxes.
  theme$legend.box.margin <- theme$legend.box.margin %||% ggplot2::margin()
  guides <- gtable_add_cols(guides, theme$legend.box.margin[4], pos = 0)
  guides <- gtable_add_cols(guides, theme$legend.box.margin[2], pos = ncol(guides))
  guides <- gtable_add_rows(guides, theme$legend.box.margin[1], pos = 0)
  guides <- gtable_add_rows(guides, theme$legend.box.margin[3], pos = nrow(guides))

  # Add legend box background
  background <- ggplot2::element_grob(theme$legend.box.background %||% element_blank())

  guides <- gtable_add_grob(guides, background, t = 1, l = 1,
                            b = -1, r = -1, z = -Inf, clip = "off", name = "legend.box.background")
  guides$name <- "guide-box"

  ## hack method to find legend-box position
  if (is_null_plot_guides) {
    df <- .get_data(plot)
    plot <- plot + ggplot2::geom_point(mapping = ggplot2::aes_all(names(df)),
                                       data = df,
                                       alpha = 0,
                                       size = 0.01,
                                       inherit.aes = FALSE)
  }

  plot <- ggplotGrob(plot)
  id <- which(plot$layout$name == "guide-box")
  xy <- plot$layout[id, , drop = FALSE]
  plot$grobs[[id]] <- guides

  if (theme$legend.position %in% c("left", "right")) {
    plot$widths[xy$l] <- grid::widthDetails(guides)
  } else {
    plot$heights[xy$t] <- grid::heightDetails(guides)
  }
  plot
}
