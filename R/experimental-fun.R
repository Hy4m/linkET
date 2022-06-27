#' @title Init pairs plot
#' @description This functions can be used to init pairs plot based on ggplot.
#' @param data a data frame.
#' @param mapping default list of aesthetic mappings to use for plot.
#' @param expansion a list of x/y axis expansion of child plot.
#' @param axis_child logical, if (TRUE) will add child plot axis.
#' @param data2 NULL or a data frame.
#' @param except character, which variable will not be contained in plot.
#' @param type character, "full" (default), "upper" or "lower", display
#' full matrix, lower triangular or upper triangular matrix.
#' @param diag logical, if TRUE (default) will keep the diagonal of matrix data.
#' @param rasterize logical, whether to convert raster image before drawing.
#' @param res positive numeric, used to set the resolution of raster.
#' @param grid_col colour of panel grid.
#' @param grid_size size of panel grid.
#' @param drop logical. If TRUE, the unused labels will be removed.
#' @param use_md logical. if TRUE, will use \code{ggtext::element_markdown()} to
#' draw the axis labels.
#' @param ... passed to \code{guide_child_axis()}.
#' @return a ggplot object.
#' @rdname qpairs
#' @author Hou Yun
#' @export
#' @examples \dontrun{
#' qpairs(iris) + geom_pairs()
#' }
qpairs <- function(data,
                   mapping = NULL,
                   expansion = NULL,
                   axis_child = TRUE,
                   data2 = NULL,
                   except = NULL,
                   type = "full",
                   diag = TRUE,
                   rasterize = TRUE,
                   res = NULL,
                   grid_col = "grey50",
                   grid_size = 0.25,
                   drop = TRUE,
                   use_md = NULL,
                   ...) {
  df <- .pairs_tbl(data = data,
                   data2 = data2,
                   except = except,
                   type = type,
                   diag = diag,
                   mapping = mapping,
                   expansion = expansion)

  ## init and add panel grid
  p <- hyplot(df, drop = drop, use_md = use_md) +
    geom_panel_grid(colour = grid_col, size = grid_size) +
    ggplot2::coord_fixed(expand = FALSE) +
    theme(panel.background = element_blank(),
          axis.text = element_text(size = 10.5, colour = "black"),
          axis.title = element_blank(),
          axis.ticks = element_blank(),
          axis.text.y.left = element_text(angle = 90, hjust = 0.5, vjust = 0.5),
          axis.text.y.right = element_text(angle = 90, hjust = 0.5, vjust = 0.5),
          axis.text.x.top = element_text(angle = 0, hjust = 0.5, vjust = 0.5),
          axis.text.x.bottom = element_text(angle = 0, hjust = 0.5, vjust = 0.5))

  ## add child axis
  if (isTRUE(axis_child)) {
    axis_info <- attr(df, "axis_info")
    if (!is.null(axis_info)) {
      params <- list(...)
      params$child <- axis_info
      child_theme <- theme(
        axis.text = element_text(size = 8, colour = "black"),
        axis.title = element_blank(),
        axis.line = element_blank(),
        axis.ticks = ggplot2::element_line(),
        axis.text.x.top = element_text(angle = 90, hjust = 0, vjust = 0.5),
        axis.text.x.bottom = element_text(angle = 90, hjust = 1, vjust = 0.5),
        axis.text.y.left = element_text(angle = 0, hjust = 1, vjust = 0.5),
        axis.text.y.right = element_text(angle = 0, hjust = 0, vjust = 0.5)
      )
      if ("theme" %in% names(params)) {
        params$theme <- child_theme + params$theme
      } else {
        params$theme <- child_theme
      }
      p <- p + ggplot2::guides(x = do.call("guide_axis_child", params),
                               y = do.call("guide_axis_child", params))
    }
  }
  class(p) <- c("qpairs", class(p))
  p
}

#' Pairs Later
#' @description This function can be used to add plot on a scatter matrix plot.
#' @inheritParams geom_ggplot
#' @param ptype plot type.
#' @param ID character, used to add elements based on ID.
#' @rdname geom_pairs
#' @author Hou Yun
#' @export
geom_pairs <- function(mapping = NULL,
                       data = NULL,
                       stat = "identity",
                       position = "identity",
                       ...,
                       ptype = NULL,
                       ID = NULL,
                       rasterize = FALSE,
                       res = 100,
                       na.rm = FALSE,
                       show.legend = "collect",
                       inherit.aes = TRUE) {
  gptype <- options("linkET.pairs.plot")$linkET.pairs.plot$ptype
  ptype <- modify_ptype(gptype, ptype)
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
    data <- object$data(plot$data)
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
                ptype = object$ptype,
                expansion = attr(data, "expansion"))
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
.build_plot <- function(plot, type, pos, ptype, expansion) {
  ptype <- .get_plot_type(type, pos, ptype)
  if (pos != "diag" && ("density" %in% ptype)) {
    ptype[which(ptype == "density")] <- "density_2d"
  }
  if ("histogram" %in% ptype) {
    if (pos == "diag") {
      if (type == "cc") {
        ptype[which(ptype == "histogram")] <- "bar"
      }
    } else {
      ptype[which(ptype == "histogram")] <- "blank"
    }
  }

  if ("corr" %in% ptype && type != "cc") {
    warning("'corr' can only be applied to continuous variables.", call. = FALSE)
    ptype[which(ptype == "corr")] <- "blank"
  }

  if (any(c("histogram", "density", "bar") %in% ptype)) {
    if (pos == "diag") {
      p <- plot + ggplot2::scale_x_discrete(expand = expansion$discrete)
    } else {
      p <- plot
    }

    plot <- switch (type,
                    cc = plot + ggplot2::scale_x_continuous(expand = expansion$continuous),
                    dc = plot + ggplot2::scale_y_discrete(expand = expansion$discrete),
                    cd = plot + ggplot2::scale_x_discrete(expand = expansion$discrete),
                    dd = p
    )
  } else {
    plot <- switch (type,
                    cc = plot +
                      ggplot2::scale_x_continuous(expand = expansion$continuous) +
                      ggplot2::scale_y_continuous(expand = expansion$continuous),
                    dc = plot +
                      ggplot2::scale_x_continuous(expand = expansion$continuous) +
                      ggplot2::scale_y_discrete(expand = expansion$discrete),
                    cd = plot +
                      ggplot2::scale_x_discrete(expand = expansion$discrete) +
                      ggplot2::scale_y_continuous(expand = expansion$continuous),
                    dd = plot +
                      ggplot2::scale_x_discrete(expand = expansion$discrete) +
                      ggplot2::scale_y_discrete(expand = expansion$discrete)
    )
  }
  layers <- .get_layer(ptype)
  id_bar <- which(ptype == "bar")
  if (length(id_bar) > 0) {
    for (i in id_bar) {
      if (type == "dc") {
        layers[[i]]$mapping <- aes_modify(layers[[i]]$mapping, aes_string(x = "..count.."))
      } else {
        layers[[i]]$mapping <- aes_modify(layers[[i]]$mapping, aes_string(y = "..count.."))
      }
    }
  }

  id_corr <- which(ptype == "corr")
  if (length(id_corr) > 0) {
    if ("colour" %in% names(plot$mapping)) {
      plot$mapping$label <- plot$mapping$colour
    }
  }

 for (i in seq_along(layers)) {
   plot <- plot + layers[[i]]
 }
  plot
}

#' @title Register pairs plot
#' @description Init pairs plot layer function.
#' @param ... any valid layer parameters.
#' @param scale a list of aesthestic scale.
#' @param ptype a plot_type object, which can be created by `plot_type()`.
#' @param reset logical, if TRUE will reset pairs plot parameters.
#' @return set global options and return NULL.
#' @author Hou Yun
#' @rdname register_pairs_plot
#' @export
register_pairs_plot <- function(...,
                                ptype = NULL,
                                scale = list(),
                                reset = FALSE) {
  layers <- list("point" = ggplot2::geom_point(),
                 "histogram" = ggplot2::geom_histogram(aes_string(y = "..count..")),
                 "bar" = ggplot2::geom_bar(),
                 "boxplot" = ggplot2::geom_boxplot(),
                 "violin" = ggplot2::geom_violin(),
                 "density" = ggplot2::geom_density(aes_string(y = "..density..")),
                 "density_2d" = ggplot2::geom_density_2d(),
                 "lm" = ggplot2::geom_smooth(method = "lm"),
                 "smooth" = ggplot2::geom_smooth(),
                 "path" = ggplot2::geom_path(),
                 "line" = ggplot2::geom_line(),
                 "hex" = ggplot2::geom_hex(),
                 "blank" = ggplot2::geom_blank(),
                 "jitter" = ggplot2::geom_jitter(),
                 "corr" = geom_corr())
  if (isTRUE(reset)) {
    options("linkET.pairs.plot" = layers)
    return(invisible(NULL))
  }
  gl <- options("linkET.pairs.plot")$linkET.pairs.plot

  if (!is.list(scale)) {
    scale <- as.list(scale)
  }
  if (!is.null(names(scale))) {
    temp <- expand.grid(scale = c("fill", "colour", "color", "alpha", "size", "shape"),
                        type = c("d", "c"))
    valid_scale <- paste(temp$scale, temp$type, sep = "_")
    scale <- scale[names(scale) %in% valid_scale]

    if (length(scale) > 1) {
      gs <- gl$scale %||% list()
      layers <- utils::modifyList(gl, utils::modifyList(layers, list(...)))
      scales <- utils::modifyList(gs, scale)
      layers$scale <- scales
    }
  }

  ## modify plot_type
  if (!inherits(ptype, "plot_type")) {
    ptype <- NULL
  }
  ptype <- ptype %||% plot_type()
  if (is.null(gl$ptype) || length(gl$ptype) < 1) {
    layers$ptype <- ptype
  } else {
    layers$ptype <- modify_ptype(gl$ptype, ptype)
  }
  options("linkET.pairs.plot" = layers)
  invisible(NULL)
}

modify_ptype <- function(ptype1, ptype2) {
  pt <- utils::modifyList(as.list(ptype1), as.list(ptype2))
  pnm <- intersect(names(ptype2), c("full", "diag", "upper", "lower"))
  for (ii in pnm) {
    if (is.null(ptype1[[ii]])) next
    pt[[ii]] <- structure(utils::modifyList(ptype1[[ii]], ptype2[[ii]]),
                          class = "plot_type")
  }
  class(pt) <- "plot_type"
  pt
}

#' @noRd
.get_layer <- function(...) {
  ll <- options("linkET.pairs.plot")$linkET.pairs.plot
  layers <- unlist(list(...))
  lapply(layers, function(x) {
    ll[[x]] %||% do.call(paste0("geom_", x), list())
  })
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
    if (!all(nm %in% c("cd", "dc", "cc", "dd", "diag", "upper", "lower", "full"))) {
      stop("Invalid plot type params in `plot_type()`.", call. = FALSE)
    }
    vv <- vapply(params, function(p) {
      is.atomic(p) || is.function(p) || inherits(p, "plot_type")
    }, logical(1))
    if (!all(vv)) {
      stop("All elements of plot_type should a atomic vecter\n",
           "or a plot_type object.", call. = FALSE)
    }
  }
  class(params) <- "plot_type"
  params
}

#' @noRd
.get_plot_type <- function(type, pos, ptype) {
  if (pos == "diag") {
    diag <- ptype[["diag"]] %||% .default_plot_type[["diag"]]
    out <- diag[[type]] %||% .default_plot_type[[diag]][[type]]
  }
  if (pos == "upper") {
    upper <- ptype[["upper"]] %||% ptype[[type]] %||% .default_plot_type[["upper"]]
    out <- if (is.list(upper)) upper[[type]] else upper
    out <- out %||% .default_plot_type[["upper"]][[type]]
  }
  if (pos == "lower") {
    lower <- ptype[["lower"]] %||% ptype[[type]] %||% .default_plot_type[["lower"]]
    out <- if (is.list(lower)) lower[[type]] else lower
    out <- out %||% .default_plot_type[["lower"]][[type]]
  }
  if (pos == "full") {
    full <- ptype[["full"]] %||% ptype[[type]] %||% .default_plot_type[["full"]]
    out <- if (is.list(full)) full[[type]] else full
    out <- out %||% .default_plot_type[["full"]][[type]]
  }
  out
}

#' @noRd
.default_plot_type <- list(diag = plot_type(dd = "bar",
                                            cc = "density"),
                           full = plot_type(dd = "jitter",
                                            cc = "point",
                                            cd = "boxplot",
                                            dc = "boxplot"),
                           lower = plot_type(dd = "jitter",
                                             cc = "corr",
                                             cd = "boxplot",
                                             dc = "boxplot"),
                           upper = plot_type(dd = "jitter",
                                             cc = "point",
                                             cd = "boxplot",
                                             dc = "boxplot"),
                           dd = "jitter",
                           cc = "point",
                           cd = "boxplot",
                           dc = "boxplot")

#' @noRd
.pairs_tbl <- function(data,
                       data2 = NULL,
                       except = NULL,
                       type = "full",
                       diag = TRUE,
                       mapping = NULL,
                       expansion = NULL) {
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

  if (!is.null(except)) {
    .rownames <- .colnames <- NULL
    df <- dplyr::filter(df, !(.rownames %in% except), !(.colnames %in% except))
  }

  if (identical(data, data2)) {
    source_data <- data
  } else {
    source_data <- cbind(data, data2[setdiff(names(data2), names(data))])
  }

  if (!is.null(except)) {
    row_names <- names(data)[!names(data) %in% except]
    col_names <- names(data2)[!names(data2) %in% except]
  } else {
    row_names <- names(data)
    col_names <- names(data2)
  }

  df <- structure(df,
                  row_names = row_names,
                  col_names = col_names,
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

  gs <- options("linkET.pairs.plot")$linkET.pairs.plot$scale
  scale <- list(NULL)
  if (!is.null(gs) && !length(gs) < 1) {
    mp <- mapping[intersect(names(mapping), c("fill", "colour", "size", "alpha", "shape"))]
    if (length(mp) > 0) {
      tp <- vapply(vapply(mapping, rlang::as_name, character(1)),
                   function(x) { if (is_binary(source_data[[x]])) "d" else "c"},
                   character(1))
      ls <- gs[paste(names(tp), unname(tp), sep = "_")]
      scale <- gs[paste(names(tp), unname(tp), sep = "_")]
    }
  }
  df$.plot <- lapply(seq_len(nrow(df)), function(ii) {
    mapping2 <- aes_string(x = df$.colnames[ii], y = df$.rownames[ii])
    mapping <- aes_modify(mapping, mapping2)
    p <- ggplot(data = source_data, mapping = mapping) + ggplot2::theme_void()
    Reduce("+", scale, init = p)
  })

  id <- vapply(source_data, function(x) is.factor(x) || is.character(x) || is.numeric(x),
               logical(1))
  source_data <- source_data[id]
  if (!empty(source_data)) {
    if (is.null(expansion)) {
      expansion <- list(continuous = ggplot2::expansion(mult = 0.05),
                        dicrete = ggplot2::expansion(add = 0.6))
    } else {
      if (is.numeric(expansion)) {
        expansion <- list(continuous = rep_len(expansion, 4),
                          dicrete = rep_len(expansion, 4))
      } else {
        expansion <- list(continuous = expansion$continuous,
                          dicrete = expansion$discrete)
      }
    }
    ct <- expansion$continuous %||% ggplot2::expansion(mult = 0.05)
    dc <- expansion$discrete %||% ggplot2::expansion(add = 0.6)
    ct <- rep_len(ct, 4)
    dc <- rep_len(dc, 4)
    axis_info <- purrr::map_dfr(names(source_data), function(vars) {
      xx <- data[[vars]]
      limits <- .get_limits(xx, expansion = if (is.numeric(xx)) ct else dc)
      if (is.numeric(xx)) {
        breaks <- pretty_in_range(xx)
      } else {
        breaks <- if (is.factor(xx)) {
          levels(xx)[levels(xx) %in% as.character(xx)]
        } else {
          unique(xx)[!is.na(unique(xx))]
        }
      }
      tibble(label = vars,
             limits = list(limits),
             breaks = list(breaks))
    })
    attr(df, "axis_info") <- axis_info
  }
  attr(df, "expansion") <- list(continuous = ct, discrete = dc)
  df
}

#' @noRd
.get_limits <- function(x, expansion) {
  if (is.numeric(x)) {
    rng <- range(x, na.rm = TRUE)
    if (diff(rng) == 0) {
      if (all(expansion == 0)) {
        limits <- c(rng[1] - 0.025, rng[2] + 0.025)
      } else if (all(expansion[c(2, 4)] == 0)) {
        limits <- c(rng[1] - expansion[1], rng[2] + expansion[3])
      } else {
        if (all(expansion[c(1, 3)] == 0)) {
          limits <- c(rng[1] - expansion[2], rng[2] + expansion[4])
        } else {
          limits <- c(rng[1] - expansion[1] - expansion[2],
                      rng[2] + expansion[3] + expansion[4])
        }
      }
    } else {
      limits <- c(rng[1] - diff(rng) * expansion[1] - expansion[2],
                  rng[2] + diff(rng) * expansion[3] + expansion[4])
    }
  } else {
    if (is.factor(x)) {
      rng <- levels(x)[levels(x) %in% as.character(x)]
    } else {
      rng <- unique(x)[!is.na(unique(x))]
    }
    if (length(rng) == 1) {
      if (all(expansion == 0)) {
        limits <- c(1 - 0.025, 1 + 0.025)
      } else if (all(expansion[c(2, 4)] == 0)) {
        limits <- c(1 - expansion[1], 1 + expansion[3])
      } else {
        if (all(expansion[c(1, 3)] == 0)) {
          limits <- c(1 - expansion[2], 1 + expansion[4])
        } else {
          limits <- c(1 - expansion[1] - expansion[2],
                      1 + expansion[3] + expansion[4])
        }
      }
    } else {
      limits <- c(1 - length(rng) * expansion[1] - expansion[2],
                  length(rng) + length(rng) * expansion[3] + expansion[4])
    }
  }
  limits
}

#' @noRd
.set_position <- function(md) {
  md$.pos <- "full"
  md$.pos <- ifelse(md$.rownames == md$.colnames, "diag", md$.pos)
  if (identical(row_names(md), col_names(md))) {
    x <- as.integer(factor(md$.rownames, levels = rev(row_names(md))))
    y <- as.integer(factor(md$.colnames, levels = col_names(md)))
    md$.pos <- ifelse(x + y < nrows(md) + 1, "lower", md$.pos)
    md$.pos <- ifelse(x + y > nrows(md) + 1, "upper", md$.pos)
  }
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
#' @param theme a ggplot theme object for child axis.
#' @rdname axis_child
#' @author Hou Yun
#' @export
guide_axis_child <- function(title = waiver(),
                             check.overlap = FALSE,
                             angle = NULL,
                             n.dodge = 1,
                             order = 0,
                             position = waiver(),
                             child = NULL,
                             only_child = FALSE,
                             theme = NULL) {

  structure(
    list(title = title,
         check.overlap = check.overlap,
         angle = angle,
         n.dodge = n.dodge,
         order = order,
         position = position,
         child = child,
         only_child = only_child,
         theme = theme,
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
                                        .value = as.character(labels),
                                        .label = as.character(labels)))
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
  child_theme <- theme + guide$theme
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
      unit.c(grobWidth(main_grobs), grid::unit(2, "mm"), grobWidth(child_grobs))
    } else {
      unit.c(grobWidth(child_grobs), grid::unit(2, "mm"), grobWidth(main_grobs))
    }
    height <- unit(1, "null")
    gt <- gtable(widths = width, heights = height)
    if (axis_position == "left") {
      gt <- gtable_add_grob(gt, grobs = list(main_grobs, child_grobs),
                            t = c(1, 1), l = c(1, 3))
    } else {
      gt <- gtable_add_grob(gt, grobs = list(main_grobs, child_grobs),
                            t = c(1, 1), l = c(3, 1))
    }
  } else {
    height <- if (axis_position == "top") {
      unit.c(grobHeight(main_grobs), grid::unit(2, "mm"), grobHeight(child_grobs))
    } else {
      unit.c(grobHeight(child_grobs), grid::unit(2, "mm"), grobHeight(main_grobs))
    }
    width <- unit(1, "null")
    gt <- gtable(widths = width, heights = height)
    if (axis_position == "top") {
      gt <- gtable_add_grob(gt, grobs = list(main_grobs, child_grobs),
                            t = c(1, 3), l = c(1, 1))
    } else {
      gt <- gtable_add_grob(gt, grobs = list(main_grobs, child_grobs),
                            t = c(3, 1), l = c(1, 1))
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

#' @title Correlation Layer
#' @description This function can be used to add corrlation marker on a pairs plot.
#' @inheritParams ggplot2::layer
#' @param ... other parameters passed to layer function.
#' @param na.rm if FALSE, the default, missing values are removed with a warning,
#' and if TRUE, missing values are silently removed.
#' @param method method of correlation.
#' @param digits,nsmall a positive integer to format correlation.
#' @param nudge_x,nudge_y horizontal and vertical adjustment to nudge labels by.
#' @return a layer object.
#' @author Hou Yun
#' @rdname geom_corr
#' @export
## TODO: add method to support lm model
geom_corr <- function(mapping = NULL,
                      data = NULL,
                      stat = "identity",
                      position = "identity",
                      ...,
                      method = "pearson",
                      digits = 2,
                      nsmall = 2,
                      nudge_x = 0,
                      nudge_y = 0,
                      na.rm = FALSE,
                      show.legend = NA,
                      inherit.aes = TRUE)
{
  layer(
    data = data,
    mapping = mapping,
    stat = stat,
    geom = GeomCorr,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      method = method,
      digits = digits,
      nsmall = nsmall,
      nudge_x = nudge_x,
      nudge_y = nudge_y,
      na.rm = na.rm,
      ...
    )
  )
}

#' @rdname geom_corr
#' @format NULL
#' @usage NULL
#' @export
GeomCorr <- ggproto(
  "GeomCorr", GeomText,
  required_aes = c("x", "y"),

  default_aes = aes(label = "", colour = "black", size = 3.88, alpha = NA,
                    hjust = 0, angle = 0, vjust = 0.5, family = "",
                    fontface = 1, lineheight = 1.2),

  draw_panel = function(data,
                        panel_params,
                        coord,
                        method = method,
                        digits = 2,
                        nsmall = 2,
                        nudge_x = 0,
                        nudge_y = 0,
                        na.rm = FALSE) {
    if (empty(data) || any(!is.numeric(data$x), !is.numeric(data$y))) {
      return(grid::nullGrob())
    }

    one_length <- length(unique(data$colour)) == 1
    if (one_length) {
      ll <- list(Corr = data)
    } else {
      ll <- split(data, data$colour)
      ll <- c(list(Corr = data), ll)
    }
    less_than_three <- vapply(ll, nrow, numeric(1)) < 3
    ll <- ll[!less_than_three]

    if (length(ll) < 1) return(grid::nullGrob())

    nm <- names(ll)
    corr <- purrr::map2_chr(ll, nm, function(.data, .nm) {
      label <- if (.nm == "Corr") "Corr: " else paste0(.data$label[1], ": ")
      if (label == ": ") label <- ""
      r <- as.vector(stats::cor(x = .data$x, y = .data$y, method = method))
      p <- stats::cor.test(x = .data$x, y = .data$y, method = method)$p.value
      mark <- sig_mark(p)
      if (mark != "") {
        mark <- paste0("\\", unlist(strsplit(mark, split = "")),
                       collapse = "")
      }
      text <- paste(format(r, nsmall = nsmall, digits = digits),
                    mark, sep = "")
      col <- if (.nm == "Corr") "black" else .nm
      paste("<span style='color:", col, "'>", paste0(label, text),
            "</span>", sep = "")
    })

    first_row <- data[1, , drop = FALSE]
    first_row$x <- panel_params$x.range[1] + 0.1 * diff(panel_params$x.range) +
                   nudge_x
    first_row$y <- mean(panel_params$y.range) + nudge_y
    first_row <- coord$transform(first_row, panel_params)

    if (one_length) {
      col <- scales::alpha(first_row$colour, first_row$alpha)
    } else {
      col <- scales::alpha("black", first_row$alpha)
    }
    richtext_grob <- get_function("gridtext", "richtext_grob")
    richtext_grob(text = paste(corr, collapse = "<br>"),
                  x = first_row$x,
                  y = first_row$y,
                  hjust = first_row$hjust,
                  vjust = first_row$vjust,
                  rot = first_row$angle,
                  gp = gpar(col = col,
                            fontsize = first_row$size * ggplot2::.pt,
                            fontfamily = first_row$family,
                            fontface = first_row$fontface,
                            lineheight = first_row$lineheight))
  },
  draw_key = ggplot2::draw_key_point
)

