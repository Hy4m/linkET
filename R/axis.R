#' @title Axis helper function
#' @description functions to add secondary and nested axis.
#' @param ... other parameters passing to:
#' \itemize{
#'      \item{\code{secondary_axis*()}: ggh4x::guide_axis_manual().}
#'      \item{\code{guide_axis_filter()}: parameters for filtering.}
#'      }
#' @param position where this guide should be drawn: one of top, bottom, left, or right.
#' @param position_aes one of "x" or "y".
#' @inheritParams ggplot2::guide_axis
#' @return a guide object.
#' @rdname axis
#' @importFrom ggplot2 guides
#' @importFrom ggplot2 waiver
#' @author Hou Yun
#' @export
#' @examples \dontrun{
#' as_md_tbl(mtcars) %>%
#' hyplot() +
#'   geom_point() +
#'   secondary_x_axis()
#' }
secondary_x_axis <- function(...) {
  set_secondary_axis(..., position_aes = "x")
}

#' @rdname axis
#' @export
secondary_y_axis <- function(...) {
  set_secondary_axis(..., position_aes = "y")
}

#' @rdname axis
#' @export
set_secondary_axis <- function(...,
                               position = waiver(),
                               position_aes = "x"
                               ) {
  structure(list(position = position,
                 position_aes = position_aes,
                 ...), class = "secondary_axis")
}

#' @method ggplot_add secondary_axis
#' @export
ggplot_add.secondary_axis <- function(object, plot, object_name) {
  stopifnot(is_hyplot(plot))
  guide_axis_manual <- get_function("ggh4x", "guide_axis_manual")
  md <- plot$data
  type <- attr(md, "type")
  position_aes <- object$position_aes
  object <- object[setdiff(names(object), "position_aes")]
  if(position_aes == "x") {
    object$position <- switch(type,
                              full = "top",
                              upper = "bottom",
                              lower = "top")
    object <- do.call(guide_axis_manual, object)
    object <- guides(x.sec = object)
  } else {
    object$position <- switch(type,
                              full = "right",
                              upper = "left",
                              lower = "right")
    object <- do.call(guide_axis_manual, object)
    object <- guides(y.sec = object)
  }

  ggplot_add(object, plot, object_name)
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
## TODO: clean and remove unused code
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

#' @rdname axis
#' @export
guide_axis_filter <- function(...,
                              title = waiver(),
                              check.overlap = FALSE,
                              angle = NULL,
                              n.dodge = 1,
                              order = 0,
                              position = waiver()) {

  structure(
    list(params = rlang::enquos(...),
         title = title,
         check.overlap = check.overlap,
         angle = angle,
         n.dodge = n.dodge,
         order = order,
         position = position,
         available_aes = c("x", "y"),
         name = "axis"),
    class = c("guide", "guide_filter", "axis")
  )
}

#' @method guide_train guide_filter
#' @importFrom ggplot2 guide_train
#' @export
guide_train.guide_filter <- function(guide, scale, aesthetic = NULL) {
  NextMethod()
}

#' @method guide_transform guide_filter
#' @importFrom ggplot2 guide_transform
#' @export
guide_transform.guide_filter <- function(guide, coord, panel_params) {

  guide$key <- filter2(guide$key, guide$params)
  NextMethod()
}

#' @method guide_gengrob guide_filter
#' @importFrom ggplot2 guide_gengrob
#' @export
guide_gengrob.guide_filter <- function(guide, theme) {
  NextMethod()
}

#' @noRd
filter2 <- function(data, params) {
  if (length(params) < 1) {
    return(data)
  }

  id <- TRUE
  for (ii in params) {
    id <- id & rlang::eval_tidy(ii, data)
  }
  data[id, , drop = FALSE]
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
