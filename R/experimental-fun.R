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

  gglist <- lapply(gglist, ggplotGrob)
  params <- c(list(mapping = mapping,
                   marker = marker(grob = gglist),
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
  } else {
    plot <- .rebuild_guides(plot)
  }
  plot
}

#' @export
print.gggplot <- function(x, ...) {
  x <- ggplot_build(x)
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
