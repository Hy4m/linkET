#' @title Custom marker for heatmap plot
#' @description This set of functions makes it easy to define shapes, and it
#' similar to points.
#' @param grob grob object, list or character.
#' \itemize{
#'      \item{\strong{grob}: a \code{grob}, \code{gList}, \code{gTree} object.}
#'      \item{\strong{list}: a list of x and y.}
#'      \item{\strong{character}: one of "square", "circle", "star", "heart",
#'      "ellipse", "cross", "triangle", "triangle2".}
#' }
#' @param width,height width or height of marker.
#' @param width_unit,height_unit units of width or height.
#' @param r only be used for ellipse marker.
#' @param n,ratio only be used for star marker.
#' @return a marker object.
#' @author Hou Yun
#' @rdname marker
#' @importFrom grid is.grob gpar get.gpar polygonGrob
#' @export
#' @examples
#' marker()
#' marker(c("square", "circle"))
#' m <- marker(c("square", "circle", "star", "heart"))
#' m[1:3]
#' rep_len(m, 10)
marker <- function(grob = "square",
                   width = 1,
                   height = 1,
                   width_unit = "cm",
                   height_unit = width_unit,
                   r = 0,
                   n = 5,
                   ratio = 0.618) {
  if (is.list(grob)) {
    grob <- tryCatch(suppressWarnings(list(customGrob(grob))),
                     error = function(e) grob)
  }
  if (is.grob(grob)) {
    grob <- list(grob)
    label <- "custom"
  }
  if (is.atomic(grob)) {
    if (!all(grob %in% all_type)) {
      stop("Invalid marker.", call. = FALSE)
    }
    label <- grob
    grob <- as.list(grob)
  }

  if (!is.list(grob)) {
    stop("Invalid marker.", call. = FALSE)
  }

  ll <- max(length(grob), length(width), length(height))
  grob_is_na <- vapply(grob, function(.grob) {
    if (is.atomic(.grob) && is.na(.grob)) TRUE else FALSE
  }, logical(1))
  grob <- rep_len(grob, ll)
  label <- rep_len(grob, ll)
  width <- rep_len(width, ll)
  height <- rep_len(height, ll)
  width_unit <- rep_len(width_unit, ll)
  height_unit <- rep_len(height_unit, ll)
  r <- rep_len(r, ll)
  n <- rep_len(n, ll)
  ratio <- rep_len(ratio, ll)

  width[grob_is_na] <- 0
  height[grob_is_na] <- 0

  is_custom <- vapply(grob, function(.grob) {
    is.grob(.grob) || is.list(.grob)
  }, logical(1))
  label <- ifelse(is_custom, "custom", grob)

  grob <- lapply(seq_len(ll), function(.id) {
    if (is.grob(grob[[.id]])) {
      grob[[.id]]
    } else if (is.list(grob[[.id]])) {
      customGrob(grob[[.id]])
    } else {
      marker2grob(grob[[.id]], r = r[.id], n = n[.id], ratio = ratio[.id])
    }
  })

  structure(list(grob = grob,
                 label = label,
                 width = width,
                 height = height,
                 width_unit = width_unit,
                 height_unit = height_unit),
            class = "marker")
}

#' @method print marker
#' @export
print.marker <- function(x, ...) {
  n <- length(x)
  label <- ifelse(is.na(x$label), "empty", x$label)
  x <- paste0(label, "(", x$width, x$width_unit, ", ",
              x$height, x$height_unit, ")")
  print(x, ...)
}

#' @export
`[.marker` <- function(x, index) {
  any_duplicate <- anyDuplicated(index)
  overflow <- index > length(x)
  grob <- x$grob[index]
  label <- x$label[index]
  width <- x$width[index]
  height <- x$height[index]
  width_unit <- x$width_unit[index]
  height_unit <- x$height_unit[index]

  if (any(overflow)) {
    grob <- lapply(grob, function(.grob) if (is.null(.grob)) grid::nullGrob() else .grob)
    width <- ifelse(overflow, 0, width)
    height <- ifelse(overflow, 0, height)
    width_unit <- ifelse(overflow, "cm", width_unit)
    height_unit <- ifelse(overflow, "cm", height_unit)
  }

  if (any_duplicate) {
    grob <- rename_grob(grob)
  }

  structure(list(grob = grob,
                 label = label,
                 width = width,
                 height = height,
                 width_unit = width_unit,
                 height_unit = height_unit),
            class = "marker")
}

#' @method length marker
#' @export
length.marker <- function(x, ...) {
  length(x$grob)
}

#' @method rep_len marker
#' @export
rep_len.marker <- function(x, length.out) {
  n <- length(x)
  if (length.out == n) {
    return(x)
  }
  if (length.out <= n) {
    return(x[seq_len(length.out)])
  }

  grob <- rep_len(x$grob, length.out)
  grob <- rename_grob(grob)
  structure(list(grob = grob,
                 label = rep_len(x$label, length.out),
                 width = rep_len(x$width, length.out),
                 height = rep_len(x$height, length.out),
                 width_unit = rep_len(x$width_unit, length.out),
                 height_unit = rep_len(x$height_unit, length.out)),
            class = "marker")
}

#' @noRd
markerGrob <- function(marker,
                       x = 0.5,
                       y = 0.5,
                       angle = 0,
                       hjust = 0.5,
                       vjust = 0.5,
                       rasterize = FALSE,
                       default.units = "npc",
                       gp = gpar(),
                       name = NULL,
                       vp = NULL) {
  if (!inherits(marker, "marker")) {
    stop("Invalid marker.", call. = FALSE)
  }
  n <- max(length(marker), length(x), length(y), length(angle), length(hjust),
           length(vjust))

  marker <- rep_len(marker, n)

  x <- rep_len(x, n)
  y <- rep_len(y, n)
  angle <- rep_len(angle, n)
  hjust <- rep_len(hjust, n)
  vjust <- rep_len(vjust, n)

  grid::gTree(marker = marker,
              x = x,
              y = y,
              angle = angle,
              hjust = hjust,
              vjust = vjust,
              rasterize = rasterize,
              default.units = default.units,
              gp = gp,
              name = name,
              vp = vp,
              cl = "markerGrob")
}

#' @title Dynamic resize the marker
#' @description This function takes care of updating the marker size based on
#' device size.
#' @param x a marker grob object.
#' @importFrom grid makeContent unit viewport rasterGrob
#' @importFrom purrr pmap
#' @importFrom grDevices dev.size dev.cur dev.off dev.set
#' @author Hou Yun
#' @rdname makeContent
#' @export
makeContent.markerGrob <- function(x) {
  marker <- x$marker
  n <- length(marker)
  width <- unit(marker$width, marker$width_unit)
  height <- unit(marker$height, marker$height_unit)
  width <- grid::convertWidth(width, "in")
  height <- grid::convertHeight(height, "in")
  grobs <- marker$grob
  gp <- split_gpar(x$gp, n)

  if (isTRUE(x$rasterize)) {
    agg_capture <- get_function("ragg", "agg_capture")
    dim_inch <- dev.size("in")
    dim_pt <- dev.size("px")
    res <- dim_pt[1] / dim_inch[1]

    grobs <- pmap(list(grobs, gp, x$x, x$y, width, height, x$angle, x$hjust, x$vjust),
                  function(.grob, .gp, .x, .y, .width, .height, .angle, .hjust, .vjust) {
                    cur <- dev.cur()
                    cap <- agg_capture(width = .width,
                                       height = .height,
                                       units = "in",
                                       background = NA,
                                       res = res,
                                       scaling = 1 )
                    .grob$gp <- .gp
                    grid.draw(.grob)
                    .grob <- rasterGrob(cap(native = TRUE))
                    try(dev.off(), silent = TRUE)
                    dev.set(cur)
                    vp <- viewport(x = .x,
                                   y = .y,
                                   width = .width,
                                   height = .height,
                                   angle = .angle,
                                   just = c(.hjust, .vjust),
                                   default.units = x$default.units)
                    .grob$vp <- vp
                    .grob
                  })
  } else {
    grobs <- pmap(list(grobs, gp, x$x, x$y, width, height, x$angle, x$hjust, x$vjust),
                  function(.grob, .gp, .x, .y, .width, .height, .angle, .hjust, .vjust) {
                    vp <- grid::viewport(x = .x,
                                         y = .y,
                                         width = .width,
                                         height = .height,
                                         angle = .angle,
                                         just = c(.hjust, .vjust),
                                         default.units = x$default.units)
                    .grob$gp <- .gp
                    .grob$vp <- vp
                    .grob
                  })
  }

  grid::setChildren(x, do.call(grid::gList, grobs))
}

#' @noRd
marker2grob <- function(x, r = 0, n = 100, ratio = 0.618) {
  if (is.na(x)) {
    grid::nullGrob()
  } else {
    switch (x,
            square = polygonGrob(x = c(0, 1, 1, 0, 0),
                                 y = c(0, 0, 1, 1, 0)),
            circle = grid::circleGrob(),
            star = starGrob(n = n,
                            ratio = ratio),
            heart = heartGrob(nstep = 100),
            ellipse = ellipseGrob(r = r, nstep = 100),
            cross = crossGrob(),
            triangle = triangleGrob(1),
            triangle2 = triangleGrob(2),
            shade = shadeGrob()
    )
  }
}


#' @noRd
split_gpar <- function(gp = gpar(), n = 1) {
  gp2 <- grid::get.gpar(c("col", "fill", "alpha", "lty", "lwd"))
  col <- rep_len(gp$col %||% gp2$col, n)
  fill <- gp$fill %||% gp2$fill
  if (inherits(fill, "GridPattern")) {
    fill <- rep_len(list(fill), n)
  } else {
    fill <- rep_len(fill, n)
  }
  alpha <- rep_len(gp$alpha %||% gp2$alpha, n)
  lty <- rep_len(gp$lty %||% gp2$lty, n)
  lwd <- rep_len(gp$lwd %||% gp2$lwd, n)
  lapply(seq_len(n), function(.n) {
    gpar(col = col[.n],
         fill = if (is.list(fill)) fill[[.n]] else fill[.n],
         alpha = alpha[.n],
         lty = lty[.n],
         lwd = lwd[.n])
  })
}

#' @noRd
modify_gpar <- function(gp1, gp2) {
  gp1 <- gp1 %||% as.list(gp1)
  gp2 <- gp2 %||% as.list(gp2)
  gp <- utils::modifyList(gp1, gp2)
  class(gp) <- "gpar"
  gp
}

#' @noRd
ellipseGrob <- function(r = 0, nstep = 100) {
  tt <- seq(0, 2 * pi, length = nstep)
  x = 0.5 * cos(tt + acos(r) / 2) + 0.5
  y = 0.5 * cos(tt - acos(r) / 2) + 0.5
  polygonGrob(x = x, y = y)
}

#' @noRd
crossGrob <- function() {
  r <- 0.3
  x <- c(0.5 - r, 0.5 + r, 0.5 - r, 0.5 + r)
  y <- c(0.5 - r, 0.5 + r, 0.5 + r, 0.5 - r)
  id <- rep(c(1, 2), each = 2)
  grid::pathGrob(x = x, y = y, id = id)
}

#' @noRd
shadeGrob <- function() {
  x <- c(0, 0, 0.5, 0.5, 1, 1)
  y <- c(0.5, 0, 0, 1, 1, 0.5)
  grid::pathGrob(x = x, y = y, id = rep(1:3, 2))
}

#' @noRd
heartGrob <- function(nstep = 300) {
  tt <- seq(0, 2 * pi, length.out = nstep)
  x <- 16 * (sin(tt))^3
  y <- 13 * cos(tt) - 5 * cos(2 * tt) - 2 * cos(3 * tt) - cos(4 * tt)
  rng <- max(diff(range(x)), diff(range(y)))
  x <- x / rng + 0.5
  y <- y / rng - range(y)[1] / rng

  polygonGrob(x = x, y = y)
}

#' @noRd
starGrob <- function(n = 5, ratio = 0.618) {
  p <- 0:n / n
  if (n %% 2 == 0) p <- p + p[2] / 2
  pos <- p * 2 * pi
  x_tmp <- 0.5 * sin(pos)
  y_tmp <- 0.5 * cos(pos)
  angle <- pi / n
  x <- numeric(2 * n + 2)
  y <- numeric(2 * n + 2)
  x[seq(2, 2 * n + 2, by = 2)] <- 0.5 + x_tmp
  y[seq(2, 2 * n + 2, by = 2)] <- 0.5 + y_tmp
  x[seq(1, 2 * n + 2, by = 2)] <- 0.5 + ratio * (x_tmp * cos(angle) - y_tmp * sin(angle))
  y[seq(1, 2 * n + 2, by = 2)] <- 0.5 + ratio * (x_tmp * sin(angle) + y_tmp * cos(angle))
  polygonGrob(x = x, y = y)
}

#' @noRd
triangleGrob <- function(type = 1) {
  if (type == 1) {
    gap <- (1 - sqrt(3) / 2) / 2
    polygonGrob(x = c(0, 1, 0.5, 0),
                y = c(gap, gap, 1 - gap, gap))
  } else {
    polygonGrob(x = c(0, 1, 1, 0),
                y = c(0, 0, 1, 0))
  }
}

#' @noRd
customGrob <- function(x) {
  if (!is.grob(x)) {
    x <- polygonGrob(x = x$x %||% x[[1]],
                     y = x$y %||% x[[2]])
  }
  x
}

#' @noRd
rename_grob <- function (grobs, prefix = "MARKER", suffix = "GRID")
{
  n <- length(grobs)
  index <- paste(ceiling(abs(stats::rnorm(n)) * 1000),
                 ceiling(abs(stats::rnorm(n)) * 1000),
                 ceiling(abs(stats::rnorm(n)) * 1000), sep = ".")
  ll <- paste(prefix, suffix, index, sep = ".")

  for (i in seq_len(n)) {
    grobs[[i]]$name <- ll[i]
  }
  grobs
}

#' @noRd
all_type <- c("square", "circle", "star", "heart", "ellipse", "cross",
              "triangle", "triangle2", "shade")

#' @noRd
as_marker <- function(x, ...) {
  UseMethod("as_marker")
}

#' @method as_marker marker
as_marker.marker <- function(x, ...) {
 x
}

#' @method as_marker grob
as_marker.grob <- function(x, ...) {
  marker(grob = x, ...)
}

#' @method as_marker ggplot
as_marker.ggplot <- function(x, ...) {
  grob <- ggplot2::ggplotGrob(x)
  marker(grob = grob, ...)
}

#' @method as_marker character
as_marker.character <- function(x, ...) {
  id <- x %in% all_type
  x <- ifelse(is.na(id), NA_character_, x)
  marker(grob = x, ...)
}

#' @method as_marker list
as_marker.list <- function(x, ...) {
  x <- polygonGrob(x = x$x %||% x[[1]],
                   y = x$y %||% x[[2]])
  marker(grob = x, ...)
}

#' @method as_marker GridPattern
as_marker.GridPattern <- function(x, shape = "rect", ...) {
  if (! r_version() >= "4.1.0") {
    stop("The R version needs to be higher than 4.1.0.", call. = FALSE)
  }
  .fun <- paste(shape, "Grob", sep = "")
  grob <- do.call(.fun, list())
  grob$gp <- gpar(fill = x)
  marker(grob = grob, ...)
}

#' @method as_marker raster
as_marker.raster <- function(x, ...) {
  grob <- grid::rasterGrob(x)
  marker(grob = grob, ...)
}

#' @method as_marker formula
as_marker.formula <- function(x, envir = parent.frame(), ...) {
  agg_capture <- get_function("ragg", "agg_capture")
  dim_inch <- dev.size("in")
  cur <- dev.cur()
  cap <- agg_capture(width = dim_inch[1],
                     height = dim_inch[2],
                     units = "in",
                     background = NA,
                     res = 100,
                     scaling = 1 )
  .fun <- parse(text = as.character(x)[2])
  eval(.fun, envir = envir)
  on.exit({
    dev.off()
    dev.set(cur)}, add = TRUE)
  grob <- grid::rasterGrob(cap(native = TRUE))
  marker(grob = grob, ...)
}

#' @method as_marker magick-image
`as_marker.magick-image` <- function(x, ...) {
  x <- grDevices::as.raster(x)
  as_marker(x = x, ...)
}
