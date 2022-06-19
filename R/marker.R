#' @title Custom marker for heatmap plot
#' @description This set of functions makes it easy to define shapes, and it
#' similar to points.
#' @param x any R object can be converted to marker.
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
#' @param ... ignore.
#' @return a marker object.
#' @author Hou Yun
#' @rdname marker
#' @importFrom grid is.grob gpar get.gpar polygonGrob
#' @export
#' @examples
#' marker("square")
#' marker(c("square", "circle"))
#' m <- marker(c("square", "circle", "star", "heart"))
#' m[1:3]
#' rep_len(m, 10)
#' library(grid)
#' marker(circleGrob())
marker <- function(x, ...) {
  UseMethod("marker")
}

#' @method marker grob
#' @rdname marker
marker.grob <- function(x,
                        width = 1,
                        height = 1,
                        width_unit = "cm",
                        height_unit = width_unit,
                        ...) {
  grob <- list(x)
  n <- max(length(grob), length(width), length(height), length(width_unit),
           length(height_unit))
  grob <- rename_grobs(rep_len(grob, n))
  width <- rep_len(width, n)
  height <- rep_len(height, n)
  width_unit <- rep_len(width_unit, n)
  height_unit <- rep_len(height_unit, n)
  name <- rep_len("custom", n)
  structure(list(grob = grob,
                 name = name,
                 width = width,
                 height = height,
                 width_unit = width_unit,
                 height_unit = height_unit),
            class = "marker")
}

#' @method marker gList
#' @rdname marker
marker.gList <- function(x,
                         width = 1,
                         height = 1,
                         width_unit = "cm",
                         height_unit = width_unit,
                         ...) {
  grob <- list(grid::gTree(x))
  marker(x = x,
         width = width,
         height = height,
         width_unit = width_unit,
         height_unit = height_unit)
}

#' @method marker ggplot
#' @rdname marker
marker.ggplot <- function(x,
                          width = 1,
                          height = 1,
                          width_unit = "cm",
                          height_unit = width_unit,
                          ...) {
  grob <- list(ggplotGrob(x))
  marker(x = x,
         width = width,
         height = height,
         width_unit = width_unit,
         height_unit = height_unit)
}

#' @method marker raster
#' @rdname marker
marker.raster <- function(x,
                          width = 1,
                          height = 1,
                          width_unit = "cm",
                          height_unit = width_unit,
                          ...) {
  grob <- list(grid::rasterGrob(x))
  marker(x = x,
         width = width,
         height = height,
         width_unit = width_unit,
         height_unit = height_unit)
}

#' @method marker magick-image
#' @rdname marker
`marker.magick-image` <- function(x,
                                  width = 1,
                                  height = 1,
                                  width_unit = "cm",
                                  height_unit = width_unit,
                                  ...) {
  grob <- list(grid::rasterGrob(grDevices::as.raster(x)))
  marker(x = x,
         width = width,
         height = height,
         width_unit = width_unit,
         height_unit = height_unit)
}

#' @method marker formula
#' @rdname marker
marker.formula <- function(x,
                           width = 1,
                           height = 1,
                           width_unit = "cm",
                           height_unit = width_unit,
                           ...) {
  echoGrob <- get_function("gridGraphics", "echoGrob")
  cur <- dev.cur()
  eval(parse(text = as.character(x)[2]), envir = parent.env())
  x <- echoGrob()
  on.exit({
    try(dev.off(), silent = TRUE)
    dev.set(cur)
  })
  marker(x = x,
         width = width,
         height = height,
         width_unit = width_unit,
         height_unit = height_unit)
}

#' @method marker character
#' @rdname marker
marker.character <- function(x,
                             width = 1,
                             height = 1,
                             width_unit = "cm",
                             height_unit = width_unit,
                             r = 0,
                             n = 5,
                             ratio = 0.618,
                             ...) {
  nn <- max(length(x), length(width), length(height), length(width_unit),
           length(height_unit))
  x <- rep_len(x, nn)
  width <- rep_len(width, nn)
  height <- rep_len(height, nn)
  width_unit <- rep_len(width_unit, nn)
  height_unit <- rep_len(height_unit, nn)
  r <- rep_len(r, nn)
  n <- rep_len(n, nn)
  ratio <- rep_len(ratio, nn)
  grob <- vector("list", nn)
  nm <- character(nn)
  for (ii in seq_len(nn)) {
    if (is.na(x[ii])) {
      nm[ii] <- "null"
      grob[[ii]] <- grid::nullGrob()
    } else if (!x[ii] %in% all_type) {
      nm[ii] <- "null"
      grob[[ii]] <- grid::textGrob(x[ii])
    } else {
      grob[[ii]] <- switch (x[ii],
                            square = polygonGrob(x = c(0, 1, 1, 0, 0),
                                                 y = c(0, 0, 1, 1, 0)),
                            circle = grid::circleGrob(),
                            star = starGrob(n = n[ii], ratio = ratio[ii]),
                            heart = heartGrob(nstep = 100),
                            ellipse = ellipseGrob(r = r[ii], nstep = 100),
                            cross = crossGrob(),
                            triangle = triangleGrob(1),
                            triangle2 = triangleGrob(2),
                            shade = shadeGrob())
    }
  }
  structure(list(grob = grob,
                 name = nm,
                 width = width,
                 height = height,
                 width_unit = width_unit,
                 height_unit = height_unit),
            class = "marker")
}

#' @method marker list
#' @rdname marker
marker.list <- function(x,
                        width = 1,
                        height = 1,
                        width_unit = "cm",
                        height_unit = width_unit,
                        r = 0,
                        n = 5,
                        ratio = 0.618,
                        ...) {
  all_num <- all(vapply(x, is.numeric, logical(1)))
  if (all_num) {
    if (!length(x) %in% c(2, 3)) {
      stop("Marker needs 2 or 3 numeric elements in list.", call. = FALSE)
    }

    if (length(x) == 2) {
      grob <- grid::polygonGrob(x = x$x %||% x[[1]],
                                y = x$y %||% x[[2]])
    } else {
      grob <- grid::polygonGrob(x = x$x %||% x[[1]],
                                y = x$y %||% x[[2]],
                                id = x$id %||% x[[3]])
    }
    out <- marker(x = grob,
                  width = width,
                  height = height,
                  width_unit = width_unit,
                  height_unit = height_unit)
  } else {
    nn <- max(length(x), length(width), length(height), length(width_unit),
              length(height_unit))
    x <- rep_len(x, nn)
    width <- rep_len(width, nn)
    height <- rep_len(height, nn)
    width_unit <- rep_len(width_unit, nn)
    height_unit <- rep_len(height_unit, nn)
    r <- rep_len(r, nn)
    n <- rep_len(n, nn)
    ratio <- rep_len(ratio, nn)

    out <- vector("list", nn)
    for (ii in seq_len(nn)) {
      out[[ii]] <- tryCatch(marker(x = x[[ii]],
                                  width = width[ii],
                                  height = height[ii],
                                  width_unit = width_unit[ii],
                                  height_unit = height_unit[ii],
                                  r = r[ii],
                                  n = n[ii],
                                  ratio = ratio[ii],
                                  ...), error = function(e) {
                               stop("Invalid marker.", call. = FALSE)
                             })
    }
    out <- do.call("c", out)
  }
  out
}

#' @method marker marker
#' @rdname marker
marker.marker <- function(x, ...) {
  x
}

#' @export
c.marker <- function(...) {
  ll <- list(...)
  grob <- list()
  name <- NULL
  width <- NULL
  height <- NULL
  width_unit <- NULL
  height_unit <- NULL
  for (ii in seq_along(ll)) {
    grob <- c(grob, ll[[ii]]$grob)
    name <- c(name, ll[[ii]]$name)
    width <- c(width, ll[[ii]]$width)
    height <- c(height, ll[[ii]]$height)
    width_unit <- c(width_unit, ll[[ii]]$width_unit)
    height_unit <- c(height_unit, ll[[ii]]$height_unit)
  }
  structure(list(grob = rename_grobs(grob),
                 name = name,
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
  x <- paste0(x$name, "(", x$width, x$width_unit, ", ",
              x$height, x$height_unit, ")")
  print(x, ...)
}

#' @export
`[.marker` <- function(x, index) {
  any_duplicate <- anyDuplicated(index)
  overflow <- index > length(x)
  grob <- x$grob[index]
  name <- x$name[index]
  width <- x$width[index]
  height <- x$height[index]
  width_unit <- x$width_unit[index]
  height_unit <- x$height_unit[index]

  if (any(overflow)) {
    grob <- lapply(grob, function(.grob) if (is.null(.grob)) grid::nullGrob() else .grob)
    name <- ifelse(overflow, "null", name)
    width <- ifelse(overflow, 0, width)
    height <- ifelse(overflow, 0, height)
    width_unit <- ifelse(overflow, "cm", width_unit)
    height_unit <- ifelse(overflow, "cm", height_unit)
  }

  structure(list(grob = rename_grobs(grob),
                 name = name,
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

  id <- rep_len(seq_len(n), length.out)
  x[id]
}

#' @noRd
markerGrob <- function(marker,
                       x = 0.5,
                       y = 0.5,
                       angle = 0,
                       hjust = 0.5,
                       vjust = 0.5,
                       rasterize = FALSE,
                       res = 100,
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
              res = res,
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
  xx <- x$x
  yy <- x$y
  angle <- x$angle
  hjust <- x$hjust
  vjust <- x$vjust
  width <- unit(marker$width, marker$width_unit)
  height <- unit(marker$height, marker$height_unit)
  width <- grid::convertWidth(width, "in")
  height <- grid::convertHeight(height, "in")
  grobs <- marker$grob
  gp <- split_gpar(x$gp, n)

  if (isTRUE(x$rasterize)) {
    agg_capture <- get_function("ragg", "agg_capture")
    # res <- dev.size("px")[1] / dev.size("in")[1]
    # if (x$res > res) {
    #   res <- x$res
    # }

    for (ii in seq_len(n)) {
      cur <- dev.cur()
      cap <- agg_capture(width = width[ii],
                         height = height[ii],
                         units = "in",
                         background = NA,
                         res = x$res,
                         scaling = 1 )
      grobs[[ii]]$gp <- gp[[ii]]
      grid.draw(grobs[[ii]])
      grobs[[ii]] <- rasterGrob(cap(native = TRUE))
      try(dev.off(), silent = TRUE)
      dev.set(cur)
      vp <- viewport(x = xx[ii],
                     y = yy[ii],
                     width = width[ii],
                     height = height[ii],
                     angle = angle[ii],
                     just = c(hjust[ii], vjust[ii]),
                     default.units = x$default.units)
      grobs[[ii]]$vp <- vp
    }
  } else {
    for (ii in seq_len(n)) {
      vp <- viewport(x = xx[ii],
                     y = yy[ii],
                     width = width[ii],
                     height = height[ii],
                     angle = angle[ii],
                     just = c(hjust[ii], vjust[ii]),
                     default.units = x$default.units)
      grobs[[ii]]$gp <- gp[[ii]]
      grobs[[ii]]$vp <- vp
    }
  }

  grid::setChildren(x, do.call(grid::gList, grobs))
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
all_type <- c("square", "circle", "star", "heart", "ellipse", "cross",
              "triangle", "triangle2", "shade")

#' @noRd
rename_grobs <- function(x, force = TRUE) {
  if (isFALSE(force)) {
    hash <- vapply(x, digest::digest, character(1))
    dup <- duplicated(hash)
    if (any(dup)) {
      n <- sum(dup)
      index <- paste(ceiling(abs(stats::rnorm(n)) * 1000),
                     ceiling(abs(stats::rnorm(n)) * 1000),
                     ceiling(abs(stats::rnorm(n)) * 1000), sep = ".")
      nm <- paste("MARKER", "GROB", index, sep = ".")
      id <- which(dup)
      for (ii in seq_along(id)) {
        x[[id[ii]]]$name <- nm[ii]
      }
    }
  } else {
    n <- length(x)
    index <- paste(ceiling(abs(stats::rnorm(n)) * 1000),
                   ceiling(abs(stats::rnorm(n)) * 1000),
                   ceiling(abs(stats::rnorm(n)) * 1000), sep = ".")
    nm <- paste("MARKER", "GROB", index, sep = ".")
    for (ii in seq_len(n)) {
      x[[ii]]$name <- nm[ii]
    }
  }
  x
}
