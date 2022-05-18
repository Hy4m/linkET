## TODO: can add labels, and can calculate the position of labels.

#' @title Draw doughnut plot
#' @description These functions create and draw doughnut.
#' @param x,y a numeric value or unit object.
#' @param r0,r1 a non-negtive numeric value.
#' @param value a numeric vector.
#' @param percent logical. If FALSE (the default) the value will be treated as
#' source value.
#' @param default.units a string indicating the default units to use if x or y
#' are only given as numeric vectors.
#' @param name a character identifier.
#' @param gp an object of class 'gpar'.
#' @param vp a viewport object or NULL.
#' @param ... other parameters for \code{DoughnutGrob()}.
#' @return a doughnut grob.
#' @importFrom grid unit
#' @importFrom grid viewport
#' @importFrom grid polygonGrob
#' @importFrom grid gpar
#' @importFrom grid grid.draw
#' @importFrom grid is.unit
#' @rdname DoughnutGrob
#' @author Hou Yun
#' @export
DoughnutGrob <- function(x = 0.5,
                         y = 0.5,
                         r0 = 0,
                         r1 = 5,
                         value = runif(5),
                         percent = FALSE,
                         default.units = "npc",
                         name = NULL,
                         gp = gpar(),
                         vp = NULL) {
  if(all(value >= 0) && all(value < 0)) {
    stop("All 'value' should be non-negative or non-positive.", call. = FALSE)
  }

  if (!is.unit(x))
    x <- unit(x, default.units)
  if (!is.unit(y))
    y <- unit(y, default.units)

  if(r0 > r1) {
    temp <- r0
    r0 <- r1
    r1 <- temp
  }

  vp <- viewport(x = x,
                 y = y,
                 width = unit(2 * r1, "mm"),
                 height = unit(2 * r1, "mm"),
                 clip = "off")
  if(isTRUE(percent)) {
    ratio <- value
  } else {
    ratio <- value / sum(value, na.rm = TRUE)
  }
  ratio[is.na(ratio)] <- 0
  n <- length(ratio)
  s <- c(0, cumsum(ratio)[-n]) * 2 * pi
  e <- cumsum(ratio) * 2 * pi
  r0 <- r0 / r1 / 2
  r1 <- 0.5
  pxy <- purrr::pmap_dfr(list(s, e, seq_len(n)),
                         function(.s, .e, .n) {
                           ll <- max(2, ceiling(degree(abs(.e - .s))))
                           tt <- seq(.s, .e, length.out = ll)
                           tibble(x = c(r0 * cos(tt), r1 * cos(rev(tt))) + 0.5,
                                  y = c(r0 * sin(tt), r1 * sin(rev(tt))) + 0.5,
                                  group = .n
                           )
                         })
  polygonGrob(pxy$x, pxy$y, pxy$group, gp = gp, vp = vp)
}

#' @rdname DoughnutGrob
#' @export
grid.doughnut <- function(...) {
  grid.draw(DoughnutGrob(...))
}

#' @noRd
degree <- function(x) x / pi * 180
