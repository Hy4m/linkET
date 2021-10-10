#' @title Draw Doughnut
#' @description These functions create and draw doughnut.
#' @param x,y a numeric value or unit object.
#' @param r0,r1 a non-negtive numeric value.
#' @param value a numeric vector.
#' @param label NULL or character vector with the same length as 'value'.
#' @param units unit of the piechart size.
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
#' @importFrom grid textGrob
#' @importFrom grid nullGrob
#' @importFrom grid grobTree
#' @importFrom grid gpar
#' @importFrom grid grid.draw
#' @importFrom grid is.unit
#' @rdname DoughnutGrob
#' @author Hou Yun
#' @export
DoughnutGrob <- function(x = 0.5,
                         y = 0.5,
                         r0 = 5,
                         r1 = 8,
                         value = runif(5),
                         label = NULL,
                         units = "mm",
                         default.units = "npc",
                         name = NULL,
                         gp = gpar(),
                         vp = NULL) {
  if(!is.null(label)) {
    if(length(label) != length(value)) {
      stop("'label' should have same length as 'value'.", call. = FALSE)
    }
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
                 width = unit(2 * r1, units),
                 height = unit(2 * r1, units),
                 clip = "off")
  ratio <- value / sum(value, na.rm = TRUE)
  ratio[is.na(ratio)] <- 0
  n <- length(ratio)
  s <- c(0, cumsum(ratio)[-n]) * 2 * pi
  e <- cumsum(ratio) * 2 * pi
  r0 <- r0 / r1 / 2
  r1 <- 0.5
  pxy <- purrr::pmap_dfr(list(s, e, seq_len(n)),
                         function(.s, .e, .n) {
                           ll <- max(2, ceiling(degree(.e - .s)))
                           tt <- seq(.s, .e, length.out = ll)
                           tibble(x = c(r0 * cos(tt), r1 * cos(rev(tt))) + 0.5,
                                  y = c(r0 * sin(tt), r1 * sin(rev(tt))) + 0.5,
                                  group = .n
                           )
                         })
  poly <- polygonGrob(pxy$x, pxy$y, pxy$group, gp = gp)

  if(!is.null(label)) {
    tt <- (s + e) / 2
    rr <- (r0 + r1) / 2
    lab <- textGrob(label, rr * cos(tt) + 0.5, rr * sin(tt) + 0.5, gp = gp)
  } else {
    lab <- nullGrob()
  }

  grobTree(poly, lab, name = name, vp = vp, cl = "doughnutGrob")
}

#' @rdname DoughnutGrob
#' @export
grid.doughnut <- function(...) {
  grid.draw(DoughnutGrob(...))
}

degree <- function(x) x / pi * 180
