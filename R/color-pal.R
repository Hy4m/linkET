#' @title Colour Palettes
#' @description Colour palettes for mantel test plot.
#' @param n integer, should be less than 6.
#' @param alpha alpha level in [0,1].
#' @return a palette.
#' @rdname color_pal
#' @author Hou Yun
#' @export
#' @examples
#' color_pal(4)
color_pal <- function (n, alpha = 0.6)
{
  stopifnot(n <= 6)
  colors <- c("#D95F02", "#1B9E77", "#7570B3", "#E7298A", "#A6761D",
              scales::alpha("#CCCCCC", alpha = alpha))
  if (n == 1)
    return(colors[1])
  col <- c(colors[1:(n - 1)], colors[6])
  col
}

#' @rdname color_pal
#' @export
colour_pal <- color_pal
