#' @title Set Default Color Pallete
#' @description Set default color pallete for \code{\link{qcorrplot}}.
#' @param colours vector of colours to use for n-colour gradient.
#' @param breaks a numeric vector of positions.
#' @param labels a character vector giving labels (must be same length as breaks).
#' @param limits a numeric vector of length two providing limits of the scale.
#' @param scale NULL or a \code{Scale} object.
#' @param ... other parameters passing to \code{scale_fill_gradientn}.
#' @rdname set_style
#' @author Hou Yun
#' @export
#' @examples
#' ## use corrplot style
#' set_corrplot_style()
#' qcorrplot(mtcars) + geom_shaping(marker = "circle")
#' ## use custom colour palette
#' set_corrplot_style(colours = c("red", "white", "blue"))
#' qcorrplot(mtcars) + geom_shaping(marker = "circle")
#' ## use custom scale
#' set_corrplot_style(scale = ggplot2::scale_fill_viridis_c(limits = c(-1, 1)))
#' qcorrplot(mtcars) + geom_shaping(marker = "circle")
#' ## reset to default scale
#' set_default_style()
#' qcorrplot(mtcars) + geom_shaping(marker = "circle")

set_corrplot_style <- function(colours = NULL,
                               breaks = seq(-1, 1, length.out = 5),
                               labels = breaks,
                               limits = c(-1, 1),
                               scale = NULL,
                               ...) {
  style <- scale %||% corrplot_style(colours = colours,
                                     breaks = breaks,
                                     labels = labels,
                                     limits = limits,
                                     ...)
  options(linkET.corrplot.style = style)
}

#' @rdname set_style
#' @export
set_default_style <- function() {
  options(linkET.corrplot.style = NULL)
}

#' @noRd
corrplot_style <- function(colours = NULL,
                           breaks = seq(-1, 1, length.out = 5),
                           labels = breaks,
                           limits = c(-1, 1),
                           ...) {
  ggplot2::scale_fill_gradientn(colours = colours %||% red_blue,
                                breaks = breaks,
                                labels = labels,
                                limits = limits,
                                ...)
}

#' @noRd
red_blue <- c("#67001F", "#B2182B", "#D6604D", "#F4A582", "#FDDBC7", "#F7F7F7",
              "#D1E5F0", "#92C5DE", "#4393C3", "#2166AC", "#053061")
