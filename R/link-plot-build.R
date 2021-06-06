#' @export
link_plot_build <- function(plot,
                            ...,
                            widths = NULL,
                            heights = NULL,
                            guides = "collect",
                            tag_level = "keep") {
  plots <- list(...)
  nm <- sort(names(plots))
  b <- plots$b
  l <- plots$l
  t <- plots$t
  r <- plots$r
  plot <- list(plot)
  width <- height <- 1

  if(!is.null(b)) {
    plot <- c(plot, list(b))
    height <- c(height, na_or_value(heights["b"]))
  }
  if(!is.null(t)) {
    plot <- c(list(t), plot)
    height <- c(na_or_value(heights["t"]), height)
  }
  if(!is.null(l)) {
    if(!is.null(b) && !is.null(t)) {
      plot <- c(list(patchwork::plot_spacer()), list(l),
                list(patchwork::plot_spacer()), plot)
    } else if(!is.null(b)) {
      plot <- c(list(l), list(patchwork::plot_spacer()), plot)
    } else if(!is.null(t)) {
      plot <- c(list(patchwork::plot_spacer()), list(l), plot)
    } else {
      plot <- c(list(l), plot)
    }
    width <- c(na_or_value(widths["l"]), width)
  }

  if(!is.null(r)) {
    if(!is.null(b) && !is.null(t)) {
      plot <- c(plot, list(patchwork::plot_spacer()), list(r),
                list(patchwork::plot_spacer()))
    } else if(!is.null(b)) {
      plot <- c(plot, list(r), list(patchwork::plot_spacer()))
    } else if(!is.null(t)) {
      plot <- c(plot, list(patchwork::plot_spacer()), list(r))
    } else {
      plot <- c(plot, list(r))
    }
    width <- c(width, na_or_value(widths["l"]))
  }

  p <- Reduce("+", plot) +
    patchwork::plot_layout(ncol = length(width),
                           nrow = length(height),
                           byrow = FALSE,
                           widths = width,
                           heights = height,
                           guides = guides,
                           tag_level = tag_level)
  p
}
