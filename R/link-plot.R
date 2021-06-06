#' @export
link_plot <- function(data,
                      mapping,
                      ...,
                      widths = NULL,
                      heights = NULL,
                      guides = "collect",
                      tag_level = "keep"
)
{
  xy <- get_xy_pos(...)
  if(!inherits(mapping, "uneval")) {
    stop("Mapping should be created with `aes()` or `aes_()`.", call. = FALSE)
  }
  if(all(c("from", "to") %in% mapping)) {
    stop("Mapping needs to contain 'from' and 'to'.", call. = FALSE)
  }

  pos_data <- extract_data(data, mapping[c("from", "to")])
  pos <- tibble::tibble(.x = xy$x[pos_data$from],
                        .y = xy$y[pos_data$from],
                        .xend = xy$x[pos_data$to],
                        .yend = xy$y[pos_data$to])
  data <- dplyr::bind_cols(pos, data)
  mapping <- modifyList(mapping[setdiff(names(mapping), c("from", "to"))],
                        ggplot2::aes_(x = ~.x, y = ~.y, xend = ~.xend, yend = ~.yend))

  nm <- names(list(...))
  if(!all(c("r", "l") %in% nm)) {
    if("r" %in% nm) {
      x_expand <- ggplot2::expansion(add = c(0.6, 0))
    } else if("l" %in% nm) {
      x_expand <- ggplot2::expansion(add = c(0, 0.6))
    } else {
      x_expand <- ggplot2::expansion(add = c(0.6, 0.6))
    }
  } else {
    x_expand <- ggplot2::expansion(add = c(0, 0))
  }

  if(!all(c("t", "b") %in% nm)) {
    if("t" %in% nm) {
      y_expand <- ggplot2::expansion(add = c(0.6, 0))
    } else if("b" %in% nm) {
      y_expand <- ggplot2::expansion(add = c(0, 0.6))
    } else {
      y_expand <- ggplot2::expansion(add = c(0.6, 0.6))
    }
  } else {
    y_expand <- ggplot2::expansion(add = c(0, 0))
  }
  p <- ggplot2::ggplot(data, mapping) +
    ggplot2::geom_segment() +
    ggplot2::scale_x_continuous(expand = x_expand) +
    ggplot2::scale_y_continuous(expand = y_expand) +
    ggplot2::theme_void()

  link_plot_build(plot = p,
                  ...,
                  widths = widths,
                  heights = heights,
                  guides = guides,
                  tag_level = tag_level)
}
