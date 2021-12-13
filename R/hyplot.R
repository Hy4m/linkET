#' Initialize a new hyplot
#' @title Initialize hyplot
#' @param md a matrix_data or md_tbl object or any can be converted to matrix_data.
#' @param mapping default list of aesthetic mappings to use for plot.
#' @param drop logical. If TRUE, the unused labels will be removed.
#' @param use_md logical. if TRUE, will use \code{ggtext::element_markdown()} to
#' draw the axis labels.
#' @param ... passing to \code{\link{as_matrix_data}}.
#' @return a ggplot object.
#' @importFrom ggplot2 ggplot
#' @importFrom ggplot2 aes_
#' @importFrom ggplot2 scale_x_discrete
#' @importFrom ggplot2 scale_y_discrete
#' @importFrom utils modifyList
#' @rdname hyplot
#' @examples
#' library(ggplot2)
#' hyplot(mtcars) +
#'   geom_tile(aes(fill = mtcars))
#' @author Hou Yun
#' @export
hyplot <- function(md,
                   mapping = NULL,
                   drop = TRUE,
                   use_md = NULL,
                   ...) {
  if (!is_matrix_data(md) && !is_md_tbl(md)) {
    if (!"name" %in% names(list(...))) {
      nm <- deparse(substitute(md))
      md <- as_matrix_data(md, name = nm, ...)
    } else {
      md <- as_matrix_data(md, ...)
    }
  }

  if (is_matrix_data(md)) {
    md <- fortify(md, ...)
  }

  type <- attr(md, "type")
  diag <- attr(md, "diag")
  row_names <- rev(row_names(md))
  col_names <- col_names(md)

  if(is.null(use_md)) {
    use_md <- requireNamespace("ggtext") && is_richtext(c(row_names, col_names))
  }

  if(type == "full" || isTRUE(diag)) {
    drop <- FALSE
  }

  if(isTRUE(drop)) {
    if(type == "upper" && isFALSE(diag)) {
      xbreaks <- xlabels <- col_names[-1]
      ybreaks <- ylabels <- row_names[-1]
    }
    if(type == "lower" && isFALSE(diag)) {
      xbreaks <- xlabels <- col_names[-ncols(md)]
      ybreaks <- ylabels <- row_names[-nrows(md)]
    }
  } else {
    xbreaks <- xlabels <- col_names
    ybreaks <- ylabels <- row_names
  }

  xaxis_pos <- switch(type, upper = "top", "bottom")
  yaxis_pos <- switch(type, upper = "right", "left")
  guide_pos <- switch (type, lower = "left", "right")

  base_mapping <- aes_(x = ~.colnames, y = ~.rownames)
  if (is.null(mapping)) {
    mapping <- base_mapping
  } else {
    mapping <- modifyList(base_mapping, mapping)
  }

  p <- ggplot(data = md,
              mapping = mapping)
  p <- p + scale_x_discrete(limits = col_names,
                            breaks = xbreaks,
                            labels = xlabels,
                            drop = FALSE,
                            position = xaxis_pos) +
           scale_y_discrete(limits = row_names,
                            breaks = ybreaks,
                            labels = ylabels,
                            drop = FALSE,
                            position = yaxis_pos)


  # adjust the default theme
  p <- p + theme_hy(legend.position = guide_pos, use_md = use_md)
  class(p) <- c("hyplot", class(p))
  p
}

#' @noRd
is_hyplot <- function(plot) {
  inherits(plot, "hyplot")
}

#' @noRd
is_upper_plot <- function(plot) {
  stopifnot(is_hyplot(plot))
  attr(plot$data, "type") == "upper"
}

#' @noRd
is_lower_plot <- function(plot) {
  stopifnot(is_hyplot(plot))
  attr(plot$data, "type") == "lower"
}
