#' Add panel grid line on hyplot
#' @description \code{geom_panel_grid} is mainly used with \code{hyplot()}.
#'     function to add a panel grid line on plot region.
#' @param colour,color colour of grid lines.
#' @param size size of grid lines.
#' @importFrom ggplot2 geom_segment
#' @rdname geom_panel_grid
#' @export
geom_panel_grid <- function(colour = "grey50",
                            size = 0.25,
                            color = NULL) {
  if(!is.null(color))
    colour <- color
  structure(.Data = list(colour = colour, size = size),
            class = "geom_panel_grid")
}


#' @importFrom ggplot2 ggplot_add geom_segment
#' @export
ggplot_add.geom_panel_grid <- function(object, plot, object_name) {
  obj <- geom_segment(aes(x = x, y = y, xend = xend, yend = yend),
                      data = get_grid_data(plot$data),
                      colour = object$colour, linewidth = object$size,
                      inherit.aes = FALSE)
  ggplot_add(object = obj, plot = plot, object_name = object_name)
}

#' @noRd
get_grid_data <- function(md) {
  if(!is_md_tbl(md))
    stop("Need a md_tbl.", call. = FALSE)
  n <- length(col_names(md))
  m <- length(row_names(md))
  type <- attr(md, "type")
  diag <- attr(md, "diag")

  if(type == "full") {
    xx <- c(0:n + 0.5, rep_len(0.5, m + 1))
    yy <- c(rep_len(0.5, n + 1), 0:m + 0.5)
    xxend <- c(0:n + 0.5, rep_len(n + 0.5, m + 1))
    yyend <- c(rep_len(m + 0.5, n + 1), 0:m + 0.5)
  } else if(type == "upper") {
    if(diag) {
      xx <- c(0:n + 0.5, c(n:1 - 0.5, 0.5))
      yy <- c(c(m:1 - 0.5, 0.5), 0:m + 0.5)
      xxend <- c(0:n + 0.5, rep_len(n + 0.5, m + 1))
      yyend <- c(rep_len(m + 0.5, n + 1), 0:m + 0.5)
    } else {
      xx <- c(1:n + 0.5, c(n:2 - 0.5, 1.5))
      yy <- c(c(m:2 - 0.5, 1.5), 1:m + 0.5)
      xxend <- c(1:n + 0.5, rep_len(n + 0.5, m))
      yyend <- c(rep_len(m + 0.5, n), 1:m + 0.5)
    }
  } else {
    if(diag) {
      xx <- c(0:n + 0.5, rep_len(0.5, m + 1))
      yy <- c(rep_len(0.5, n + 1), 0:m + 0.5)
      xxend <- c(0:n + 0.5, c(n + 0.5, n:1 + 0.5))
      yyend <- c(c(m + 0.5, m:1 + 0.5), 0:m + 0.5)
    } else {
      xx <- c(1:n - 0.5, rep_len(0.5, m))
      yy <- c(rep_len(0.5, n), 1:m - 0.5)
      xxend <- c(1:n - 0.5, c(n - 0.5, n:2 - 0.5))
      yyend <- c(c(m - 0.5, m:2 - 0.5), 1:m - 0.5)
    }
  }
  new_data_frame(list(x = xx, y = yy, xend = xxend, yend = yyend))
}
