#' @title Correlation Plot
#' These functions can be used to visualize simply and directly a correlation matrix.
#' @param data one of 'correlate', 'rcorr' or 'corr.test' object.
#' @param mapping default list of aesthetic mappings to use for plot.
#' @param drop logical.
#' @param grid_col colour of panel grid.
#' @param grid_size size of panel grid.
#' @param fixed if TRUE (default), the coordinates will have a fixed aspect ratio.
#' @param ... other parameters.
#' @return a gg object.
#' @importFrom ggplot2 coord_fixed
#' @rdname qcorrplot
#' @author Hou Yun
#' @export
qcorrplot <- function(data, ...) {
  UseMethod("qcorrplot")
}

#' @rdname qcorrplot
#' @method qcorrplot cor_md_tbl
#' @export
qcorrplot.cor_md_tbl <- function(data,
                                 mapping = NULL,
                                 drop = FALSE,
                                 grid_col = "grey50",
                                 grid_size = 0.25,
                                 fixed = TRUE,
                                 ...) {
  if("p" %in% names(data)) {
    base_mapping <- aes_(fill = ~r, r = ~r, r0 = ~r, pvalue = ~p)
  } else {
    base_mapping <- aes_(fill = ~r, r = ~r, r0 = ~r)
  }

  p <- hyplot(md = data,
              mapping = aes_modify(base_mapping, mapping),
              drop = drop)

  ## add panel grid
  p <- p + geom_panel_grid(colour = grid_col, size = grid_size)

  ## coord fixed?
  if(isTRUE(fixed)) {
    p <- p + coord_fixed()
  }

  ## remove the panel background
  p <- p + theme(panel.background = element_blank(),
                 axis.ticks = element_blank())

  p
}

#' @rdname qcorrplot
#' @method qcorrplot correlate
#' @export
qcorrplot.correlate <- function(data,
                                mapping = NULL,
                                drop = FALSE,
                                grid_col = "grey50",
                                grid_size = 0.25,
                                fixed = TRUE,
                                ...) {
  data <- as_md_tbl(data, ...)
  qcorrplot(data = data,
            mapping = mapping,
            drop = drop,
            grid_col = grid_col,
            grid_size = grid_size,
            fixed = fixed)
}

#' @rdname qcorrplot
#' @method qcorrplot rcorr
#' @export
qcorrplot.rcorr <- function(data,
                            mapping = NULL,
                            drop = FALSE,
                            grid_col = "grey50",
                            grid_size = 0.25,
                            fixed = TRUE,
                            ...) {
  data <- as_md_tbl(data, ...)
  qcorrplot(data = data,
            mapping = mapping,
            drop = drop,
            grid_col = grid_col,
            grid_size = grid_size,
            fixed = fixed)
}

#' @rdname qcorrplot
#' @method qcorrplot corr.test
#' @export
qcorrplot.corr.test <- function(data,
                                mapping = NULL,
                                drop = FALSE,
                                grid_col = "grey50",
                                grid_size = 0.25,
                                fixed = TRUE,
                                ...) {
  data <- as_md_tbl(data, ...)
  qcorrplot(data = data,
            mapping = mapping,
            drop = drop,
            grid_col = grid_col,
            grid_size = grid_size,
            fixed = fixed)
}



#' @rdname qcorrplot
#' @method qcorrplot mantel_tbl
#' @export
qcorrplot.mantel_tbl <- function(data,
                                 mapping = NULL,
                                 drop = FALSE,
                                 grid_col = "grey50",
                                 grid_size = 0.25,
                                 fixed = TRUE,
                                 ...) {
  data <- as_md_tbl(data, ...)
  qcorrplot(data = data,
            mapping = mapping,
            drop = drop,
            grid_col = grid_col,
            grid_size = grid_size,
            fixed = fixed)
}

#' @rdname qcorrplot
#' @method qcorrplot pro_tbl
#' @export
qcorrplot.pro_tbl <- function(data,
                              mapping = NULL,
                              drop = FALSE,
                              grid_col = "grey50",
                              grid_size = 0.25,
                              fixed = TRUE,
                              ...) {
  data <- as_md_tbl(data, ...)
  qcorrplot(data = data,
            mapping = mapping,
            drop = drop,
            grid_col = grid_col,
            grid_size = grid_size,
            fixed = fixed)
}

#' @rdname qcorrplot
#' @method qcorrplot easycorrelation
#' @export
qcorrplot.easycorrelation <- function(data,
                                      mapping = NULL,
                                      drop = FALSE,
                                      grid_col = "grey50",
                                      grid_size = 0.25,
                                      fixed = TRUE,
                                      ...) {
  data <- as_md_tbl(data, ...)
  qcorrplot(data = data,
            mapping = mapping,
            drop = drop,
            grid_col = grid_col,
            grid_size = grid_size,
            fixed = fixed)
}

#' @rdname qcorrplot
#' @method qcorrplot matrix
#' @export
qcorrplot.matrix <- function(data,
                             mapping = NULL,
                             drop = FALSE,
                             grid_col = "grey50",
                             grid_size = 0.25,
                             fixed = TRUE,
                             ...) {
  data <- as_correlate(data)
  qcorrplot(data = data,
            mapping = mapping,
            drop = drop,
            grid_col = grid_col,
            grid_size = grid_size,
            fixed = fixed,
            ...)
}

#' @rdname qcorrplot
#' @method qcorrplot data.frame
#' @export
qcorrplot.data.frame <- function(data,
                                 mapping = NULL,
                                 drop = FALSE,
                                 grid_col = "grey50",
                                 grid_size = 0.25,
                                 fixed = TRUE,
                                 ...) {
  data <- as_correlate(data)
  qcorrplot(data = data,
            mapping = mapping,
            drop = drop,
            grid_col = grid_col,
            grid_size = grid_size,
            fixed = fixed,
            ...)
}


