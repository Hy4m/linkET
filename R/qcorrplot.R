#' @title Correlation Plot
#' These functions can be used to visualize simply and directly a correlation matrix.
#' @param data one of 'correlate', 'rcorr' or 'corr.test' object.
#' @param mapping default list of aesthetic mappings to use for plot.
#' @param byrow a logical value indicating whether arrange the 'spec' columns on y axis.
#' @param drop logical.
#' @param grid_col colour of panel grid.
#' @param grid_size size of panel grid.
#' @param is_corr if TRUE (default), the data will be regarded as the correlation
#' coefficient matrix.
#' @param fixed if TRUE (default), the coordinates will have a fixed aspect ratio.
#' @param ... passing to \code{\link{as_matrix_data}}.
#' @return a gg object.
#' @importFrom ggplot2 coord_fixed
#' @rdname qcorrplot
#' @author Hou Yun
#' @export
qcorrplot <- function(data, ...) {
  UseMethod("qcorrplot")
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
  data <- as_matrix_data(data, ...)
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
#' @method qcorrplot rcorr
#' @export
qcorrplot.rcorr <- function(data,
                            mapping = NULL,
                            drop = FALSE,
                            grid_col = "grey50",
                            grid_size = 0.25,
                            fixed = TRUE,
                            ...) {
  p <- data$P
  diag(p) <- 0
  data <- structure(.Data = list(r = data$r, p = p), class = "correlate")
  qcorrplot(data = data,
            mapping = mapping,
            drop = drop,
            grid_col = grid_col,
            grid_size = grid_size,
            fixed = fixed,
            ...)
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
  data <- structure(.Data = list(r = data$r, p = data$p), class = "correlate")
  qcorrplot(data = data,
            mapping = mapping,
            drop = drop,
            grid_col = grid_col,
            grid_size = grid_size,
            fixed = fixed,
            ...)
}

#' @rdname qcorrplot
#' @method qcorrplot mantel_tbl
#' @export
qcorrplot.mantel_tbl <- function(data,
                                 mapping = NULL,
                                 byrow = TRUE,
                                 drop = FALSE,
                                 grid_col = "grey50",
                                 grid_size = 0.25,
                                 fixed = TRUE,
                                 ...) {
  env_nm <- unique(data$env)
  spec_nm <- unique(data$spec)
  spec <- env <- NULL
  if(byrow) {
    col_names <- env_nm
    row_names <- spec_nm
    data <- dplyr::rename(data,
                          .rownames = spec,
                          .colnames = env)
  } else {
    col_names <- spec_nm
    row_names <- env_nm
    data <- dplyr::rename(data,
                          .rownames = env,
                          .colnames = spec)
  }

  data <- structure(
    .Data = data,
    row_names = row_names,
    col_names = col_names,
    type = "full",
    diag = TRUE,
    class = c("md_tbl", "tbl_df", "tbl", "data.frame"))
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
#' @method qcorrplot pro_tbl
#' @export
qcorrplot.pro_tbl <- function(data,
                              mapping = NULL,
                              byrow = TRUE,
                              drop = FALSE,
                              grid_col = "grey50",
                              grid_size = 0.25,
                              fixed = TRUE,
                              ...) {
  env_nm <- unique(data$env)
  spec_nm <- unique(data$spec)
  spec <- env <- NULL
  if(byrow) {
    col_names <- env_nm
    row_names <- spec_nm
    data <- dplyr::rename(.rownames = spec,
                          .colnames = env)
  } else {
    col_names <- spec_nm
    row_names <- env_nm
    data <- dplyr::rename(.rownames = env,
                          .colnames = spec)
  }

  data <- structure(
    .Data = data,
    row_names = row_names,
    col_names = col_names,
    type = "full",
    diag = TRUE,
    class = c("md_tbl", "tbl_df", "tbl", "data.frame"))
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
#' @method qcorrplot matrix
#' @export
qcorrplot.matrix <- function(data,
                             mapping = NULL,
                             drop = FALSE,
                             grid_col = "grey50",
                             grid_size = 0.25,
                             is_corr = NULL,
                             fixed = TRUE,
                             ...) {
  is_corr <- is_corr %||% check_corr(data)

  if(isFALSE(is_corr)) {
    data <- cor(data)
  }
  data <- structure(.Data = list(r = data), class = "correlate")
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
                                 is_corr = NULL,
                                 fixed = TRUE,
                                 ...) {
  data <- as.matrix(data)
  is_corr <- is_corr %||% check_corr(data)

  if(isFALSE(is_corr)) {
    data <- cor(data)
  }
  data <- structure(.Data = list(r = data), class = "correlate")
  qcorrplot(data = data,
            mapping = mapping,
            drop = drop,
            grid_col = grid_col,
            grid_size = grid_size,
            fixed = fixed,
            ...)
}

#' @noRd
check_corr <- function(x) {
  if(!is.numeric(x)) {
    stop("Not support non-numeric data.", call. = FALSE)
  }
  rnm <- rownames(x)
  cnm <- colnames(x)
  if(!is.null(cnm) && is.null(rnm)) {
    return(FALSE)
  }

  if(is.null(cnm) && !is.null(rnm)) {
    return(FALSE)
  }

  if(any(x > 1) || any(x < -1)) {
    return(FALSE)
  }

  if(identical(rnm, cnm) && !is.null(rnm)) {
    if(isSymmetric(x)) {
      return(TRUE)
    } else {
      return(FALSE)
    }
  }
  TRUE
}
