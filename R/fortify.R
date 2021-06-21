#' Fortify a matrix_data to data.frame
#' @title Fortify a matrix_data
#' @param model a matrix_data object.
#' @param data ignore.
#' @param ... ignore.
#' @return a tibble object.
#' @importFrom ggplot2 fortify
#' @method fortify matrix_data
#' @rdname fortify
#' @author Hou Yun
#' @export
fortify.matrix_data <- function(model, data, ...)
{
  as_tibble(model, ...)
}

#' @method fortify md_tbl
#' @rdname fortify
#' @export
fortify.md_tbl <- function(model, data, ...)
{
  model
}
