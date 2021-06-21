#' Coerce matrix_data to data frames
#' @title Coerce matrix_data to data frames
#' @param md a matrix_data object.
#' @return a tibble object.
#' @importFrom tibble as_tibble
#' @importFrom dplyr bind_cols
#' @rdname as_tibble
#' @author Hou Yun
#' @export
as_tibble.matrix_data <- function(md)
{
  type <- attr(md, "type")
  diag <- attr(md, "diag")
  value <- new_data_frame(lapply(md, as.vector))
  id <- new_data_frame(list(.rownames = rep(row_names(md), ncols(md)),
                            .colnames = rep(col_names(md), each = nrows(md))))
  md_tbl <- structure(.Data = bind_cols(id, value),
                      row_names = row_names(md),
                      col_names = col_names(md),
                      type = type,
                      diag = diag,
                      class = c("md_tbl", "tbl_df", "tbl", "data.frame"))
  if(type == "upper") {
    extract_upper(md_tbl, diag = diag)
  } else if(type == "lower") {
    extract_lower(md_tbl, diag = diag)
  } else {
    if(isFALSE(diag)) {
      trim_diag(md_tbl)
    } else {
      md_tbl
    }
  }
}

