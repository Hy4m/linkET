#' Reorder matrix_data by hierarchical cluster
#' @title Hierarchical Clustering
#' @param md a matrix_data object.
#' @param index a integer or character index, specifying which matrix to cluster by.
#' @param cluster one of "all", "row", "col" or "none".
#' @param cluster_method the clustering agglomeration method to be used.
#' @param row_dist,col_dist dist function, see details.
#' @param ... extra parameters for dist.
#' @return a modified matrix_data object.
#' @rdname make_cluster
#' @author Hou Yun
#' @export
make_cluster <- function(md,
                         index = 1,
                         cluster = "none",
                         cluster_method = "complete",
                         row_dist = NULL,
                         col_dist = NULL,
                         ...)
{
  stopifnot(is_matrix_data(md))
  cluster <- match.arg(cluster, c("all", "row", "col", "none"))
  if (cluster == "none") {
    return(md)
  }

  x <- md[[index]]
  attrs <- attributes(md)

  row_dist <- substitute(row_dist)
  col_dist <- substitute(col_dist)
  row_dist <- if (is.null(row_dist)) substitute(x)
  col_dist <- if (is.null(col_dist)) substitute(x)

  if (cluster == "all") {
    row_hc <- hclust(dist(eval(row_dist), ...), method = cluster_method)
    col_hc <- hclust(dist(t(eval(col_dist)), ...), method = cluster_method)

    md <- lapply(md, function(.md) {
      .md[get_order(row_hc), get_order(col_hc)]
    })
  } else if (cluster == "row") {
    row_hc <- hclust(dist(eval(row_dist), ...), method = cluster_method)
    md <- lapply(md, function(.md) {
      .md[get_order(row_hc), , drop = FALSE]
    })
  } else {
    col_hc <- hclust(dist(t(eval(col_dist)), ...), method = cluster_method)
    md <- lapply(md, function(.md) {
      .md[get_order(row_hc), , drop = FALSE]
    })
  }

  md <- do.call("set_attrs", c(x = md, attrs))
  structure(.Data = md, class = "matrix_data")
}


