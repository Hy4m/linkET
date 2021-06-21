#' Extract order
#' @title Extract order
#' @param x any \code{R} object.
#' @param name old name.
#' @param ... ignore.
#' @return a numeric vector.
#' @importFrom stats hclust dist
#' @rdname get_order
#' @examples
#' hc <- hclust(dist(mtcars))
#' get_order(hc)
#' @author Hou Yun
#' @export
get_order <- function(x, ...) {
  UseMethod("get_order")
}

#' @rdname get_order
#' @export
#' @method get_order numeric
get_order.numeric <- function(x, ...) {
  x
}

#' @rdname get_order
#' @export
#' @method get_order character
get_order.character <- function(x, name = NULL, ...) {
  if(is.null(name)) {
    order <- seq_along(x)
  } else {
    order <- order(rlang::set_names(seq_along(x), x)[name])
  }
  unname(order)[seq_along(x)]
}

#' @rdname get_order
#' @export
#' @method get_order hclust
get_order.hclust <- function(x, ...) {
  x$order
}

#' @rdname get_order
#' @export
#' @method get_order dendrogram
get_order.dendrogram <- function(x, ...) {
  get_order(stats::as.hclust(x))
}

#' @rdname get_order
#' @export
#' @method get_order dist
get_order.dist <- function(x, ...) {
  hc <- hclust(x)
  hc$order
}
