## ## Notes: all functions about network will be moved to netET packages
## #' @title Circular Layout
## #' @description arranges nodes in circles according to group information.
## #' @param g graph.
## #' @param group if NULL (default), will use group attributes of `g`.
## #' @param zoom numeric, range in [0, 1].
## #' @param circular not used.
## #' @return a two-columns matrix.
## #' @importFrom purrr pmap_dfr
## #' @importFrom igraph V
## #' @importFrom igraph vertex_attr
## #' @rdname layout
## #' @author Hou Yun
## #' @export
## layout_with_circular <- function(g,
##                                  group = NULL,
##                                  zoom = 0.618) {
##   v <- V(g)
##   node <- names(v)
##   ll <- length(node)
##
##   if(is.null(group)) {
##     if(is.null(v$group)) {
##       group <- list(Group = rep_len(1L, ll))
##     } else {
##       group <- v$group
##     }
##   }
##
##   as.matrix(gen_circle(node, group, zoom)[1:2])
## }
##
## #' @rdname layout
## #' @export
## layout_tbl_graph_circular <- function(g,
##                                       group = NULL,
##                                       zoom = 0.618,
##                                       circular) {
##   xy <- layout_with_circular(g, group, zoom)
##   nodes <- data.frame(x = xy[, 1], y = xy[, 2])
##   nodes$circular <- FALSE
##   extra <- as.data.frame(vertex_attr(g))
##   nodes <- cbind(nodes, extra[, !names(extra) %in% names(nodes),
##                               drop = FALSE])
##   nodes
## }
##
## #' @noRd
## gen_circle <- function(nodes, group, zoom = 0.618) {
##   if(!is.list(group)) {
##     group <- split(nodes, group)
##   }
##   group <- make_list_names(group, "Group")
##
##   ll <- length(group)
##   nm <- names(group)
##   n <- purrr::map_dbl(group, length)
##
##   if(ll == 1L) {
##     t <- seq(0, 2 * pi, length.out = n + 1L)[-(n + 1L)]
##     xy <- tibble::tibble(x = cos(t),
##                          y = sin(t),
##                          node = nodes,
##                          group = nm)
##   } else {
##     t1 <- seq(0, 2 * pi, length.out = ll + 1L)[-(ll + 1L)]
##     cx <- cos(t1)
##     cy <- sin(t1)
##     r <- if(ll == 2L) {
##       zoom * sqrt(n) / max(sqrt(n))
##     } else {
##       sin(2 * pi / ll) * zoom * sqrt(n) / max(sqrt(n))
##     }
##     xy <- purrr::pmap_dfr(list(group, n, nm, cx, cy, r),
##                     function(.group, .n, .nm, .cx, .cy, .r) {
##                       t <- seq(0, 2 * pi, length.out = .n + 1L)[-(.n + 1L)]
##                       tibble::tibble(x = .r * cos(t) + .cx,
##                                      y = .r * sin(t) + .cy,
##                                      node = .group,
##                                      group = .nm)
##                     })
##   }
##   xy <- xy[get_order(nodes, xy$node), ]
##   xy
## }
