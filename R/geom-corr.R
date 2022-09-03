#' @title Correlation Layer
#' @description This function can be used to add corrlation marker on a pairs plot.
#' @inheritParams ggplot2::layer
#' @param ... other parameters passed to layer function.
#' @param na.rm if FALSE, the default, missing values are removed with a warning,
#' and if TRUE, missing values are silently removed.
#' @param method method of correlation.
#' @param position position of correlation marker.
#' @param contain_all logical, if FALSE will contain each group marker only.
#' @param digits,nsmall a positive integer to format correlation.
#' @param nudge_x,nudge_y horizontal and vertical adjustment to nudge labels by.
#' @return a layer object.
#' @author Hou Yun
#' @rdname geom_corr
#' @export
## TODO: can customize group order
geom_corr <- function(mapping = NULL,
                      data = NULL,
                      stat = "identity",
                      position = "lc",
                      ...,
                      method = "pearson",
                      contain_all = TRUE,
                      digits = 2,
                      nsmall = 2,
                      nudge_x = NULL,
                      nudge_y = NULL,
                      na.rm = FALSE,
                      show.legend = NA,
                      inherit.aes = TRUE)
{
  layer(
    data = data,
    mapping = mapping,
    stat = stat,
    geom = GeomCorr,
    position = "identity",
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      method = method,
      position = position,
      contain_all = contain_all,
      digits = digits,
      nsmall = nsmall,
      nudge_x = nudge_x,
      nudge_y = nudge_y,
      na.rm = na.rm,
      ...
    )
  )
}

#' @rdname geom_corr
#' @format NULL
#' @usage NULL
#' @export
GeomCorr <- ggproto(
  "GeomCorr", GeomText,
  required_aes = c("x", "y"),

  default_aes = aes(label = "", colour = "black", size = 3.88, alpha = NA,
                    hjust = 0, angle = 0, vjust = 0.5, family = "",
                    fontface = 1, lineheight = 1.2),

  draw_panel = function(data,
                        panel_params,
                        coord,
                        method = method,
                        contain_all = TRUE,
                        position = "lc",
                        digits = 2,
                        nsmall = 2,
                        nudge_x = NULL,
                        nudge_y = NULL,
                        na.rm = FALSE) {
    if (empty(data) || any(!is.numeric(data$x), !is.numeric(data$y))) {
      return(grid::nullGrob())
    }

    one_length <- length(unique(data$label)) == 1
    if (one_length) {
      ll <- list(Corr = data)
    } else {
      ll <- split(data, data$label)
      if (isTRUE(contain_all)) {
        ll <- c(list(Corr = data), ll)
      }
    }
    less_than_three <- vapply(ll, nrow, numeric(1)) < 3
    ll <- ll[!less_than_three]

    if (length(ll) < 1) return(grid::nullGrob())

    nm <- names(ll)
    corr <- purrr::map2_chr(ll, nm, function(.data, .nm) {
      label <- paste0(.nm, ": ")
      if (label == ": ") label <- ""
      r <- as.vector(stats::cor(x = .data$x, y = .data$y, method = method))
      p <- stats::cor.test(x = .data$x, y = .data$y, method = method)$p.value
      mark <- sig_mark(p)
      if (mark != "") {
        mark <- paste0("\\", unlist(strsplit(mark, split = "")),
                       collapse = "")
      }
      text <- paste(format(r, nsmall = nsmall, digits = digits),
                    mark, sep = "")
      col <- if (.nm == "Corr") "black" else .data$colour[1]
      paste("<span style='color:", col, "'>", paste0(label, text),
            "</span>", sep = "")
    })

    if (is.null(position)) {
      position <- "lc"
    }
    if (is.list(position)) {
      x_pos <- "l"
      y_pos <- "c"
      x <- position$x
      y <- position$y
      just_x <- 0
      just_y <- 0.5
    } else {
      position <- match.arg(position,
                            as.vector(outer(c("l", "r", "c"),
                                            c("t", "b", "c"),
                                            paste0)))
      x_pos <- unlist(strsplit(position, ""))[1]
      y_pos <- unlist(strsplit(position, ""))[2]
      x <- just_x <- switch(x_pos, "l" = 0, "r" = 1, "c" = 0.5)
      y <- just_y <- switch(y_pos, "t" = 1, "b" = 0, "c" = 0.5)
    }

    if (!grid::is.unit(x)) {
      x <- grid::unit(x, "npc")
    }
    if (!grid::is.unit(y)) {
      y <- grid::unit(y, "npc")
    }

    nudge_x <- abs(nudge_x %||% 2)
    nudge_y <- abs(nudge_y %||% 2)
    x <- switch(x_pos,
                "l" = x + grid::unit(nudge_x, "mm"),
                "r" = x - grid::unit(nudge_x, "mm"),
                "c" = x)
    y <- switch(y_pos,
                "t" = y - grid::unit(nudge_y, "mm"),
                "b" = y + grid::unit(nudge_y, "mm"),
                "c" = y)

    first_row <- data[1, , drop = FALSE]

    if (one_length) {
      col <- scales::alpha(first_row$colour, first_row$alpha)
    } else {
      col <- scales::alpha("black", first_row$alpha)
    }
    richtext_grob <- get_function("gridtext", "richtext_grob")
    richtext_grob(text = paste(corr, collapse = "<br>"),
                  x = x,
                  y = y,
                  hjust = just_x,
                  vjust = just_y,
                  rot = first_row$angle,
                  gp = gpar(col = col,
                            fontsize = first_row$size * ggplot2::.pt,
                            fontfamily = first_row$family,
                            fontface = first_row$fontface,
                            lineheight = first_row$lineheight))
  },
  draw_key = ggplot2::draw_key_point
)

