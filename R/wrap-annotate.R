#' @title Add Margin Annotation
#' @description This function is a wrapper of \code{aplot}, and can be used to
#' insert annotation.
#' @param plot a ggplot object.
#' @param ... annotation elements.
#' @param width,height width/height of each annotation plot.
#' @param k an integer scalar with the desired number of groups.
#' @param border,colour,color fill and border colour of the hclust bar.
#' @author Hou Yun
#' @rdname wrap_annotate
#' @references Yu G (2022). aplot: Decorate a 'ggplot' with Associated Information. R package
#' version 0.1.6, https://CRAN.R-project.org/package=aplot.
#' @export
wrap_annotate <- function(plot,
                          ...,
                          width = NULL,
                          height = NULL,
                          k = 2,
                          colour = NULL,
                          border = NA,
                          color = NULL) {
  anno <- list(...)
  anno <- anno[names(anno)[names(anno) %in% c("r", "l", "t", "b")]]

  if (empty(anno)) {
    return(plot)
  }

  if (is.null(width)) {
    width <- 0.2
  }
  if (is.null(height)) {
    height <- 0.2
  }
  if (is.numeric(width)) {
    width <- rep_len(width, length(anno$l) + length(anno$r))
    width <- list(l = width[seq_along(anno$l)],
                  r = width[seq_along(anno$r) + length(anno$l)])
  } else {
    width$l <- width$l %||% 0.2
    width$r <- width$r %||% 0.2
    width$l <- rep_len(width$l, length(anno$l))
    width$r <- rep_len(width$r, length(anno$r))
  }
  if (is.numeric(height)) {
    height <- rep_len(height, length(anno$t) + length(anno$b))
    height <- list(t = height[seq_along(anno$t)],
                   b = height[seq_along(anno$b) + length(anno$t)])
  } else {
    height$t <- height$t %||% 0.2
    height$b <- height$b %||% 0.2
    height$t <- rep_len(height$t, length(anno$t))
    height$b <- rep_len(height$b, length(anno$b))
  }

  anno <- lapply(anno, function(x) {
    if (inherits(x, "ggplot") || identical(x, "tree") || identical(x, "hc_bar")) {
      list(x)
    } else {
      x
    }
  })

  nm <- names(anno)
  for (ii in nm) {
    ele_name <- names(anno[[ii]])
    tree_id <- if (ii %in% c("r", "l")) {"row_tree"} else "col_tree"
    hc <- attr(plot$data, tree_id)
    for (jj in seq_along(anno[[ii]])) {
      if (identical(anno[[ii]][[jj]],  "tree")) {
        anno[[ii]][[jj]] <- gen_tree(hc, side = ii)
      }
      if (identical(anno[[ii]][[jj]],  "hc_bar")) {
        anno[[ii]][[jj]] <- gen_hc_bar(hc,
                                       k = k,
                                       side = ii,
                                       colour = colour,
                                       border = border,
                                       color = color)
      }
      if (!is.null(ele_name)) {
        if (ele_name[jj] != "") {
          anno[[ii]][[jj]] <- anno[[ii]][[jj]] + ggplot2::labs(tag = ele_name[jj])
        }
      }

      if (ii %in% c("r", "l")) {
        anno[[ii]][[jj]] <- suppressMessages(
          anno[[ii]][[jj]] + aplot::ylim2(plot)  + theme_no_axis("y")
          )
      } else {
        anno[[ii]][[jj]] <- suppressMessages(
          anno[[ii]][[jj]] + aplot::xlim2(plot) + theme_no_axis("x")
        )
      }
    }
  }

  for (ii in nm) {
    n <- length(anno[[ii]])
    for (jj in seq_along(anno[[ii]])) {
      if (ii == "r") {
        plot <- plot %>% aplot::insert_right(anno[[ii]][[jj]],
                                             width = width[[ii]][jj])
      }
      if (ii == "l") {
        plot <- plot %>% aplot::insert_left(anno[[ii]][[n - jj + 1]],
                                            width = width[[ii]][n - jj + 1])
      }
      if (ii == "t") {
        plot <- plot %>% aplot::insert_top(anno[[ii]][[jj]],
                                           height = height[[ii]][jj])
      }
      if (ii == "b") {
        plot <- plot %>% aplot::insert_bottom(anno[[ii]][[n - jj + 1]],
                                              height = height[[ii]][n - jj + 1])
      }
    }
  }

  plot
}

#' @noRd
gen_tree <- function(hc, side = "t") {
  if (is.null(hc)) {
    stop("Tree annotate cannot be added to unclustered data.", call. = FALSE)
  }
  if (inherits(hc, "ggtree")) {
    return(hc)
  }

  data <- ggdendro::dendro_data(hc)$segments
  max_x <- max(data$x, data$xend) + 1
  max_y <- max(data$y, data$yend)
  if (side == "b") {
    data$y <- max_y - data$y
    data$yend <- max_y - data$yend
  }
  if (side %in% c("r", "l")) {
    data <- data.frame(x = data$yend,
                       xend = data$y,
                       y = data$xend,
                       yend = data$x)
    data$y <- max_x - data$y
    data$yend <- max_x - data$yend
    if (side == "l") {
      data$x <- max_y - data$x
      data$xend <- max_y - data$xend
    }
  }

  ggplot(data, aes(x = x, xend = xend, y = y, yend = yend)) +
    ggplot2::geom_segment() +
    ggplot2::theme_void()
}

#' @noRd
gen_hc_bar <- function(hc,
                       k = 2,
                       side = "x",
                       colour = NULL,
                       border = NA,
                       color = NULL) {
  if (!inherits(hc, "hclust")) {
    stop("Hclust bar annotate cannot be added to unclustered data.")
  }
  tree <- stats::cutree(hc, k)[hc$labels[hc$order]]
  if (!is.null(color)) {
    colour <- color
  }
  if (is.null(colour)) {
    colour <- sample(grDevices::colors(TRUE), 2)
  }
  colour <- rep_len(colour, k)

  if (side == "x") {
    df <- tibble::tibble(x = names(tree),
                         y = "y",
                         fill = colour[unname(tree)])
    ggplot(df, aes(x = x, y = y, fill = fill)) +
      ggplot2::geom_tile(colour = border) +
      ggplot2::scale_fill_identity() +
      ggplot2::scale_x_discrete(limits = names(tree)) +
      ggplot2::theme_void() +
      theme_no_axis(side)
  } else {
    df <- tibble::tibble(x = "x",
                         y = names(tree),
                         fill = colour[unname(tree)])
    ggplot(df, aes(x = x, y = y, fill = fill)) +
      ggplot2::geom_tile(colour = border) +
      ggplot2::scale_fill_identity() +
      ggplot2::scale_y_discrete(limits = names(tree)) +
      ggplot2::theme_void() +
      theme_no_axis(side) +
      ggplot2::theme(plot.margin = ggplot2::margin())
  }
}

#' @noRd
theme_no_axis <- function(side = "x") {
  no_x <- ggplot2::theme(axis.title.x = element_blank(),
                         axis.title.x.top = element_blank(),
                         axis.title.x.bottom = element_blank(),
                         axis.text.x = element_blank(),
                         axis.text.x.top = element_blank(),
                         axis.text.x.bottom = element_blank(),
                         axis.ticks.x = element_blank(),
                         axis.ticks.x.top = element_blank(),
                         axis.ticks.x.bottom = element_blank(),
                         axis.line.x = element_blank(),
                         axis.line.x.top = element_blank(),
                         axis.line.x.bottom = element_blank())
  no_y <- ggplot2::theme(axis.title.y = element_blank(),
                         axis.title.y.left = element_blank(),
                         axis.title.y.right = element_blank(),
                         axis.text.y = element_blank(),
                         axis.text.y.left = element_blank(),
                         axis.text.y.right = element_blank(),
                         axis.ticks.y = element_blank(),
                         axis.ticks.y.left = element_blank(),
                         axis.ticks.y.right = element_blank(),
                         axis.line.y = element_blank(),
                         axis.line.y.left = element_blank(),
                         axis.line.y.right = element_blank())
  if (side == "all") {
    no_x + no_y
  } else if (side == "x") {
    no_x
  } else {
    no_y
  }
}
