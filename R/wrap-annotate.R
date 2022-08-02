wrap_annotate <- function(plot,
                          ...,
                          width = NULL,
                          height = NULL) {
  anno <- list(...)
  anno <- anno[names(anno)[names(anno) %in% c("r", "l", "t", "b")]]

  if (empty(anno)) {
    return(plot)
  }

  anno <- lapply(anno, function(x) {
    if (inherits(x, "ggplot")) {
      list(x)
    } else {
      x
    }
  })

  nm <- names(anno)

  for (ii in nm) {
    ele_name <- names(anno[[ii]])
    for (jj in seq_along(anno[[ii]])) {
      if (identical(anno[[ii]][[jj]],  "tree")) {
        anno[[ii]][[jj]] <- gen_tree(attr(plot$data,
                                          if (is_row) "row_tree" else "col_tree"),
                                     side = ii)
      }
      if (!is.null(ele_name)) {
        if (ele_name[jj] != "") {
          anno[[ii]][[jj]] <- anno[[ii]][[jj]] + ggplot2::labs(tag = ele_name[jj])
        }
      }

      if (ii %in% c("r", "l")) {
        anno[[ii]][[jj]] <- anno[[ii]][[jj]] + aplot::ylim2(plot)  + theme_no_axis("y")
      } else {
        anno[[ii]][[jj]] <- anno[[ii]][[jj]] + aplot::ylim2(plot) + theme_no_axis("x")
      }
    }
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

  for (ii in nm) {
    if (ii == "r") {
      for (jj in seq_along(anno[[ii]])) {
        plot <- plot %>% aplot::insert_right(anno[[ii]][[jj]], width = width[[ii]][jj])
      }
    }
    if (ii == "l") {
      plot <- plot %>% aplot::insert_left(anno[[ii]][[jj]], width = width[[ii]][jj])
    }
    if (ii == "t") {
      plot <- plot %>% aplot::insert_top(anno[[ii]][[jj]], height = height[[ii]][jj])
    }
    if (ii == "b") {
      plot <- plot %>% aplot::insert_bottom(anno[[ii]][[jj]], height = height[[ii]][jj])
    }
  }

  plot
}

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

  ggplot(data, aes_string(x = "x", xend = "xend", y = "y", yend = "yend")) +
    ggplot2::geom_segment() +
    ggplot2::theme_void()
}


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
