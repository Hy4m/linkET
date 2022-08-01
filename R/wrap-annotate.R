wrap_annotate <- function(plot,
                          ...,
                          width = NULL,
                          height = NULL,
                          guides = "collect") {
  anno <- list(...)
  anno <- anno[names(anno)[names(anno) %in% c("r", "l", "t", "b")]]

  if (empty(anno)) {
    return(plot)
  }

  anno <- lapply(anno, function(x) {
    if (inherits(x, "ggplot")) {
      list(x)
    } else x
  })

  nm <- names(anno)

  for (ii in nm) {
    ele_name <- names(anno[[ii]])
    if (!is.null(ele_name)) {
      for (jj in seq_along(anno[[ii]])) {
        if (ele_name[jj] != "") {
          anno[[ii]][[jj]] <- anno[[ii]][[jj]] + ggplot2::labs(tag = ele_name[jj])
        }
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

  n <- length(anno$t) + length(anno$b)
  m <- length(anno$l) + length(anno$r)
  ll <- rep_len(list(patchwork::plot_spacer()), (n + 1) * (m + 1))

  id <- seq(length(anno$t) * (m + 1) + 1, length.out = m + 1)
  id2 <- seq(length(anno$l) + 1, by = m + 1, length.out = n + 1)
  ll[id] <- c(anno$l, list(plot), anno$r)
  ll[id2] <- c(anno$t, list(plot), anno$b)

  width <- c(width$l, 1, width$r)
  height <- c(height$t, 1, height$b)
  Reduce("+", ll) + patchwork::plot_layout(ncol = m + 1,
                                           nrow = n + 1,
                                           byrow = TRUE,
                                           widths = width,
                                           heights = height,
                                           guides = guides)
}

gen_tree <- function(hc, side = "t") {
  if (is.null(hc)) {
    stop("Tree annotate cannot be added to unclustered data.", call. = FALSE)
  }
  if (inherits(hc, "ggtree")) {
    return(hc)
  }

  data <- ggdendro::dendro_data(hc)$segments
  max_x <- max(data$x, data$xend) + 0.5
  max_y <- max(data$y, data$yend) + 0.5
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
    ggplot2::geom_segment()
}
