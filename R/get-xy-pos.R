#' @noRd
get_xy_pos <- function(...) {
  check_plot(...)
  plots <- list(...)
  nm <- sort(names(plots))

  if(identical(nm, c("b", "l")) || identical(nm, c("b", "r")) ||
     identical(nm, c("l", "t")) || identical(nm, c("r", "t"))) {
    lr_plot <- plots$l %||% plots$r
    bt_plot <- plots$b %||% plots$t
    xrng <- xrange(bt_plot)
    yrng <- yrange(lr_plot)
    xpos <- seq_along(xrng)
    ypos <- seq_along(yrng)

    m <- length(xrng)
    n <- length(yrng)

    if(identical(nm, c("b", "l"))) {
      x <- rlang::set_names(c(rep_len(0.4, n), xpos), c(yrng, xrng))
      y <- rlang::set_names(c(ypos, rep_len(0.4, m)), c(yrng, xrng))
    }
    if(identical(nm, c("b", "r"))) {
      x <- rlang::set_names(c(rep_len(m + 0.6, n), xpos), c(yrng, xrng))
      y <- rlang::set_names(c(ypos, rep_len(0.4, m)), c(yrng, xrng))
    }
    if(identical(nm, c("l", "t"))) {
      x <- rlang::set_names(c(rep_len(0.4, n), xpos), c(yrng, xrng))
      y <- rlang::set_names(c(ypos, rep_len(n + 0.6, m)), c(yrng, xrng))
    }
    if(identical(nm, c("r", "t"))) {
      x <- rlang::set_names(c(rep_len(m + 0.6, n), xpos), c(yrng, xrng))
      y <- rlang::set_names(c(ypos, rep_len(n + 0.6, m)), c(yrng, xrng))
    }
  }

  if(identical(nm, c("l", "r"))) {
    lrng <- yrange(plots$l)
    rrng <- yrange(plots$r)
    m <- length(lrng)
    n <- length(rrng)
    x <- rlang::set_names(rep(c(0, 1), times = c(m, n)), c(lrng, rrng))
    if(m == n) {
      y <- rlang::set_names(rep(seq_len(m), 2), c(lrng, rrng))
    } else {
      if(m > n) {
        y1 <- seq_len(m)
        y2 <- seq_len(n) * m / n
        y <- rlang::set_names(c(y1, y2), c(lrng, rrng))
      } else {
        y1 <- seq_len(m) * n / m
        y2 <- seq_len(n)
        y <- rlang::set_names(c(y1, y2), c(lrng, rrng))
      }
    }
  }

  if(identical(nm, c("b", "t"))) {
    brng <- xrange(plots$b)
    trng <- xrange(plots$t)
    m <- length(brng)
    n <- length(trng)
    y <- rlang::set_names(rep(c(0, 1), times = c(m, n)), c(brng, trng))
    if(m == n) {
      x <- rlang::set_names(rep(seq_len(m), 2), c(brng, trng))
    } else {
      if(m > n) {
        x1 <- seq_len(m)
        x2 <- seq_len(n) * m / n
        x <- rlang::set_names(c(x1, x2), c(brng, trng))
      } else {
        x1 <- seq_len(m) * n / m
        x2 <- seq_len(n)
        x <- rlang::set_names(c(x1, x2), c(brng, trng))
      }
    }
  }

  if(identical(nm, c("b", "l", "r")) || identical(nm, c("l", "r", "t"))) {
    bt_plot <- plots$b %||% plots$t
    l_plot <- plots$l
    r_plot <- plots$r
    btrng <- xrange(bt_plot)
    m <- length(btrng)
    lrng <- yrange(l_plot)
    rrng <- yrange(r_plot)
    n1 <- length(lrng)
    n2 <- length(rrng)
    x <- rlang::set_names(c(rep_len(0.4, n1), seq_len(m), rep_len(m + 0.6, n2)),
                          c(lrng, btrng, rrng))
    if("b" %in% nm) {
      if(n1 == n2) {
        y <- rlang::set_names(c(seq_len(n1), rep_len(0.4, m), seq_len(n2)),
                              c(lrng, btrng, rrng))
      } else if(n1 > n2) {
        y1 <- seq_len(n1)
        y2 <- seq_len(n2) * n1 / n2
        y <- rlang::set_names(c(y1, rep_len(0.4, m), y2),
                              c(lrng, btrng, rrng))
      } else {
        y1 <- seq_len(n1) * n2 / n1
        y2 <- seq_len(n2)
        y <- rlang::set_names(c(y1, rep_len(0.4, m), y2),
                              c(lrng, btrng, rrng))
      }
    } else {
      if(n1 == n2) {
        y <- rlang::set_names(c(seq_len(n1), rep_len(m + 0.6, m), seq_len(n2)),
                              c(lrng, btrng, rrng))
      } else if(n1 > n2) {
        y1 <- seq_len(n1)
        y2 <- seq_len(n2) * n1 / n2
        y <- rlang::set_names(c(y1, rep_len(m + 0.6, m), y2),
                              c(lrng, btrng, rrng))
      } else {
        y1 <- seq_len(n1) * n2 / n1
        y2 <- seq_len(n2)
        y <- rlang::set_names(c(y1, rep_len(m + 0.6, m), y2),
                              c(lrng, btrng, rrng))
      }
    }
  }

  if(identical(nm, c("b", "r", "t")) ||
     identical(nm, c("b", "l", "t"))) {
    rl_plot <- plots$r %||% plots$l
    b_plot <- plots$b
    t_plot <- plots$t
    rlrng <- yrange(rl_plot)
    brng <- xrange(b_plot)
    trng <- xrange(t_plot)
    m <- length(rlrng)
    n1 <- length(brng)
    n2 <- length(trng)
    y <- rlang::set_names(c(rep_len(0.4, n1), seq_len(m), rep_len(m + 0.6, n2)),
                          c(brng, rlrng, trng))
    if("r" %in% nm) {
      if(n1 == n2) {
        x <- rlang::set_names(c(seq_len(n1), rep_len(m + 0.6, m), seq_len(n2)),
                              c(brng, rlrng, trng))
      } else if(n1 > n2) {
        x1 <- seq_len(n1)
        x2 <- seq_len(n2) * n1 / n2
        x <- rlang::set_names(c(x1, rep_len(m + 0.6, m), x2),
                              c(brng, rlrng, trng))
      } else {
        x1 <- seq_len(n1) * n2 / n1
        x2 <- seq_len(n2)
        x <- rlang::set_names(c(x1, rep_len(m + 0.6, m), x2),
                              c(brng, rlrng, trng))
      }
    } else {
      if(n1 == n2) {
        x <- rlang::set_names(c(seq_len(n1), rep_len(0.4, m), seq_len(n2)),
                              c(brng, rlrng, trng))
      } else if(n1 > n2) {
        x1 <- seq_len(n1)
        x2 <- seq_len(n2) * n1 / n2
        x <- rlang::set_names(c(x1, rep_len(0.4, m), x2),
                              c(brng, rlrng, trng))
      } else {
        x1 <- seq_len(n1) * n2 / n1
        x2 <- seq_len(n2)
        x <- rlang::set_names(c(x1, rep_len(0.4, m), x2),
                              c(brng, rlrng, trng))
      }
    }
  }


  if(identical(nm, c("b", "l", "r", "t"))) {
    brng <- xrange(plots$b)
    trng <- xrange(plots$t)
    lrng <- yrange(plots$l)
    rrng <- yrange(plots$r)

    m1 <- length(brng)
    m2 <- length(trng)
    n1 <- length(lrng)
    n2 <- length(rrng)
    lx <- rlang::set_names(rep(0.4, n1), lrng)
    rx <- rlang::set_names(rep(max(m1, m2) + 0.6, n2), rrng)
    by <- rlang::set_names(rep(0.4, m1), brng)
    ty <- rlang::set_names(rep(max(n1, n2) + 0.6, m2), trng)
    if(m1 == m2) {
      bx <- rlang::set_names(seq_len(m1), brng)
      tx <- rlang::set_names(seq_len(m2), trng)
    } else if(m1 > m2) {
      bx <- rlang::set_names(seq_len(m1), brng)
      tx <- rlang::set_names(seq_len(m2) * m1 / m2, trng)
    } else {
      bx <- rlang::set_names(seq_len(m1) * m2 / m1, brng)
      tx <- rlang::set_names(seq_len(m2), trng)
    }

    if(n1 == n2) {
      ly <- rlang::set_names(seq_len(n1), lrng)
      ry <- rlang::set_names(seq_len(n2), rrng)
    } else if(n1 > n2) {
      ly <- rlang::set_names(seq_len(n1), lrng)
      ry <- rlang::set_names(seq_len(n2) * n1 / n2, rrng)
    } else {
      ly <- rlang::set_names(seq_len(n1) * n2 / n1, lrng)
      ry <- rlang::set_names(seq_len(n2), rrng)
    }
    x <- c(bx, lx, tx, rx)
    y <- c(by, ly, ty, ry)
  }
  list(x = x, y = y)
}

#' @noRd
check_plot <- function(...) {
  ll <- list(...)
  nm <- names(ll)
  if(is.null(nm) || !nm %in% c("b", "l", "t", "r")) {
    stop("Invalid plot names.", call. = FALSE)
  }
  if(length(ll) < 2) {
    stop("At least two of b, l, t and r are not NULL", call. = FALSE)
  }
  null_or_gg <- vapply(ll, function(.x) {
    is.null(.x) || inherits(.x, "gg")
  }, logical(1))
  if(!all(null_or_gg)) {
    stop("b, l, t and r should be NULL or gg.", call. = FALSE)
  }
}
