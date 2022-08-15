#' Significant Marks
#'
#' @description layer function to add significant marks on a plot.
#'
#' @param digits integer indicating the number of decimal places (round) or
#'     significant digits (signif) to be used, the default value is 2.
#' @param nsmall the minimum number of digits to the right of the decimal
#'     point in formatting real/complex numbers in non-scientific formats,
#'     the default value is 2.
#' @param only_mark if TRUE, only the significant marks add on the plot.
#' @param sig_level significance level，the default values is [0.05, 0.01, 0.001].
#' @param mark significance mark，the default values is ["*", "**", "***"].
#' @param sig_thres if not NULL, just when pvalue is not larger than sig_thres will be ploted.
#' @param sep a character string to separate the number and mark symbols.
#' @inheritParams ggplot2::layer
#' @inheritParams ggplot2::geom_text
#' @section Aesthetics:
#'     \code{geom_mark()} understands the following aesthetics (required
#'     aesthetics are in bold):
#'     \itemize{
#'       \item \strong{\code{x}}
#'       \item \strong{\code{y}}
#'       \item \strong{\code{pvalue}}
#'       \item \code{r}
#'       \item \code{alpha}
#'       \item \code{colour}
#'       \item \code{size}
#'       \item \code{angle}
#'       \item \code{hjust}
#'       \item \code{vjust}
#'       \item \code{family}
#'       \item \code{fontface}
#'       \item \code{lineheight}
#'    }
#' @importFrom ggplot2 GeomText
#' @importFrom ggplot2 draw_key_text
#' @importFrom ggplot2 position_nudge
#' @rdname geom_mark
#' @export
#' @examples
#' correlate(mtcars) %>%
#'   qcorrplot() + geom_mark()
geom_mark <- function(mapping = NULL,
                      data = NULL,
                      stat = "identity",
                      position = "identity",
                      ...,
                      nudge_x = 0,
                      nudge_y = 0,
                      digits = 2,
                      nsmall = 2,
                      only_mark = FALSE,
                      sig_level = c(0.05, 0.01, 0.001),
                      mark = c("*", "**", "***"),
                      sig_thres = NULL,
                      sep = "",
                      na.rm = FALSE,
                      show.legend = NA,
                      inherit.aes = TRUE)
{
  if (!missing(nudge_x) || !missing(nudge_y)) {
    if (!missing(position)) {
      stop("You must specify either `position` or `nudge_x`/`nudge_y`.", call. = FALSE)
    }
    position <- position_nudge(nudge_x, nudge_y)
  }

  layer(
    data = data,
    mapping = mapping,
    stat = stat,
    geom = GeomMark,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      digits = digits,
      nsmall = nsmall,
      only_mark = only_mark,
      sig_level = sig_level,
      mark = mark,
      sig_thres = sig_thres,
      sep = sep,
      na.rm = na.rm,
      ...
    )
  )
}

#' @rdname linkET-extensions
#' @format NULL
#' @usage NULL
#' @export
GeomMark <- ggproto("GeomMark", GeomText,
                   required_aes = c("x", "y"),

                   default_aes = aes(
                     r = NA, pvalue = NA, colour = "black", size = 3.88,
                     angle = 0, hjust = 0.5, vjust = 0.5, alpha = NA,
                     family = "",  fontface = 1, lineheight = 1.2),

                   draw_panel = function(data,
                                         panel_params,
                                         coord,
                                         digits = 2,
                                         nsmall = 2,
                                         only_mark = FALSE,
                                         sig_level = c(0.05, 0.01, 0.001),
                                         mark = c("*", "**", "***"),
                                         sig_thres = NULL,
                                         sep = "",
                                         parse = FALSE,
                                         na.rm = FALSE) {
                     if(empty(data)) {
                       return(grid::nullGrob())
                     }

                     if(all(is.na(data$pvalue))) {
                       if(all(is.na(data$r))) {
                         grid::nullGrob()
                       } else {
                         data$label <- format_number(data$r, digits, nsmall)
                         GeomText$draw_panel(data, panel_params, coord)
                       }
                     } else {
                       if(!is.null(sig_thres))
                         data <- dplyr::filter(data, pvalue <= sig_thres)
                       if(all(is.na(data$r))) {
                         if(isTRUE(only_mark)) {
                           data$label <- sig_mark(data$pvalue, sig_level, mark)
                         } else {
                           data$label <- format_number(data$pvalue, digits, nsmall)
                         }
                         GeomText$draw_panel(data, panel_params, coord)
                       } else {
                         star <- sig_mark(data$pvalue, sig_level, mark)
                         num <- format_number(data$r, digits, nsmall)
                         if(isTRUE(only_mark)) {
                           data$label <- star
                           GeomText$draw_panel(data, panel_params, coord)
                         } else {
                           data$label <- paste_with_na(num, star, sep = sep)
                           GeomText$draw_panel(data, panel_params, coord)
                         }
                       }
                     }
                   },
                   draw_key = draw_key_text
)


#' @noRd
sig_mark <- function(pvalue,
                     sig_level = c(0.05, 0.01, 0.001),
                     mark = c("*", "**", "***"))
{
  if (!is.numeric(pvalue))
    pvalue <- as.numeric(pvalue)
  ord <- order(sig_level)
  sig_level <- sig_level[ord]
  mark <- mark[ord]
  brks <- c(0, sig_level, 1)
  lbs <- c(mark, "")
  pp <- cut(pvalue, breaks = brks, labels = lbs, include.lowest = FALSE,
            right = TRUE)
  ifelse(pvalue == 0, mark[1], as.character(pp))
}

#' @noRd
format_number <- function (x, digits = 2, nsmall = 2)
{
  if (!is.numeric(x))
    stop("`x` must be a numeric vector.", call. = FALSE)
  is_na <- is.na(x)
  x <- format(round(x, digits = digits), nsmall = nsmall)
  x[is_na] <- ""
  x
}

#' @noRd
paste_with_na <- function(...,
                          sep = "",
                          collapse = NULL) {
  ll <- list(...)
  if(length(ll) == 0) {
    return(character(0))
  }
  n <- max(vapply(ll, length, numeric(1)))
  ll <- lapply(ll, function(.x) {rep_len(.x, n)})

  out <- character(length = n)
  for (ii in seq_len(n)) {
    ele <- lapply(ll, `[`, ii)
    if (all(is.na(unlist(ele)))) {
      out[ii] <- NA
    } else {
      id <- vapply(ele, is.na, logical(1))
      ele <- ele[!id]
      ele$sep <- sep
      out[ii] <- do.call("paste", ele)
    }
  }

  if (is.null(collapse)) {
    out <- ifelse(is.na(out), "", out)
  } else {
    if (length(out) > 1) {
      if (all(is.na(out))) {
        out <- ""
      } else {
        out <- paste(out[!is.na(out)], collapse = collapse)
      }
    } else {
      out <- if (is.na(out)) "" else out
    }
  }
  out
}
