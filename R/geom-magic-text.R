#' @title Magic Method to Draw labels
#' @description a wrapper function of \code{ggtext::geom_richtext}, but can
#' formatted labels by \code{LaTeX} style.
#' @param mapping set of aesthetic mappings created by \code{aes()} or \code{aes_()}.
#' @param data the data to be displayed in this layer.
#' @param stat the statistical transformation to use on the data for this layer,
#' as a string.
#' @param position position adjustment, either as a string, or the result of a
#' call to a position adjustment function.
#' @param parse logical. IF TRUE (default), the labels will be parsed into richtext.
#' @param sup one-length character, indicates that characters after this is superscript.
#' @param sub one-length character, indicates that characters after this is subscript.
#' @param br string,  separator of lines.
#' @param ... others passing to \code{ggtext::geom_richtext()}.
#' @return a gg layer object.
#' @rdname geom_magic_text
#' @author Hou Yun
#' @export
geom_magic_text <- function(mapping = NULL,
                            data = NULL,
                            stat = "identity",
                            position = "identity",
                            parse = TRUE,
                            sup = "^",
                            sub = "_",
                            br = "\n",
                            ...) {
  if(!suppressMessages(requireNamespace("ggtext"))) {
    stop("ggtext package has not been installed", call. = FALSE)
  }
  structure(list(mapping = mapping,
                 data = data,
                 stat = stat,
                 position = position,
                 parse = parse,
                 sup = sup,
                 sub = sub,
                 br = br,
                 ...), class = "magic_text")
}

#' @importFrom ggplot2 ggplot_add
#' @export
ggplot_add.magic_text <- function(object, plot, object_name) {
  inherit.aes <- object$inherit.aes %||% TRUE
  if(isTRUE(inherit.aes)) {
    object$mapping <- aes_modify(plot$mapping, object$mapping)
  }

  object$data <- object$data %||% plot$data
  label <- aes_vars(object$mapping, "label")
  if(is.null(label) && is.null(object$label)) {
    stop("geom_magic_text requires the label aesthetics.", call. = FALSE)
  }

  if(isTRUE(object$parse)) {
    if(!is.null(object$label)) {
     object$label <- latex_richtext(object$label,
                                    sup = object$sup,
                                    sub = object$sub,
                                    br = object$br)
    } else {
      object$data[[label]] <- latex_richtext(object$data[[label]],
                                             sup = object$sup,
                                             sub = object$sub,
                                             br = object$br)
    }
  }

  object <- object[setdiff(names(object), c("sub", "sup", "br", "parse"))]
  if(is.null(object$fill %||% object$mapping$fill)) {
    object$fill <- NA
  }
  if(is.null(object$label.colour %||% object$mapping$label.colour)) {
    object$label.colour <- NA
  }
  geom_richtext <- get_function("ggtext", "geom_richtext")
  object <- do.call(geom_richtext, object)
  ggplot_add(object, plot, object_name)
}

#' @title Converts a LaTeX String to a Rich Text
#' @description Helper function to convert a LaTeX string to a rich text.
#' @param x a character vector.
#' @param sup one-length character, indicates that characters after this is superscript.
#' @param sub one-length character, indicates that characters after this is subscript.
#' @param br string,  separator of lines.
#' @rdname latex_richtext
#' @author Hou Yun
#' @export
#' @examples
#' name <- c("A_2", "B^3", "C_2", "D^{123 + x}")
#' name <- latex_richtext(name)

latex_richtext <- function(x,
                           sup = "^",
                           sub = "_",
                           br = "\n") {
  if(!is.character(x)) {
    x <- as.character(x)
  }

  x <- sub(br, "<br>", x, fixed = "TRUE")
  x <- vapply(x, function(.x) {
    if(is.na(.x)) {
      return(.x)
    }

    ll <- unlist(strsplit(.x, ""))

    any_tex <- any(c(sup, sub) %in% ll)
    if(!any_tex) {
      return(.x)
    }

    if(sum(ll == "{") != sum(ll == "}")) {
      stop("Invalid latex mode character.", call. = FALSE)
    }

    n <- length(ll)
    sub_id <- which(ll == sub)
    sub_id <- sub_id[sub_id < n]
    sup_id <- which(ll == sup)
    sup_id <- sup_id[sup_id < n]

    if(length(sub_id) > 0) {
      purrr::walk(sub_id, function(.id) {
        if(ll[.id + 1] == "{") {
          eid <- end_bracket(.id + 1, ll)
          ll[.id] <<- "<sub>"
          ll[.id + 1] <<- ""
          ll[eid] <<- "</sub>"
        } else {
          ll[.id] <<- "<sub>"
          ll[.id + 1] <<- paste0(ll[.id + 1], "</sub>")
        }
      })
    }

    if(length(sup_id) > 0) {
      purrr::walk(sup_id, function(.id) {
        if(ll[.id + 1] == "{") {
          eid <- end_bracket(.id + 1, ll)
          ll[.id] <<- "<sup>"
          ll[.id + 1] <<- ""
          ll[eid] <<- "</sup>"
        } else {
          ll[.id] <<- "<sup>"
          ll[.id + 1] <<- paste0(ll[.id + 1], "</sup>")
        }
      })
    }

    paste0(ll, collapse = "")
  }, character(1), USE.NAMES = FALSE)
  x
}

#' @noRd
end_bracket <- function(id, ll) {
  out <- integer(length(ll))
  out[ll == "{"] <- -1L
  out[ll == "}"] <- 1L
  s <- which(cumsum(out) == 0L)
  s[s > id][1L]
}


