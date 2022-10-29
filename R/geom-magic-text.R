#' @title Magic Method to Draw labels
#' @description a wrapper function of \code{ggtext::geom_richtext}, but can
#' formatted labels by \code{LaTeX} style.
#' @param mapping set of aesthetic mappings created by \code{aes()} or \code{aes_()}.
#' @param data the data to be displayed in this layer.
#' @param stat the statistical transformation to use on the data for this layer,
#' as a string.
#' @param position position adjustment, either as a string, or the result of a
#' call to a position adjustment function.
#' @param geom one of text, label or richtext.
#' @param parse logical or a parse function. IF TRUE (default), the labels will
#' be parsed into expression.
#' @param ... others passing to \code{ggplot2::geom_<geom>()}.
#' @return a gg layer object.
#' @rdname geom_magic_text
#' @author Hou Yun
#' @export
geom_magic_text <- function(mapping = NULL,
                            data = NULL,
                            stat = "identity",
                            position = "identity",
                            geom = "text",
                            parse = TRUE,
                            ...) {
  geom <- gsub("geom_", "", geom, fixed = TRUE)
  geom <- match.arg(geom, c("text", "label", "richtext", "text_repel", "label_repel"))
  structure(list(mapping = mapping,
                 data = data,
                 stat = stat,
                 position = position,
                 geom = geom,
                 parse = parse,
                 ...), class = "magic_text")
}

#' @importFrom ggplot2 ggplot_add
#' @export
ggplot_add.magic_text <- function(object, plot, object_name) {
  inherit.aes <- object$inherit.aes %||% TRUE
  if(isTRUE(inherit.aes)) {
    object$mapping <- aes_modify(plot$mapping, object$mapping)
  }

  object$data <- tibble::as_tibble(object$data %||% plot$data)
  label <- aes_vars(object$mapping, "label")
  if(is.null(label) && is.null(object$label)) {
    stop("geom_magic_text requires the label aesthetics.", call. = FALSE)
  }

  parse_fun <- object$parse
  if(isTRUE(parse_fun) || is.function(parse_fun)) {
    if (!is.function(parse_fun)) {
      parse_fun <- parse_func()
    }
    if(!is.null(object$label)) {
      object$label <- parse_fun(object$label)
    } else {
      object$data[[label]] <- parse_fun(object$data[[label]])
    }
  }

  geom <- switch (object$geom,
    "text" = ggplot2::geom_text,
    "label" = ggplot2::geom_label,
    "richtext" = get_function("ggtext", "geom_richtext"),
    "label_repel" = get_function("ggrepel", "geom_label_repel"),
    "text_repel" = get_function("ggrepel", "geom_text_repel")
  )

  if (object$geom == "richtext") {
    object <- object[setdiff(names(object), c("geom", "parse"))]
  } else {
    object <- object[setdiff(names(object), "geom")]
  }

  object <- do.call(geom, object)
  ggplot_add(object, plot, object_name)
}

#' @title Converts a LaTeX String to Richtext
#' @description Helper function to convert a LaTeX string to richtext.
#' @param x a character vector.
#' @param sup one-length character, indicates that characters after this is superscript.
#' @param sub one-length character, indicates that characters after this is subscript.
#' @param br string,  separator of lines.
#' @param env environment to evaluate each expression in.
#' @rdname latex_richtext
#' @author Hou Yun
#' @export
#' @examples
#' name <- c("A_2", "B^3", "C_2", "D^{123 + x}")
#' name <- latex_richtext(name)

latex_richtext <- function(x,
                           sup = "^",
                           sub = "_",
                           br = "\n",
                           env = parent.frame()) {
  if(!is.character(x)) {
    x <- as.character(x)
  }
  ## more safely glue: glue always trim double {{ or }} to single
  x <- gsub("}}", "}}}}", x, fixed = TRUE)
  x <- vapply(x, function(.x) {
    glue::glue(.x, .envir = env, .open = ".val{", .close = "}")
  }, character(1), USE.NAMES = FALSE)

  x <- gsub(br, "<br>", x, fixed = "TRUE")
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

#' @title Converts a LaTeX String to Expression
#' @description Helper function to convert a LaTeX string to expression.
#' @param x a character vector.
#' @param sup one-length character, indicates that characters after this is superscript.
#' @param sub one-length character, indicates that characters after this is subscript.
#' @param space if `mode = "formula"`, will replace space in x with it.
#' @param user_defined user-defined command, see \code{?latex2exp::TeX} for details.
#' @param env environment to evaluate each expression in.
#' @param mode 'inline' means string is normal text with some formula,
#' and 'formula' means all string is formula.
#' @param output the type of returned object, should be expression or character.
#' @param ... other parameters passing to \code{latex2exp::TeX()}.
#' @rdname latex_expression
#' @author Hou Yun
#' @export
#' @examples
#' name <- c("A_2", "B^3", "C_2", "D^{123 + x}")
#' name <- latex_expression(name)
latex_expression <- function(x,
                             sup = NULL,
                             sub = NULL,
                             space = "\\ ",
                             user_defined = list(),
                             env = parent.frame(),
                             mode = "formula",
                             output = "character",
                             ...) {
  if (!is.character(x)) {
    x <- as.character(x)
  }
  mode <- match.arg(mode, c("inline", "formula"))
  output <- match.arg(output, c("expression", "character"))

  if (length(x) < 1) {
    if (output == "character") {
      return(character(0))
    } else {
      return(expression())
    }
  }

  is_na <- is.na(x)
  x <- ifelse(is_na, "", x)

  ## more safely glue: glue always trim double {{ or }} to single
  x <- gsub("}}", "}}}}", x, fixed = TRUE)
  x <- vapply(x, function(.x) {
    glue::glue(.x, .envir = env, .open = ".val{", .close = "}")
  }, character(1), USE.NAMES = FALSE)

  if (mode == "formula") {
    x <- paste0("$", x, "$")
    x <- gsub(" ", space, x, fixed = TRUE)
  }

  if (!is.null(sup)) {
    x <- vapply(x, function(.x) {
      gsub("^", "SUPPUS", .x, fixed = TRUE)
    }, character(1), USE.NAMES = FALSE)
    x <- vapply(x, function(.x) {
      gsub(sup, "^", .x, fixed = TRUE)
    }, character(1), USE.NAMES = FALSE)
  }

  if (!is.null(sub)) {
    x <- vapply(x, function(.x) {
      gsub("_", "SUBBUS", .x, fixed = TRUE)
    }, character(1), USE.NAMES = FALSE)
    x <- vapply(x, function(.x) {
      gsub(sub, "_", .x, fixed = TRUE)
    }, character(1), USE.NAMES = FALSE)
  }

  TeX <- get_function("latex2exp", "TeX")
  x <- TeX(x, user_defined = user_defined, output = "character", ...)
  x <- gsub("SUPPUS", "^", x, fixed = TRUE)
  x <- gsub("SUBBUS", "_", x, fixed = TRUE)

  if (output == "expression") {
    out <- parse_safe(x)
  } else {
    out <- unname(x)
    out <- ifelse(is_na, NA_character_, out)
  }
  out
}

#' @rdname latex_expression
#' @export
parse_func <- function(..., output = "character") {
  output <- match.arg(output, c("character", "richtext", "expression"))
  if (output == "richtext") {
    function(x) {
      latex_richtext(x = x, ...)
    }
  } else {
    function(x) {
      if (any(grepl("\n", x, fixed = TRUE))) {
        warning("R expression not supports multiline formula,\n",
                "please use richtext instead.", call. = FALSE)
      }
      latex_expression(x = x, ..., output = output)
    }
  }
}

#' @noRd
parse_safe <- function(x) {
  out <- vector("expression", length(x))
  for (ii in seq_along(x)) {
    expr <- parse(text = x[ii])
    out[[ii]] <- if (length(expr) == 0) NA else expr[[1]]
  }
  out
}

#' @noRd
is_richtext <- function(x, pattern = NULL) {
  if (!is.character(x)) {
    return(FALSE)
  }

  if(is.null(pattern)) {
    pattern <- c("<sub>", "<sup>", "<br>", "<span")
  }
  if(length(pattern) > 1) {
    pattern <- paste(pattern, collapse = "|")
  }
  x <- gsub("\\s+", "", x)
  any(grepl(pattern, x))
}
