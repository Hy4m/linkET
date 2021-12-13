#' Create the default hyplot theme
#' @title hyplot theme
#' @param ... extra params passing to \code{\link[ggplot2]{theme}}.
#' @param use_md logical. if TRUE, will use \code{ggtext::element_markdown()} to
#' draw the axis labels.
#' @return The theme.
#' @importFrom ggplot2 theme
#' @importFrom ggplot2 element_text
#' @importFrom ggplot2 element_blank
#' @rdname theme_hy
#' @author Hou Yun
#' @export
theme_hy <- function(..., use_md = FALSE)
{
  if(isFALSE(use_md)) {
    theme(
      axis.text = element_text(size = 10.5, colour = "black"),
      axis.title = element_blank(),
      axis.line = element_blank(),
      axis.text.x.top = element_text(angle = 90, hjust = 0, vjust = 0),
      axis.text.x.bottom = element_text(angle = 90, hjust = 1, vjust = 1),
      ...
    )
  } else {
    element_markdown <- get_function("ggtext", "element_markdown")
    theme(
      axis.text = element_text(size = 10.5, colour = "black"),
      axis.title = element_blank(),
      axis.line = element_blank(),
      axis.text.x.top = element_markdown(angle = 90, hjust = 0, vjust = 0),
      axis.text.x.bottom = element_markdown(angle = 90, hjust = 1, vjust = 1),
      ...
    )
  }
}
