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
      axis.text.x.top = element_text(angle = 90, hjust = 0, vjust = 0.5),
      axis.text.x.bottom = element_text(angle = 90, hjust = 1, vjust = 0.5),
      ...
    )
  } else {
    element_markdown <- get_function("ggtext", "element_markdown")
    theme(
      axis.text = element_markdown(size = 10.5, colour = "black"),
      axis.title = element_blank(),
      axis.line = element_blank(),
      axis.text.y.left = element_markdown(),
      axis.text.y.right = element_markdown(),
      axis.text.x.top = element_markdown(angle = 90, hjust = 0, vjust = 0.5),
      axis.text.x.bottom = element_markdown(angle = 90, hjust = 1, vjust = 0.5),
      ...
    )
  }
}

#' @noRd
theme_no_axis <- function() {
  theme(
    axis.title = element_blank(),
    axis.title.x = element_blank(),
    axis.title.x.top = element_blank(),
    axis.title.x.bottom = element_blank(),
    axis.title.y = element_blank(),
    axis.title.y.left = element_blank(),
    axis.title.y.right = element_blank(),
    axis.text = element_blank(),
    axis.text.x = element_blank(),
    axis.text.x.top = element_blank(),
    axis.text.x.bottom = element_blank(),
    axis.text.y = element_blank(),
    axis.text.y.left = element_blank(),
    axis.text.y.right = element_blank(),
    axis.ticks = element_blank(),
    axis.ticks.x = element_blank(),
    axis.ticks.x.top = element_blank(),
    axis.ticks.x.bottom = element_blank(),
    axis.ticks.y = element_blank(),
    axis.ticks.y.left = element_blank(),
    axis.ticks.y.right = element_blank(),
    axis.line = element_blank(),
    axis.line.x = element_blank(),
    axis.line.x.top = element_blank(),
    axis.line.x.bottom = element_blank(),
    axis.line.y = element_blank(),
    axis.line.y.left = element_blank(),
    axis.line.y.right = element_blank(),
  )
}
