#' Create the default hyplot theme
#' @title hyplot theme
#' @param ... extra params passing to \code{\link[ggplot2]{theme}}.
#' @return The theme.
#' @importFrom ggplot2 theme element_text element_blank element_rect element_line
#' @rdname theme_hy
#' @author Hou Yun
#' @export
theme_hy <- function(...)
{
  theme(
    axis.text = element_text(size = 10.5, colour = "black"),
    axis.title = element_blank(),
    axis.line = element_blank(),
    axis.text.x.top = element_text(angle = 45, hjust = 0, vjust = 0),
    axis.text.x.bottom = element_text(angle = 45, hjust = 1, vjust = 1),
    ...
  )
}
