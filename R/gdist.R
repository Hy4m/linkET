#' @title Distance Matrix Computation
#' @description This function can computes and returns the distance matrix
#' based on the data type.
#' @param .data a data frame object.
#' @param .FUN the name of dist function.
#' @param .formula lambda function.
#' @param ... other parameters passing to dist function.
#' @return dist matrix.
#' @rdname gdist
#' @author Hou Yun
#' @export
#' @examples
#' gdist(mtcars)
#' gdist(iris)
#'
#' m <- matrix(sample(LETTERS, 100, TRUE), nrow = 10)
#' gdist(m)
gdist <- function(.data,
                  .FUN = NULL,
                  .formula = NULL,
                  ...) {
  if(!is.data.frame(.data)) {
    .data <- as.data.frame(.data)
  }

  if(nrow(.data) < 1 || ncol(.data) < 1) {
    stop("Empty data.", call. = FALSE)
  }

  for(nm in names(.data)) {
    if(!is.numeric(.data[[nm]]) && !is.factor(.data[[nm]])) {
      .data[[nm]] <- as.factor(.data[[nm]])
    }
  }

  any_factor <- any(vapply(.data, is.factor, logical(1)))

  if(any_factor) {
    if(is.null(.FUN)) .FUN <- "gowdis"
    if(.FUN != "gowdis") {
      message("The data contains factors, I recommend setting:\n",
              "`.FUN = \"gowdis\"`\n")
      .FUN <- "gowdis"
    }
  } else {
    if(is.null(.FUN)) .FUN <- "dist"
  }
  .FUN <- switch (.FUN,
    "dist" = get_function("stats", "dist"),
    "vegdist" = get_function("vegan", "vegdist"),
    "gowdis" = get_function("FD", "gowdis"),
    match.fun(.FUN)
  )

  if(!is.null(.formula)) {
    trans <- rlang::as_function(.formula)
    .data <- trans(.data)
  }
  .FUN(.data, ...)
}

#' @rdname gdist
#' @export
dist_func <- function(...) {
  function(.data) {
    gdist(.data = .data, ...)
  }
}
