#' Testy function #1
#'
#' @returns a color vector
#' @export

pesto_colors <- function(...) {
  cols <- c(...)

  if (is.null(cols))
    return (pesto_pal)

  pesto_pal[cols]
}

pesto_pal <- c(
  `verylow`  = "#A50026",
  `low`      = "#FDAE61",
  `medium`   = "#FFFFBF",
  `high`     = "#ABD9E9",
  `veryhigh` = "#313695"
)
