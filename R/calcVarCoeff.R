#' @title Calculate the Coefficient of Variation
#'
#' @export
calcVarCoeff <- function(x) {
  ifelse(mean(x, na.rm = T) == 0, 0, sd(x, na.rm = T) / mean(x, na.rm = T))
}
