#' Convert a Factor to Numeric
#'
#' Convert a factor with numeric levels to a non-factor (numeric).
#'
#' @param x A vector containing a factor with numeric levels.
#'
#' @return The input factor made into a numeric vector.
#'
#' @export
#'
#' @examples
#' x <- factor(c(3, 4, 9, 4, 9), levels = c(3, 4, 9))
#' factor_to_num(x)
factor_to_num <- function(x) {
  names_factor <- names(x)
  x <- as.numeric(as.character(x))
  names(x) <- names_factor
  return(x)
}
