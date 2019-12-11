#' Discretize a Vector by Quantiles
#'
#' Discretizes a non-factor input vector and returns the result as numeric.
#'
#' @param x A vector containing arbitrary data.
#' @param ... Additional arguments passed to \code{\link[gtools]{quantcut}}.
#'
#' @return A numeric vector with the data re-coded to based on the quantiles.
#'
#' @importFrom gtools quantcut
#'
#' @export
#'
#' @examples
#' x <- rnorm(1000)
#' discrete_by_quantile(x)
discrete_by_quantile <- function(x, ...) {
  if (!is.factor(x)) {
    as.numeric(gtools::quantcut(x, ...))
  } else {
    as.numeric(x)
  }
}
