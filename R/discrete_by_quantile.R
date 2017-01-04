#' Discretize a vector
#'
#' Discretizes a non-factor input vector and returns the result as numeric.
#'
#' @param x A vector containing arbitrary data.
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

discrete_by_quantile <- function(x) {
  if( class(x) != "factor" ) {
    as.numeric(gtools::quantcut(x))
  } else {
    as.numeric(x)
  }
}
