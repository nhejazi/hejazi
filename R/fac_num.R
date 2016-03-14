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
#' x <- factor(c(3, 4, 9, 4, 9), levels=c(3,4,9))
#' fac_num(x)

fac_num <- function(x) {
    fac_names <- names(x)
    x <- as.numeric(as.character(x))
    names(x) <- fac_names
    return(x)
}
