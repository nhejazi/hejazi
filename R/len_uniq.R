#' Number of unique values
#'
#' Get the number of unique values in a input vector.
#'
#' @param vec A vector of any type.
#' @param na.rm If \code{TRUE}, remove any missing values
#'
#' @return Number of unique values.
#'
#' @export
#'
#' @keywords
#'
#' @examples
#' x <- c(1, 3, 1, 1, NA, 2, 2, 3, NA, NA, 1, 3, 1)
#' len_uniq(x)
#' len_uniq(x, na.rm=FALSE)

len_uniq <- function(vec, na.rm=TRUE) {
    if(na.rm && !is.null(vec)) {
        vec <- vec[!is.na(vec)]
    }
    length(unique(vec))
}
