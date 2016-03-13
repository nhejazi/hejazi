#'
#' Compare two similar objects, including the pattern of missing data.
#'
#' Check whether two objects are the same, including patterns of \code{NA}s.
#'
#' @aliases cf.default cf.list
#'
#' @param a An object of a given type.
#'
#' @param b An object similar in type to the above.
#'
#' @export
#'
#' @return
#' Boolean object with \code{TRUE} indicating an element is the same.
#'
#' @examples
#' x <- c(5, 8, 9, NA, 3, NA)
#' y <- c(5, 2, 9, 4, NA, NA)
#' cf(x,y)
#'
#' x <- matrix(rnorm(1000), ncol=20)
#' x[sample(seq(along=x), 100)] <- NA
#' all(cf(x,x))
#' dim(cf(x,x))
#'
#' y <- x
#' y[4,8] <- NA
#' sum(!cf(x,y))
#' y[6,2] <- 18
#' sum(!cf(x,y))
#' y[6,5] <- 32
#' sum(!cf(x,y))
#'
#' x <- as.data.frame(x)
#' y <- as.data.frame(y)
#' sum(!cf(x,y))
#'
#' x <- as.list(x)
#' y <- as.list(y)
#' sapply(cf(x,y), function(a) sum(!a))
#'
#' @keywords data

compare <- function(a, b) UseMethod("cf")

#' @export
cf.default <-
    function(a, b)
    ((is.na(a) & is.na(b)) | (!is.na(a) & !is.na(b) & a == b))

#' @export
cf.list <-
    function(a,b)
{
    for(i in seq(along=a)) a[[i]] <- cf(a[[i]], b[[i]])
    a
}
