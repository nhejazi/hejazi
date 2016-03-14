#' Compare Two Similar Objects including Missing Data Patterns.
#'
#' Check whether two objects are the same, including patterns of \code{NA}s.
#'
#' @aliases compFun.default compFun.list
#'
#' @param a An object of a given type.
#'
#' @param b An object similar in type to that given above.
#'
#' @return
#' Boolean object with \code{TRUE} indicating an element is the same.
#'
#' @export
#'
#' @examples
#' x <- c(5, 8, 9, NA, 3, NA)
#' y <- c(5, 2, 9, 4, NA, NA)
#' compare(x,y)
#'
#' x <- matrix(rnorm(1000), ncol = 20)
#' x[sample(seq(along = x), 100)] <- NA
#' all(compare(x,x))
#' dim(compare(x,x))
#'
#' x <- as.list(x)
#' y <- as.list(y)
#' sapply(compare(x,y), function(a) sum(!a))
#'
#' x <- as.data.frame(x)
#' y <- as.data.frame(y)
#' sum(!compare(x,y))
#'
#' y <- x
#' y[4,8] <- NA
#' sum(!compare(x,y))
#' y[6,2] <- 18
#' sum(!compare(x,y))
#' y[6,5] <- 32
#' sum(!compare(x,y))

compare <- function(a, b) {
  UseMethod("comp_fun")
}

#' @export
comp_fun.default <- function(a, b) {
    ((is.na(a) & is.na(b)) | (!is.na(a) & !is.na(b) & a == b))
}

#' @export
comp_fun.list <- function(a,b) {
    for (i in seq(along = a)) {
      a[[i]] <- com_fun(a[[i]], b[[i]])
    }
    a
}
