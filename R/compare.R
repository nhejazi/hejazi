#' Compare Two Similar Objects including Missing Data Patterns.
#'
#' Check whether two objects are the same, including patterns of \code{NA}s.
#'
#' @aliases compFun.default compFun.list
#'
#' @param a An object of a given type.
#' @param b An object similar in type to that given above.
#'
#' @importFrom assertthat assert_that
#'
#' @return
#' Boolean object with \code{TRUE} indicating an element is the same.
#'
#' @examples
#' x <- c(5, 8, 9, NA, 3, NA)
#' y <- c(5, 2, 9, 4, NA, NA)
#' compFun(x,y)
#'
#' x <- matrix(rnorm(1000), ncol = 20)
#' x[sample(seq(along = x), 100)] <- NA
#' all(compFun(x,x))
#' dim(compFun(x,x))
#'
#' x <- as.list(x)
#' y <- as.list(y)
#' sapply(compFun(x,y), function(a) sum(!a))
#'
#' x <- as.data.frame(x)
#' y <- as.data.frame(y)
#' sum(!compFun(x,y))
#'
#' y <- x
#' y[4,8] <- NA
#' sum(!compFun(x,y))
#' y[6,2] <- 18
#' sum(!compFun(x,y))
#' y[6,5] <- 32
#' sum(!compFun(x,y))

compFun <- function(a, b) {
  assertthat::assert_that(class(a) == class(b))
  comp_fun <- ((is.na(a) & is.na(b)) | (!is.na(a) & !is.na(b) & a == b))
  if (class(a) == "list") {
    for (i in seq(along = a)) {
          a[[i]] <- comp_fun(a[[i]], b[[i]])
        }
        a
  } else {
    comp_fun(a, b)
  }
}
