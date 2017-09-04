#' Conveniently Load Bioconductor
#'
#' Load Bioconductor's "biocLite" function by wrapping "source".
#'
#' @details Wrapper to \code{source("https://bioconductor.org/biocLite.R")}
#'
#' @export
#'

bioc <- function() {
  source("https://bioconductor.org/biocLite.R")
}

