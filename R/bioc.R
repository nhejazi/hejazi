#' Conveniently Load Bioconductor
#'
#' Load Bioconductor's \code{biocLite} function by wrapping \code{source}.
#'
#' @details Wrapper to \code{source("https://bioconductor.org/biocLite.R")}
#'
#' @export
#'
#
bioc <- function() {
  source("https://bioconductor.org/biocLite.R")
}
