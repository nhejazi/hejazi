#' Add missingness indicators to existing data object
#'
#' Add indicator columns to a data.frame showing the pattern of missingness.
#'
#' @param data A numeric vector or array.
#' @param prefix A string used to name the indicator variables..
#'
#' @return An augmented data.frame with indicators for missingness patterns.
#'
#' @export
#'
#' @examples
#' data <- data.frame(cbind(rnorm(10), runif(10)))
#' data[sample(nrow(data), 3), 1] <- NA
#' data[sample(nrow(data), 4), 2] <- NA
#' data <- miss_ind(data)
#
miss_ind <- function(data, prefix = "miss_") {
  indicators <- sapply(data, FUN = function(col) as.numeric(is.na(col)))
  colnames(indicators) <- paste0(prefix, colnames(data))
  indicators <- indicators[, !colMeans(indicators) %in% c(0, 1)]
  out <- cbind(data, indicators)
  return(out)
}
