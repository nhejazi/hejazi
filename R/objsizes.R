#' Calculate Sizes of Objects in Workspace
#'
#' Calculate the sizes of all of the objects in the workspace.
#'
#' @param obj Vector of object names. If missing, pull out all object names.
#' @param bysize If \code{TRUE}, sort the objects from smallest to largest.
#'
#' @details
#' Calls \code{\link[utils]{object.size}} to get the sizes of a list of objects.
#'
#' @return
#' A data frame with the only column being the size of each object in
#' megabytes (Mb). The row names are the names of the objects.
#'
#' @export
#'
#' @examples
#' print(output <- objsizes())
#' \dontrun{sum(output)}

objsizes <- function(obj, bysize = TRUE) {
    if (missing(obj)) {
      obj <- objects(pos = 1)
    }
    result <- data.frame(Mb = rep(0, length(obj)))
    rownames(result) <- obj
    for (i in seq(along = obj)) {
      result[i, 1] <- (utils::object.size(get(obj[i], pos = 1)) / 1024 ^ 2)
    }
    if (sortbysize) {
      result <- result[order(result[,1], decreasing = FALSE), drop = FALSE]
    }
    result
}
