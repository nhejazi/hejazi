#' New Package Skeleton
#'
#' Generate a skeleton for new R packages by invoking a series of utilities from
#' the Devtools package. This is merely a convenience utility for creating a new
#' directory with minimal package contents.
#'
#' @param name - Character for the name of the new package, to be passed
#'        directly to \code{devtools::create()}.
#'
#' @importFrom devtools create use_build_ignore use_testthat use_travis
#'
#' @export newpkg
#'

newpkg <- function(name = "new_pkg") {
  if (class(name) != "character") {
    stop("Argument 'name' must be a character specifying the name of the new R
         package. Try again.")
  }
  devtools::create(name)
  devtools::use_build_ignore(".gitignore")
  devtools::use_testthat()
  devtools::use_travis()
}

