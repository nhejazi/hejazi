#' New Project Skeleton
#'
#' Generate a skeleton for a data analysis project by calling ProjectTemplate
#'
#' @param name (character) - the name to be given to the new project directory
#' @param minimal (character) - option to set up only a minimal project
#'        directory, passed to \code{ProjectTemplate::create.project()}
#' @param ... - options to be passed to \code{ProjectTemplate::create.project()}
#'
#' @importFrom ProjectTemplate create.project
#'
#' @export
#'
newproj <- function(name = "ProjectTemplate", minimal = TRUE, ...) {
  ProjectTemplate::create.project(project.name = name, minimal = minimal, ...)
}

