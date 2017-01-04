#' Exit R Without Saving
#'
#' Exit R without saving workspace, using the ubiquitous UNIX syntax.
#'
#' @details This function is merely a call to \code{q("no")}.
#'
#' @export

exit <- function() q("no")


#' View HTML Version of Help Files
#'
#' View the HTML version of a help file while running R from the terminal.
#'
#' @param ... Help topics.
#'
#' @details
#' Calls function \code{\link[utils]{help}} using argument \code{htmlhelp=TRUE}.
#'
#' @export
#'
#' @examples
#' \dontrun{hweb(read.table)}
#'
#' @seealso
#' \code{\link[utils]{help}}, \code{\link[utils]{help.start}}

hweb <- function(...) {
    utils::help(..., help_type = "html")
}


#' Clear the Current Screen/Buffer
#'
#' Clear the screen with a call to \code{\link[base]{system}} and \code{clear}.
#'
#' @details This function is merely a call to \code{system("clear")}
#'
#' @export
#'
#' @examples
#' \dontrun{system("clear")}

clear <- function() system("clear")


#' Open a File
#'
#' Open a file using \code{\link[base]{system}} and \code{open}.
#'
#' @param file File name (as character string).
#'
#' @details Open files from R by using the default operating system program.
#'
#' @export
#'
#' @examples
#' \dontrun{openfile("myplot.pdf")}

openfile <- function(file) {
    system( paste("open", file) )
}
