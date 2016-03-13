#' A Function to Change Directories using UNIX Shorthand
#'
#' This function allows you to change directories with 'cd'.
#' @param dir Where do you want to go relative to the home directory? 
#'        No defaults, so the target path must be specified in full.
#'        Tilde expansion is performed when possible (OS X and Linux).
#' @keywords
#' @export
#' @examples
#' cd()
 
cd <- function(dir) {
    if (getwd() != paste0(path.expand("~"),"/",dir)) {
        setwd(paste0(path.expand("~"),"/",dir))
    } else {
        print("You are already there.")
    }
}

#' exit R without saving
#'
#' exit R without saving workspace.
#'
#' @details This just calls \code{q("no")}
#'
#' @export
#' @return None.
#' @keywords utilities

exit <- function() q("no")


#  h
#'
#' View html version of help file
#'
#' View the html version of a help file while running R via ESS within emacs.
#'
#' @param ... Help topics.
#'
#' @details
#' This just calls the function \code{\link[utils]{help}} using the
#'   argument \code{htmlhelp=TRUE}.
#'
#' @export
#' @return
#' No return value.
#'
#' @examples
#' h(read.cross)
#'
#' @seealso
#' \code{\link[utils]{help}}, \code{\link[utils]{help.start}}
#'
#' @keywords
#' documentation
h <-
    function(...)
{
    utils::help(..., help_type="html")
}
