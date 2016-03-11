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
