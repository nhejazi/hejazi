.onAttach <- function(...) {
  packageStartupMessage("nima: Nima Hejazi's Miscellaneous R Tools")
  packageStartupMessage("Version: ", utils::packageDescription("nima")$Version)
}
