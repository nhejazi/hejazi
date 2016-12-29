.onAttach <- function(...) {
  packageStartupMessage("nima: Nima Hejazi's R Toolbox")
  packageStartupMessage("Version: ", utils::packageDescription("nima")$Version)
}
