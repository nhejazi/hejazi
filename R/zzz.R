.onAttach <- function(...) {
  packageStartupMessage(paste0(
    "nima v",
    utils::packageDescription("nima")$Version,
    ": Nima Hejazi's R Toolbox"
  ))
}
