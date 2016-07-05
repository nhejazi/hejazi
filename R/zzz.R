.onAttach <- function(...) {
  packageStartupMessage("nima: Nima Hejazi's Miscellaneous R Tools")
  packageStartupMessage("Version: ", utils::packageDescription('nima')$Version)
  packageStartupMessage("Package created on ", utils::packageDescription("nima")$Date, "\n")
  if (utils::packageDescription("nima")$RemoteType == "github" & utils::packageDescription("nima")$RemoteRef == "develop") {
    packageStartupMessage("This is the development version. \n Make sure to update frequently.")
  }
}
