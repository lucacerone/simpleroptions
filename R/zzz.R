.onAttach <- function(libname, pkgname) {
  loading_message <- paste0("Loading ", pkgname, " v", packageVersion(pkgname))
  packageStartupMessage(loading_message)
}
