#' @importFrom utils packageVersion

.onAttach <- function(libname, pkgname){

  # Extract version number
  v <- utils::packageVersion("pronghornLT")

  # Startup message
  packageStartupMessage(paste0("pronghornLT (version ", v ,")"))

}
