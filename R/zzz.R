#nocov start
# Create environment for the package
CF <- new.env(parent = emptyenv())

.onLoad <- function(libname, pkgname) {
  assign("memory_cell_limit", 1e8, envir = CF)
  assign("eps", .Machine$double.eps^0.5, envir = CF)
  assign("digits", 6L, envir = CF)
}
#nocov end
