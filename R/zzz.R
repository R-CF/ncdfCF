#nocov start
# Create environment for global ncdfCF variables
globals <- new.env(parent = emptyenv())

.onLoad <- function(libname, pkgname) {
  assign("df_column_width", 50, envir = globals)
}
#nocov end
