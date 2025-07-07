#nocov start
# Create environment for the package
CF <- new.env(parent = emptyenv())
CF.options <- new.env(parent = emptyenv())

.onLoad <- function(libname, pkgname) {
  assign("CFtypes", c("unknown", "data", "coordinate", "auxiliary_coordinate",
                      "scalar_coordinate", "boundary", "domain", "grid_mapping",
                      "cell_measure", "ancillary_data", "mesh_topology",
                      "location_index_set", "quantization",
                      "geometry_container"), envir = CF)
  assign("eps", .Machine$double.eps^0.5, envir = CF)
  assign("standard_names", CFStandardNames$new(), envir = CF)

  assign("memory_cell_limit", 1e8, envir = CF.options)
  assign("digits", 6L, envir = CF.options)
  assign("cache_stale_days", 90, envir = CF.options)
}
#nocov end
