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

  # The below variables are used to generate unique id's for temporary objects.
  # When data sets or data arrays are written to file, new id's as reported by
  # the netCDF library are assigned. Don't use these variables directly, instead
  # use functions .newVarId() and .newDimId().
  assign("currentGroupId", 0L, envir = CF)
  assign("currentVarId", 0L, envir = CF)
  assign("currentDimId", 0L, envir = CF)
  assign("newGroupId", function() {CF$currentGroupId <- CF$currentGroupId - 1L; CF$currentGroupId}, envir = CF)
  assign("newVarId", function() {CF$currentVarId <- CF$currentVarId - 1L; CF$currentVarId}, envir = CF)
  assign("newDimId", function() {CF$currentDimId <- CF$currentDimId - 1L; CF$currentDimId}, envir = CF)

  # User-modifiable options
  assign("memory_cell_limit", 1e8, envir = CF.options)
  assign("digits", 6L, envir = CF.options)
  assign("cache_stale_days", 90, envir = CF.options)
}
#nocov end
