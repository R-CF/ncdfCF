# Helpers to write small netCDF files for testing how `open_ncdf()` builds axes.

# Write a netCDF-4 file. `dims` is a named integer vector of dimension lengths.
# `vars` is a list of variable definitions, each a list with elements `name`,
# `type`, `dims` (R order, may be NULL for a scalar variable), `atts` (named
# list of character attributes) and `data`. A variable name may be prefixed
# with a group, as in "g1/lat"; the group is created as needed. Returns the
# file name.
.write_nc <- function(dims, vars, gatts = list()) {
  fn <- tempfile(fileext = ".nc")
  nc <- RNetCDF::create.nc(fn, format = "netcdf4")
  on.exit(RNetCDF::close.nc(nc))
  for (d in names(dims)) RNetCDF::dim.def.nc(nc, d, dims[[d]])
  for (a in names(gatts)) RNetCDF::att.put.nc(nc, "NC_GLOBAL", a, "NC_CHAR", gatts[[a]])
  grps <- list()
  for (v in vars) {
    parts <- strsplit(v$name, "/", fixed = TRUE)[[1L]]
    h <- nc
    if (length(parts) == 2L) {
      if (is.null(grps[[parts[1L]]])) grps[[parts[1L]]] <- RNetCDF::grp.def.nc(nc, parts[1L])
      h <- grps[[parts[1L]]]
    }
    nm <- parts[length(parts)]
    RNetCDF::var.def.nc(h, nm, v$type, if (is.null(v$dims)) NA else v$dims)
    for (a in names(v$atts)) RNetCDF::att.put.nc(h, nm, a, "NC_CHAR", v$atts[[a]])
    if (!is.null(v$data)) RNetCDF::var.put.nc(h, nm, v$data)
  }
  fn
}

# A file with the structure of a glider trajectory in a discrete sampling
# geometry: a single `row` dimension without coordinate variable, string-length
# dimensions, auxiliary coordinates and ancillary variables.
.write_dsg <- function() {
  n <- 5L
  .write_nc(c(row = n, TRAJECTORY_strlen = 6L), list(
    list(name = "time", type = "NC_DOUBLE", dims = "row", data = as.numeric(1:n * 60),
         atts = list(units = "seconds since 1970-01-01", standard_name = "time", ancillary_variables = "TIME_QC")),
    list(name = "TIME_QC", type = "NC_BYTE", dims = "row", data = rep(1L, n)),
    list(name = "latitude", type = "NC_DOUBLE", dims = "row", data = seq(49, 49.4, length.out = n),
         atts = list(units = "degrees_north")),
    list(name = "longitude", type = "NC_DOUBLE", dims = "row", data = seq(-64, -63.6, length.out = n),
         atts = list(units = "degrees_east")),
    list(name = "TRAJECTORY", type = "NC_CHAR", dims = c("TRAJECTORY_strlen", "row"), data = rep("fundy1", n),
         atts = list(cf_role = "trajectory_id")),
    list(name = "PRES", type = "NC_DOUBLE", dims = "row", data = as.numeric(1:n),
         atts = list(units = "decibar", ancillary_variables = "PRES_QC", coordinates = "time latitude longitude")),
    list(name = "PRES_QC", type = "NC_BYTE", dims = "row", data = rep(1L, n))
  ), gatts = list(featureType = "trajectory", Conventions = "CF-1.13"))
}

# Names of the axes of a CF variable.
.axis_names <- function(v) vapply(v$axes, function(a) a$name, character(1L), USE.NAMES = FALSE)

# The messages of all warnings that evaluating `expr` gives.
.warnings_of <- function(expr) {
  w <- character()
  withCallingHandlers(expr, warning = function(cnd) {
    w <<- c(w, conditionMessage(cnd))
    invokeRestart("muffleWarning")
  })
  w
}
