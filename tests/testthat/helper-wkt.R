# Is `wkt` a WKT2 string that GDAL/PROJ accept? Uses package terra.
.valid_wkt <- function(wkt) {
  testthat::skip_if_not_installed("terra")
  r <- tryCatch(terra::rast(nrows = 1L, ncols = 1L, crs = wkt),
                error = function(e) NULL, warning = function(w) NULL)
  !is.null(r) && nzchar(terra::crs(r))
}

# Make a CFGridMapping in memory with grid mapping name `gmn` and attributes
# `atts`, a named list of character or numeric values.
.grid_mapping <- function(gmn, atts = list(), name = "crs") {
  gm <- CFGridMapping$new(name, CFGroup$new("", NULL), gmn)
  for (a in names(atts))
    gm$set_attribute(a, if (is.character(atts[[a]])) "NC_CHAR" else "NC_DOUBLE", atts[[a]])
  gm
}
