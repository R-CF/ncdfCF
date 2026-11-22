# Tests for the translation of CF missing data attributes to the GeoZarr
# encoding, `.cf_missing_encoding()`, and for aggregating CF variables into a
# GeoZarr array. Package `geozarr` is a suggested package, so the tests in this
# file are skipped when it is not installed. It imports `zarr`.
skip_if_not_installed("geozarr")

# A stand-in for an `NCVariable`: only `vtype`, `is_packed` and `attribute()` are
# used. Absent attributes return `NA`, like the real thing.
mock_nc <- function(vtype, atts = list()) {
  list(vtype = vtype,
       is_packed = !is.null(atts$scale_factor) || !is.null(atts$add_offset),
       attribute = function(name, field = "value") atts[[name]] %||% NA)
}

# Write a small netCDF file with one variable "v" with attributes `atts`, a
# named list of list(type, value).
make_nc <- function(vtype, atts = list(), data = 1:3) {
  fn <- tempfile(fileext = ".nc")
  nc <- RNetCDF::create.nc(fn, format = "netcdf4")
  RNetCDF::dim.def.nc(nc, "x", 3L)
  RNetCDF::var.def.nc(nc, "v", vtype, "x")
  for (a in names(atts))
    RNetCDF::att.put.nc(nc, "v", a, atts[[a]]$type, atts[[a]]$value)
  RNetCDF::var.put.nc(nc, "v", data)
  RNetCDF::close.nc(nc)
  fn
}

# --- missing_value --------------------------------------------------------

test_that("no attributes gives no missing value and no valid range", {
  res <- .cf_missing_encoding(mock_nc("NC_DOUBLE"))
  expect_null(res$missing_value)
  expect_null(res$valid_range)
})

test_that("a non-default _FillValue becomes the missing value", {
  expect_equal(.cf_missing_encoding(mock_nc("NC_FLOAT", list(`_FillValue` = 1e20)))$missing_value, 1e20)
  expect_equal(.cf_missing_encoding(mock_nc("NC_SHORT", list(`_FillValue` = -9999L)))$missing_value, -9999L)
})

test_that("a _FillValue equal to the library default is not a missing value", {
  expect_null(.cf_missing_encoding(mock_nc("NC_DOUBLE", list(`_FillValue` = 9.9692099683868690e+36)))$missing_value)
  expect_null(.cf_missing_encoding(mock_nc("NC_FLOAT", list(`_FillValue` = 9.9692099683868690e+36)))$missing_value)
  expect_null(.cf_missing_encoding(mock_nc("NC_SHORT", list(`_FillValue` = -32767L)))$missing_value)
  expect_null(.cf_missing_encoding(mock_nc("NC_UBYTE", list(`_FillValue` = 255L)))$missing_value)
})

test_that("the library default is type-specific", {
  # The NC_SHORT default is not the default of NC_INT
  expect_equal(.cf_missing_encoding(mock_nc("NC_INT", list(`_FillValue` = -32767L)))$missing_value, -32767L)
})

test_that("the missing_value attribute takes precedence over _FillValue", {
  res <- .cf_missing_encoding(mock_nc("NC_FLOAT", list(`_FillValue` = 1e20, missing_value = -999)))
  expect_equal(res$missing_value, -999)
})

test_that("missing_value is used when _FillValue is the library default", {
  res <- .cf_missing_encoding(mock_nc("NC_SHORT", list(`_FillValue` = -32767L, missing_value = -1L)))
  expect_equal(res$missing_value, -1L)
})

test_that("a vector missing_value collapses to its first element", {
  res <- .cf_missing_encoding(mock_nc("NC_FLOAT", list(missing_value = c(-999, -888))))
  expect_equal(res$missing_value, -999)
})

test_that("packed variables have no missing value", {
  res <- .cf_missing_encoding(mock_nc("NC_SHORT", list(`_FillValue` = -9999L, missing_value = -1L,
                                                       scale_factor = 0.01, add_offset = 300)))
  expect_null(res$missing_value)
  res <- .cf_missing_encoding(mock_nc("NC_SHORT", list(`_FillValue` = -9999L, add_offset = 300)))
  expect_null(res$missing_value)
})

test_that("character types have no missing value or valid range", {
  for (vt in c("NC_CHAR", "NC_STRING")) {
    res <- .cf_missing_encoding(mock_nc(vt, list(`_FillValue` = "x", valid_range = c(0, 1))))
    expect_null(res$missing_value)
    expect_null(res$valid_range)
  }
})

# --- valid_range ----------------------------------------------------------

test_that("valid_range is passed through as a numeric vector", {
  res <- .cf_missing_encoding(mock_nc("NC_SHORT", list(valid_range = c(0L, 1000L))))
  expect_equal(res$valid_range, c(0, 1000))
  expect_true(is.numeric(res$valid_range))
})

test_that("valid_min and valid_max combine into a valid range", {
  res <- .cf_missing_encoding(mock_nc("NC_FLOAT", list(valid_min = -5, valid_max = 5)))
  expect_equal(res$valid_range, c(-5, 5))
})

test_that("a single open bound is replaced by the largest finite double", {
  expect_equal(.cf_missing_encoding(mock_nc("NC_FLOAT", list(valid_min = 0)))$valid_range,
               c(0, .Machine$double.xmax))
  expect_equal(.cf_missing_encoding(mock_nc("NC_FLOAT", list(valid_max = 10)))$valid_range,
               c(-.Machine$double.xmax, 10))
  expect_true(all(is.finite(.cf_missing_encoding(mock_nc("NC_FLOAT", list(valid_max = 10)))$valid_range)))
})

test_that("valid_range takes precedence over valid_min and valid_max", {
  res <- .cf_missing_encoding(mock_nc("NC_FLOAT", list(valid_range = c(1, 2), valid_min = -5, valid_max = 5)))
  expect_equal(res$valid_range, c(1, 2))
})

test_that("a packed valid_range is unpacked", {
  res <- .cf_missing_encoding(mock_nc("NC_SHORT", list(valid_range = c(0L, 1000L),
                                                       scale_factor = 0.01, add_offset = 300)))
  expect_equal(res$valid_range, c(300, 310))
})

test_that("a packed valid_range with a negative scale_factor is re-sorted", {
  res <- .cf_missing_encoding(mock_nc("NC_SHORT", list(valid_range = c(0L, 1000L),
                                                       scale_factor = -0.01, add_offset = 300)))
  expect_equal(res$valid_range, c(290, 300))
})

test_that("packing with only add_offset or only scale_factor uses defaults for the other", {
  expect_equal(.cf_missing_encoding(mock_nc("NC_SHORT", list(valid_range = c(0L, 10L), add_offset = 5)))$valid_range,
               c(5, 15))
  expect_equal(.cf_missing_encoding(mock_nc("NC_SHORT", list(valid_range = c(0L, 10L), scale_factor = 2)))$valid_range,
               c(0, 20))
})

test_that("a packed valid_min/valid_max pair is unpacked", {
  res <- .cf_missing_encoding(mock_nc("NC_SHORT", list(valid_min = 0L, valid_max = 100L,
                                                       scale_factor = 0.5)))
  expect_equal(res$valid_range, c(0, 50))
})

# --- against real NCVariables ---------------------------------------------

test_that("a real NCVariable with a non-default _FillValue", {
  skip_if_not_installed("RNetCDF")
  fn <- make_nc("NC_SHORT", list(`_FillValue` = list(type = "NC_SHORT", value = -9999L),
                                 valid_range = list(type = "NC_SHORT", value = c(0L, 100L))))
  on.exit(unlink(fn))

  ds <- open_ncdf(fn)
  nc <- ds[[ds$var_names[1L]]]$NC
  res <- .cf_missing_encoding(nc)
  expect_equal(res$missing_value, -9999)
  expect_equal(res$valid_range, c(0, 100))
})

test_that("a real NCVariable with default fill gives no missing value", {
  skip_if_not_installed("RNetCDF")
  fn <- make_nc("NC_DOUBLE", data = c(1, 2, 3))
  on.exit(unlink(fn))

  ds <- open_ncdf(fn)
  res <- .cf_missing_encoding(ds[[ds$var_names[1L]]]$NC)
  expect_null(res$missing_value)
  expect_null(res$valid_range)
})

test_that("a real packed NCVariable", {
  skip_if_not_installed("RNetCDF")
  fn <- make_nc("NC_SHORT", list(`_FillValue` = list(type = "NC_SHORT", value = -9999L),
                                 scale_factor = list(type = "NC_DOUBLE", value = -0.01),
                                 add_offset = list(type = "NC_DOUBLE", value = 300),
                                 valid_range = list(type = "NC_SHORT", value = c(0L, 1000L))))
  on.exit(unlink(fn))

  ds <- open_ncdf(fn)
  res <- .cf_missing_encoding(ds[[ds$var_names[1L]]]$NC)
  expect_null(res$missing_value)
  expect_equal(res$valid_range, c(290, 300))
})

# --- copy_to_geozarr() ----------------------------------------------------

# A 4 x 3 variable "v" on lon and lat axes. `fill` is an optional _FillValue
# for the variable; when `NA`, no attribute is set. `valid_range` is an optional
# valid range for the variable. Cell [1, 1] is written as
# `NA`, which RNetCDF stores as the _FillValue or, without one, the library
# default.
make_grid_nc <- function(fill = NA, valid_range = NULL) {
  fn <- tempfile(fileext = ".nc")
  nc <- RNetCDF::create.nc(fn, format = "netcdf4")
  RNetCDF::dim.def.nc(nc, "lon", 4L)
  RNetCDF::dim.def.nc(nc, "lat", 3L)

  RNetCDF::var.def.nc(nc, "lon", "NC_DOUBLE", "lon")
  RNetCDF::att.put.nc(nc, "lon", "units", "NC_CHAR", "degrees_east")
  RNetCDF::att.put.nc(nc, "lon", "standard_name", "NC_CHAR", "longitude")
  RNetCDF::att.put.nc(nc, "lon", "axis", "NC_CHAR", "X")
  RNetCDF::var.put.nc(nc, "lon", 0:3)

  RNetCDF::var.def.nc(nc, "lat", "NC_DOUBLE", "lat")
  RNetCDF::att.put.nc(nc, "lat", "units", "NC_CHAR", "degrees_north")
  RNetCDF::att.put.nc(nc, "lat", "standard_name", "NC_CHAR", "latitude")
  RNetCDF::att.put.nc(nc, "lat", "axis", "NC_CHAR", "Y")
  RNetCDF::var.put.nc(nc, "lat", 0:2)

  RNetCDF::var.def.nc(nc, "v", "NC_FLOAT", c("lon", "lat"))
  if (!is.na(fill))
    RNetCDF::att.put.nc(nc, "v", "_FillValue", "NC_FLOAT", fill)
  if (!is.null(valid_range))
    RNetCDF::att.put.nc(nc, "v", "valid_range", "NC_FLOAT", valid_range)
  data <- matrix(as.numeric(1:12), nrow = 4L, ncol = 3L)
  data[1L, 1L] <- NA_real_
  RNetCDF::var.put.nc(nc, "v", data)
  RNetCDF::close.nc(nc)
  fn
}

# A GeoZarr array matching the axes of `make_grid_nc()`.
make_grid_gza <- function() {
  grp <- zarr::create_zarr()$add_group("/", "g")
  geozarr::create_geozarr_array("v", grp, data_type = "float32",
    axes = list(lon = list(coordinates = 0:3, abbreviation = "X", unit = "degrees"),
                lat = list(coordinates = 0:2, abbreviation = "Y", unit = "degrees")))
}

test_that("copy_to_geozarr() stores missing cells with the missing value", {
  skip_if_not_installed("RNetCDF")
  skip_if_not_installed("geozarr")
  fn <- make_grid_nc(fill = -9999)
  on.exit(unlink(fn))

  ds <- open_ncdf(fn)
  v <- ds[[ds$var_names[1L]]]
  gza <- make_grid_gza()
  copy_to_geozarr(v, gza)

  # The Zarr fill value stays the default; the missing cell holds the sentinel
  expect_false(isTRUE(all.equal(gza$data_type$fill_value, -9999)))
  vals <- gza[]
  expect_equal(vals[1L, 1L], -9999)
  expect_equal(as.vector(vals)[-1L], as.numeric(2:12))
})

test_that("copy_to_geozarr() leaves missing cells as the fill value without a missing value", {
  skip_if_not_installed("RNetCDF")
  skip_if_not_installed("geozarr")
  fn <- make_grid_nc()
  on.exit(unlink(fn))

  ds <- open_ncdf(fn)
  v <- ds[[ds$var_names[1L]]]
  gza <- make_grid_gza()
  copy_to_geozarr(v, gza)

  # The library default fill value of the file is the Zarr default fill value,
  # which reads back as NA
  vals <- gza[]
  expect_true(is.na(vals[1L, 1L]))
  expect_equal(as.vector(vals)[-1L], as.numeric(2:12))
})

test_that("copy_to_geozarr() writes valid cells unchanged and returns the variable", {
  skip_if_not_installed("RNetCDF")
  skip_if_not_installed("geozarr")
  fn <- make_grid_nc(fill = -9999)
  on.exit(unlink(fn))

  ds <- open_ncdf(fn)
  v <- ds[[ds$var_names[1L]]]
  gza <- make_grid_gza()
  expect_identical(copy_to_geozarr(v, gza), v)
  expect_equal(gza[][2:4, 3L], c(10, 11, 12))
})

test_that("copy_to_geozarr() records the missing value in the array attributes", {
  skip_if_not_installed("RNetCDF")
  skip_if_not_installed("geozarr")
  fn <- make_grid_nc(fill = -9999)
  on.exit(unlink(fn))

  ds <- open_ncdf(fn)
  gza <- make_grid_gza()
  copy_to_geozarr(ds[[ds$var_names[1L]]], gza)
  expect_equal(gza$attribute("missing_value"), -9999)
  expect_null(gza$attribute("valid_range"))
})

test_that("copy_to_geozarr() sets no missing value attribute without a missing value", {
  skip_if_not_installed("RNetCDF")
  skip_if_not_installed("geozarr")
  fn <- make_grid_nc()
  on.exit(unlink(fn))

  ds <- open_ncdf(fn)
  gza <- make_grid_gza()
  copy_to_geozarr(ds[[ds$var_names[1L]]], gza)
  expect_null(gza$attribute("missing_value"))
  expect_null(gza$attribute("valid_range"))
})

test_that("copy_to_geozarr() records the valid range and stores out-of-range cells as missing", {
  skip_if_not_installed("RNetCDF")
  skip_if_not_installed("geozarr")
  fn <- make_grid_nc(fill = -9999, valid_range = c(0, 5))
  on.exit(unlink(fn))

  ds <- open_ncdf(fn)
  gza <- make_grid_gza()
  copy_to_geozarr(ds[[ds$var_names[1L]]], gza)
  expect_equal(unlist(gza$attribute("valid_range")), c(0, 5))
  # RNetCDF masks cells outside the valid range on read, so they are stored as
  # the missing value, as is the cell that was NA in the file
  expect_equal(as.vector(gza[]), c(-9999, 2, 3, 4, 5, rep(-9999, 7L)))
})

# --- write_geozarr() ------------------------------------------------------

test_that("write_geozarr() keeps the Zarr default fill value and stores the missing value", {
  skip_if_not_installed("RNetCDF")
  skip_if_not_installed("geozarr")
  fn <- make_grid_nc(fill = -9999, valid_range = c(0, 100))
  on.exit(unlink(fn))

  ds <- open_ncdf(fn)
  grp <- zarr::create_zarr()$add_group("/", "g")
  ds[[ds$var_names[1L]]]$write_geozarr(grp)
  arr <- grp$children[["v"]]

  expect_true(arr$data_type$fill_value > 1e36)
  expect_equal(arr$attribute("missing_value"), -9999)
  expect_equal(unlist(arr$attribute("valid_range")), c(0, 100))
  expect_null(arr$attribute("valid_min"))
  expect_equal(arr[][1L, 1L], -9999)
  expect_equal(as.vector(arr[])[-1L], as.numeric(2:12))
})

test_that("write_geozarr() sets no missing value attributes without a missing value", {
  skip_if_not_installed("RNetCDF")
  skip_if_not_installed("geozarr")
  fn <- make_grid_nc()
  on.exit(unlink(fn))

  ds <- open_ncdf(fn)
  grp <- zarr::create_zarr()$add_group("/", "g")
  ds[[ds$var_names[1L]]]$write_geozarr(grp)
  arr <- grp$children[["v"]]

  expect_null(arr$attribute("missing_value"))
  expect_null(arr$attribute("valid_range"))
  expect_true(is.na(arr[][1L, 1L]))
})

# --- Persistence ----------------------------------------------------------

test_that("copy_to_geozarr() attributes and data survive re-opening the store", {
  skip_if_not_installed("RNetCDF")
  skip_if_not_installed("geozarr")
  fn <- make_grid_nc(fill = -9999, valid_range = c(0, 100))
  zfn <- tempfile(fileext = ".zarr")
  on.exit(unlink(c(fn, zfn), recursive = TRUE))

  ds <- open_ncdf(fn)
  grp <- zarr::create_zarr(zfn)$add_group("/", "g")
  gza <- geozarr::create_geozarr_array("v", grp, data_type = "float32",
    axes = list(lon = list(coordinates = 0:3, abbreviation = "X", unit = "degrees"),
                lat = list(coordinates = 0:2, abbreviation = "Y", unit = "degrees")))
  copy_to_geozarr(ds[[ds$var_names[1L]]], gza)

  arr <- zarr::open_zarr(zfn)[["/g/v"]]
  expect_equal(arr$attribute("missing_value"), -9999)
  expect_equal(unlist(arr$attribute("valid_range")), c(0, 100))
  expect_equal(arr[][1L, 1L], -9999)
  expect_equal(as.vector(arr[])[-1L], as.numeric(2:12))
})

# --- .aggregate_axes() ----------------------------------------------------

# A variable "v" on lon (2), lat and time axes, `times` in days since 2000-01-01.
# With `bounds` the time axis has bounds [t, t + 1]. The data variable is of type
# `vtype` and has attributes `atts`, a named list of list(type, value). With
# `crs`, a named list of attribute values, the variable has a grid mapping.
make_time_nc <- function(times, lat = 0:2, bounds = FALSE, vtype = "NC_FLOAT", atts = list(), crs = NULL, lon = 0:1) {
  fn <- tempfile(fileext = ".nc")
  nc <- RNetCDF::create.nc(fn, format = "netcdf4")
  RNetCDF::dim.def.nc(nc, "lon", length(lon))
  RNetCDF::dim.def.nc(nc, "lat", length(lat))
  RNetCDF::dim.def.nc(nc, "time", length(times))

  RNetCDF::var.def.nc(nc, "lon", "NC_DOUBLE", "lon")
  RNetCDF::att.put.nc(nc, "lon", "units", "NC_CHAR", "degrees_east")
  RNetCDF::att.put.nc(nc, "lon", "standard_name", "NC_CHAR", "longitude")
  RNetCDF::att.put.nc(nc, "lon", "axis", "NC_CHAR", "X")
  RNetCDF::var.put.nc(nc, "lon", lon)

  RNetCDF::var.def.nc(nc, "lat", "NC_DOUBLE", "lat")
  RNetCDF::att.put.nc(nc, "lat", "units", "NC_CHAR", "degrees_north")
  RNetCDF::att.put.nc(nc, "lat", "standard_name", "NC_CHAR", "latitude")
  RNetCDF::att.put.nc(nc, "lat", "axis", "NC_CHAR", "Y")
  RNetCDF::var.put.nc(nc, "lat", lat)

  RNetCDF::var.def.nc(nc, "time", "NC_DOUBLE", "time")
  RNetCDF::att.put.nc(nc, "time", "units", "NC_CHAR", "days since 2000-01-01")
  RNetCDF::att.put.nc(nc, "time", "standard_name", "NC_CHAR", "time")
  RNetCDF::att.put.nc(nc, "time", "axis", "NC_CHAR", "T")
  RNetCDF::att.put.nc(nc, "time", "calendar", "NC_CHAR", "standard")
  if (bounds) {
    RNetCDF::dim.def.nc(nc, "nv", 2L)
    RNetCDF::att.put.nc(nc, "time", "bounds", "NC_CHAR", "time_bnds")
    RNetCDF::var.def.nc(nc, "time_bnds", "NC_DOUBLE", c("nv", "time"))
    RNetCDF::var.put.nc(nc, "time_bnds", rbind(times, times + 1))
  }
  RNetCDF::var.put.nc(nc, "time", times)

  RNetCDF::var.def.nc(nc, "v", vtype, c("lon", "lat", "time"))
  for (a in names(atts))
    RNetCDF::att.put.nc(nc, "v", a, atts[[a]]$type, atts[[a]]$value)
  if (!is.null(crs)) {
    RNetCDF::var.def.nc(nc, "crs", "NC_INT", NA)
    for (a in names(crs))
      RNetCDF::att.put.nc(nc, "crs", a, if (is.character(crs[[a]])) "NC_CHAR" else "NC_DOUBLE", crs[[a]])
    RNetCDF::att.put.nc(nc, "v", "grid_mapping", "NC_CHAR", "crs")
    RNetCDF::var.put.nc(nc, "crs", 0L)
  }
  RNetCDF::var.put.nc(nc, "v", array(0, dim = c(length(lon), length(lat), length(times))))
  RNetCDF::close.nc(nc)
  fn
}

# Open the data variable of each file. The datasets are returned too so that
# they stay alive.
open_vars <- function(fns) {
  dss <- lapply(fns, open_ncdf)
  list(ds = dss, vars = lapply(dss, function(ds) ds[[ds$var_names[1L]]]))
}

test_that(".union_axis() sorts and drops duplicates", {
  expect_equal(.union_axis(list(c(5, 6, 7), c(1, 2, 3)), list(NULL, NULL), FALSE)$values, c(1, 2, 3, 5, 6, 7))
  expect_equal(.union_axis(list(1:3, 3:5), list(NULL, NULL), FALSE)$values, 1:5)
  expect_equal(.union_axis(list(c(1, 2), c(2 + 1e-9, 3)), list(NULL, NULL), FALSE)$values, c(1, 2, 3))
})

test_that(".union_axis() keeps the direction of decreasing axes", {
  expect_equal(.union_axis(list(c(6, 5), c(9, 8, 7)), list(NULL, NULL), FALSE)$values, c(9, 8, 7, 6, 5))
})

test_that(".union_axis() fails on mixed directions", {
  expect_error(.union_axis(list(1:3, 3:1), list(NULL, NULL), FALSE, "x"), "increasing.*decreasing")
})

test_that(".union_axis() keeps character coordinates in order of appearance", {
  expect_equal(.union_axis(list(c("b", "a"), c("a", "c")), list(NULL, NULL), TRUE)$values, c("b", "a", "c"))
})

test_that(".union_axis() orders boundary values with the coordinates", {
  b1 <- rbind(c(0.5, 1.5), c(1.5, 2.5))
  b2 <- rbind(c(2.5, 3.5), c(3.5, 4.5))
  res <- .union_axis(list(c(3, 4), c(1, 2)), list(b2, b1), FALSE)
  expect_equal(res$boundaries, rbind(c(0.5, 1.5, 2.5, 3.5), c(1.5, 2.5, 3.5, 4.5)))
})

test_that(".union_axis() has no boundary values unless all axes have them", {
  b1 <- rbind(c(0.5, 1.5), c(1.5, 2.5))
  expect_null(.union_axis(list(c(1, 2), c(3, 4)), list(b1, NULL), FALSE)$boundaries)
})

test_that(".contiguous_in() tests for a contiguous subset", {
  expect_true(.contiguous_in(c(3, 4), c(1, 2, 3, 4, 5), FALSE))
  expect_false(.contiguous_in(c(3, 5), c(1, 2, 3, 4, 5), FALSE))
  expect_false(.contiguous_in(c(4, 5, 6), c(1, 2, 3, 4, 5), FALSE))
  expect_false(.contiguous_in(7, c(1, 2, 3, 4, 5), FALSE))
  expect_true(.contiguous_in(c("b", "c"), c("a", "b", "c"), TRUE))
  expect_false(.contiguous_in(c("a", "c"), c("a", "b", "c"), TRUE))
  expect_false(.contiguous_in("z", c("a", "b", "c"), TRUE))
})

test_that(".aggregate_axes() extends the time axis over files in any order", {
  skip_if_not_installed("RNetCDF")
  fns <- c(make_time_nc(6:8), make_time_nc(0:2), make_time_nc(3:5))
  on.exit(unlink(fns))

  ov <- open_vars(fns)
  res <- .aggregate_axes(ov$vars, "time")
  expect_named(res, c("lon", "lat", "time"))
  expect_equal(res$time$coordinates, 0:8)
  expect_equal(res$time$abbreviation, "T")
  expect_equal(res$time$unit, "days")
  expect_equal(res$time$calendar, "standard")
  expect_equal(res$time$epoch, "2000-01-01")
  expect_null(res$time$boundaries)
})

test_that(".aggregate_axes() takes shared axes from the first variable", {
  skip_if_not_installed("RNetCDF")
  fns <- c(make_time_nc(0:2), make_time_nc(3:5))
  on.exit(unlink(fns))

  res <- .aggregate_axes(open_vars(fns)$vars, "time")
  expect_equal(res$lon$coordinates, 0:1)
  expect_equal(res$lon$abbreviation, "X")
  expect_equal(res$lon$unit, "degree")
  expect_equal(res$lat$coordinates, 0:2)
  expect_equal(res$lat$abbreviation, "Y")
})

test_that(".aggregate_axes() works with a single variable", {
  skip_if_not_installed("RNetCDF")
  fn <- make_time_nc(0:2)
  on.exit(unlink(fn))

  res <- .aggregate_axes(open_vars(fn)$vars, "time")
  expect_equal(res$time$coordinates, 0:2)
})

test_that(".aggregate_axes() does not fail on overlapping coordinates", {
  skip_if_not_installed("RNetCDF")
  fns <- c(make_time_nc(0:3), make_time_nc(2:5))
  on.exit(unlink(fns))

  expect_no_error(res <- .aggregate_axes(open_vars(fns)$vars, "time"))
  expect_equal(res$time$coordinates, 0:5)
})

test_that(".aggregate_axes() allows gaps between variables", {
  skip_if_not_installed("RNetCDF")
  fns <- c(make_time_nc(0:2), make_time_nc(5:7))
  on.exit(unlink(fns))

  res <- .aggregate_axes(open_vars(fns)$vars, "time")
  expect_equal(res$time$coordinates, c(0, 1, 2, 5, 6, 7))
})

test_that(".aggregate_axes() fails when coordinates interleave", {
  skip_if_not_installed("RNetCDF")
  fns <- c(make_time_nc(c(0, 2, 4)), make_time_nc(c(1, 3, 5)))
  on.exit(unlink(fns))

  expect_error(.aggregate_axes(open_vars(fns)$vars, "time"), "contiguous subset")
})

test_that(".aggregate_axes() fails when an axis outside `along` differs", {
  skip_if_not_installed("RNetCDF")
  fns <- c(make_time_nc(0:2), make_time_nc(3:5, lat = c(0.5, 1.5, 2.5)))
  on.exit(unlink(fns))

  expect_error(.aggregate_axes(open_vars(fns)$vars, "time"), "'lat'.*not in `along`")
})

test_that(".aggregate_axes() fails when an axis in `along` interleaves and another differs", {
  skip_if_not_installed("RNetCDF")
  fns <- c(make_time_nc(0:2), make_time_nc(3:5, lat = c(0.5, 1.5, 2.5)))
  on.exit(unlink(fns))

  # lat is combined but the coordinates of the variables interleave
  expect_error(.aggregate_axes(open_vars(fns)$vars, "lat"), "'lat'.*contiguous subset")
})

test_that(".aggregate_axes() fails on bad arguments", {
  skip_if_not_installed("RNetCDF")
  fns <- c(make_time_nc(0:2), make_grid_nc())
  on.exit(unlink(fns))

  expect_error(.aggregate_axes(list(), "time"), "list of `CFVariable`")
  expect_error(.aggregate_axes("a", "time"), "list of `CFVariable`")
  ov <- open_vars(fns)
  expect_error(.aggregate_axes(ov$vars, "time"), "same axes")
  expect_error(.aggregate_axes(ov$vars[1L], "depth"), "`along`")
  expect_error(.aggregate_axes(ov$vars[1L], 1L), "`along`")
})

test_that(".aggregate_axes() combines boundary values", {
  skip_if_not_installed("RNetCDF")
  fns <- c(make_time_nc(3:5, bounds = TRUE), make_time_nc(0:2, bounds = TRUE))
  on.exit(unlink(fns))

  res <- .aggregate_axes(open_vars(fns)$vars, "time")
  expect_equal(res$time$boundaries, rbind(0:5, 1:6))
})

test_that(".aggregate_axes() drops boundary values that are not in all variables", {
  skip_if_not_installed("RNetCDF")
  fns <- c(make_time_nc(0:2, bounds = TRUE), make_time_nc(3:5))
  on.exit(unlink(fns))

  expect_warning(res <- .aggregate_axes(open_vars(fns)$vars, "time"), "boundary values")
  expect_null(res$time$boundaries)
})

test_that(".aggregate_axes() sets chunk weights for named axes only", {
  skip_if_not_installed("RNetCDF")
  fn <- make_time_nc(0:2)
  on.exit(unlink(fn))

  res <- .aggregate_axes(open_vars(fn)$vars, "time", chunking = c(time = 2))
  expect_equal(res$time$chunk_weight, 2)
  expect_null(res$lon$chunk_weight)
})

# --- Scalar axes ----------------------------------------------------------

# As `make_time_nc()` but with a scalar "height" axis, a scalar coordinate
# variable listed in the "coordinates" attribute of the data variable. All
# data values are `value`.
make_scalar_nc <- function(height, value = height, times = 0:2, positive = "up", units = "m", ...) {
  fn <- make_time_nc(times, ...)
  nc <- RNetCDF::open.nc(fn, write = TRUE)
  RNetCDF::var.def.nc(nc, "height", "NC_DOUBLE", NA)
  RNetCDF::att.put.nc(nc, "height", "units", "NC_CHAR", units)
  RNetCDF::att.put.nc(nc, "height", "standard_name", "NC_CHAR", "height")
  RNetCDF::att.put.nc(nc, "height", "axis", "NC_CHAR", "Z")
  if (!is.null(positive))
    RNetCDF::att.put.nc(nc, "height", "positive", "NC_CHAR", positive)
  RNetCDF::att.put.nc(nc, "v", "coordinates", "NC_CHAR", "height")
  RNetCDF::var.put.nc(nc, "height", height)
  RNetCDF::var.put.nc(nc, "v", array(value, dim = c(2L, 3L, length(times))))
  RNetCDF::close.nc(nc)
  fn
}

test_that(".aggregate_axes() makes a dimension from scalar axes in `along`", {
  skip_if_not_installed("RNetCDF")
  fns <- c(make_scalar_nc(10), make_scalar_nc(2))
  on.exit(unlink(fns))

  res <- .aggregate_axes(open_vars(fns)$vars, "height")
  expect_named(res, c("lon", "lat", "time", "height"))
  expect_equal(res$height$coordinates, c(2, 10))
  expect_equal(res$height$abbreviation, "Z")
  expect_equal(res$height$unit, "m")
  expect_equal(res$time$coordinates, 0:2)
})

test_that(".aggregate_axes() aggregates along a dimension and a scalar axis together", {
  skip_if_not_installed("RNetCDF")
  fns <- c(make_scalar_nc(2), make_scalar_nc(2, times = 3:5), make_scalar_nc(10))
  on.exit(unlink(fns))

  res <- .aggregate_axes(open_vars(fns)$vars, c("time", "height"))
  expect_named(res, c("lon", "lat", "time", "height"))
  expect_equal(res$time$coordinates, 0:5)
  expect_equal(res$height$coordinates, c(2, 10))
})

test_that(".aggregate_axes() ignores scalar axes that are not in `along`", {
  skip_if_not_installed("RNetCDF")
  fns <- c(make_scalar_nc(10), make_scalar_nc(2, times = 3:5))
  on.exit(unlink(fns))

  res <- .aggregate_axes(open_vars(fns)$vars, "time")
  expect_named(res, c("lon", "lat", "time"))
})

test_that(".aggregate_axes() fails when a scalar axis is not present in all variables", {
  skip_if_not_installed("RNetCDF")
  fns <- c(make_scalar_nc(10), make_time_nc(0:2))
  on.exit(unlink(fns))

  expect_error(.aggregate_axes(open_vars(fns)$vars, "height"), "`along`")
})

test_that("copy_to_geozarr() places variables along a new dimension from a scalar axis", {
  skip_if_not_installed("RNetCDF")
  skip_if_not_installed("geozarr")
  fns <- c(make_scalar_nc(10), make_scalar_nc(2))
  on.exit(unlink(fns))

  ov <- open_vars(fns)
  axes <- .aggregate_axes(ov$vars, "height")
  grp <- zarr::create_zarr()$add_group("/", "g")
  gza <- geozarr::create_geozarr_array("v", grp, axes, "float32")
  for (v in ov$vars)
    copy_to_geozarr(v, gza)

  vals <- gza[]
  expect_equal(dim(vals), c(2L, 3L, 3L, 2L))
  expect_true(all(vals[, , , 1L] == 2))
  expect_true(all(vals[, , , 2L] == 10))
})

test_that("copy_to_geozarr() fails on mismatched axes", {
  skip_if_not_installed("RNetCDF")
  skip_if_not_installed("geozarr")
  fn <- make_scalar_nc(10)
  on.exit(unlink(fn))

  v <- open_vars(fn)$vars[[1L]]
  # The GeoZarr array lacks the time dimension of the variable
  expect_error(copy_to_geozarr(v, make_grid_gza()), "dimensions of the data variable")

  # The GeoZarr array has an axis that the variable does not have
  grp <- zarr::create_zarr()$add_group("/", "g")
  gza <- geozarr::create_geozarr_array("v", grp, data_type = "float32",
    axes = list(lon = list(coordinates = 0:1, abbreviation = "X", unit = "degrees"),
                lat = list(coordinates = 0:2, abbreviation = "Y", unit = "degrees"),
                depth = list(coordinates = 1:2)))
  expect_error(copy_to_geozarr(v, gza), "does not have all the axes")
})

# --- .aggregate_array() ---------------------------------------------------

test_that(".aggregate_array() creates the array with the aggregated axes", {
  skip_if_not_installed("RNetCDF")
  skip_if_not_installed("geozarr")
  fns <- c(make_time_nc(3:5), make_time_nc(0:2))
  on.exit(unlink(fns))

  ov <- open_vars(fns)
  grp <- zarr::create_zarr()$add_group("/", "g")
  gza <- .aggregate_array(ov$vars, "time", grp)
  expect_s3_class(gza, "geozarr_array")
  expect_equal(gza$shape, c(2L, 3L, 6L))
  expect_equal(gza$data_type$data_type, "float32")
  expect_true(gza$data_type$fill_value > 1e36)
  expect_named(gza$coordinate_system$axes, c("lon", "lat", "time"))
})

test_that(".aggregate_array() uses the given name", {
  skip_if_not_installed("RNetCDF")
  skip_if_not_installed("geozarr")
  fn <- make_time_nc(0:2)
  on.exit(unlink(fn))

  grp <- zarr::create_zarr()$add_group("/", "g")
  .aggregate_array(open_vars(fn)$vars, "time", grp, name = "pr")
  expect_false(is.null(grp$children[["pr"]]))
})

test_that(".aggregate_array() maps the netCDF data type", {
  skip_if_not_installed("RNetCDF")
  skip_if_not_installed("geozarr")
  fn <- make_time_nc(0:2, vtype = "NC_DOUBLE")
  on.exit(unlink(fn))

  gza <- .aggregate_array(open_vars(fn)$vars, "time", zarr::create_zarr()$add_group("/", "g"))
  expect_equal(gza$data_type$data_type, "float64")
})

test_that(".aggregate_array() fails on different data types", {
  skip_if_not_installed("RNetCDF")
  skip_if_not_installed("geozarr")
  fns <- c(make_time_nc(0:2), make_time_nc(3:5, vtype = "NC_DOUBLE"))
  on.exit(unlink(fns))

  grp <- zarr::create_zarr()$add_group("/", "g")
  expect_error(.aggregate_array(open_vars(fns)$vars, "time", grp), "same data type")
})

test_that(".aggregate_array() sets identical attributes without warning", {
  skip_if_not_installed("RNetCDF")
  skip_if_not_installed("geozarr")
  atts <- list(long_name = list(type = "NC_CHAR", value = "Precipitation"))
  fns <- c(make_time_nc(0:2, atts = atts), make_time_nc(3:5, atts = atts))
  on.exit(unlink(fns))

  grp <- zarr::create_zarr()$add_group("/", "g")
  expect_no_warning(gza <- .aggregate_array(open_vars(fns)$vars, "time", grp))
  expect_equal(gza$attribute("long_name"), "Precipitation")
})

test_that(".aggregate_array() warns on differing attributes and uses the last", {
  skip_if_not_installed("RNetCDF")
  skip_if_not_installed("geozarr")
  fns <- c(make_time_nc(0:2, atts = list(long_name = list(type = "NC_CHAR", value = "first"))),
           make_time_nc(3:5, atts = list(long_name = list(type = "NC_CHAR", value = "last"))))
  on.exit(unlink(fns))

  grp <- zarr::create_zarr()$add_group("/", "g")
  expect_warning(gza <- .aggregate_array(open_vars(fns)$vars, "time", grp), "'long_name'")
  expect_equal(gza$attribute("long_name"), "last")
})

test_that(".aggregate_array() warns on attributes that are not in all variables", {
  skip_if_not_installed("RNetCDF")
  skip_if_not_installed("geozarr")
  fns <- c(make_time_nc(0:2, atts = list(comment = list(type = "NC_CHAR", value = "only first"))),
           make_time_nc(3:5))
  on.exit(unlink(fns))

  grp <- zarr::create_zarr()$add_group("/", "g")
  expect_warning(gza <- .aggregate_array(open_vars(fns)$vars, "time", grp), "'comment'")
  expect_equal(gza$attribute("comment"), "only first")
})

test_that(".aggregate_array() leaves out structural and derived attributes", {
  skip_if_not_installed("RNetCDF")
  skip_if_not_installed("geozarr")
  atts <- list(actual_range = list(type = "NC_FLOAT", value = c(0, 1)),
               valid_range = list(type = "NC_FLOAT", value = c(0, 10)),
               valid_min = list(type = "NC_FLOAT", value = 0),
               units = list(type = "NC_CHAR", value = "kg m-2 s-1"))
  fn <- make_time_nc(0:2, atts = atts)
  on.exit(unlink(fn))

  gza <- .aggregate_array(open_vars(fn)$vars, "time", zarr::create_zarr()$add_group("/", "g"))
  expect_null(gza$attribute("actual_range"))
  expect_null(gza$attribute("valid_range"))
  expect_null(gza$attribute("valid_min"))
  expect_equal(gza$attribute("units"), "kg m-2 s-1")
})

test_that(".aggregate_array() takes scalar axes along as new dimensions", {
  skip_if_not_installed("RNetCDF")
  skip_if_not_installed("geozarr")
  fns <- c(make_scalar_nc(10), make_scalar_nc(2))
  on.exit(unlink(fns))

  gza <- .aggregate_array(open_vars(fns)$vars, "height", zarr::create_zarr()$add_group("/", "g"))
  expect_equal(gza$shape, c(2L, 3L, 3L, 2L))
})

# --- Vertical axes --------------------------------------------------------

test_that(".aggregate_axes() sets the direction of a vertical axis from `positive`", {
  skip_if_not_installed("RNetCDF")
  fns <- c(make_scalar_nc(10, positive = "up"), make_scalar_nc(2, positive = "up"))
  on.exit(unlink(fns))
  expect_equal(.aggregate_axes(open_vars(fns)$vars, "height")$height$direction, "UP")

  fns2 <- c(make_scalar_nc(10, positive = "down"), make_scalar_nc(2, positive = "down"))
  on.exit(unlink(fns2), add = TRUE)
  expect_equal(.aggregate_axes(open_vars(fns2)$vars, "height")$height$direction, "DOWN")
})

test_that(".aggregate_axes() defaults the direction of a pressure axis to down without `positive`", {
  skip_if_not_installed("RNetCDF")
  skip_if_not_installed("units")
  fn <- make_scalar_nc(850, positive = NULL, units = "hPa")
  on.exit(unlink(fn))

  expect_equal(.aggregate_axes(open_vars(fn)$vars, "height")$height$direction, "DOWN")
})

test_that(".aggregate_axes() sets no direction for a metre axis without `positive`", {
  skip_if_not_installed("RNetCDF")
  fn <- make_scalar_nc(10, positive = NULL)
  on.exit(unlink(fn))

  # Not a vertical axis in ncdfCF: the direction is left to GeoZarr
  expect_null(.aggregate_axes(open_vars(fn)$vars, "height")$height$direction)
})

test_that(".aggregate_axes() fails when `positive` differs between variables", {
  skip_if_not_installed("RNetCDF")
  fns <- c(make_scalar_nc(10, positive = "up"), make_scalar_nc(2, positive = "down"))
  on.exit(unlink(fns))

  expect_error(.aggregate_axes(open_vars(fns)$vars, "height"), "'positive'")
})

test_that(".aggregate_axes() sets no direction for non-vertical axes", {
  skip_if_not_installed("RNetCDF")
  fn <- make_time_nc(0:2)
  on.exit(unlink(fn))

  res <- .aggregate_axes(open_vars(fn)$vars, "time")
  expect_null(res$lon$direction)
  expect_null(res$time$direction)
})

# --- CRS ------------------------------------------------------------------

wgs84 <- list(grid_mapping_name = "latitude_longitude", semi_major_axis = 6378137,
              inverse_flattening = 298.257223563)

test_that(".aggregate_crs() is NULL when no variable has a CRS", {
  skip_if_not_installed("RNetCDF")
  fns <- c(make_time_nc(0:2), make_time_nc(3:5))
  on.exit(unlink(fns))

  expect_null(.aggregate_crs(open_vars(fns)$vars))
})

test_that(".aggregate_crs() returns the CRS when it is the same in all variables", {
  skip_if_not_installed("RNetCDF")
  fns <- c(make_time_nc(0:2, crs = wgs84), make_time_nc(3:5, crs = wgs84))
  on.exit(unlink(fns))

  crs <- .aggregate_crs(open_vars(fns)$vars)
  expect_s3_class(crs, "CFGridMapping")
})

test_that(".aggregate_crs() fails when only some variables have a CRS", {
  skip_if_not_installed("RNetCDF")
  fns <- c(make_time_nc(0:2, crs = wgs84), make_time_nc(3:5))
  on.exit(unlink(fns))

  expect_error(.aggregate_crs(open_vars(fns)$vars), "same CRS")
})

test_that(".aggregate_crs() fails when the CRS differs", {
  skip_if_not_installed("RNetCDF")
  other <- wgs84
  other$semi_major_axis <- 6371000
  fns <- c(make_time_nc(0:2, crs = wgs84), make_time_nc(3:5, crs = other))
  on.exit(unlink(fns))

  expect_error(.aggregate_crs(open_vars(fns)$vars), "same CRS")
})

test_that(".aggregate_array() fails when the CRS differs", {
  skip_if_not_installed("RNetCDF")
  skip_if_not_installed("geozarr")
  other <- wgs84
  other$semi_major_axis <- 6371000
  fns <- c(make_time_nc(0:2, crs = wgs84), make_time_nc(3:5, crs = other))
  on.exit(unlink(fns))

  grp <- zarr::create_zarr()$add_group("/", "g")
  expect_error(.aggregate_array(open_vars(fns)$vars, "time", grp), "same CRS")
})

# --- CRS and auxiliary labels in .aggregate_array() -------------------------

# Add a label set to the lat axis (and optionally time) of a file made by
# make_time_nc(). Labels are NC_CHAR with a string-length dimension.
add_labels <- function(fn, name, labels, dim = "lat") {
  nc <- RNetCDF::open.nc(fn, write = TRUE)
  on.exit(RNetCDF::close.nc(nc))
  RNetCDF::dim.def.nc(nc, paste0(name, "_len"), max(nchar(labels)))
  RNetCDF::var.def.nc(nc, name, "NC_CHAR", c(paste0(name, "_len"), dim))
  RNetCDF::att.put.nc(nc, name, "long_name", "NC_CHAR", "Label set")
  RNetCDF::var.put.nc(nc, name, labels)
  old <- tryCatch(RNetCDF::att.get.nc(nc, "v", "coordinates"), error = function(e) NULL)
  RNetCDF::att.put.nc(nc, "v", "coordinates", "NC_CHAR", paste(c(old, name), collapse = " "))
  fn
}

test_that(".aggregate_array() sets the CRS on the array", {
  skip_if_not_installed("RNetCDF")
  skip_if_not_installed("geozarr")
  fns <- c(make_time_nc(0:2, crs = wgs84), make_time_nc(3:5, crs = wgs84))
  on.exit(unlink(fns))

  grp <- zarr::create_zarr()$add_group("/", "g")
  gza <- .aggregate_array(open_vars(fns)$vars, "time", grp)
  expect_false(is.null(gza$attributes$cs) && is.null(gza$attributes[["proj:code"]]) &&
               is.null(gza$attributes[["proj:wkt2"]]))
})

test_that(".aggregate_array() adds label sets shared by all data variables", {
  skip_if_not_installed("RNetCDF")
  skip_if_not_installed("geozarr")
  fns <- c(make_time_nc(0:2), make_time_nc(3:5))
  on.exit(unlink(fns))
  for (f in fns) add_labels(f, "site", c("a", "b", "c"))

  grp <- zarr::create_zarr()$add_group("/", "g")
  gza <- .aggregate_array(open_vars(fns)$vars, "time", grp)
  ax <- gza$coordinate_system$axes[["lat"]]
  expect_true("site" %in% vapply(ax$coordinates_list, function(crd) crd$name, character(1L)))
})

test_that(".aggregate_labels() assembles labels along the aggregated axis", {
  skip_if_not_installed("RNetCDF")
  skip_if_not_installed("geozarr")
  fns <- c(make_time_nc(0:2, lat = 0:2), make_time_nc(0:2, lat = 3:5))
  on.exit(unlink(fns))
  add_labels(fns[1L], "site", c("a", "b", "c"))
  add_labels(fns[2L], "site", c("d", "e", "f"))

  vars <- open_vars(fns)$vars
  axes <- .aggregate_axes(vars, "lat")
  lab <- .aggregate_labels(vars, "lat", axes)
  expect_equal(names(lab), "lat")
  expect_equal(lab$lat$site$values, letters[1:6])
})

test_that(".aggregate_labels() follows the order of the aggregated axis", {
  skip_if_not_installed("RNetCDF")
  skip_if_not_installed("geozarr")
  fns <- c(make_time_nc(0:2, lat = 3:5), make_time_nc(0:2, lat = 0:2))
  on.exit(unlink(fns))
  add_labels(fns[1L], "site", c("d", "e", "f"))
  add_labels(fns[2L], "site", c("a", "b", "c"))

  vars <- open_vars(fns)$vars
  lab <- .aggregate_labels(vars, "lat", .aggregate_axes(vars, "lat"))
  expect_equal(lab$lat$site$values, letters[1:6])
})

test_that(".aggregate_labels() fails when labels differ on a shared axis", {
  skip_if_not_installed("RNetCDF")
  skip_if_not_installed("geozarr")
  fns <- c(make_time_nc(0:2), make_time_nc(3:5))
  on.exit(unlink(fns))
  add_labels(fns[1L], "site", c("a", "b", "c"))
  add_labels(fns[2L], "site", c("a", "b", "x"))

  vars <- open_vars(fns)$vars
  expect_error(.aggregate_labels(vars, "time", .aggregate_axes(vars, "time")), "not identical")
})

test_that(".aggregate_labels() drops label sets that are not in all variables", {
  skip_if_not_installed("RNetCDF")
  skip_if_not_installed("geozarr")
  fns <- c(make_time_nc(0:2), make_time_nc(3:5))
  on.exit(unlink(fns))
  add_labels(fns[1L], "site", c("a", "b", "c"))

  vars <- open_vars(fns)$vars
  expect_warning(lab <- .aggregate_labels(vars, "time", .aggregate_axes(vars, "time")), "not in all data variables")
  expect_length(lab, 0L)
})

test_that(".aggregate_labels() is empty without label sets", {
  skip_if_not_installed("RNetCDF")
  skip_if_not_installed("geozarr")
  fns <- c(make_time_nc(0:2), make_time_nc(3:5))
  on.exit(unlink(fns))
  vars <- open_vars(fns)$vars
  expect_length(.aggregate_labels(vars, "time", .aggregate_axes(vars, "time")), 0L)
})

# --- aggregate_geozarr() ---------------------------------------------------

test_that("aggregate_geozarr() aggregates files along the time axis", {
  skip_if_not_installed("RNetCDF")
  skip_if_not_installed("geozarr")
  fns <- c(make_time_nc(3:5), make_time_nc(0:2))
  on.exit(unlink(fns))
  for (i in 1:2) {
    nc <- RNetCDF::open.nc(fns[i], write = TRUE)
    RNetCDF::var.put.nc(nc, "v", array(c(3, 0)[i] + rep(0:2, each = 6), dim = c(2L, 3L, 3L)))
    RNetCDF::close.nc(nc)
  }

  z <- zarr::create_zarr()
  gza <- aggregate_geozarr(fns, z$add_group("/", "g"), verbose = FALSE)
  expect_s3_class(gza, "geozarr_array")
  expect_equal(gza$shape, c(2L, 3L, 6L))
  expect_equal(unname(gza[1L, 1L, ]), as.numeric(0:5))
})

test_that("aggregate_geozarr() accepts a zarr object and a list of CFVariables", {
  skip_if_not_installed("RNetCDF")
  skip_if_not_installed("geozarr")
  fns <- c(make_time_nc(0:2), make_time_nc(3:5))
  on.exit(unlink(fns))

  z <- zarr::create_zarr()
  z$add_group("/", "g")
  gza <- aggregate_geozarr(open_vars(fns)$vars, z, name = "pr", along = "time", verbose = FALSE)
  expect_equal(gza$name, "pr")
  expect_equal(gza$shape, c(2L, 3L, 6L))
})

test_that("aggregate_geozarr() fails on wrong arguments", {
  skip_if_not_installed("RNetCDF")
  skip_if_not_installed("geozarr")
  fns <- make_time_nc(0:2)
  on.exit(unlink(fns))
  grp <- zarr::create_zarr()$add_group("/", "g")

  expect_error(aggregate_geozarr(fns, "nope", verbose = FALSE), "location")
  expect_error(aggregate_geozarr(1:3, grp, verbose = FALSE), "file names")
  expect_error(aggregate_geozarr(character(), grp, verbose = FALSE), "at least one")
  expect_error(aggregate_geozarr(fns, grp, var = "nope", verbose = FALSE), "data variable")
})

# --- Auxiliary longitude-latitude grids ------------------------------------

# A file with a curvilinear grid: 2-D lon and lat over dimensions x and y,
# which are bare dimensions without coordinate variables.
make_ll_nc <- function(times = 0:2, lon_shift = 0, grid = TRUE) {
  fn <- tempfile(fileext = ".nc")
  nc <- RNetCDF::create.nc(fn, format = "netcdf4")
  RNetCDF::dim.def.nc(nc, "x", 3L)
  RNetCDF::dim.def.nc(nc, "y", 2L)
  RNetCDF::dim.def.nc(nc, "time", length(times))

  RNetCDF::var.def.nc(nc, "time", "NC_DOUBLE", "time")
  RNetCDF::att.put.nc(nc, "time", "units", "NC_CHAR", "days since 2000-01-01")
  RNetCDF::att.put.nc(nc, "time", "standard_name", "NC_CHAR", "time")
  RNetCDF::att.put.nc(nc, "time", "axis", "NC_CHAR", "T")
  RNetCDF::att.put.nc(nc, "time", "calendar", "NC_CHAR", "standard")
  RNetCDF::var.put.nc(nc, "time", times)

  RNetCDF::var.def.nc(nc, "v", "NC_FLOAT", c("x", "y", "time"))
  if (grid) {
    RNetCDF::var.def.nc(nc, "lon", "NC_DOUBLE", c("x", "y"))
    RNetCDF::att.put.nc(nc, "lon", "units", "NC_CHAR", "degrees_east")
    RNetCDF::att.put.nc(nc, "lon", "standard_name", "NC_CHAR", "longitude")
    RNetCDF::var.put.nc(nc, "lon", matrix(10 + lon_shift + 0:5 / 10, 3L, 2L))
    RNetCDF::var.def.nc(nc, "lat", "NC_DOUBLE", c("x", "y"))
    RNetCDF::att.put.nc(nc, "lat", "units", "NC_CHAR", "degrees_north")
    RNetCDF::att.put.nc(nc, "lat", "standard_name", "NC_CHAR", "latitude")
    RNetCDF::var.put.nc(nc, "lat", matrix(40 + 0:5 / 10, 3L, 2L))
    RNetCDF::att.put.nc(nc, "v", "coordinates", "NC_CHAR", "lon lat")
  }
  RNetCDF::var.put.nc(nc, "v", array(0, dim = c(3L, 2L, length(times))))
  RNetCDF::close.nc(nc)
  fn
}

test_that(".aggregate_geolocation() is NULL without auxiliary grids", {
  skip_if_not_installed("RNetCDF")
  skip_if_not_installed("geozarr")
  fns <- c(make_time_nc(0:2), make_time_nc(3:5))
  on.exit(unlink(fns))
  vars <- open_vars(fns)$vars
  expect_null(.aggregate_geolocation(vars, "time", .aggregate_axes(vars, "time")))
})

test_that(".aggregate_geolocation() compiles the grid and orients its axes", {
  skip_if_not_installed("RNetCDF")
  skip_if_not_installed("geozarr")
  fns <- c(make_ll_nc(0:2), make_ll_nc(3:5))
  on.exit(unlink(fns))
  vars <- open_vars(fns)$vars
  geo <- .aggregate_geolocation(vars, "time", .aggregate_axes(vars, "time"))
  expect_equal(geo$args$dims, c("x", "y"))
  expect_equal(geo$args$names, c("lon", "lat"))
  expect_equal(dim(geo$args$x), c(3L, 2L))
  expect_equal(geo$axes$x$abbreviation, "X")
  expect_equal(geo$axes$y$abbreviation, "Y")
})

test_that(".aggregate_geolocation() does not allow the X and Y axes in along", {
  skip_if_not_installed("RNetCDF")
  skip_if_not_installed("geozarr")
  fns <- c(make_ll_nc(0:2), make_ll_nc(3:5))
  on.exit(unlink(fns))
  vars <- open_vars(fns)$vars
  axes <- .aggregate_axes(vars, "time")
  expect_error(.aggregate_geolocation(vars, c("time", "x"), axes), "cannot be in `along`")
  expect_error(.aggregate_geolocation(vars, "y", axes), "cannot be in `along`")
})

test_that(".aggregate_geolocation() fails when only some variables have a grid", {
  skip_if_not_installed("RNetCDF")
  skip_if_not_installed("geozarr")
  fns <- c(make_ll_nc(0:2), make_ll_nc(3:5, grid = FALSE))
  on.exit(unlink(fns))
  vars <- open_vars(fns)$vars
  expect_error(.aggregate_geolocation(vars, "time", .aggregate_axes(vars, "time")), "Not all data variables")
})

test_that(".aggregate_geolocation() fails when the grids differ", {
  skip_if_not_installed("RNetCDF")
  skip_if_not_installed("geozarr")
  fns <- c(make_ll_nc(0:2), make_ll_nc(3:5, lon_shift = 1))
  on.exit(unlink(fns))
  vars <- open_vars(fns)$vars
  expect_error(.aggregate_geolocation(vars, "time", .aggregate_axes(vars, "time")), "different values")
})

test_that(".aggregate_array() sets the auxiliary grid as geolocation", {
  skip_if_not_installed("RNetCDF")
  skip_if_not_installed("geozarr")
  fns <- c(make_ll_nc(0:2), make_ll_nc(3:5))
  on.exit(unlink(fns))

  grp <- zarr::create_zarr()$add_group("/", "g")
  gza <- .aggregate_array(open_vars(fns)$vars, "time", grp)
  expect_equal(gza$shape, c(3L, 2L, 6L))
  geo <- gza$geolocation()
  expect_equal(geo$x$name, "lon")
  expect_equal(geo$y$name, "lat")
  expect_equal(geo$x[], matrix(10 + 0:5 / 10, 3L, 2L))
  expect_equal(geo$y[], matrix(40 + 0:5 / 10, 3L, 2L))
})

test_that("aggregate_geozarr() works with auxiliary grids", {
  skip_if_not_installed("RNetCDF")
  skip_if_not_installed("geozarr")
  fns <- c(make_ll_nc(3:5), make_ll_nc(0:2))
  on.exit(unlink(fns))

  gza <- aggregate_geozarr(fns, zarr::create_zarr()$add_group("/", "g"), verbose = FALSE)
  expect_equal(gza$shape, c(3L, 2L, 6L))
  expect_false(is.null(gza$geolocation()))
  # The bare dimension is a discrete axis, which is rejected before the grid check
  expect_error(aggregate_geozarr(fns, zarr::create_zarr()$add_group("/", "g"), along = "x", verbose = FALSE),
               "discrete axis")
})

# --- Label contents on the aggregated array --------------------------------

test_that(".aggregate_array() stores the label values on a shared axis", {
  skip_if_not_installed("RNetCDF")
  skip_if_not_installed("geozarr")
  fns <- c(make_time_nc(0:2), make_time_nc(3:5))
  on.exit(unlink(fns))
  for (f in fns) add_labels(f, "site", c("a", "b", "c"))

  gza <- .aggregate_array(open_vars(fns)$vars, "time", zarr::create_zarr()$add_group("/", "g"))
  lst <- gza$coordinate_system$axes$lat$coordinates_list
  expect_length(lst, 2L)
  expect_identical(lst[[2L]]$name, "site")
  expect_identical(lst[[2L]]$values, c("a", "b", "c"))
})

test_that(".aggregate_array() stores the label values on an aggregated axis in order", {
  skip_if_not_installed("RNetCDF")
  skip_if_not_installed("geozarr")
  fns <- c(make_time_nc(0:2, lat = 3:5), make_time_nc(0:2, lat = 0:2))
  on.exit(unlink(fns))
  add_labels(fns[1L], "site", c("d", "e", "f"))
  add_labels(fns[2L], "site", c("a", "b", "c"))

  gza <- .aggregate_array(open_vars(fns)$vars, "lat", zarr::create_zarr()$add_group("/", "g"))
  lst <- gza$coordinate_system$axes$lat$coordinates_list
  expect_identical(lst[[2L]]$values, letters[1:6])
})

test_that(".aggregate_array() persists the label values in the store", {
  skip_if_not_installed("RNetCDF")
  skip_if_not_installed("geozarr")
  fns <- c(make_time_nc(0:2), make_time_nc(3:5))
  zfn <- tempfile(fileext = ".zarr")
  on.exit(unlink(c(fns, zfn), recursive = TRUE))
  for (f in fns) add_labels(f, "site", c("a", "b", "c"))

  .aggregate_array(open_vars(fns)$vars, "time", zarr::create_zarr(zfn)$add_group("/", "g"))
  re <- zarr::open_zarr(zfn)[["/g/v"]]
  expect_identical(re$coordinate_system$axes$lat$coordinates_list[[2L]]$values, c("a", "b", "c"))
})

test_that("aggregation along an X axis works without an auxiliary grid", {
  skip_if_not_installed("RNetCDF")
  skip_if_not_installed("geozarr")
  fns <- c(make_time_nc(0:2, lon = 0:1), make_time_nc(0:2, lon = 2:3))
  on.exit(unlink(fns))

  vars <- open_vars(fns)$vars
  expect_null(.aggregate_geolocation(vars, "lon", .aggregate_axes(vars, "lon")))
  gza <- .aggregate_array(vars, "lon", zarr::create_zarr()$add_group("/", "g"))
  expect_equal(gza$shape, c(4L, 3L, 3L))
})

test_that("aggregation along a Y axis works without an auxiliary grid", {
  skip_if_not_installed("RNetCDF")
  skip_if_not_installed("geozarr")
  fns <- c(make_time_nc(0:2, lat = 0:2), make_time_nc(0:2, lat = 3:5))
  on.exit(unlink(fns))

  gza <- aggregate_geozarr(fns, zarr::create_zarr()$add_group("/", "g"), along = "lat", verbose = FALSE)
  expect_equal(gza$shape, c(2L, 6L, 3L))
})

test_that("write_geozarr() writes an auxiliary grid on bare dimensions", {
  skip_if_not_installed("RNetCDF")
  skip_if_not_installed("geozarr")
  fn <- make_ll_nc(0:2)
  on.exit(unlink(fn))
  var <- open_vars(fn)$vars[[1L]]

  grp <- zarr::create_zarr()$add_group("/", "g")
  expect_no_error(var$write_geozarr(grp))
  arr <- grp$children[["v"]]
  expect_equal(arr$geolocation()$x[], matrix(10 + 0:5 / 10, 3L, 2L))
  expect_equal(arr$geolocation()$y[], matrix(40 + 0:5 / 10, 3L, 2L))
})

test_that(".llgrid_dims() finds the axes of the grid by dimension id", {
  skip_if_not_installed("RNetCDF")
  fn <- make_ll_nc(0:2)
  on.exit(unlink(fn))
  expect_equal(.llgrid_dims(open_vars(fn)$vars[[1L]]), c("x", "y"))
})

# --- Attributes of axes and global attributes -------------------------------

# Set a character attribute on variable `var` of a file; `var` is "NC_GLOBAL"
# for a global attribute.
set_nc_att <- function(fn, var, name, value) {
  nc <- RNetCDF::open.nc(fn, write = TRUE)
  on.exit(RNetCDF::close.nc(nc))
  RNetCDF::att.put.nc(nc, var, name, "NC_CHAR", value)
  fn
}

test_that(".merge_attributes() merges with the last value winning", {
  expect_warning(m <- .merge_attributes(list(list(a = 1, b = 2), list(a = 3, b = 2)), "Attributes of x"),
                 "Attributes of x differ.*'a'")
  expect_equal(m, list(a = 3, b = 2))
  expect_warning(.merge_attributes(list(list(a = 1), list(b = 1)), "Attributes of x"), "'a', 'b'")
  expect_no_warning(m <- .merge_attributes(list(list(a = 1), list(a = 1)), "Attributes of x"))
  expect_equal(m, list(a = 1))
})

test_that(".aggregate_axes() carries the axis attributes", {
  skip_if_not_installed("RNetCDF")
  fns <- c(make_time_nc(0:2), make_time_nc(3:5))
  on.exit(unlink(fns))
  vars <- open_vars(fns)$vars
  axes <- .aggregate_axes(vars, "time")
  expect_equal(axes$lat$attributes$standard_name, "latitude")
  expect_equal(axes$time$attributes$standard_name, "time")
  # Structural attributes are left out
  expect_null(axes$lat$attributes$axis)
  expect_null(axes$lat$attributes$units)
  expect_null(axes$time$attributes$axis)
})

test_that(".aggregate_axes() warns when axis attributes differ", {
  skip_if_not_installed("RNetCDF")
  fns <- c(make_time_nc(0:2), make_time_nc(3:5))
  on.exit(unlink(fns))
  set_nc_att(fns[1L], "lat", "long_name", "Latitude")
  set_nc_att(fns[2L], "lat", "long_name", "Lat")
  vars <- open_vars(fns)$vars
  expect_warning(axes <- .aggregate_axes(vars, "time"), "Attributes of axis 'lat' differ.*'long_name'")
  expect_equal(axes$lat$attributes$long_name, "Lat")
})

test_that(".aggregate_array() sets the axis attributes on the coordinates", {
  skip_if_not_installed("RNetCDF")
  skip_if_not_installed("geozarr")
  fns <- c(make_time_nc(0:2), make_time_nc(3:5))
  on.exit(unlink(fns))
  set_nc_att(fns[1L], "lat", "long_name", "Latitude")
  set_nc_att(fns[2L], "lat", "long_name", "Latitude")

  gza <- .aggregate_array(open_vars(fns)$vars, "time", zarr::create_zarr()$add_group("/", "g"))
  lat <- gza$coordinate_system$axes$lat$coordinates
  expect_equal(lat$attributes$long_name, "Latitude")
  expect_equal(lat$attributes$standard_name, "latitude")
})

test_that(".aggregate_array() sets identical global attributes on the location", {
  skip_if_not_installed("RNetCDF")
  skip_if_not_installed("geozarr")
  fns <- c(make_time_nc(0:2), make_time_nc(3:5))
  on.exit(unlink(fns))
  for (f in fns) set_nc_att(f, "NC_GLOBAL", "title", "A data set")

  grp <- zarr::create_zarr()$add_group("/", "g")
  .aggregate_array(open_vars(fns)$vars, "time", grp)
  expect_equal(grp$metadata$attributes$title, "A data set")
})

test_that(".aggregate_array() leaves out global attributes that differ", {
  skip_if_not_installed("RNetCDF")
  skip_if_not_installed("geozarr")
  fns <- c(make_time_nc(0:2), make_time_nc(3:5))
  on.exit(unlink(fns))
  for (f in fns) set_nc_att(f, "NC_GLOBAL", "title", "A data set")
  set_nc_att(fns[1L], "NC_GLOBAL", "tracking_id", "abc")
  set_nc_att(fns[2L], "NC_GLOBAL", "tracking_id", "def")
  set_nc_att(fns[1L], "NC_GLOBAL", "only_one", "x")

  grp <- zarr::create_zarr()$add_group("/", "g")
  expect_no_warning(.aggregate_array(open_vars(fns)$vars, "time", grp))
  atts <- grp$metadata$attributes
  expect_equal(atts$title, "A data set")
  expect_null(atts$tracking_id)
  expect_null(atts$only_one)
})

test_that(".aggregate_global_attributes() returns the names that were set", {
  skip_if_not_installed("RNetCDF")
  skip_if_not_installed("geozarr")
  fns <- c(make_time_nc(0:2), make_time_nc(3:5))
  on.exit(unlink(fns))
  for (f in fns) set_nc_att(f, "NC_GLOBAL", "title", "A data set")

  grp <- zarr::create_zarr()$add_group("/", "g")
  expect_equal(.aggregate_global_attributes(open_vars(fns)$vars, grp, "noname"), c("title", "history"))
})

test_that(".aggregate_global_attributes() always sets a history entry for the aggregation", {
  skip_if_not_installed("RNetCDF")
  skip_if_not_installed("geozarr")
  fns <- c(make_time_nc(0:2), make_time_nc(3:5))
  on.exit(unlink(fns))

  grp <- zarr::create_zarr()$add_group("/", "g")
  .aggregate_global_attributes(open_vars(fns)$vars, grp, "noname")
  h <- grp$metadata$attributes$history
  expect_type(h, "list")
  expect_length(h, 1L)
  expect_match(h[[1L]], "^\\d{4}-\\d{2}-\\d{2}T\\d{2}:\\d{2}:\\d{2}Z ncdfCF [0-9.]+: Aggregated data variables \"v\" from 2 netCDF files into Zarr array \"noname\"$")
})

test_that(".aggregate_global_attributes() splits an identical history once", {
  skip_if_not_installed("RNetCDF")
  skip_if_not_installed("geozarr")
  fns <- c(make_time_nc(0:2), make_time_nc(3:5))
  on.exit(unlink(fns))
  for (f in fns) set_nc_att(f, "NC_GLOBAL", "history", "2021-01-01T00:00:00Z b;\n2020-01-01T00:00:00Z a")

  grp <- zarr::create_zarr()$add_group("/", "g")
  .aggregate_global_attributes(open_vars(fns)$vars, grp, "noname")
  expect_equal(grp$metadata$attributes$history[-1L],
               list("2021-01-01T00:00:00Z b", "2020-01-01T00:00:00Z a"))
})

test_that(".aggregate_global_attributes() keeps differing histories together per source", {
  skip_if_not_installed("RNetCDF")
  skip_if_not_installed("geozarr")
  fns <- c(make_time_nc(0:2), make_time_nc(3:5))
  on.exit(unlink(fns))
  set_nc_att(fns[1L], "NC_GLOBAL", "history", "2021-01-01T00:00:00Z b1;\n2020-01-01T00:00:00Z a1")
  set_nc_att(fns[2L], "NC_GLOBAL", "history", "2021-06-01T00:00:00Z b2;\n2020-06-01T00:00:00Z a2")

  grp <- zarr::create_zarr()$add_group("/", "g")
  .aggregate_global_attributes(open_vars(fns)$vars, grp, "noname")
  expect_equal(grp$metadata$attributes$history[-1L],
               list("2021-01-01T00:00:00Z b1", "2020-01-01T00:00:00Z a1",
                    "2021-06-01T00:00:00Z b2", "2020-06-01T00:00:00Z a2"))
})

test_that(".aggregate_global_attributes() uses the history of the files that have one", {
  skip_if_not_installed("RNetCDF")
  skip_if_not_installed("geozarr")
  fns <- c(make_time_nc(0:2), make_time_nc(3:5))
  on.exit(unlink(fns))
  set_nc_att(fns[2L], "NC_GLOBAL", "history", "Created by hand")

  grp <- zarr::create_zarr()$add_group("/", "g")
  .aggregate_global_attributes(open_vars(fns)$vars, grp, "noname")
  expect_equal(grp$metadata$attributes$history[-1L], list("Created by hand"))
})

test_that("a history with a single entry is a JSON array in the store", {
  skip_if_not_installed("RNetCDF")
  skip_if_not_installed("geozarr")
  fns <- c(make_time_nc(0:2), make_time_nc(3:5))
  zfn <- tempfile(fileext = ".zarr")
  on.exit(unlink(c(fns, zfn), recursive = TRUE))

  aggregate_geozarr(fns, zarr::create_zarr(zfn)$add_group("/", "g"), verbose = FALSE)
  json <- paste(readLines(file.path(zfn, "g", "zarr.json")), collapse = "\n")
  expect_match(json, "\"history\": \\[")
})

test_that("global attributes persist in the store", {
  skip_if_not_installed("RNetCDF")
  skip_if_not_installed("geozarr")
  fns <- c(make_time_nc(0:2), make_time_nc(3:5))
  zfn <- tempfile(fileext = ".zarr")
  on.exit(unlink(c(fns, zfn), recursive = TRUE))
  for (f in fns) set_nc_att(f, "NC_GLOBAL", "title", "A data set")

  aggregate_geozarr(fns, zarr::create_zarr(zfn)$add_group("/", "g"), verbose = FALSE)
  expect_equal(zarr::open_zarr(zfn)[["/g"]]$metadata$attributes$title, "A data set")
})

test_that("axis attributes leave out actual_range, which differs per file", {
  skip_if_not_installed("RNetCDF")
  fns <- c(make_time_nc(0:2), make_time_nc(3:5))
  on.exit(unlink(fns))
  vars <- open_vars(fns)$vars
  for (ax in vars[[1L]]$axes)
    expect_false("actual_range" %in% names(ax$geozarr_attributes()))
  expect_no_warning(.aggregate_axes(vars, "time"))
})

# --- Progress messages ------------------------------------------------------

test_that("copy_to_geozarr() prints progress per chunk when verbose", {
  skip_if_not_installed("RNetCDF")
  skip_if_not_installed("geozarr")
  fns <- c(make_time_nc(0:2), make_time_nc(3:5))
  on.exit(unlink(fns))
  vars <- open_vars(fns)$vars
  gza <- .aggregate_array(vars, "time", zarr::create_zarr()$add_group("/", "g"))

  expect_output(copy_to_geozarr(vars[[1L]], gza, verbose = TRUE), "Chunk 1 of \\d+ \\(0%\\)")
  expect_output(copy_to_geozarr(vars[[2L]], gza, verbose = TRUE), "\\(100%\\)")
})

test_that("copy_to_geozarr() is silent by default", {
  skip_if_not_installed("RNetCDF")
  skip_if_not_installed("geozarr")
  fns <- c(make_time_nc(0:2), make_time_nc(3:5))
  on.exit(unlink(fns))
  vars <- open_vars(fns)$vars
  gza <- .aggregate_array(vars, "time", zarr::create_zarr()$add_group("/", "g"))
  expect_silent(copy_to_geozarr(vars[[1L]], gza))
})

test_that("aggregate_geozarr() prints progress when verbose", {
  skip_if_not_installed("RNetCDF")
  skip_if_not_installed("geozarr")
  fns <- c(make_time_nc(0:2), make_time_nc(3:5))
  on.exit(unlink(fns))

  out <- capture.output(aggregate_geozarr(fns, zarr::create_zarr()$add_group("/", "g"), verbose = TRUE))
  out <- paste(out, collapse = "\n")
  expect_match(out, "Opening file 1 of 2")
  expect_match(out, "Opening file 2 of 2")
  expect_match(out, "Array 'v' with shape \\(2, 3, 6\\)")
  expect_match(out, "Copying data variable 2 of 2")
  expect_match(out, "Done")
})

test_that("aggregate_geozarr() is silent when not verbose", {
  skip_if_not_installed("RNetCDF")
  skip_if_not_installed("geozarr")
  fns <- c(make_time_nc(0:2), make_time_nc(3:5))
  on.exit(unlink(fns))
  expect_silent(aggregate_geozarr(fns, zarr::create_zarr()$add_group("/", "g"), verbose = FALSE))
})

# --- External coordinates group ---------------------------------------------

test_that(".external_group() resolves and creates the group", {
  grp <- zarr::create_zarr()$add_group("/", "g")
  expect_null(.external_group(grp, NULL))
  expect_null(.external_group(grp, ""))
  expect_equal(.external_group(grp, "coords"), "../coords")
  expect_equal(.external_group(grp, "/a/b/"), "../a/b")
  expect_false(is.null(grp$children$coords))
  expect_false(is.null(grp$children$a$children$b))
})

test_that(".external_group() validates the path", {
  grp <- zarr::create_zarr()$add_group("/", "g")
  expect_error(.external_group(grp, 1), "single character string")
  expect_error(.external_group(grp, c("a", "b")), "single character string")
})

test_that(".external_group() may go up the hierarchy", {
  z <- zarr::create_zarr()
  z$add_group("/", "my_data")
  grp <- z$add_group("/my_data", "variables")
  expect_equal(.external_group(grp, "../shared"), "../../shared")
  expect_false(is.null(z[["/my_data"]]$children$shared))
  expect_null(grp$children$shared)
})

test_that(".external_group() fails beyond the root group", {
  grp <- zarr::create_zarr()$add_group("/", "g")
  expect_error(.external_group(grp, "../../x"), "beyond the root")
})

test_that(".aggregate_array() writes external coordinates to the given group", {
  skip_if_not_installed("RNetCDF")
  skip_if_not_installed("geozarr")
  old <- geozarr::geozarr_options()$max_explicit
  geozarr::geozarr_options("max_explicit", 2L)
  on.exit(geozarr::geozarr_options("max_explicit", old), add = TRUE)
  fns <- c(make_time_nc(0:2, lat = c(0, 1, 3)), make_time_nc(3:5, lat = c(0, 1, 3)))
  on.exit(unlink(fns), add = TRUE)

  grp <- zarr::create_zarr()$add_group("/", "g")
  gza <- .aggregate_array(open_vars(fns)$vars, "time", grp, external = "coords")
  expect_false(is.null(grp$children$coords$children$lat))
  expect_null(grp$children$lat)
  expect_equal(grp$children$coords$children$lat[], c(0, 1, 3))
})

test_that(".aggregate_array() writes external coordinates next to the array by default", {
  skip_if_not_installed("RNetCDF")
  skip_if_not_installed("geozarr")
  old <- geozarr::geozarr_options()$max_explicit
  geozarr::geozarr_options("max_explicit", 2L)
  on.exit(geozarr::geozarr_options("max_explicit", old), add = TRUE)
  fns <- c(make_time_nc(0:2, lat = c(0, 1, 3)), make_time_nc(3:5, lat = c(0, 1, 3)))
  on.exit(unlink(fns), add = TRUE)

  grp <- zarr::create_zarr()$add_group("/", "g")
  .aggregate_array(open_vars(fns)$vars, "time", grp)
  expect_false(is.null(grp$children$lat))
})

test_that(".aggregate_array() writes label sets to the external group", {
  skip_if_not_installed("RNetCDF")
  skip_if_not_installed("geozarr")
  old <- geozarr::geozarr_options()$max_explicit
  geozarr::geozarr_options("max_explicit", 2L)
  on.exit(geozarr::geozarr_options("max_explicit", old), add = TRUE)
  fns <- c(make_time_nc(0:2), make_time_nc(3:5))
  on.exit(unlink(fns), add = TRUE)
  for (f in fns) add_labels(f, "site", c("a", "b", "c"))

  grp <- zarr::create_zarr()$add_group("/", "g")
  .aggregate_array(open_vars(fns)$vars, "time", grp, external = "coords")
  expect_false(is.null(grp$children$coords$children$site))
  expect_null(grp$children$site)
})

test_that(".aggregate_array() writes geolocation arrays to the external group", {
  skip_if_not_installed("RNetCDF")
  skip_if_not_installed("geozarr")
  fns <- c(make_ll_nc(0:2), make_ll_nc(3:5))
  on.exit(unlink(fns))

  grp <- zarr::create_zarr()$add_group("/", "g")
  gza <- .aggregate_array(open_vars(fns)$vars, "time", grp, external = "coords")
  expect_false(is.null(grp$children$coords$children$lon))
  expect_false(is.null(grp$children$coords$children$lat))
  expect_null(grp$children$lon)
  expect_equal(gza$geolocation()$x[], matrix(10 + 0:5 / 10, 3L, 2L))
})

test_that("aggregate_geozarr() passes on external_coordinates", {
  skip_if_not_installed("RNetCDF")
  skip_if_not_installed("geozarr")
  fns <- c(make_ll_nc(0:2), make_ll_nc(3:5))
  on.exit(unlink(fns))

  grp <- zarr::create_zarr()$add_group("/", "g")
  aggregate_geozarr(fns, grp, external_coordinates = "coords", verbose = FALSE)
  expect_false(is.null(grp$children$coords$children$lon))
})

test_that("aggregate_geozarr() puts external coordinates in a group next to location", {
  skip_if_not_installed("RNetCDF")
  skip_if_not_installed("geozarr")
  fns <- c(make_ll_nc(0:2), make_ll_nc(3:5))
  on.exit(unlink(fns))

  z <- zarr::create_zarr()
  z$add_group("/", "my_data")
  grp <- z$add_group("/my_data", "variables")
  gza <- aggregate_geozarr(fns, grp, external_coordinates = "../shared", verbose = FALSE)
  shared <- z[["/my_data"]]$children$shared
  expect_false(is.null(shared$children$lon))
  expect_null(grp$children$lon)
  expect_equal(gza$geolocation()$x[], matrix(10 + 0:5 / 10, 3L, 2L))
})

test_that("time axis attributes leave out the calendar and the structural ones", {
  skip_if_not_installed("RNetCDF")
  fn <- make_time_nc(0:2, bounds = TRUE)
  on.exit(unlink(fn))
  ax <- Filter(function(a) a$name == "time", open_vars(fn)$vars[[1L]]$axes)[[1L]]
  atts <- ax$geozarr_attributes()
  expect_false(any(c("axis", "bounds", "units", "calendar", "actual_range") %in% names(atts)))
  expect_equal(atts$standard_name, "time")
})

# --- Attribute "history" splitting --------------------------------------------

test_that("CMOR entries split on ';\\n' but not on inner semicolons", {
  x <- "2019-03-01T12:00:00Z ; CMOR rewrote data;\n2019-02-28T10:00:00Z altered by CMOR: a; b."
  expect_equal(.split_history(x),
               c("2019-03-01T12:00:00Z ; CMOR rewrote data",
                 "2019-02-28T10:00:00Z altered by CMOR: a; b."))
})

test_that("NCO/CDO ctime entries split on newline, keeping embedded semicolons", {
  x <- "Sun Nov 15 16:34:00 2026: cdo expr,'a=b;c=d' in.nc out.nc\nSat Nov  7 09:00:00 2026: ncks -O x.nc y.nc"
  expect_equal(.split_history(x),
               c("Sun Nov 15 16:34:00 2026: cdo expr,'a=b;c=d' in.nc out.nc",
                 "Sat Nov  7 09:00:00 2026: ncks -O x.nc y.nc"))
})

test_that("oldest-first histories are reversed", {
  x <- "2020-01-01T00:00:00Z a\n2021-01-01T00:00:00Z b"
  expect_equal(.split_history(x), c("2021-01-01T00:00:00Z b", "2020-01-01T00:00:00Z a"))
})

test_that("mixed stamp forms compare correctly", {
  x <- "Sun Nov 15 16:34:00 2026: cdo x\n2019-03-01T12:00:00Z ; CMOR"
  expect_equal(.split_history(x), c("Sun Nov 15 16:34:00 2026: cdo x", "2019-03-01T12:00:00Z ; CMOR"))
})

test_that("unstamped text stays a single string", {
  expect_equal(.split_history("line one\nline two"), "line one\nline two")
})

test_that("empty history gives an empty vector", {
  expect_equal(.split_history(""), character(0))
})

test_that("export entry starts with a UTC ISO 8601 stamp", {
  expect_match(.history_entry("Exported"),
               "^\\d{4}-\\d{2}-\\d{2}T\\d{2}:\\d{2}:\\d{2}Z ncdfCF [0-9.]+: Exported$")
})
