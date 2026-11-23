# Building axes from dimension ids in `open_ncdf()`.

test_that("DSG: a dimension without coordinate variable becomes a discrete axis", {
  ds <- open_ncdf(.write_dsg())
  row <- ds[["row"]]
  expect_s3_class(row, "CFAxisDiscrete")
  expect_equal(row$length, 5L)
})

test_that("DSG: string-length dimensions do not become axes", {
  ds <- open_ncdf(.write_dsg())
  expect_false("TRAJECTORY_strlen" %in% ds$axis_names)
})

test_that("DSG: 1D data variables use the discrete axis", {
  ds <- open_ncdf(.write_dsg())
  expect_equal(.axis_names(ds[["PRES"]]), "row")
  expect_equal(.axis_names(ds[["PRES_QC"]]), "row")
})

test_that("DSG: an ancillary variable of an auxiliary coordinate gets the discrete axis", {
  ds <- open_ncdf(.write_dsg())
  qc <- ds[["TIME_QC"]]
  expect_s3_class(qc, "CFVariable")
  expect_equal(.axis_names(qc), "row")
  expect_equal(as.vector(qc$raw()), rep(1L, 5L))
})

test_that("DSG: a NC_CHAR data variable is read as strings on its domain axes", {
  ds <- open_ncdf(.write_dsg())
  tr <- ds[["TRAJECTORY"]]
  expect_equal(.axis_names(tr), "row")
  expect_equal(as.vector(tr$raw()), rep("fundy1", 5L))
})

test_that("A data variable on the dimension of a coordinate variable is not an axis", {
  # ERA5 case: `tp(valid_time)` is defined before `valid_time(valid_time)`
  fn <- .write_nc(c(valid_time = 3L), list(
    list(name = "tp", type = "NC_FLOAT", dims = "valid_time", data = c(1, 2, 3), atts = list(units = "m")),
    list(name = "valid_time", type = "NC_DOUBLE", dims = "valid_time", data = c(0, 1, 2),
         atts = list(units = "hours since 1970-01-01"))
  ))
  ds <- open_ncdf(fn)
  expect_equal(ds$axis_names, "valid_time")
  expect_equal(ds$var_names, "tp")
  expect_equal(.axis_names(ds[["tp"]]), "valid_time")
})

test_that("A variable named after a dimension it does not use is a data variable", {
  # ESA Sentinel-1 OCN case: `oswPartitions` is both a dimension and a variable
  fn <- .write_nc(c(x = 2L, oswPartitions = 1L), list(
    list(name = "oswPartitions", type = "NC_BYTE", dims = "x", data = 1:2),
    list(name = "oswHs", type = "NC_FLOAT", dims = c("x", "oswPartitions"), data = array(c(1, 2), c(2, 1)))
  ))
  ds <- open_ncdf(fn)
  expect_s3_class(ds[["oswPartitions"]], "CFAxisDiscrete")
  expect_true(all(c("oswPartitions", "oswHs") %in% ds$var_names))
  expect_equal(.axis_names(ds[["oswHs"]]), c("x", "oswPartitions"))
})

test_that("A 1D auxiliary coordinate on a discrete dimension is not promoted to axis", {
  # `member_id(member)` must not become the axis for dimension `member`
  fn <- .write_nc(c(member = 3L), list(
    list(name = "member_id", type = "NC_INT", dims = "member", data = 11:13),
    list(name = "tas", type = "NC_FLOAT", dims = "member", data = c(1, 2, 3),
         atts = list(units = "K", coordinates = "member_id"))
  ))
  ds <- open_ncdf(fn)
  expect_s3_class(ds[["member"]], "CFAxisDiscrete")
  expect_equal(.axis_names(ds[["tas"]]), "member")
})

test_that("The vertex dimension of a boundary variable does not become an axis", {
  fn <- .write_nc(c(time = 2L, bnds = 2L), list(
    list(name = "time", type = "NC_DOUBLE", dims = "time", data = c(0.5, 1.5),
         atts = list(units = "days since 2000-01-01", bounds = "time_bnds")),
    list(name = "time_bnds", type = "NC_DOUBLE", dims = c("bnds", "time"), data = array(c(0, 1, 1, 2), c(2, 2))),
    list(name = "pr", type = "NC_FLOAT", dims = "time", data = c(1, 2), atts = list(units = "mm"))
  ))
  ds <- open_ncdf(fn)
  expect_false("bnds" %in% ds$axis_names)
  expect_equal(.axis_names(ds[["pr"]]), "time")
})

test_that("An unmatched `bounds` reference gives a warning", {
  fn <- .write_nc(c(lat = 2L), list(
    list(name = "lat", type = "NC_DOUBLE", dims = "lat", data = c(10, 20),
         atts = list(units = "degrees_north", bounds = "no_such_variable")),
    list(name = "pr", type = "NC_FLOAT", dims = "lat", data = c(1, 2), atts = list(units = "mm"))
  ))
  expect_warning(open_ncdf(fn), "Unmatched `bounds` value 'no_such_variable'")
})

test_that("Coordinate variables in subgroups are found from subgroup dimensions", {
  fn <- tempfile(fileext = ".nc")
  nc <- RNetCDF::create.nc(fn, format = "netcdf4")
  g <- RNetCDF::grp.def.nc(nc, "g1")
  RNetCDF::dim.def.nc(g, "lat", 2L)
  RNetCDF::var.def.nc(g, "lat", "NC_DOUBLE", "lat")
  RNetCDF::att.put.nc(g, "lat", "units", "NC_CHAR", "degrees_north")
  RNetCDF::var.def.nc(g, "t", "NC_FLOAT", "lat")
  RNetCDF::var.put.nc(g, "lat", c(10, 20))
  RNetCDF::var.put.nc(g, "t", c(1, 2))
  RNetCDF::close.nc(nc)

  ds <- open_ncdf(fn)
  expect_s3_class(ds[["/g1/lat"]], "CFAxisLatitude")
  expect_equal(.axis_names(ds[["/g1/t"]]), "lat")
})

test_that("A dimension with coordinate variables in several groups uses the nearest", {
  fn <- .write_nc(c(lat = 2L), list(
    list(name = "g1/lat", type = "NC_DOUBLE", dims = "lat", data = c(10, 20), atts = list(units = "degrees_north")),
    list(name = "g1/t", type = "NC_FLOAT", dims = "lat", data = c(1, 2)),
    list(name = "g2/lat", type = "NC_DOUBLE", dims = "lat", data = c(30, 40), atts = list(units = "degrees_north")),
    list(name = "g2/t", type = "NC_FLOAT", dims = "lat", data = c(3, 4))
  ))
  ds <- open_ncdf(fn)
  expect_equal(ds[["/g1/t"]]$axes[[1L]]$values, c(10, 20))
  expect_equal(ds[["/g2/t"]]$axes[[1L]]$values, c(30, 40))
})

test_that("A coordinate variable in a sibling group is found by lateral search", {
  fn <- .write_nc(c(lat = 2L), list(
    list(name = "g1/lat", type = "NC_DOUBLE", dims = "lat", data = c(10, 20), atts = list(units = "degrees_north")),
    list(name = "g2/lat", type = "NC_DOUBLE", dims = "lat", data = c(30, 40), atts = list(units = "degrees_north")),
    list(name = "g3/t", type = "NC_FLOAT", dims = "lat", data = c(1, 2))
  ))
  ds <- open_ncdf(fn)
  expect_equal(ds[["/g3/t"]]$axes[[1L]]$values, c(10, 20))
})

test_that("A scalar ancillary variable has no axes", {
  fn <- .write_nc(c(x = 2L), list(
    list(name = "x", type = "NC_DOUBLE", dims = "x", data = c(1, 2)),
    list(name = "pr", type = "NC_FLOAT", dims = "x", data = c(1, 2), atts = list(ancillary_variables = "pr_err")),
    list(name = "pr_err", type = "NC_FLOAT", dims = NULL, data = 0.1)
  ))
  ds <- open_ncdf(fn)
  err <- ds[["pr_err"]]
  expect_s3_class(err, "CFVariable")
  expect_length(err$axes, 0L)
})

test_that("raw() of a 1D variable has the axis coordinates as names", {
  ds <- open_ncdf(.write_dsg())
  expect_equal(names(ds[["PRES"]]$raw()), as.character(1:5))
})

test_that("A NC_CHAR data variable can be subset from file", {
  fn <- .write_nc(c(station = 4L, name_strlen = 3L), list(
    list(name = "station_name", type = "NC_CHAR", dims = c("name_strlen", "station"),
         data = c("abc", "def", "ghi", "jkl"))
  ))
  ds <- open_ncdf(fn)
  nm <- ds[["station_name"]]
  expect_equal(as.vector(nm[2:3]), c("def", "ghi"))
  sub <- nm$subset(station = 3:4)
  expect_equal(.axis_names(sub), "station")
  expect_equal(as.vector(sub$raw()), c("ghi", "jkl"))
})

test_that("The dimid of an axis is NA when it has no dimension", {
  fn <- .write_nc(c(x = 2L), list(
    list(name = "x", type = "NC_DOUBLE", dims = "x", data = c(1, 2)),
    list(name = "height", type = "NC_DOUBLE", dims = NULL, data = 2, atts = list(units = "m")),
    list(name = "tas", type = "NC_FLOAT", dims = "x", data = c(1, 2), atts = list(coordinates = "height"))
  ))
  ds <- open_ncdf(fn)
  expect_identical(ds[["height"]]$dimid, NA_integer_)
  expect_false(is.na(ds[["x"]]$dimid))
})

test_that("A NC_CHAR label can be subset from file", {
  fn <- .write_nc(c(station = 4L, name_strlen = 3L), list(
    list(name = "station_name", type = "NC_CHAR", dims = c("name_strlen", "station"),
         data = c("abc", "def", "ghi", "jkl")),
    list(name = "tas", type = "NC_FLOAT", dims = "station", data = c(1, 2, 3, 4),
         atts = list(coordinates = "station_name"))
  ))
  ds <- open_ncdf(fn)
  lbl <- ds[["station_name"]]
  expect_s3_class(lbl, "CFLabel")
  expect_equal(lbl$length, 4L)
  sub <- lbl$subset(group = lbl$group, rng = c(2L, 3L))
  expect_equal(sub$values, c("def", "ghi"))
})

# A file with a chunked NC_CHAR variable of 10 strings of 3 characters
.write_chunked_char <- function() {
  fn <- tempfile(fileext = ".nc")
  nc <- RNetCDF::create.nc(fn, format = "netcdf4")
  RNetCDF::dim.def.nc(nc, "station", 10L)
  RNetCDF::dim.def.nc(nc, "name_strlen", 3L)
  RNetCDF::var.def.nc(nc, "station_name", "NC_CHAR", c("name_strlen", "station"),
                      chunking = TRUE, chunksizes = c(3L, 2L))
  RNetCDF::var.put.nc(nc, "station_name", sprintf("s%02d", 1:10))
  RNetCDF::close.nc(nc)
  fn
}

test_that("A NC_CHAR data variable larger than the memory limit is read in chunks", {
  old_limit <- CF.options$memory_cell_limit
  assign("memory_cell_limit", 9L, envir = CF.options)  # 3 strings
  on.exit(assign("memory_cell_limit", old_limit, envir = CF.options), add = TRUE)

  ds <- open_ncdf(.write_chunked_char())
  nm <- ds[["station_name"]]
  expect_warning(vals <- nm$raw(), "exceeding the memory limit")
  expect_equal(as.vector(vals), sprintf("s%02d", 1:10))
  expect_warning(win <- nm$read_window(3L, 5L), "exceeding the memory limit")
  expect_equal(as.vector(win), sprintf("s%02d", 3:7))
})
