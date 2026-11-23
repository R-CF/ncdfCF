# Internal classes and utilities: CFData, CFObject, the NC classes and helpers.

.simple_file <- function() {
  .write_nc(c(x = 3L), list(
    list(name = "x", type = "NC_DOUBLE", dims = "x", data = c(1, 2, 3)),
    list(name = "v", type = "NC_FLOAT", dims = "x", data = c(4, 5, 6), atts = list(units = "m"))))
}

# --- CFData -------------------------------------------------------------------

test_that("The dimensions of data can only be set to the same number of values", {
  v <- as_CF("v", array(1:6, c(2, 3)))
  priv <- v$.__enclos_env__$private
  priv$set_dims(c(2L, 3L, 1L))
  expect_equal(dim(v$values), c(2L, 3L, 1L))
  expect_error(priv$set_dims(c(4L, 2L)), "do not agree")
})

test_that("Start and count must match the dimensions of the data", {
  v <- open_ncdf(.simple_file())[["v"]]
  expect_error(v$read_chunk(c(1, 1), NA), "`start` vector is not the length")
  expect_error(v$read_chunk(1, c(1, 1)), "`count` vector is not the length")
  expect_equal(as.vector(v$read_chunk(2, 2)), c(5, 6))
  expect_equal(v$dim(1L), 3L)
  expect_equal(v$NC_map, list(start = 1L, count = 3L))
})

test_that("Data without dimensions reads no chunks or windows", {
  v <- CFVariable$new("v", group = CFGroup$new("", NULL), axes = list())
  expect_null(v$read_chunk(NA, NA))
  expect_null(v$read_window(NA, NA))
})

test_that("The data type is set only on data not on file and only to valid types", {
  v <- open_ncdf(.simple_file())[["v"]]
  expect_error(v$data_type <- "NC_INT", "Cannot set the data type")
  m <- as_CF("m", c(1.5, 2.5))
  m$data_type <- "NC_FLOAT"
  expect_equal(m$data_type, "NC_FLOAT")
  expect_error(m$data_type <- "NC_NOT_A_TYPE", "Unrecognized data type")
})

test_that("Values of an unsupported type are refused", {
  expect_error(CFVariable$new("v", group = CFGroup$new("", NULL), values = list(1, 2), axes = list()),
               "Bad data type")
})

test_that("The data type is taken from the fill value when the data is packed", {
  fn <- tempfile(fileext = ".nc")
  nc <- RNetCDF::create.nc(fn, format = "netcdf4")
  RNetCDF::dim.def.nc(nc, "x", 3L)
  RNetCDF::var.def.nc(nc, "x", "NC_DOUBLE", "x")
  RNetCDF::var.def.nc(nc, "v", "NC_SHORT", "x")
  RNetCDF::att.put.nc(nc, "v", "_FillValue", "NC_SHORT", -1L)
  RNetCDF::att.put.nc(nc, "v", "scale_factor", "NC_FLOAT", 0.1)
  RNetCDF::var.put.nc(nc, "x", c(1, 2, 3))
  RNetCDF::var.put.nc(nc, "v", c(10, 20, 30), pack = TRUE)
  RNetCDF::close.nc(nc)
  v <- open_ncdf(fn)[["v"]]
  expect_equal(as.vector(v$raw()), c(10, 20, 30), tolerance = 1e-6)
  expect_equal(v$data_type, "NC_FLOAT")
})

# --- CFObject -----------------------------------------------------------------

test_that("Attributes of CF objects are set, compared and appended", {
  v <- as_CF("v", 1:3)
  v$set_attribute("flag", "NC_INT", TRUE)
  expect_equal(v$attribute("flag"), 1L)
  expect_true(v$attributes_identical(v$attributes))
  expect_false(v$attributes_identical(v$attributes[0L, ]))
  other <- v$attributes
  other$value[[1L]] <- 99L
  expect_false(v$attributes_identical(other))
  expect_error(v$append_attribute(c("a", "b"), "x"), "Must name one attribute")
  expect_error(v$append_attribute("comment", 1:2), "single character string")
  v$append_attribute("comment", "first")
  expect_equal(v$attribute("comment"), "first")
  v$set_attribute("note", "NC_STRING", "a")
  v$append_attribute("note", "b")
  expect_equal(v$attribute("note"), "a; b")
})

test_that("CF objects report their class and dirty state", {
  v <- as_CF("v", 1:3)
  expect_equal(CFObject$new("o", group = NULL)$friendlyClassName, "Generic CF object")
  v$is_dirty <- FALSE
  expect_false(v$is_dirty)
  expect_error(v$is_dirty <- "yes", "single logical value")
})

test_that("A CF object is moved to another group", {
  v <- as_CF("v", 1:3)
  grp <- CFGroup$new("", NULL)
  v$group <- grp
  expect_identical(v$group, grp)
})

# --- NC classes ---------------------------------------------------------------

test_that("NC objects print themselves", {
  ds <- open_ncdf(.simple_file())
  nc <- ds$root$NC
  expect_output(print(nc$NCdims[["x"]]), "<netCDF dimension>")
  expect_output(print(nc$NCdims[["x"]]), "Unlimited    : FALSE")
  out <- capture.output(print(nc$NCvars[["v"]]))
  expect_true(any(grepl("<netCDF variable>", out, fixed = TRUE)))
  expect_true(any(grepl("Data type    : NC_FLOAT", out, fixed = TRUE)))
  expect_true(grepl("v]", nc$NCvars[["v"]]$shard(), fixed = TRUE))
  expect_equal(nc$NCvars[["v"]]$dim(1L), 3L)
  res <- ds$resource
  expect_output(print(res), "<netCDF resource>")
  expect_equal(res$friendlyClassName, "NetCDF resource connection details")
  expect_error(res$handle <- 1, "Can't assign")
  expect_error(res$uri <- "x", "Can't assign")
})

test_that("A NC object returns one attribute at a time", {
  v <- open_ncdf(.simple_file())$root$NC$NCvars[["v"]]
  expect_equal(v$attribute("units"), "m")
  expect_error(v$attribute(c("units", "long_name")), "only one attribute")
  expect_length(v$CF, 1L)
})

test_that("A NC variable is renamed on a writable file", {
  fn <- .simple_file()
  ds <- open_ncdf(fn, write = TRUE)
  v <- ds[["v"]]
  v$name <- "w"
  expect_equal(v$NC$name, "w")
  rm(ds); gc()
  expect_true("w" %in% open_ncdf(fn)$var_names)
})

test_that("Attributes are rewritten on file, deleting those removed", {
  fn <- .simple_file()
  ds <- open_ncdf(fn, write = TRUE)
  v <- ds[["v"]]
  v$delete_attribute("units")
  v$set_attribute("long_name", "NC_CHAR", "A variable")
  ds$save()
  w <- open_ncdf(fn)[["v"]]
  expect_true(is.na(w$attribute("units")))
  expect_equal(w$attribute("long_name"), "A variable")
})

test_that("A new file is not created in a location that cannot be written", {
  res <- NCResource$new(file.path(tempfile(), "no_dir", "x.nc"), write = TRUE)
  expect_error(res$create(), "Could not create the netCDF file")
})

test_that("A dimension is written to file, also when it already exists", {
  fn <- tempfile(fileext = ".nc")
  ds <- create_ncdf()
  ds$add_variable(as_CF("v", array(1:6, c(2, 3))))
  ds$save(fn)
  nc <- RNetCDF::open.nc(fn, write = TRUE)
  on.exit(RNetCDF::close.nc(nc))
  d <- NCDimension$new(0L, "u", 4L, TRUE, open_ncdf(fn)$root$NC)
  expect_identical(d$write(nc), d)
  expect_identical(d$write(nc), d)
  expect_true(RNetCDF::dim.inq.nc(nc, "u")$unlim)
})

# --- Utilities ----------------------------------------------------------------

test_that("Storage modes map to netCDF types", {
  expect_equal(.nc_type("double"), "NC_DOUBLE")
  expect_equal(.nc_type("integer64"), "NC_INT64")
  expect_equal(.nc_type("raw"), "NC_NAT")
  expect_false(.compatible_type("character", "NC_DOUBLE"))
})

test_that("Names are made valid", {
  expect_equal(.make_valid_name("1 bad-name"), "x1_bad_name")
  expect_equal(.make_valid_name("__x"), "x")
  expect_equal(nchar(.make_valid_name(strrep("a", 300))), 255L)
})

test_that("Values are rounded half away from zero", {
  expect_equal(.round.5up(c(1.5, -1.5, 2.4)), c(2, -2, 2))
  expect_equal(.round.5down(c(1.5, -1.5)), c(1, -1))
})

test_that("Monotonicity of vectors and their concatenation is tested", {
  expect_equal(.monotonicity(c(3, 2, 1)), -1L)
  expect_equal(.monotonicity(c(1, 3, 2)), 0L)
  expect_true(.c_is_monotonic(1, 2))
  expect_true(.c_is_monotonic(1, c(2, 3)))
  expect_false(.c_is_monotonic(2, c(1, 3)))
  expect_true(.c_is_monotonic(c(1, 2), 3))
  expect_false(.c_is_monotonic(c(1, 2), 1.5))
})

test_that("Data is processed over a factor for vectors and multiple results", {
  f <- factor(c("a", "a", "b"))
  expect_equal(unname(.process.data(c(1, 2, 3), 1L, f, sum)[[1L]]), c(3, 3))
  expect_equal(.process.data(c(1, 2, 3), 1L, NULL, range), list(1, 3))
})
