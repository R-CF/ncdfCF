# CFDataset: the data set and its S3 methods.

# A small data set on file with a subgroup.
.write_grouped <- function() {
  .write_nc(c(lon = 2L, lat = 2L), list(
    list(name = "lon", type = "NC_DOUBLE", dims = "lon", data = c(10, 20),
         atts = list(units = "degrees_east", standard_name = "longitude")),
    list(name = "lat", type = "NC_DOUBLE", dims = "lat", data = c(40, 50),
         atts = list(units = "degrees_north", standard_name = "latitude")),
    list(name = "tas", type = "NC_FLOAT", dims = c("lon", "lat"), data = array(1:4, c(2, 2)),
         atts = list(units = "K", long_name = "temperature", standard_name = "air_temperature")),
    list(name = "g1/pr", type = "NC_FLOAT", dims = c("lon", "lat"), data = array(5:8, c(2, 2)),
         atts = list(units = "mm"))
  ), gatts = list(Conventions = "CF-1.13", title = "Test"))
}

test_that("A data set prints its properties, variables and attributes", {
  ds <- open_ncdf(.write_grouped())
  out <- capture.output(print(ds))
  expect_true(any(grepl("^<Dataset>", out)))
  expect_true(any(grepl("Conventions: CF-1.13", out)))
  expect_true(any(grepl("Has groups : TRUE", out)))
  expect_true(any(grepl("Variables:", out)))
  expect_true(any(grepl("title", out)))
})

test_that("A data set with one variable without units or long name prints compactly", {
  ds <- open_ncdf(.write_nc(c(x = 2L), list(
    list(name = "v", type = "NC_FLOAT", dims = "x", data = c(1, 2)))))
  out <- capture.output(print(ds))
  expect_true(any(grepl("^Variable:", out)))
  expect_false(any(grepl("units", out)))
  expect_true(any(grepl("Conventions: \\(not indicated\\)", out)))
})

test_that("A data set prints its hierarchy", {
  ds <- open_ncdf(.write_grouped())
  out <- capture.output(ds$hierarchy())
  expect_true(grepl("^<CF objects>", out[1L]))
  expect_true(any(grepl("g1", out)))
})

test_that("Objects are found by standard name", {
  ds <- open_ncdf(.write_grouped())
  expect_equal(ds$objects_by_standard_name("air_temperature"), "tas")
  all <- ds$objects_by_standard_name()
  expect_true(all(c("tas", "lon", "lat") %in% names(all)))
})

test_that("find_by_name() falls back to NC objects", {
  ds <- open_ncdf(.write_grouped())
  expect_s3_class(ds$find_by_name("tas"), "CFVariable")
  expect_s3_class(ds$find_by_name("lon"), "CFAxisLongitude")
  expect_null(ds$find_by_name("no_such_thing"))
})

test_that("Attributes are retrieved for the data set and its groups", {
  ds <- open_ncdf(.write_grouped())
  expect_true("title" %in% ds$attributes()$name)
  expect_equal(nrow(ds$attributes("/g1")), 0L)
  expect_null(ds$attributes("/no_such_group"))
  expect_equal(ds$attribute("title"), "Test")
})

test_that("Attributes are set, appended and deleted", {
  ds <- create_ncdf()
  expect_identical(ds$set_attribute("title", "NC_CHAR", "A"), ds)
  ds$append_attribute("title", "B")
  expect_equal(ds$attribute("title"), "A; B")
  ds$append_attribute("title", "Z", prepend = TRUE)
  expect_equal(ds$attribute("title"), "Z; A; B")
  ds$delete_attribute("title")
  expect_true(is.na(ds$attribute("title")))
})

test_that("add_variable() checks its arguments", {
  ds <- create_ncdf()
  v <- as_CF("v", 1:3)
  expect_error(ds$add_variable(v, group = "root"), "must be a CFGroup")
  expect_error(ds$add_variable(1:3), "must be a CFVariable")
  expect_identical(ds$add_variable(v), v)
  expect_equal(ds$var_names, "v")
})

test_that("A virtual data set needs a file name to save", {
  ds <- create_ncdf()
  expect_error(ds$save(), "Must supply file name")
})

test_that("A data set is saved to a new file and read back", {
  ds <- create_ncdf()
  ds$add_variable(as_CF("v", array(1:6, c(2, 3))))
  fn <- tempfile(fileext = ".nc")
  ds$save(fn)
  expect_equal(ds$uri, fn)
  ds2 <- open_ncdf(fn)
  expect_equal(ds2$var_names, "v")
  expect_equal(as.vector(ds2[["v"]]$raw()), 1:6)
})

test_that("Variable and axis names are qualified when there are groups", {
  ds <- open_ncdf(.write_grouped())
  expect_setequal(ds$var_names, c("/tas", "/g1/pr"))
  expect_setequal(ds$axis_names, c("/lon", "/lat"))
  expect_setequal(names(ds), c("/tas", "/g1/pr"))
  expect_setequal(dimnames(ds), c("/lon", "/lat"))
  expect_equal(groups(ds), c("/", "/g1"))
})

test_that("S3 methods of a data set without groups", {
  ds <- open_ncdf(.write_nc(c(x = 2L), list(
    list(name = "x", type = "NC_DOUBLE", dims = "x", data = c(1, 2)),
    list(name = "v", type = "NC_FLOAT", dims = "x", data = c(1, 2)))))
  expect_equal(names(ds), "v")
  expect_equal(dimnames(ds), "x")
  expect_output(str(ds), "CFDataset with 1 data variable")
  expect_equal(ds$friendlyClassName, "Data set")
  expect_s3_class(ds$resource, "NCResource")
})

test_that("S3 methods of an empty data set", {
  ds <- create_ncdf()
  expect_null(names(ds))
  expect_null(dimnames(ds))
  expect_output(str(ds), "CFDataset with 0 data variables")
})

test_that("The name of a data set without .nc extension is the file name", {
  fn <- tempfile()
  file.copy(.write_nc(c(x = 1L), list(list(name = "v", type = "NC_FLOAT", dims = "x", data = 1))), fn)
  expect_equal(open_ncdf(fn)$name, basename(fn))
})

test_that("The name of a data set drops the .nc or .ncml extension", {
  fn <- .write_nc(c(x = 1L), list(list(name = "v", type = "NC_FLOAT", dims = "x", data = 1)))
  expect_equal(open_ncdf(fn)$name, sub("\\.nc$", "", basename(fn)))
})

test_that("A data set is written to a Zarr store", {
  skip_if_not_installed("zarr")
  skip_if_not_installed("geozarr")
  ds <- open_ncdf(.write_grouped())
  z <- ds$geozarr()
  expect_s3_class(z, "zarr")

  store <- tempfile(fileext = ".zarr")
  z <- ds$geozarr(store, anchor_group = "/a/b")
  expect_false(is.null(z$get_node("/a/b")))
  z2 <- ds$geozarr(z, anchor_group = "/a/b")
  expect_s3_class(z2, "zarr")
})

test_that("Writing to a Zarr store checks the anchor group", {
  skip_if_not_installed("zarr")
  skip_if_not_installed("geozarr")
  ds <- open_ncdf(.write_grouped())
  store <- tempfile(fileext = ".zarr")
  expect_error(ds$geozarr(store, anchor_group = "a"), "must start with a slash")
  z <- zarr::create_zarr()
  expect_error(ds$geozarr(store, anchor_group = z$root), "must be a character path")
})
