# Constructors of CF objects in memory.

test_that("create_ncdf() makes a virtual data set with minimal attributes", {
  ds <- create_ncdf()
  expect_s3_class(ds, "CFDataset")
  expect_equal(ds$conventions, "CF-1.13")
  expect_true(grepl("Newly created.", ds$attribute("history"), fixed = TRUE))
  expect_null(ds$uri)
})

test_that("makeGroup() makes a group", {
  grp <- makeGroup("g1")
  expect_s3_class(grp, "CFGroup")
  expect_equal(grp$name, "g1")
})

test_that("makeAxis() dispatches on the orientation", {
  expect_s3_class(makeAxis("x", "X", c(10, 20)), "CFAxisLongitude")
  expect_s3_class(makeAxis("y", "Y", c(10, 20)), "CFAxisLatitude")
  expect_s3_class(makeAxis("z", "Z", c(10, 20)), "CFAxisVertical")
  t <- CFtime::CFTime$new("days since 2000-01-01", "standard", 0:2)
  expect_s3_class(makeAxis("t", "T", t), "CFAxisTime")
  expect_error(makeAxis("q", "Q", 1:3), "Bad `orientation`")
})

test_that("makeAxis() without orientation infers longitude and latitude from the name", {
  expect_s3_class(makeAxis("lon", values = c(10, 20)), "CFAxisLongitude")
  expect_s3_class(makeAxis("Latitude", values = c(10, 20)), "CFAxisLatitude")
  expect_s3_class(makeAxis("lat", values = c(100, 200)), "CFAxisNumeric")
})

test_that("makeAxis() without orientation makes numeric and character axes", {
  ax <- makeAxis("depth", values = c(1, 2, 3), bounds = matrix(c(0.5, 1.5, 1.5, 2.5, 2.5, 3.5), nrow = 2))
  expect_s3_class(ax, "CFAxisNumeric")
  expect_equal(ax$attribute("bounds"), "depth_bnds")
  expect_s3_class(ax$bounds, "CFBounds")
  expect_s3_class(makeAxis("names", values = c("a", "b")), "CFAxisCharacter")
})

test_that("makeLongitudeAxis() sets default attributes and bounds", {
  ax <- makeLongitudeAxis("lon", c(10, 20), bounds = matrix(c(5, 15, 15, 25), nrow = 2))
  expect_equal(ax$attribute("standard_name"), "longitude")
  expect_equal(ax$attribute("units"), "degrees_east")
  expect_equal(ax$attribute("bounds"), "lon_bnds")
  expect_error(makeLongitudeAxis("1lon", c(10, 20)), "not valid")
})

test_that("makeLatitudeAxis() sets default attributes and bounds", {
  ax <- makeLatitudeAxis("lat", c(10, 20), bounds = matrix(c(5, 15, 15, 25), nrow = 2))
  expect_equal(ax$attribute("standard_name"), "latitude")
  expect_equal(ax$attribute("units"), "degrees_north")
  expect_equal(ax$attribute("bounds"), "lat_bnds")
  expect_error(makeLatitudeAxis("1lat", c(10, 20)), "not valid")
})

test_that("makeVerticalAxis() sets bounds", {
  ax <- makeVerticalAxis("lev", c(10, 20), bounds = matrix(c(5, 15, 15, 25), nrow = 2),
                         attributes = data.frame(name = "positive", type = "NC_CHAR", length = 4L, value = "down"))
  expect_equal(ax$attribute("bounds"), "lev_bnds")
  expect_error(makeVerticalAxis("1lev", c(10, 20)), "not valid")
})

test_that("makeTimeAxis() takes the calendar and bounds from the CFTime instance", {
  t <- CFtime::CFTime$new("days since 2000-01-01", "360_day", 0:2)
  t$bounds <- TRUE
  ax <- makeTimeAxis("time", t)
  expect_equal(ax$attribute("standard_name"), "time")
  expect_equal(ax$attribute("units"), "days since 2000-01-01")
  expect_equal(ax$attribute("calendar"), "360_day")
  expect_equal(ax$attribute("bounds"), "time_bnds")
  expect_error(makeTimeAxis("1time", t), "not valid")
})

test_that("makeDiscreteAxis() and makeCharacterAxis() make their axes", {
  ax <- makeDiscreteAxis("n", 4)
  expect_s3_class(ax, "CFAxisDiscrete")
  expect_equal(ax$length, 4L)
  expect_error(makeDiscreteAxis("1n", 4), "not valid")
  expect_equal(makeCharacterAxis("s", c("a", "b"))$values, c("a", "b"))
})

test_that("as_CF() makes a variable from a named vector and a logical vector", {
  v <- as_CF("v", c(a = 1, b = 2))
  expect_s3_class(v$axes[[1L]], "CFAxisCharacter")
  l <- as_CF("l", c(TRUE, FALSE, TRUE))
  expect_equal(l$data_type, "NC_INT")
  expect_s3_class(l$axes[[1L]], "CFAxisDiscrete")
})

test_that("as_CF() makes axes from the dimnames of an array", {
  arr <- array(1:12, c(2, 3, 2), dimnames = list(lon = c(10, 20), lat = c(1, 2, 3), c("2000-01-01", "2000-01-02")))
  v <- as_CF("arr", arr)
  expect_s3_class(v$axes[["lon"]], "CFAxisLongitude")
  expect_s3_class(v$axes[["lat"]], "CFAxisLatitude")
  expect_s3_class(v$axes[["axis_3"]], "CFAxisTime")
})

test_that("as_CF() makes a variable from a SpatRaster with one variable", {
  skip_if_not_installed("terra")
  r <- terra::rast(nrows = 3, ncols = 4, xmin = 0, xmax = 4, ymin = 0, ymax = 3, crs = "EPSG:4326", vals = 1:12)
  names(r) <- "tas"
  terra::varnames(r) <- "tas"
  terra::longnames(r) <- "temperature"
  terra::units(r) <- "K"
  v <- as_CF("t", r)
  expect_s3_class(v, "CFVariable")
  expect_equal(v$name, "t")
  expect_equal(v$attribute("units"), "K")
  expect_equal(v$attribute("long_name"), "temperature")
  expect_equal(names(v$axes), c("longitude", "latitude", "layers"))
  expect_equal(v$axes[["longitude"]]$values, 0.5:3.5)
  expect_equal(v$axes[["latitude"]]$values, 2.5:0.5)
})

test_that("as_CF() of a SpatRaster with a time dimension makes a time axis", {
  skip_if_not_installed("terra")
  r <- terra::rast(nrows = 2, ncols = 2, crs = "EPSG:4326", vals = 1:4)
  names(r) <- terra::varnames(r) <- "tas"
  terra::time(r) <- as.Date("2020-01-01")
  v <- as_CF("t", r)
  expect_s3_class(v$axes[["time"]], "CFAxisTime")
})

test_that("as_CF() of a SpatRaster with several variables makes a data set", {
  skip_if_not_installed("terra")
  a <- terra::rast(nrows = 2, ncols = 2, crs = "EPSG:4326", vals = 1:4)
  names(a) <- terra::varnames(a) <- "tas"
  b <- terra::rast(nrows = 2, ncols = 2, crs = "EPSG:4326", vals = 5:8)
  names(b) <- terra::varnames(b) <- "pr"
  ds <- as_CF("ds", c(a, b))
  expect_s3_class(ds, "CFDataset")
  expect_equal(ds$name, "ds")
  expect_setequal(ds$var_names, c("tas", "pr"))
})

test_that("as_CF() of a projected SpatRaster uses projection coordinates", {
  skip_if_not_installed("terra")
  r <- terra::rast(nrows = 2, ncols = 2, xmin = 0, xmax = 2000, ymin = 0, ymax = 2000, crs = "EPSG:32633", vals = 1:4)
  names(r) <- terra::varnames(r) <- "z"
  v <- as_CF("z", r)
  expect_equal(names(v$axes)[1:2], c("x", "y"))
  expect_equal(v$axes[["x"]]$attribute("standard_name"), "projected_x_coordinate")
})

test_that("as_CF() of a SpatRaster does not depend on the layer names", {
  skip_if_not_installed("terra")
  r <- terra::rast(nrows = 2, ncols = 2, nlyrs = 3, crs = "EPSG:4326", vals = 1:12)
  terra::varnames(r) <- "tas"
  names(r) <- c("jan", "feb", "mar")
  v <- as_CF("t", r)
  expect_equal(v$axes[["layers"]]$values, c("jan", "feb", "mar"))
  expect_equal(dim(v$raw()), c(2L, 2L, 3L))
})

test_that("as_CF() of a SpatRaster keeps variables apart whose names share a prefix", {
  skip_if_not_installed("terra")
  a <- terra::rast(nrows = 2, ncols = 2, nlyrs = 2, crs = "EPSG:4326", vals = 1:8)
  names(a) <- paste0("tas_", 1:2)
  terra::varnames(a) <- "tas"
  b <- terra::rast(nrows = 2, ncols = 2, nlyrs = 3, crs = "EPSG:4326", vals = 101:112)
  names(b) <- paste0("tasmax_", 1:3)
  terra::varnames(b) <- "tasmax"
  ds <- as_CF("ds", c(a, b))
  expect_equal(ds[["tas"]]$axes[["layers"]]$length, 2L)
  expect_equal(ds[["tasmax"]]$axes[["layers"]]$length, 3L)
  expect_true(all(ds[["tas"]]$raw() <= 8))
  expect_true(all(ds[["tasmax"]]$raw() > 100))
})
