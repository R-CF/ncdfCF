# AOI, aoi() and CFAuxiliaryLongLat: areas of interest on auxiliary
# longitude-latitude grids.

.nam44 <- function() {
  open_ncdf(system.file("extdata", "tasmax_NAM-44_day_20410701-vncdfCF.nc", package = "ncdfCF"))[["tasmax"]]
}

# --- aoi() and AOI ------------------------------------------------------------

test_that("aoi() makes an area of interest", {
  a <- aoi(-90, -70, 40, 50, 0.5)
  expect_s3_class(a, "AOI")
  expect_equal(a$extent, c(-90, -70, 40, 50))
  expect_equal(a$resolution, c(0.5, 0.5))
  expect_equal(dim(a), c(20L, 40L))
  expect_equal(a$dimnames[[1L]], seq(40.25, 49.75, by = 0.5))
  expect_equal(aoi(-90, -70, 40, 50, 0.5, 0.25)$resolution, c(0.5, 0.25))
})

test_that("aoi() without arguments covers the full extent", {
  a <- aoi()
  expect_null(a$extent)
  expect_length(a$dim, 0L)
  out <- capture.output(print(a))
  expect_true(any(grepl("(full extent)", out, fixed = TRUE)))
  expect_true(any(grepl("(from variable)", out, fixed = TRUE)))
  expect_equal(aoi(resX = 0.5)$resolution, c(0.5, 0.5))
})

test_that("aoi() checks its arguments", {
  expect_warning(aoi(0, 10, 0, 10, 20), "outside of the safe range")
  expect_error(aoi(-200, 300, 0, 10), "Longitude range is not valid")
  expect_error(aoi(0, 10, 50, 40), "Latitude range is not valid")
  expect_error(aoi(0, 10, -100, 40), "Latitude range is not valid")
})

test_that("An AOI prints its extent and resolution", {
  out <- capture.output(print(aoi(-90, -70, 40, 50, 0.5)))
  expect_true(any(grepl("Longitude:  [-90.000 ... -70.000]", out, fixed = TRUE)))
  expect_true(any(grepl("Resolution: [0.500, 0.500]", out, fixed = TRUE)))
})

test_that("The fields of an AOI are set and checked", {
  a <- aoi(-90, -70, 40, 50, 0.5)
  a$lonMin <- -95
  a$lonMax <- -60
  a$latMin <- 30
  a$latMax <- 55
  expect_equal(a$extent, c(-95, -60, 30, 55))
  a$extent <- c(0, 10, 0, 10)
  expect_equal(c(a$lonMin, a$lonMax, a$latMin, a$latMax), c(0, 10, 0, 10))
  a$resolution <- 1
  expect_equal(a$resolution, c(1, 1))
  expect_error(a$latMin <- 20, "Latitude range is not valid")
  expect_error(a$lonMax <- 500, "Longitude range is not valid")
  expect_warning(a$dim <- c(1, 1), "Cannot set the grid dimensions")
  expect_warning(a$dimnames <- list(), "Cannot set the dimension names")
})

# --- CFAuxiliaryLongLat -------------------------------------------------------

test_that("An auxiliary grid prints and describes itself", {
  ll <- .nam44()$gridLongLat
  expect_s3_class(ll, "CFAuxiliaryLongLat")
  expect_equal(ll$friendlyClassName, "Auxiliary longitude-latitude grid")
  expect_equal(ll$name, "lon_lat")
  expect_equal(ll$grid_names, c("lon", "lat"))
  expect_equal(ll$dim, c(148L, 140L))
  out <- capture.output(print(ll))
  expect_true(any(grepl("Longitude grid : lon", out, fixed = TRUE)))
  expect_true(any(grepl("AOI            : (not set)", out, fixed = TRUE)))
  b <- ll$brief()
  expect_equal(b$axis, c("X", "Y"))
  expect_equal(b$unit, c("degrees_east", "degrees_north"))
  expect_length(ll$extent, 4L)
  expect_s3_class(ll$lon, "CFVariable")
  expect_s3_class(ll$lat, "CFVariable")
  expect_null(ll$lon_bounds)
  expect_null(ll$lat_bounds)
})

test_that("An auxiliary grid prints its AOI once set", {
  ll <- .nam44()$gridLongLat
  ll$aoi <- aoi(-90, -70, 40, 50, 0.5)
  out <- capture.output(print(ll))
  expect_true(any(grepl("AOI  longitude : [-90.000 ...", out, fixed = TRUE)))
  expect_true(any(grepl("extent : [20 rows x 40 columns]", out, fixed = TRUE)))
})

test_that("An auxiliary grid finds the grid cells for points", {
  ll <- .nam44()$gridLongLat
  idx <- ll$sample_index(c(-80, 0), c(45, 0))
  expect_equal(dim(idx), c(2L, 2L))
  expect_false(anyNA(idx[1L, ]))
  expect_true(all(is.na(idx[2L, ])))
  expect_error(ll$sample_index(1:2, 1), "same length")
})

test_that("The grid index covers the AOI and is cached", {
  ll <- .nam44()$gridLongLat
  ll$aoi <- aoi(-90, -70, 40, 50, 0.5)
  idx <- ll$grid_index()
  expect_equal(dim(idx), c(40L, 20L)) # In the orientation of the grid
  expect_identical(ll$grid_index(), idx)
  ll$aoi <- aoi(100, 110, 0, 10, 0.5)
  expect_true(all(is.na(ll$grid_index())))
})

test_that("An AOI with a resolution derived from the grid is used", {
  ll <- .nam44()$gridLongLat
  ll$aoi <- aoi(-90, -70, 40, 50)
  expect_length(ll$aoi$resolution, 2L)
  expect_error(ll$aoi <- aoi(100, 110, 0, 10), "Center of AOI contains no data")
  ll$aoi <- aoi()
  expect_length(ll$aoi$extent, 4L)
})

test_that("A variable on an auxiliary grid prints the grid", {
  expect_output(print(.nam44()), "Auxiliary longitude-latitude grid:")
})

test_that("A variable on an auxiliary grid is subset with a resolution", {
  tm <- .nam44()
  sub <- tm$subset(lon = c(-90, -70), lat = c(40, 50), .resolution = 1)
  expect_equal(sapply(sub$axes, function(x) x$length), c(lon = 20, lat = 10, time = 1, height = 1))
  expect_error(tm$subset(lon = c(-90, -70)), "both auxiliary grid axes")
})

test_that("A profile is extracted on an auxiliary grid", {
  tm <- .nam44()
  p <- tm$profile(lon = -80, lat = 45)
  expect_s3_class(p, "CFVariable")
  expect_equal(names(p$axes)[1:2], c("lon", "lat"))
  expect_true(is.na(p$attribute("grid_mapping")))
})

test_that("A variable on an auxiliary grid is written to a Zarr store", {
  skip_if_not_installed("zarr")
  skip_if_not_installed("geozarr")
  fn <- system.file("extdata", "tasmax_NAM-44_day_20410701-vncdfCF.nc", package = "ncdfCF")
  z <- open_ncdf(fn)$geozarr()
  expect_false(is.null(z$get_node("/tasmax")))
})

# --- geom_ncdf() --------------------------------------------------------------

test_that("geom_ncdf() makes a raster layer", {
  skip_if_not_installed("ggplot2")
  fn <- system.file("extdata", "ERA5land_Rwanda_20160101.nc", package = "ncdfCF")
  t2m <- open_ncdf(fn)[["t2m"]]$subset(time = "2016-01-01T00:00:00")
  layer <- geom_ncdf(data = t2m)
  expect_s3_class(layer, "LayerInstance")
  expect_s3_class(ggplot2::ggplot() + layer, "ggplot")
})

test_that("geom_ncdf() uses the first two axes when there are no X and Y axes", {
  skip_if_not_installed("ggplot2")
  v <- as_CF("v", array(1:6, c(2, 3), dimnames = list(a = c("p", "q"), b = c("x", "y", "z"))))
  expect_s3_class(geom_ncdf(data = v), "LayerInstance")
})

test_that("geom_ncdf() checks its data", {
  skip_if_not_installed("ggplot2")
  expect_error(geom_ncdf(data = 1:3), "must be a CFVariable")
  expect_error(geom_ncdf(data = as_CF("v", 1)), "Not enough data to plot")
})
