# CFAxisTime: time axes, their bounds and climatologies.

# A file with a daily time axis of `n` days with bounds, or a climatology. With
# `xy = TRUE` the data variable also has longitude and latitude axes.
.write_time <- function(n = 5L, bounds = TRUE, climatology = FALSE, offsets = seq_len(n) - 0.5, xy = FALSE) {
  atts <- list(units = "days since 2001-01-01", calendar = "gregorian", standard_name = "time")
  vars <- list(list(name = "time", type = "NC_DOUBLE", dims = "time", data = offsets, atts = atts))
  bv <- rbind(offsets - 0.5, offsets + 0.5)
  if (climatology) {
    vars[[1L]]$atts$climatology <- "climate_bnds"
    vars <- c(vars, list(list(name = "climate_bnds", type = "NC_DOUBLE", dims = c("nv", "time"), data = bv)))
  } else if (bounds) {
    vars[[1L]]$atts$bounds <- "time_bnds"
    vars <- c(vars, list(list(name = "time_bnds", type = "NC_DOUBLE", dims = c("nv", "time"), data = bv)))
  }
  if (xy) {
    vars <- c(vars, list(
      list(name = "lon", type = "NC_DOUBLE", dims = "lon", data = c(10, 20), atts = list(units = "degrees_east")),
      list(name = "lat", type = "NC_DOUBLE", dims = "lat", data = c(40, 50), atts = list(units = "degrees_north")),
      list(name = "pr", type = "NC_FLOAT", dims = c("lon", "lat", "time"), data = array(seq_len(4 * n) * 1.0, c(2, 2, n)))))
    .write_nc(c(time = n, nv = 2L, lon = 2L, lat = 2L), vars)
  } else {
    vars <- c(vars, list(list(name = "pr", type = "NC_FLOAT", dims = "time", data = seq_len(n) * 1.0)))
    .write_nc(c(time = n, nv = 2L), vars)
  }
}

test_that("A time axis is read with its calendar and bounds", {
  ax <- open_ncdf(.write_time())[["time"]]
  expect_s3_class(ax, "CFAxisTime")
  expect_equal(ax$friendlyClassName, "Time axis")
  expect_equal(ax$time$calendar$name, "standard")
  expect_s3_class(ax$bounds, "CFBounds")
  out <- capture.output(print(ax))
  expect_true(any(grepl("Calendar   : standard", out)))
  expect_true(any(grepl("Range      : 2001-01-01T12:00:00 ... 2001-01-05T12:00:00 (days)", out, fixed = TRUE)))
  expect_true(any(grepl("Bounds     : 2001-01-01 ... 2001-01-06", out, fixed = TRUE)))
})

test_that("A time axis without bounds prints that", {
  ax <- open_ncdf(.write_time(bounds = FALSE))[["time"]]
  expect_output(print(ax), "Bounds     : (not set)", fixed = TRUE)
  expect_equal(ax$brief()$values, "[2001-01-01T12:00:00 ... 2001-01-05T12:00:00]")
})

test_that("A time axis with one value is described compactly", {
  ax <- open_ncdf(.write_time(n = 1L))[["time"]]
  expect_equal(ax$brief()$values, "[2001-01-01T12:00:00]")
  expect_output(print(ax), "Range      : 2001-01-01T12:00:00 (days)", fixed = TRUE)
})

test_that("A climatological time axis is read", {
  ax <- open_ncdf(.write_time(climatology = TRUE))[["time"]]
  expect_s3_class(ax$time, "CFClimatology")
})

test_that("Unmatched bounds or climatology references give warnings", {
  fn <- .write_time(bounds = FALSE)
  nc <- RNetCDF::open.nc(fn, write = TRUE)
  RNetCDF::att.put.nc(nc, "time", "bounds", "NC_CHAR", "no_bounds")
  RNetCDF::close.nc(nc)
  expect_true(any(grepl("Unmatched `bounds` value 'no_bounds'", .warnings_of(open_ncdf(fn)), fixed = TRUE)))

  fn <- .write_time(bounds = FALSE)
  nc <- RNetCDF::open.nc(fn, write = TRUE)
  RNetCDF::att.put.nc(nc, "time", "climatology", "NC_CHAR", "no_clim")
  RNetCDF::close.nc(nc)
  expect_warning(open_ncdf(fn), "Unmatched `climatology` value 'no_clim'")
})

test_that("A time axis without units is not a time axis", {
  fn <- .write_nc(c(time = 2L), list(
    list(name = "time", type = "NC_DOUBLE", dims = "time", data = c(1, 2), atts = list(standard_name = "time")),
    list(name = "pr", type = "NC_FLOAT", dims = "time", data = c(1, 2))))
  expect_false(inherits(open_ncdf(fn)[["time"]], "CFAxisTime"))
})

test_that("Indices are found for timestamps", {
  ax <- open_ncdf(.write_time())[["time"]]
  expect_equal(as.vector(ax$indexOf(c("2001-01-02T12:00:00", "2001-01-04T06:00:00"))), c(2L, 4L))
  ax <- open_ncdf(.write_time(bounds = FALSE))[["time"]]
  expect_equal(as.vector(ax$indexOf("2001-01-02T00:00:00", method = "linear")), 1.5)
})

test_that("Time axes are sliced by abbreviated timestamps", {
  ax <- open_ncdf(.write_time(n = 800L, bounds = FALSE))[["time"]]
  expect_equal(ax$slice(2001), c(1L, 365L))
  expect_equal(ax$slice("2002"), c(366L, 730L))
  expect_equal(ax$slice(c("2001-02", "2001-03")), c(32L, 90L))
  expect_equal(ax$slice("2001-12"), c(335L, 365L))
  expect_equal(ax$slice("2001-Q1"), c(1L, 90L))
  expect_equal(ax$slice("2001-Q4"), c(274L, 365L))
  expect_equal(ax$slice("2001-S2"), c(60L, 151L))
  expect_equal(ax$slice("2002-S1"), c(335L, 424L))
  expect_equal(ax$slice("2001-D01"), c(1L, 10L))
  expect_equal(ax$slice("2001-D02"), c(11L, 20L))
  expect_equal(ax$slice("2001-D03"), c(21L, 31L))
  expect_equal(ax$slice("2001-D36"), c(355L, 365L))
  expect_equal(ax$slice("2001-03-01"), c(60L, 60L))
  expect_equal(ax$slice(c("2001-01-10", "2001-01-20")), c(10L, 19L))
  expect_null(ax$slice(1990))
})

test_that("Bad timestamps for slicing give errors", {
  ax <- open_ncdf(.write_time())[["time"]]
  expect_error(ax$slice(list(1)), "Bad format for timestamps")
  expect_error(ax$slice(c("2001", "2002", "2003")), "Bad format for timestamps")
  expect_error(ax$slice("2001-02-30"), "Date not valid")
})

test_that("A time axis is copied and subset, on file and in memory", {
  grp <- CFGroup$new("", NULL)
  ax <- open_ncdf(.write_time())[["time"]]
  expect_equal(ax$copy("t2", grp)$name, "t2")
  expect_equal(ax$subset(group = grp)$length, 5L)
  sub <- ax$subset("t3", grp, c(2, 3))
  expect_equal(sub$name, "t3")
  expect_equal(sub$length, 2L)

  t <- CFtime::CFTime$new("days since 2000-01-01", "standard", 0:4)
  mem <- makeTimeAxis("time", t, group = grp)
  expect_equal(mem$copy(group = grp)$length, 5L)
  sub <- mem$subset("tm", CFGroup$new("", NULL), c(2, 4))
  expect_equal(sub$time$offsets, 1:3)
})

test_that("A time axis is copied with new values", {
  grp <- CFGroup$new("", NULL)
  ax <- open_ncdf(.write_time())[["time"]]
  cp <- ax$copy_with_values("t2", grp, c(10, 11))
  expect_equal(cp$time$offsets, c(10, 11))
  t <- CFtime::CFTime$new("days since 2000-01-01", "standard", 0:1)
  expect_equal(ax$copy_with_values(group = CFGroup$new("", NULL), values = t)$name, "time")
  expect_error(ax$copy_with_values("t3", grp, c("x", "y")), "")
})

test_that("Time axes are appended", {
  grp <- CFGroup$new("", NULL)
  a <- makeTimeAxis("time", CFtime::CFTime$new("days since 2000-01-01", "standard", 0:2), group = grp)
  b <- makeTimeAxis("time", CFtime::CFTime$new("days since 2000-01-01", "standard", 3:4), group = CFGroup$new("", NULL))
  ap <- a$append(b, CFGroup$new("", NULL))
  expect_equal(ap$time$offsets, 0:4)
  expect_error(a$append(makeDiscreteAxis("time", 2, group = CFGroup$new("", NULL)), grp), "cannot be appended")
})

test_that("A time axis with a label set uses the labels", {
  fn <- .write_time(n = 2L, bounds = FALSE)
  nc <- RNetCDF::open.nc(fn, write = TRUE)
  RNetCDF::dim.def.nc(nc, "strlen", 3L)
  RNetCDF::var.def.nc(nc, "season", "NC_CHAR", c("strlen", "time"))
  RNetCDF::var.put.nc(nc, "season", c("DJF", "MAM"))
  RNetCDF::att.put.nc(nc, "pr", "coordinates", "NC_CHAR", "season")
  RNetCDF::close.nc(nc)
  t <- open_ncdf(fn)[["pr"]]$axes[["time"]]
  t$active_coordinates <- "season"
  expect_equal(t$brief()$values, "[DJF ... MAM]")
  expect_output(print(t), "DJF ... MAM")
})

test_that("A time axis is written to a Zarr store", {
  skip_if_not_installed("zarr")
  skip_if_not_installed("geozarr")
  # Regular with regular bounds
  expect_s3_class(open_ncdf(.write_time(xy = TRUE))$geozarr(), "zarr")
  # Irregular and longer than geozarr's explicit limit, with irregular bounds
  off <- cumsum(c(0.5, rep(c(1, 2), 20)))
  fn <- .write_time(n = length(off), offsets = off, xy = TRUE)
  z <- open_ncdf(fn)$geozarr()
  expect_false(is.null(z$get_node("/coordinates/time")))
})
