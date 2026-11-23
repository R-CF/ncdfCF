# CFVariable: data variables.

# A lon-lat-time file with daily data from 2001-01-01 for `ndays` days. The
# latitude increases so that arrays must be flipped to R order.
.write_tas <- function(ndays = 60L, bounds = TRUE) {
  nlon <- 3L; nlat <- 2L
  vals <- array(seq_len(nlon * nlat * ndays) * 1.0, c(nlon, nlat, ndays))
  vars <- list(
    list(name = "lon", type = "NC_DOUBLE", dims = "lon", data = c(10, 20, 30),
         atts = c(list(units = "degrees_east"), if (bounds) list(bounds = "lon_bnds"))),
    list(name = "lat", type = "NC_DOUBLE", dims = "lat", data = c(40, 50),
         atts = c(list(units = "degrees_north"), if (bounds) list(bounds = "lat_bnds"))),
    list(name = "time", type = "NC_DOUBLE", dims = "time", data = seq_len(ndays) - 0.5,
         atts = list(units = "days since 2001-01-01", calendar = "standard")),
    list(name = "crs", type = "NC_INT", dims = NULL, atts = list(grid_mapping_name = "latitude_longitude")),
    list(name = "tas", type = "NC_FLOAT", dims = c("lon", "lat", "time"), data = vals,
         atts = list(units = "K", long_name = "Temperature", grid_mapping = "crs",
                     ancillary_variables = "tas_qc")),
    list(name = "tas_qc", type = "NC_BYTE", dims = c("lon", "lat", "time"), data = array(1L, c(nlon, nlat, ndays)),
         atts = list(long_name = "Quality"))
  )
  if (bounds)
    vars <- c(vars, list(
      list(name = "lon_bnds", type = "NC_DOUBLE", dims = c("nv", "lon"), data = rbind(c(5, 15, 25), c(15, 25, 35))),
      list(name = "lat_bnds", type = "NC_DOUBLE", dims = c("nv", "lat"), data = rbind(c(35, 45), c(45, 55)))))
  .write_nc(c(lon = nlon, lat = nlat, time = ndays, nv = 2L), vars)
}

# --- Printing and describing --------------------------------------------------

test_that("A variable prints its details before and after reading its data", {
  tas <- open_ncdf(.write_tas())[["tas"]]
  out <- capture.output(print(tas))
  expect_true(any(grepl("<Variable> tas", out, fixed = TRUE)))
  expect_true(any(grepl("Long name: Temperature", out, fixed = TRUE)))
  expect_true(any(grepl("Values: (not loaded)", out, fixed = TRUE)))
  expect_true(any(grepl("Coordinate reference system:", out, fixed = TRUE)))
  expect_true(any(grepl("Ancillary variable:", out, fixed = TRUE)))
  invisible(tas$raw())
  out <- capture.output(print(tas))
  expect_true(any(grepl("Values: [1 ... 360] K", out, fixed = TRUE)))
  expect_true(any(grepl("NA: 0 (0.0%)", out, fixed = TRUE)))
})

test_that("A variable with only missing values prints that", {
  grp <- CFGroup$new("", NULL)
  v <- CFVariable$new("v", group = grp, values = c(NA_real_, NA_real_),
                      axes = list(n = makeDiscreteAxis("n", 2, group = grp)))
  expect_output(print(v), "Values: -")
  expect_output(print(v), "NA: 2 (100%)", fixed = TRUE)
})

test_that("A variable without axes prints that", {
  ds <- open_ncdf(.write_nc(c(x = 1L), list(list(name = "s", type = "NC_DOUBLE", dims = NULL, data = 3,
                                                  atts = list(units = "1")))))
  s <- CFVariable$new(ds$root$NC$NCvars[["s"]], group = CFGroup$new("", NULL), axes = list())
  expect_output(print(s), "(none)", fixed = TRUE)
})

test_that("A variable in a subgroup prints its path name", {
  fn <- .write_nc(c(x = 2L), list(
    list(name = "x", type = "NC_DOUBLE", dims = "x", data = c(1, 2)),
    list(name = "g1/v", type = "NC_FLOAT", dims = "x", data = c(1, 2))))
  expect_output(print(open_ncdf(fn)[["/g1/v"]]), "Path name: /g1/v", fixed = TRUE)
})

test_that("A variable describes itself in brief, peek and shard", {
  tas <- open_ncdf(.write_tas())[["tas"]]
  b <- tas$brief()
  expect_equal(b$name, "tas")
  expect_equal(b$axes, "lon, lat, time")
  pk <- tas$peek()
  expect_equal(pk$units, "K")
  expect_equal(pk$axes, "lon, lat, time")
  expect_true(grepl("tas", tas$shard()))
})

test_that("The time axis or time object of a variable is returned", {
  tas <- open_ncdf(.write_tas())[["tas"]]
  expect_s3_class(tas$time("axis"), "CFAxisTime")
  expect_s3_class(tas$time(), "CFTime")
  grp <- CFGroup$new("", NULL)
  v <- CFVariable$new("v", group = grp, values = 1:2, axes = list(n = makeDiscreteAxis("n", 2, group = grp)))
  expect_null(v$time())
})

test_that("A variable without data has no raw data or array", {
  grp <- CFGroup$new("", NULL)
  v <- CFVariable$new("v", group = grp, axes = list(n = makeDiscreteAxis("n", 2, group = grp)))
  expect_null(v$raw())
  expect_null(v$array())
})

test_that("array() flips increasing latitudes so that north is at the top", {
  tas <- open_ncdf(.write_tas())[["tas"]]
  arr <- tas$array()
  expect_equal(dim(arr), c(2L, 3L, 60L))
  expect_equal(dimnames(arr)[[1L]], c("50", "40"))
  expect_equal(arr["50", "10", 1L], tas$raw()["10", "50", 1L])
  v <- as_CF("v", 1:3)
  expect_equal(as.vector(v$array()), 1:3)
})

# --- Subsetting ---------------------------------------------------------------

test_that("Subset arguments must name distinct axes", {
  tas <- open_ncdf(.write_tas())[["tas"]]
  expect_error(tas$subset(lon = 10, lon = 20), "Duplicated axis names")
  expect_error(tas$subset(X = 10, X = 20), "Duplicated axis orientations")
  expect_error(tas$subset(depth = 10), "names not corresponding to an axis")
  expect_s3_class(tas$subset(X = c(10, 20), lat = 40), "CFVariable")
  ds <- open_ncdf(.write_nc(c(x = 1L), list(list(name = "s", type = "NC_DOUBLE", dims = NULL, data = 3))))
  s <- CFVariable$new(ds$root$NC$NCvars[["s"]], group = CFGroup$new("", NULL), axes = list())
  expect_error(s$subset(x = 1), "Cannot subset a scalar variable")
})

test_that("A subset outside the range of an axis is NULL", {
  tas <- open_ncdf(.write_tas())[["tas"]]
  expect_null(tas$subset(lon = c(100, 200)))
})

# --- Summarising --------------------------------------------------------------

test_that("A variable is summarised by period", {
  tas <- open_ncdf(.write_tas())[["tas"]]
  mon <- tas$summarise("tas_mon", mean, "month")
  expect_s3_class(mon, "CFVariable")
  expect_equal(mon$axes[["time"]]$length, 3L)
  expect_equal(unname(mon$raw()[1, 1, 1]), mean(seq(1, by = 6, length.out = 31)))
})

test_that("A summary with several results names them", {
  tas <- open_ncdf(.write_tas())[["tas"]]
  rng <- tas$summarise("lo", range, "month")
  expect_equal(names(rng), c("lo", "result_2"))
  expect_equal(unname(rng[["result_2"]]$raw()[1, 1, 1]), 1 + 6 * 30)
})

test_that("A summary over eras gives a climatology for each era", {
  tas <- open_ncdf(.write_tas(ndays = 800L, bounds = FALSE))[["tas"]]
  res <- tas$summarise("clim", mean, "month", era = list(early = 2001, late = 2002))
  expect_equal(names(res), c("early", "late"))
  expect_s3_class(res$early$axes[["time"]]$time, "CFClimatology")
  expect_null(tas$summarise("clim", mean, "month", era = 1990))
})

test_that("Large summaries are processed in chunks", {
  old <- CF.options$memory_cell_limit
  assign("memory_cell_limit", 200L, envir = CF.options)
  on.exit(assign("memory_cell_limit", old, envir = CF.options))

  tas <- open_ncdf(.write_tas())[["tas"]]
  ref <- suppressWarnings(open_ncdf(.write_tas())[["tas"]]$raw())
  s <- tas$summarise("s", sum, "month")
  # Results have the time axis first
  expect_equal(unname(s$raw()[1, 2, 2]), sum(ref[2, 2, 1:31]))
  m <- tas$summarise("m", mean, "month")
  expect_equal(unname(m$raw()[2, 2, 2]), mean(ref[2, 2, 32:59]))
  md <- suppressWarnings(tas$summarise("md", median, "month"))
  expect_equal(unname(md$raw()[1, 1, 1]), median(ref[1, 1, 1:31]))
})

test_that("A summary over non-contiguous eras reads the data in runs", {
  old <- CF.options$memory_cell_limit
  assign("memory_cell_limit", 1000L, envir = CF.options)
  on.exit(assign("memory_cell_limit", old, envir = CF.options))
  tas <- open_ncdf(.write_tas(ndays = 800L, bounds = FALSE))[["tas"]]
  res <- suppressWarnings(tas$summarise("djf", mean, "season", era = 2001:2002))
  expect_s3_class(res, "CFVariable")
})

# --- Profiles -----------------------------------------------------------------

test_that("A profile is extracted at a location", {
  tas <- open_ncdf(.write_tas())[["tas"]]
  p <- tas$profile(lon = 20, lat = 40)
  expect_s3_class(p, "CFVariable")
  expect_equal(p$name, "location_1")
  expect_equal(dim(p$raw()), c(1L, 1L, 60L))
  expect_equal(as.vector(p$raw()), as.vector(tas$raw()[2, 1, ]))
})

test_that("Profiles at several locations are named", {
  tas <- open_ncdf(.write_tas())[["tas"]]
  p <- tas$profile(lon = c(10, 30, 90), lat = c(40, 50, 50), .names = "a")
  expect_equal(names(p), c("a", "location_2", "location_3"))
  expect_null(p[[3L]])
  p2 <- tas$profile(lon = 10, lat = 40, .names = c("x", "y"))
  expect_equal(p2$name, "x")
})

test_that("Profiles are returned as a table", {
  skip_if_not_installed("data.table")
  tas <- open_ncdf(.write_tas())[["tas"]]
  dt <- tas$profile(lon = c(10, 30), lat = c(40, 50), .as_table = TRUE)
  expect_s3_class(dt, "data.table")
  expect_equal(nrow(dt), 120L)
  expect_equal(attr(dt, "value")$unit, "K")
  expect_error(tas$profile(lon = c(10, 30), lat = 40, .as_table = TRUE), "same length")
})

test_that("A scalar variable cannot be profiled", {
  ds <- open_ncdf(.write_nc(c(x = 1L), list(list(name = "s", type = "NC_DOUBLE", dims = NULL, data = 3))))
  s <- CFVariable$new(ds$root$NC$NCvars[["s"]], group = CFGroup$new("", NULL), axes = list())
  expect_error(s$profile(x = 1), "Cannot profile a scalar variable")
})

# --- Appending and comparing --------------------------------------------------

test_that("Variables are appended along an axis", {
  a <- open_ncdf(.write_tas(ndays = 4L))[["tas"]]
  b <- open_ncdf(.write_tas(ndays = 4L))[["tas"]]$subset(time = c("2001-01-03", "2001-01-05"))
  a <- a$subset(time = c("2001-01-01", "2001-01-03"))
  expect_error(a$append(b, "depth"), "must be the name of an axis")
  expect_error(a$append(as_CF("v", 1:3), "time"), "same number of axes")
  expect_error(a$append(b, "lon"), "is not identical")
  a$append(b, "time")
  expect_equal(a$axes[["time"]]$length, 4L)
  expect_equal(dim(a$raw()), c(3L, 2L, 4L))
})

test_that("Variables are coincident when their axes are identical", {
  tas <- open_ncdf(.write_tas())[["tas"]]
  qc <- open_ncdf(.write_tas())[["tas_qc"]]
  expect_true(tas$is_coincident(qc))
  expect_false(tas$is_coincident(1:3))
  expect_false(tas$is_coincident(as_CF("v", 1:3)))
  expect_false(tas$is_coincident(tas$subset(lon = 10)))
})

test_that("A variable cannot be added twice to a group", {
  ds <- create_ncdf()
  v <- as_CF("v", 1:3)
  ds$add_variable(v)
  expect_error(ds$add_variable(as_CF("v", 4:6)), "already exists in group")
})

# --- Conversion to other formats ----------------------------------------------

test_that("A variable is converted to a terra SpatRaster", {
  skip_if_not_installed("terra")
  tas <- open_ncdf(.write_tas())[["tas"]]
  r <- tas$terra()
  expect_s4_class(r, "SpatRaster")
  expect_equal(terra::nlyr(r), 60L)
  expect_equal(as.vector(terra::ext(r)), c(xmin = 5, xmax = 35, ymin = 35, ymax = 55))

  r2 <- open_ncdf(.write_tas(bounds = FALSE))[["tas"]]$subset(time = "2001-01-01")$terra()
  expect_equal(as.vector(terra::ext(r2)), c(xmin = 5, xmax = 35, ymin = 35, ymax = 55))
  expect_equal(terra::nlyr(r2), 1L)
})

test_that("A 4D variable is converted to a terra SpatRasterDataset", {
  skip_if_not_installed("terra")
  fn <- .write_nc(c(lon = 2L, lat = 2L, lev = 2L, time = 3L), list(
    list(name = "lon", type = "NC_DOUBLE", dims = "lon", data = c(10, 20), atts = list(units = "degrees_east")),
    list(name = "lat", type = "NC_DOUBLE", dims = "lat", data = c(50, 40), atts = list(units = "degrees_north")),
    list(name = "lev", type = "NC_DOUBLE", dims = "lev", data = c(1000, 500), atts = list(units = "hPa")),
    list(name = "time", type = "NC_DOUBLE", dims = "time", data = 0:2, atts = list(units = "days since 2001-01-01")),
    list(name = "ta", type = "NC_FLOAT", dims = c("lon", "lat", "lev", "time"), data = array(1:24, c(2, 2, 2, 3)))))
  r <- open_ncdf(fn)[["ta"]]$terra()
  expect_s4_class(r, "SpatRasterDataset")
  expect_equal(length(r), 3L)
})

test_that("A variable without X and Y axes cannot be converted to terra", {
  skip_if_not_installed("terra")
  expect_error(as_CF("v", 1:3)$terra(), "")
})

test_that("A variable is converted to a data.table", {
  skip_if_not_installed("data.table")
  tas <- open_ncdf(.write_tas(ndays = 2L))[["tas"]]
  dt <- tas$data.table()
  expect_equal(names(dt), c("lon", "lat", "time", "tas"))
  expect_equal(nrow(dt), 12L)
  expect_equal(attr(dt, "value"), list(name = "Temperature", unit = "K"))
  dt2 <- tas$data.table(var_as_column = TRUE)
  expect_equal(names(dt2), c("lon", "lat", "time", ".variable", ".value"))
  dt3 <- as_CF("v", 1:3)$data.table()
  expect_equal(attr(dt3, "value"), list(name = "", unit = ""))
})

# --- Writing ------------------------------------------------------------------

test_that("A variable is written with its grid mapping", {
  ds <- open_ncdf(.write_tas(ndays = 3L))
  fn <- tempfile(fileext = ".nc")
  ds$save(fn)
  ds2 <- open_ncdf(fn)
  expect_s3_class(ds2[["tas"]]$crs, "CFGridMapping")
  expect_equal(ds2[["tas"]]$attribute("grid_mapping"), "crs")
})

test_that("A variable is written packed", {
  ds <- open_ncdf(.write_tas(ndays = 3L))
  fn <- tempfile(fileext = ".nc")
  ds$save(fn, pack = TRUE)
  nc <- RNetCDF::open.nc(fn)
  on.exit(RNetCDF::close.nc(nc))
  expect_equal(RNetCDF::var.inq.nc(nc, "tas")$type, "NC_SHORT")
  vals <- RNetCDF::var.get.nc(nc, "tas", unpack = TRUE)
  expect_equal(as.vector(vals), seq_len(18), tolerance = 1e-3)
})
