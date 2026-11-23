# CFBounds: boundary values of axes.

test_that("Bounds print their values, short and long", {
  grp <- CFGroup$new("", NULL)
  b <- CFBounds$new("b", group = grp, values = rbind(0:2, 1:3))
  b$set_attribute("long_name", "NC_CHAR", "Boundaries")
  out <- capture.output(print(b))
  expect_true(any(grepl("Long name  : Boundaries", out)))
  expect_true(any(grepl("Bounds     : 0, 1, 2", out)))
  expect_true(any(grepl("           : 1, 2, 3", out)))

  long <- CFBounds$new("bl", group = grp, values = rbind(0:9, 1:10))
  expect_output(print(long), "Bounds     : 0, 1, 2 ... 7, 8, 9")
  expect_equal(long$friendlyClassName, "Boundary values object")
})

test_that("Bounds with more than two vertices are not printed", {
  b <- CFBounds$new("b", group = CFGroup$new("", NULL), values = matrix(1:12, nrow = 4))
  expect_output(print(b), "can't print multi-dimensional bounds")
  expect_equal(b$vertices, 4L)
  expect_equal(b$length, 3L)
})

test_that("Bounds without values say so", {
  b <- CFBounds$new("b", group = CFGroup$new("", NULL))
  expect_output(b$print_boundary_values(), "(no values)", fixed = TRUE)
  expect_null(b$range())
})

test_that("Bounds report their range", {
  b <- CFBounds$new("b", group = CFGroup$new("", NULL), values = rbind(0:2, 1:3))
  expect_equal(b$range(), c(0, 3))
})

test_that("Bounds values can be replaced with values of the same length", {
  b <- CFBounds$new("b", group = CFGroup$new("", NULL), values = rbind(0:2, 1:3))
  b$values <- rbind(10:12, 11:13)
  expect_equal(b$values, rbind(10:12, 11:13))
})

test_that("Bounds are copied, subset and appended in memory", {
  grp <- CFGroup$new("", NULL)
  b <- CFBounds$new("b", group = grp, values = rbind(0:3, 1:4))
  expect_equal(b$copy(group = CFGroup$new("", NULL))$values, rbind(0:3, 1:4))
  expect_equal(b$subset(CFGroup$new("", NULL), NULL)$values, rbind(0:3, 1:4))
  expect_equal(b$subset(CFGroup$new("", NULL), c(2, 3))$values, rbind(1:2, 2:3))
  more <- CFBounds$new("b", group = CFGroup$new("", NULL), values = rbind(4:5, 5:6))
  expect_equal(b$append(more, CFGroup$new("", NULL))$values, rbind(0:5, 1:6))
  expect_null(b$append(NULL, grp))
})

test_that("Bounds on file are copied and subset", {
  fn <- .write_nc(c(lat = 3L, nv = 2L), list(
    list(name = "lat", type = "NC_DOUBLE", dims = "lat", data = c(0, 10, 20),
         atts = list(units = "degrees_north", bounds = "lat_bnds")),
    list(name = "lat_bnds", type = "NC_DOUBLE", dims = c("nv", "lat"), data = rbind(c(-5, 5, 15), c(5, 15, 25))),
    list(name = "v", type = "NC_FLOAT", dims = "lat", data = c(1, 2, 3))))
  b <- open_ncdf(fn)[["lat"]]$bounds
  expect_output(print(b), "Boundary values object")
  expect_equal(b$copy("b2", CFGroup$new("", NULL))$name, "b2")
  sub <- b$subset(CFGroup$new("", NULL), c(2, 3))
  expect_equal(sub$values, rbind(c(5, 15), c(15, 25)))
})

test_that("Bounds with additional dimensions use the first layer", {
  # CMIP-style bounds that also vary in time
  bv <- array(c(-5, 5, 5, 15, 15, 25, -6, 6, 6, 16, 16, 26), c(2, 3, 2))
  fn <- .write_nc(c(lat = 3L, nv = 2L, time = 2L), list(
    list(name = "lat", type = "NC_DOUBLE", dims = "lat", data = c(0, 10, 20),
         atts = list(units = "degrees_north", bounds = "lat_bnds")),
    list(name = "lat_bnds", type = "NC_DOUBLE", dims = c("nv", "lat", "time"), data = bv),
    list(name = "v", type = "NC_FLOAT", dims = c("lat", "time"), data = array(1:6, c(3, 2)))))
  b <- open_ncdf(fn)[["lat"]]$bounds
  expect_equal(b$values, rbind(c(-5, 5, 15), c(5, 15, 25)))
  sub <- b$subset(CFGroup$new("", NULL), c(1, 2))
  expect_equal(sub$values, rbind(c(-5, 5), c(5, 15)))
})

test_that("Bounds need an integer id of their owner to be written", {
  b <- CFBounds$new("b", group = CFGroup$new("", NULL), values = rbind(0:2, 1:3))
  expect_error(b$write("x"), "Must have integer id")
})

test_that("Bounds of longitude and latitude are written to Zarr in degrees", {
  skip_if_not_installed("zarr")
  skip_if_not_installed("geozarr")
  lon <- seq(0.5, 39.5, by = 1) + c(0, rep(0.1, 39))
  fn <- .write_nc(c(lon = 40L, lat = 2L, nv = 2L), list(
    list(name = "lon", type = "NC_DOUBLE", dims = "lon", data = lon,
         atts = list(units = "degrees_east", bounds = "lon_bnds")),
    list(name = "lon_bnds", type = "NC_DOUBLE", dims = c("nv", "lon"), data = rbind(lon - c(0.4, rep(0.45, 39)), lon + 0.6),
         atts = list(units = "degrees_east")),
    list(name = "lat", type = "NC_DOUBLE", dims = "lat", data = c(40, 50), atts = list(units = "degrees_north")),
    list(name = "v", type = "NC_FLOAT", dims = c("lon", "lat"), data = array(1:80, c(40, 2)))))
  z <- open_ncdf(fn)$geozarr()
  b <- z$get_node("/coordinates/lon_bounds")
  expect_false(is.null(b))
  expect_equal(b$attributes$units, "degree")
})
