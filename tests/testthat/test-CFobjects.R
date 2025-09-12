test_that("numeric axes", {
  ax <- CFAxisNumeric$new("test", values = 0:9)
  expect_equal(ax$name, "test")
  expect_equal(ax$length, 10)
  expect_false(ax$has_resource)
  expect_null(ax$bounds)
  expect_equal(ax$attribute("actual_range"), c(0, 9))
  expect_equal(ax$values, 0:9)
  expect_equal(ax$coordinates, 0:9)
  expect_equal(ax$active_coordinates, "test")

  # Selecting values on axis with increasing values
  expect_equal(ax$indexOf(c(3.1, 3.7, 4.0, 20, 1.7, 4.3), method = "linear"), c(4.1, 4.7, 5.0, NA, 2.7, 5.3))
  expect_equal(ax$indexOf(c(3.1, 3.7, 4.0, 20, 1.7, 4.3), method = "constant"), c(4, 4, 5, NA, 2, 5))

  # Selecting values on axis with decreasing values
  ax <- CFAxisNumeric$new("test", values = 19:3)
  expect_equal(ax$indexOf(c(3.1, 3.7, 4.0, 20, 1.7, 4.3), method = "linear"), c(16.9, 16.3, 16.0, NA, NA, 15.7))
  expect_equal(ax$indexOf(c(3.1, 3.7, 4.0, 20, 1.7, 4.3), method = "constant"), c(17, 17, 16, NA, NA, 16))

  # Selecting values on an axis with 1 value
  ax <- CFAxisNumeric$new("test", values = 19)
  expect_equal(ax$indexOf(c(3.1, 3.7, 4.0, 19, 18.7, 4.3), method = "constant"), c(NA, NA, NA, 1, NA, NA))
  expect_equal(ax$indexOf(c(3.1, 3.7, 4.0, 19, 18.7, 4.3), method = "linear"), c(NA, NA, NA, 1, NA, NA))

  # Selecting on an axis without values
  ax <- CFAxisNumeric$new("test")
  expect_equal(ax$indexOf(c(3.1, 3.7, 20.0, 19, 18.7, 4.3), method = "constant"), c(NA, NA, NA, NA, NA, NA))

  # Slicing axis without values
  expect_null(ax$slice(c(3.2, 5.7)))

  # Slicing axis with one value, no bounds
  ax <- CFAxisNumeric$new("test", values = 7)
  expect_null(ax$slice(c(3.2, 5.7)))

  # Slicing axis with one value, with bounds
  ax$bounds <- CFBounds$new("bnds", values = matrix(c(5, 9), nrow = 2))
  expect_equal(ax$slice(c(3.2, 5.7)), c(1L, 1L))

  # Slicing axis with increasing values, no bounds
  ax <- CFAxisNumeric$new("test", values = 0:9)
  expect_equal(ax$slice(3:5), c(4L, 6L))
  expect_null(ax$slice(-5:-0.05))            # All below
  expect_null(ax$slice(50:55))               # All above
  expect_equal(ax$slice(-5:1), c(1L, 2L))    # Part below
  expect_equal(ax$slice(8:55), c(9L, 10L))   # Part above

  # Slicing axis with increasing values, with partially non-contiguous bounds
  ax$bounds <- CFBounds$new("bnds", values = matrix(c(0:4 - 0.5, 5:9 - 0.25, 0:4 + 0.5, 5:9 + 0.25), nrow = 2, byrow = TRUE))
  expect_equal(ax$slice(3:5), c(4L, 6L))
  expect_equal(ax$slice(c(-5, -0.05)), c(1L, 1L))   # -0.05 is within lowest boundary value
  expect_null(ax$slice(50:55))                      # All above
  expect_equal(ax$slice(-5:1), c(1L, 2L))           # Part below
  expect_equal(ax$slice(8:55), c(9L, 10L))          # Part above
  expect_equal(ax$slice(c(9.2, 10)), c(10L, 10L))   # 9.2 is within high boundary value
  expect_equal(ax$slice(c(6.6, 8.5)), c(8L, 9L))    # range in between boundary values

  # Slicing axis with decreasing values, with partially non-contiguous bounds
  ax <- CFAxisNumeric$new("test", values = 9:0)
  ax$bounds <- CFBounds$new("bnds", values = matrix(c(9:4 + 0.5, 3:0 + 0.25, 9:4 - 0.5, 3:0 - 0.25), nrow = 2, byrow = TRUE))
  expect_equal(ax$slice(3:5), c(5L, 7L))
  expect_equal(ax$slice(c(-5, -0.05)), c(10L, 10L)) # -0.05 is within highest boundary value
  expect_null(ax$slice(50:55))                      # All below
  expect_equal(ax$slice(-5:1), c(9L, 10L))          # Part below
  expect_equal(ax$slice(8:55), c(1L, 2L))           # Part below
  expect_equal(ax$slice(c(9.2, 10)), c(1L, 1L))     # 9.2 is within lower boundary value
  expect_equal(ax$slice(c(2.5, 0.5)), c(8L, 9L))    # range in between boundary values
})

test_that("discrete axes", {
  ax <- CFAxisDiscrete$new("disc", count = 13)
  expect_equal(ax$values, 1:13)

  # Slicing a discrete axis
  expect_equal(ax$slice(8:10), c(8L, 10L))
  expect_null(ax$slice(-5:-3))
  expect_equal(ax$slice(c(4.67, 6.23)), c(5L, 6L))
  expect_equal(ax$slice(-5:20), c(1L, 13L))
})


test_that("Create from scratch", {
  arr <- array(rnorm(120), dim = c(6, 5, 4))
  da <- as_CF("my_first_CF_object", arr)
  expect_equal(da$name, "my_first_CF_object")
  expect_equal(names(da$axes), c("axis_1", "axis_2", "axis_3"))

  dimnames(arr) <- list(y = c(40, 41, 42, 43, 44, 45), x = c(0, 1, 2, 3, 4),
                        time = c("2025-07-01", "2025-07-02", "2025-07-03", "2025-07-04"))
  da <- as_CF("better_CF_object", arr)
  expect_true(da$id < 0L)
  expect_equal(names(da$axes), c("y", "x", "time"))
  taxis <- da$axes[["time"]]
  expect_true(taxis$id < 0L)
  expect_true(inherits(taxis, "CFAxisTime"))
  t <- taxis$time
  expect_true(inherits(t, "CFTime"))
  t$bounds <- TRUE
  taxis$bounds <- CFBounds$new("time_bnds", values = t$bounds)
  expect_equal(t$range(bounds = TRUE), c("2025-06-30T12:00:00", "2025-07-04T12:00:00"))

  # Write to file and read back in
  fn <- tempfile(fileext = ".nc")
  da$save(fn)
  ds <- open_ncdf(fn)
  expect_equal(names(ds), "better_CF_object")
  expect_equal(ds$axis_names, c("y", "x", "time"))
  dv <- ds[["better_CF_object"]]
  expect_true(dv$id >= 0L)
  taxis2 <- dv$axes[["time"]]
  expect_true(taxis2$id >= 0L)
  expect_equal(taxis2$bounds$values, taxis$bounds$values)
  t2 <- taxis2$time
  expect_equal(t2$range(), t$range())
  expect_equal(t2$range(bounds = TRUE), t$range(bounds = TRUE))
  arr2 <- dv$raw()
  expect_true(all(dim(arr2) == dim(arr)))
  expect_true(identical(arr2, arr))
  unlink(fn)

  # Write to file in canonical axis order and read back in
  dimnames(arr) <- list(latitude = c(40, 41, 42, 43, 44, 45), longitude = c(0, 1, 2, 3, 4),
                        time = c("2025-07-01", "2025-07-02", "2025-07-03", "2025-07-04"))
  da <- as_CF("compliant_CF_object", arr)
  ap7_a9 <- CFLabel$new("ap7_a9", c("Castellon-de-la-Plana", "L-Hospitalet-de-l-Infant", "Girona", "Sigean", "Avignon", "Pont-de-l-Isere"))
  da$add_auxiliary_coordinate(ap7_a9, da$axes[["latitude"]])
  fn <- tempfile(fileext = ".nc")
  da$save(fn)
  ds <- open_ncdf(fn)
  dv <- ds[["compliant_CF_object"]]
  expect_equal(names(dv$axes), c("longitude", "latitude", "time"))
  expect_true(inherits(dv$axes[["longitude"]], "CFAxisLongitude"))
  expect_true(inherits(dv$axes[["latitude"]], "CFAxisLatitude"))
  expect_true(inherits(dv$axes[["time"]], "CFAxisTime"))
  expect_equal(dv$attribute("coordinates"), "ap7_a9")
  expect_equal(dv$axes[["latitude"]]$coordinate_names, c("latitude", "ap7_a9"))
  arr3 <- dv$raw()
  expect_true(all(dim(arr3)[1] == dim(arr)[2], dim(arr3)[2] == dim(arr)[1], dim(arr3)[3] == dim(arr)[3]))
  expect_true(identical(aperm(arr3, c(2, 1, 3)), arr))
  unlink(fn)
})

