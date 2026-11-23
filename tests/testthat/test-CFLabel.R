# CFLabel: sets of labels for an axis.

.write_labels <- function(n = 3L, labels = c("abc", "def", "ghi")) {
  .write_nc(c(station = n, strlen = 3L), list(
    list(name = "station_name", type = "NC_CHAR", dims = c("strlen", "station"), data = labels,
         atts = list(long_name = "Station name")),
    list(name = "tas", type = "NC_FLOAT", dims = "station", data = seq_len(n) * 1.0,
         atts = list(coordinates = "station_name"))))
}

test_that("A label set prints its details", {
  lbl <- open_ncdf(.write_labels())[["station_name"]]
  out <- capture.output(print(lbl))
  expect_true(any(grepl("<Label set> station_name", out, fixed = TRUE)))
  expect_true(any(grepl("Long name: Station name", out, fixed = TRUE)))
  expect_true(any(grepl("Length   : 3", out, fixed = TRUE)))
  expect_equal(lbl$friendlyClassName, "Label set")
})

test_that("Label sets are compared", {
  grp <- CFGroup$new("", NULL)
  a <- CFLabel$new("l", group = grp, values = c("x", "y"))
  expect_true(a$identical(CFLabel$new("l", group = CFGroup$new("", NULL), values = c("x", "y"))))
  expect_false(a$identical(CFLabel$new("l", group = CFGroup$new("", NULL), values = c("x", "z"))))
})

test_that("A label set is sliced by its values", {
  lbl <- open_ncdf(.write_labels())[["station_name"]]
  expect_equal(lbl$slice(c("abc", "def")), c(1L, 2L))
  expect_equal(lbl$slice(c("def", "zzz")), c(2L, 2L))
  expect_equal(lbl$slice(c("zzz", "abc")), c(1L, 1L))
  expect_null(lbl$slice(c("yyy", "zzz")))
})

test_that("A label set is copied, on file and in memory", {
  lbl <- open_ncdf(.write_labels())[["station_name"]]
  cp <- lbl$copy("names", CFGroup$new("", NULL))
  expect_equal(cp$name, "names")
  expect_equal(cp$values, c("abc", "def", "ghi"))
  mem <- CFLabel$new("l", group = CFGroup$new("", NULL), values = c("x", "y"))
  expect_equal(mem$copy(group = CFGroup$new("", NULL))$values, c("x", "y"))
})

test_that("A label set is subset, on file and in memory", {
  lbl <- open_ncdf(.write_labels())[["station_name"]]
  expect_equal(lbl$subset(group = CFGroup$new("", NULL), rng = NULL)$values, c("abc", "def", "ghi"))
  expect_null(lbl$subset(group = CFGroup$new("", NULL), rng = c(0, 2)))
  expect_null(lbl$subset(group = CFGroup$new("", NULL), rng = c(2, 4)))
  mem <- CFLabel$new("l", group = CFGroup$new("", NULL), values = c("x", "y", "z"))
  expect_equal(mem$subset("l2", CFGroup$new("", NULL), c(2, 3))$values, c("y", "z"))
})

test_that("The values of a label set are replaced with values of the same length", {
  lbl <- CFLabel$new("l", group = CFGroup$new("", NULL), values = c("x", "y"))
  lbl$values <- c("p", "q")
  expect_equal(lbl$values, c("p", "q"))
  lbl$values <- c("p", "q", "r")
  expect_equal(lbl$values, c("p", "q"))
})

test_that("A label set without values on file reports its length", {
  lbl <- open_ncdf(.write_labels())[["station_name"]]
  lbl$.__enclos_env__$private$.values <- NULL
  expect_equal(lbl$length, 3L)
})

test_that("Label sets are written to a Zarr store, short and long", {
  skip_if_not_installed("zarr")
  skip_if_not_installed("geozarr")
  .labelled <- function(n) {
    labels <- sprintf("s%02d", seq_len(n))
    .write_nc(c(station = n, strlen = 3L, lon = 2L, lat = 2L), list(
      list(name = "lon", type = "NC_DOUBLE", dims = "lon", data = c(10, 20), atts = list(units = "degrees_east")),
      list(name = "lat", type = "NC_DOUBLE", dims = "lat", data = c(40, 50), atts = list(units = "degrees_north")),
      list(name = "station_name", type = "NC_CHAR", dims = c("strlen", "station"), data = labels),
      list(name = "tas", type = "NC_FLOAT", dims = c("lon", "lat", "station"), data = array(seq_len(4 * n) * 1.0, c(2, 2, n)),
           atts = list(coordinates = "station_name"))))
  }
  expect_s3_class(open_ncdf(.labelled(3L))$geozarr(), "zarr")
  z <- open_ncdf(.labelled(40L))$geozarr()
  expect_false(is.null(z$get_node("/coordinates/station_name")))
})

test_that("A subset of a subset of a label refers to the right records on file", {
  lbl <- open_ncdf(.write_labels(5L, c("abc", "def", "ghi", "jkl", "mno")))[["station_name"]]
  sub <- lbl$subset(group = CFGroup$new("", NULL), rng = c(2L, 5L))
  expect_equal(sub$NC_map$start, c(1L, 2L))
  sub2 <- sub$subset(group = CFGroup$new("", NULL), rng = c(2L, 3L))
  expect_equal(sub2$NC_map$start, c(1L, 3L))
  expect_equal(sub2$NC_map$count, c(3L, 2L))
  expect_equal(as.vector(sub2$read_data(refresh = TRUE)), c("ghi", "jkl"))
})
