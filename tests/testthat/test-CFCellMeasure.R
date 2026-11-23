# CFCellMeasure: cell measures, internal to the data set or external.

.write_measures <- function(external = FALSE, x = c(1, 2)) {
  vars <- list(
    list(name = "x", type = "NC_DOUBLE", dims = "x", data = x),
    list(name = "y", type = "NC_DOUBLE", dims = "y", data = c(1, 2, 3)),
    list(name = "tas", type = "NC_FLOAT", dims = c("x", "y"), data = array(1:6, c(2, 3)),
         atts = list(cell_measures = "area: cell_area")))
  gatts <- list()
  if (external)
    gatts <- list(external_variables = "cell_area")
  else
    vars <- c(vars, list(list(name = "cell_area", type = "NC_DOUBLE", dims = c("x", "y"),
                              data = array(10, c(2, 3)), atts = list(long_name = "Area of cells", units = "m2"))))
  .write_nc(c(x = 2L, y = 3L), vars, gatts = gatts)
}

.write_area <- function(x = c(1, 2)) {
  .write_nc(c(x = 2L, y = 3L), list(
    list(name = "x", type = "NC_DOUBLE", dims = "x", data = x),
    list(name = "y", type = "NC_DOUBLE", dims = "y", data = c(1, 2, 3)),
    list(name = "cell_area", type = "NC_DOUBLE", dims = c("x", "y"), data = array(10, c(2, 3)))))
}

test_that("An internal cell measure is linked to its data variable", {
  ds <- open_ncdf(.write_measures())
  cm <- ds[["tas"]]$cell_measures[[1L]]
  expect_s3_class(cm, "CFCellMeasure")
  expect_equal(cm$measure, "area")
  expect_equal(cm$name, "cell_area")
  expect_s3_class(cm$data(), "CFVariable")
  out <- capture.output(print(cm))
  expect_true(any(grepl("Measure  : area", out, fixed = TRUE)))
  expect_true(any(grepl("Long name: Area of cells", out, fixed = TRUE)))
  expect_true(any(grepl("Linked to: tas", out, fixed = TRUE)))
  expect_identical(cm$detach(), cm)
})

test_that("An external cell measure is reported and linked to its file", {
  ds <- open_ncdf(.write_measures(external = TRUE))
  expect_output(print(ds), "External variable: cell_area")
  cm <- ds[["tas"]]$cell_measures[[1L]]
  expect_output(print(cm), "external (not linked)", fixed = TRUE)
  fn <- .write_area()
  cm$link(fn)
  expect_output(print(cm), paste("Location :", fn), fixed = TRUE)
  expect_s3_class(cm$data(), "CFVariable")
})

test_that("Linking an external cell measure checks the axes", {
  ds <- open_ncdf(.write_measures(external = TRUE))
  cm <- ds[["tas"]]$cell_measures[[1L]]
  expect_error(cm$link(.write_area(x = c(5, 6))), "not identical")
})

test_that("A cell measure needs a valid measure and compatible axes", {
  expect_error(CFCellMeasure$new("length", "cl"), "Invalid 'measure'")
  cm <- CFCellMeasure$new("volume", "cv")
  expect_equal(cm$measure, "volume")
  cm$name <- "cv2"
  expect_equal(cm$name, "cv2")
  ds <- open_ncdf(.write_measures())
  int <- ds[["tas"]]$cell_measures[[1L]]
  other <- open_ncdf(.write_nc(c(z = 2L), list(
    list(name = "z", type = "NC_DOUBLE", dims = "z", data = c(1, 2)),
    list(name = "v", type = "NC_FLOAT", dims = "z", data = c(1, 2)))))[["v"]]
  expect_error(int$register(other), "Incompatible sets of axes")
})

test_that("An unmatched cell measure gives a warning", {
  fn <- .write_measures(external = TRUE)
  nc <- RNetCDF::open.nc(fn, write = TRUE)
  RNetCDF::att.delete.nc(nc, "NC_GLOBAL", "external_variables")
  RNetCDF::close.nc(nc)
  expect_warning(open_ncdf(fn), "Unmatched `cell_measures` value 'cell_area'")
})
