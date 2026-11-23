# The axis classes: CFAxis and its numeric, latitude, longitude, character and
# discrete descendants.

# A file with axes of every kind, all with a label set or bounds where useful.
.write_axes <- function() {
  .write_nc(c(lon = 3L, lat = 3L, depth = 4L, name = 2L, n = 3L, nv = 2L, strlen = 3L), list(
    list(name = "lon", type = "NC_DOUBLE", dims = "lon", data = c(10, 20, 30),
         atts = list(units = "degrees_east", long_name = "Longitude", bounds = "lon_bnds")),
    list(name = "lon_bnds", type = "NC_DOUBLE", dims = c("nv", "lon"), data = array(c(5, 15, 15, 25, 25, 35), c(2, 3))),
    list(name = "lat", type = "NC_DOUBLE", dims = "lat", data = c(-10, 0, 10), atts = list(units = "degrees_north")),
    list(name = "depth", type = "NC_DOUBLE", dims = "depth", data = c(1, 2, 3, 4), atts = list(units = "1")),
    list(name = "name", type = "NC_STRING", dims = "name", data = c("a", "b"), atts = list(long_name = "names")),
    list(name = "n_label", type = "NC_CHAR", dims = c("strlen", "n"), data = c("one", "two", "thr")),
    list(name = "v", type = "NC_FLOAT", dims = c("lon", "lat", "depth", "name", "n"),
         data = array(1:216, c(3, 3, 4, 2, 3)), atts = list(coordinates = "n_label"))
  ))
}

# --- CFAxis base class --------------------------------------------------------

test_that("An axis prints its details", {
  ax <- open_ncdf(.write_axes())[["lon"]]
  out <- capture.output(print(ax))
  expect_true(any(grepl("Long name  : Longitude", out, fixed = TRUE)))
  expect_true(any(grepl("Bounds", out)))
})

test_that("An axis in a subgroup prints its path name", {
  fn <- .write_nc(c(x = 2L), list(
    list(name = "g1/x", type = "NC_DOUBLE", dims = "x", data = c(1, 2)),
    list(name = "g1/v", type = "NC_FLOAT", dims = "x", data = c(1, 2))))
  expect_output(print(open_ncdf(fn)[["/g1/x"]]), "Path name  : /g1/x")
})

test_that("An axis with label sets prints them and uses the active label set", {
  ds <- open_ncdf(.write_axes())
  n <- ds[["v"]]$axes[["n"]]
  expect_output(print(n), "Label sets : n, n_label")
  n$active_coordinates <- "n_label"
  expect_equal(n$active_coordinates, "n_label")
  expect_equal(n$coordinates, c("one", "two", "thr"))
  expect_equal(n$brief()$long_name, "n_label")
  expect_equal(n$slice(c("two", "thr")), c(2L, 3L))
  n$active_coordinates <- "no_such_set"
  expect_equal(n$active_coordinates, "n_label")
  n$active_coordinates <- "n"
  expect_null(n$auxiliary)
})

test_that("An axis describes itself in brief, peek, shard and str", {
  ax <- open_ncdf(.write_axes())[["depth"]]
  expect_equal(ax$brief()$unit, "")
  pk <- ax$peek()
  expect_equal(pk$name, "depth")
  expect_equal(pk$length, 4L)
  expect_false(pk$has_bounds)
  expect_equal(ax$shard(), "[depth (4): 1 ... 4]")
  expect_output(str(ax), "Numeric axis")
  expect_equal(dim(ax), 4L)
  expect_equal(dimnames(ax), c(1, 2, 3, 4))
  expect_true(ax$regular)
  expect_equal(ax$coordinate_range, c(1, 4))
})

test_that("A single-value axis has a compact shard", {
  grp <- CFGroup$new("", NULL)
  ax <- CFAxisNumeric$new("h", group = grp, values = 2)
  expect_equal(ax$shard(), "[h (1): 2]")
  expect_output(print(ax), "Coordinates: 2")
})

test_that("An unlimited axis is marked as such", {
  grp <- CFGroup$new("", NULL)
  ax <- CFAxisNumeric$new("t", group = grp, values = 1:3)
  ax$unlimited <- TRUE
  expect_true(ax$unlimited)
  expect_equal(ax$shard(), "[t (3-U): 1 ... 3]")
  expect_equal(ax$brief()$length, "3-U")
  expect_output(print(ax), "(unlimited)", fixed = TRUE)
})

test_that("The generic axis methods have no effect in the base class", {
  grp <- CFGroup$new("", NULL)
  ax <- CFAxis$new("a", group = grp, values = 1:3)
  expect_null(ax$copy(group = grp))
  expect_null(ax$copy_with_values(group = grp, values = 1:2))
  expect_null(ax$subset(group = grp))
  expect_null(ax$copy_terms())
  expect_null(ax$configure_terms())
  expect_null(ax$time)
  expect_false(ax$is_parametric)
  expect_equal(ax$friendlyClassName, "Generic CF axis")
  expect_error(ax$indexOf(1), "must be implemented")
})

test_that("Axes are compared and checked for appending", {
  grp <- CFGroup$new("", NULL)
  a <- CFAxisNumeric$new("a", group = grp, values = 1:3)
  b <- CFAxisNumeric$new("a", group = CFGroup$new("", NULL), values = 1:3)
  expect_true(a$identical(b))
  expect_false(a$identical(CFAxisNumeric$new("a", group = CFGroup$new("", NULL), values = 2:4)))
  expect_true(a$can_append(b))
  expect_false(a$can_append(CFAxisCharacter$new("a", group = CFGroup$new("", NULL), values = c("x", "y", "z"))))
})

# --- Numeric axis -------------------------------------------------------------

test_that("A numeric axis prints long coordinate lists in short", {
  grp <- CFGroup$new("", NULL)
  ax <- CFAxisNumeric$new("x", group = grp, values = 1:10)
  ax$set_attribute("units", "NC_CHAR", "m")
  expect_output(print(ax), "Coordinates: 1, 2, 3 ... 8, 9, 10 (m)", fixed = TRUE)
})

test_that("A numeric axis with bounds finds indices with the bounds", {
  grp <- CFGroup$new("", NULL)
  ax <- makeAxis("x", values = c(1, 2, 3), bounds = rbind(c(0.5, 1.5, 2.5), c(1.5, 2.5, 3.5)), group = grp)
  expect_equal(ax$indexOf(c(0.7, 2.2, 3.4, 5), method = "constant"), c(1, 2, 3, NA))
})

test_that("A numeric axis is copied, subset and appended in memory", {
  grp <- CFGroup$new("", NULL)
  ax <- makeAxis("x", values = c(1, 2, 3), bounds = rbind(c(0.5, 1.5, 2.5), c(1.5, 2.5, 3.5)), group = grp)
  expect_equal(ax$copy(group = grp)$values, c(1, 2, 3))
  expect_equal(ax$copy_with_values("x2", grp, c(7, 8))$name, "x2")
  expect_equal(ax$copy_with_values(group = grp, values = c(7, 8))$name, "x")
  expect_equal(ax$subset(group = grp)$values, c(1, 2, 3))
  sub <- ax$subset("xs", grp, c(2, 3))
  expect_equal(sub$values, c(2, 3))
  expect_equal(sub$bounds$values, rbind(c(1.5, 2.5), c(2.5, 3.5)))

  more <- makeAxis("x", values = c(4, 5), bounds = rbind(c(3.5, 4.5), c(4.5, 5.5)), group = grp)
  ap <- ax$append(more, grp)
  expect_equal(ap$values, c(1, 2, 3, 4, 5))
  expect_equal(ncol(ap$bounds$values), 5L)
  expect_error(ax$append(ax, grp), "cannot be appended")
})

test_that("A numeric axis on file is copied and subset", {
  ax <- open_ncdf(.write_axes())[["depth"]]
  grp <- CFGroup$new("", NULL)
  expect_equal(ax$copy("d2", grp)$name, "d2")
  sub <- ax$subset("d3", grp, c(2, 3))
  expect_equal(sub$name, "d3")
  expect_equal(sub$values, c(2, 3))
})

# --- Longitude and latitude axes ----------------------------------------------

test_that("A longitude axis is copied, subset and appended", {
  grp <- CFGroup$new("", NULL)
  ax <- open_ncdf(.write_axes())[["lon"]]
  expect_equal(ax$friendlyClassName, "Longitude axis")
  expect_equal(ax$copy("lon2", grp)$name, "lon2")
  cp <- ax$copy(group = grp)
  expect_equal(cp$name, "lon")
  expect_equal(ax$subset(group = grp)$values, c(10, 20, 30))
  expect_equal(ax$subset("lon3", grp, c(1, 2))$name, "lon3")

  mem <- makeLongitudeAxis("lon", c(10, 20), bounds = rbind(c(5, 15), c(15, 25)), group = grp)
  expect_equal(mem$copy(group = grp)$values, c(10, 20))
  expect_equal(mem$copy_with_values(group = grp, values = c(1, 2))$values, c(1, 2))
  ap <- mem$append(makeLongitudeAxis("lon", c(30, 40), bounds = rbind(c(25, 35), c(35, 45)), group = grp), grp)
  expect_equal(ap$values, c(10, 20, 30, 40))
  expect_error(mem$append(mem, grp), "cannot be appended")
})

test_that("A latitude axis is copied, subset and appended", {
  grp <- CFGroup$new("", NULL)
  ax <- open_ncdf(.write_axes())[["lat"]]
  expect_equal(ax$friendlyClassName, "Latitude axis")
  expect_equal(ax$copy("lat2", grp)$name, "lat2")
  expect_equal(ax$subset(group = grp)$values, c(-10, 0, 10))
  expect_equal(ax$subset("lat3", grp, c(1, 2))$name, "lat3")

  mem <- makeLatitudeAxis("lat", c(10, 20), bounds = rbind(c(5, 15), c(15, 25)), group = grp)
  expect_equal(mem$copy(group = grp)$values, c(10, 20))
  expect_equal(mem$copy_with_values("lat4", grp, c(1, 2))$name, "lat4")
  ap <- mem$append(makeLatitudeAxis("lat", c(30, 40), bounds = rbind(c(25, 35), c(35, 45)), group = grp), grp)
  expect_equal(ap$values, c(10, 20, 30, 40))
  expect_error(mem$append(mem, grp), "cannot be appended")
})

test_that("Latitude values outside the domain give a warning", {
  expect_warning(CFAxisLatitude$new("lat", group = CFGroup$new("", NULL), values = c(0, 100)), "not in the domain")
  expect_true(.check_latitude_domain(numeric(0)))
  expect_true(.check_longitude_domain(numeric(0)))
})

# --- Character axis -----------------------------------------------------------

test_that("A character axis on file is read and described", {
  ax <- open_ncdf(.write_axes())[["name"]]
  expect_s3_class(ax, "CFAxisCharacter")
  expect_equal(ax$friendlyClassName, "Character axis")
  expect_equal(ax$values, c("a", "b"))
  expect_equal(ax$brief()$values, '["a" ... "b"]')
  expect_equal(ax$dimnames, c("a", "b"))
  expect_equal(ax$indexOf(c("b", "z")), c(2L, NA))
})

test_that("A character axis is sliced by its values", {
  ax <- open_ncdf(.write_axes())[["name"]]
  expect_equal(ax$slice(c("a", "b")), c(1L, 2L))
  expect_equal(ax$slice(c("b", "z")), c(2L, 2L))
  expect_equal(ax$slice(c("z", "a")), c(1L, 1L))
  expect_null(ax$slice(c("y", "z")))
})

test_that("A character axis is copied and subset, on file and in memory", {
  grp <- CFGroup$new("", NULL)
  ax <- open_ncdf(.write_axes())[["name"]]
  expect_equal(ax$copy("n2", grp)$values, c("a", "b"))
  expect_equal(ax$subset(group = grp)$values, c("a", "b"))
  sub <- ax$subset("n3", grp, c(2, 2))
  expect_equal(sub$name, "n3")
  expect_equal(sub$values, "b")
  expect_equal(sub$brief()$values, '["b"]')

  mem <- makeCharacterAxis("s", c("x", "y", "z"), group = grp)
  expect_equal(mem$copy(group = grp)$values, c("x", "y", "z"))
  expect_equal(mem$copy_with_values(group = grp, values = c("p", "q"))$values, c("p", "q"))
  expect_equal(mem$subset("s2", grp, c(1, 2))$values, c("x", "y"))
  expect_equal(mem$subset(group = grp, rng = c(3, 3))$name, "s")
})

test_that("Character axes are compared and appended", {
  grp <- CFGroup$new("", NULL)
  a <- makeCharacterAxis("s", c("x", "y"), group = grp)
  expect_true(a$identical(makeCharacterAxis("s", c("x", "y"), group = CFGroup$new("", NULL))))
  expect_false(a$identical(makeCharacterAxis("s", c("x", "z"), group = CFGroup$new("", NULL))))
  ap <- a$append(makeCharacterAxis("s", c("z"), group = grp), grp)
  expect_equal(ap$values, c("x", "y", "z"))
  expect_error(a$append(a, grp), "not unique")
})

# --- Discrete axis ------------------------------------------------------------

test_that("A discrete axis prints and describes itself", {
  grp <- CFGroup$new("", NULL)
  ax <- makeDiscreteAxis("n", 3, group = grp)
  expect_equal(ax$friendlyClassName, "Discrete axis")
  expect_output(print(ax), "Discrete axis")
  expect_equal(ax$brief()$values, "[1 ... 3]")
  expect_equal(makeDiscreteAxis("m", 1, group = grp)$brief()$values, "[1]")
  expect_equal(ax$indexOf(c(0, 1, 2.6, 4)), c(NA, 1L, 2L, NA))
})

test_that("A discrete axis is copied, subset and appended", {
  grp <- CFGroup$new("", NULL)
  ax <- makeDiscreteAxis("n", 3, group = grp)
  expect_equal(ax$copy(group = grp)$length, 3L)
  expect_equal(ax$copy("n2", grp)$name, "n2")
  expect_equal(ax$subset(group = grp)$length, 3L)
  expect_equal(ax$append(makeDiscreteAxis("n", 2, group = CFGroup$new("", NULL)), CFGroup$new("", NULL))$length, 5L)
  expect_error(ax$append(makeCharacterAxis("n", c("a"), group = CFGroup$new("", NULL)), grp), "cannot be appended")
})

test_that("A discrete axis with a label set slices on the labels", {
  n <- open_ncdf(.write_axes())[["v"]]$axes[["n"]]
  n$active_coordinates <- "n_label"
  expect_equal(n$slice(c("one", "two")), c(1L, 2L))
  sub <- n$subset("n", CFGroup$new("", NULL), c(2, 3))
  expect_equal(sub$coordinate_names, c("n", "n_label"))
})

# --- Writing axes -------------------------------------------------------------

test_that("Axes are written to a new file and read back", {
  ds <- open_ncdf(.write_axes())
  fn <- tempfile(fileext = ".nc")
  ds2 <- create_ncdf()
  ds2$add_variable(ds[["v"]]$subset(lon = c(10, 20)))
  ds2$save(fn)
  ds3 <- open_ncdf(fn)
  expect_true(all(c("lon", "lat", "depth", "name", "n") %in% ds3$axis_names))
  expect_equal(ds3[["lon"]]$values, c(10, 20))
  expect_s3_class(ds3[["lon"]]$bounds, "CFBounds")
})

test_that("An axis is written to an existing dimension or variable on file", {
  fn <- .write_nc(c(x = 3L), list(
    list(name = "x", type = "NC_DOUBLE", dims = "x", data = c(1, 2, 3))))
  ds <- open_ncdf(fn, write = TRUE)
  grp <- ds$root
  ax <- ds[["x"]]
  v <- CFVariable$new("w", group = grp, values = c(4, 5, 6), axes = list(x = ax))
  ds$save()
  ds2 <- open_ncdf(fn)
  expect_true("w" %in% ds2$var_names)
  expect_equal(as.vector(ds2[["w"]]$raw()), c(4, 5, 6))
})

test_that("A 1D variable holding a plain vector is saved", {
  ds <- create_ncdf()
  ds$add_variable(as_CF("v", c(1, 2, 3)))
  fn <- tempfile(fileext = ".nc")
  ds$save(fn)
  expect_equal(as.vector(open_ncdf(fn)[["v"]]$raw()), c(1, 2, 3))
})

test_that("An axis is not written over an incompatible dimension", {
  # Dimension "x" is not used by any variable so it has no axis
  fn <- .write_nc(c(x = 3L, z = 2L), list(list(name = "y", type = "NC_DOUBLE", dims = "z", data = c(1, 2))))
  ds <- open_ncdf(fn, write = TRUE)
  x <- CFAxisNumeric$new("x", group = ds$root, values = c(1, 2))
  expect_error(x$write(), "Incompatible dimension")
  y <- CFAxisNumeric$new("y", group = CFGroup$new("", NULL), values = c(1, 2, 3))
  y$.__enclos_env__$private$.group <- ds$root
  expect_error(y$write(), "Incompatible object")
  ax <- CFAxisNumeric$new("x", group = CFGroup$new("", NULL), values = c(1, 2, 3))
  expect_error(ax$write(), "no netCDF group")
})
