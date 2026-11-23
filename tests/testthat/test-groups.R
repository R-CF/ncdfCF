# CFGroup and NCGroup: the group hierarchy and finding objects in it.

# Groups: / with g1 and g3; g1 with g2. Dimension x is in the root, dimension
# z only in g2.
.write_hierarchy <- function() {
  fn <- tempfile(fileext = ".nc")
  nc <- RNetCDF::create.nc(fn, format = "netcdf4")
  RNetCDF::dim.def.nc(nc, "x", 2L)
  RNetCDF::var.def.nc(nc, "x", "NC_DOUBLE", "x")
  RNetCDF::var.put.nc(nc, "x", c(1, 2))
  RNetCDF::var.def.nc(nc, "top", "NC_FLOAT", "x")
  RNetCDF::att.put.nc(nc, "NC_GLOBAL", "title", "NC_CHAR", "Hierarchy")
  g1 <- RNetCDF::grp.def.nc(nc, "g1")
  RNetCDF::var.def.nc(g1, "v1", "NC_FLOAT", "x")
  g2 <- RNetCDF::grp.def.nc(g1, "g2")
  RNetCDF::dim.def.nc(g2, "z", 3L)
  RNetCDF::var.def.nc(g2, "z", "NC_DOUBLE", "z")
  RNetCDF::var.put.nc(g2, "z", c(1, 2, 3))
  RNetCDF::var.def.nc(g2, "v2", "NC_FLOAT", c("x", "z"))
  g3 <- RNetCDF::grp.def.nc(nc, "g3")
  RNetCDF::var.def.nc(g3, "v3", "NC_FLOAT", "x")
  RNetCDF::close.nc(nc)
  fn
}

# --- CFGroup ------------------------------------------------------------------

test_that("A group prints its details and subgroups", {
  ds <- open_ncdf(.write_hierarchy())
  out <- capture.output(ds$root$print())
  expect_true(any(grepl("<CF Group> \\[.*\\] /", out)))
  expect_true(any(grepl("Subgroups : g1, g3", out, fixed = TRUE)))
  expect_output(ds[["/g1"]]$print(), "Path      : /g1")
  expect_output(CFGroup$new("vg", NULL)$print(), "(virtual)", fixed = TRUE)
})

test_that("The hierarchy of groups lists axes and variables", {
  ds <- open_ncdf(.write_hierarchy())
  hier <- paste(ds$root$hierarchy(), collapse = "")
  expect_true(grepl("* /", hier, fixed = TRUE))
  expect_true(grepl("   * g1", hier, fixed = TRUE))
  expect_true(grepl("Variables: ", hier, fixed = TRUE))
})

test_that("Subgroups are created and added", {
  grp <- CFGroup$new("", NULL)
  sub <- grp$create_subgroup("s1")
  expect_s3_class(sub, "CFGroup")
  expect_equal(sub$fullname, "/s1")
  expect_error(grp$create_subgroup("s1"), "duplicate")
  expect_error(grp$create_subgroup("1s"), "not valid")
  grp$add_subgroups(CFGroup$new("s2", grp))
  expect_equal(unname(grp$subgroup_names()), c("/s1", "/s2"))
  expect_true(grp$has_subgroups)
  expect_equal(sub$root$name, "/")
})

test_that("Objects are found by relative and absolute paths", {
  ds <- open_ncdf(.write_hierarchy())
  g2 <- ds[["/g1/g2"]]
  expect_s3_class(g2, "CFGroup")
  expect_equal(g2$find_by_name("v2")$fullname, "/g1/g2/v2")
  expect_equal(g2$find_by_name("../v1")$fullname, "/g1/v1")
  expect_equal(g2$find_by_name("..")$fullname, "/g1")
  expect_equal(g2$find_by_name("/g3/v3")$fullname, "/g3/v3")
  expect_equal(ds$root$find_by_name("g1/g2/v2")$fullname, "/g1/g2/v2")
  expect_null(ds$root$find_by_name(NULL))
})

test_that("Unqualified names are found upwards and then laterally", {
  ds <- open_ncdf(.write_hierarchy())
  g2 <- ds[["/g1/g2"]]
  expect_equal(g2$find_by_name("top")$fullname, "/top")
  expect_equal(ds[["/g3"]]$find_by_name("v2")$fullname, "/g1/g2/v2")
  expect_null(g2$find_by_name("no_such_thing"))
})

test_that("Adding CF objects checks for names and types", {
  grp <- CFGroup$new("", NULL)
  ax <- CFAxisNumeric$new("a", group = grp, values = 1:2)
  expect_error(grp$add_CF_object(CFAxisNumeric$new("a", group = CFGroup$new("", NULL), values = 1:3), silent = FALSE),
               "already present")
  expect_error(grp$add_CF_object(list(1)), "Unknown CF object type")
  cm <- CFCellMeasure$new("area", "cell_area")
  grp$add_CF_object(list(cm))
  expect_identical(grp$find_by_name("cell_area"), cm)
  expect_equal(names(grp$CFmeasures), "cell_area")
  expect_true(all(c("a", "cell_area") %in% names(grp$CFobjects)))
})

test_that("Groups are renamed, except the root group", {
  ds <- open_ncdf(.write_hierarchy())
  expect_error(ds$root$name <- "root", "Cannot change the name of the root group")
  g3 <- ds[["/g3"]]
  g3$name <- "g4"
  expect_equal(g3$name, "g4")
  expect_equal(g3$fullname, "/g4")
})

test_that("Groups are renamed on file when it is writable", {
  fn <- .write_hierarchy()
  ds <- open_ncdf(fn, write = TRUE)
  g3 <- ds[["/g3"]]
  g3$name <- "g4"
  expect_equal(g3$NC$name, "g4")
  rm(ds); gc()
  expect_true("/g4" %in% groups(open_ncdf(fn)))
})

test_that("A virtual group cannot be written", {
  expect_error(CFGroup$new("", NULL)$write(), "Can't write a virtual group")
})

test_that("A group hierarchy is written to a new file", {
  ds <- open_ncdf(.write_hierarchy())
  fn <- tempfile(fileext = ".nc")
  ds$save(fn)
  expect_setequal(groups(open_ncdf(fn)), c("/", "/g1", "/g1/g2", "/g3"))
})

# --- NCGroup ------------------------------------------------------------------

test_that("A NC group prints its details", {
  ds <- open_ncdf(.write_hierarchy())
  out <- capture.output(ds$root$NC$print())
  expect_true(any(grepl("Subgroups : g1, g3", out, fixed = TRUE)))
  expect_true(any(grepl("title", out)))
  expect_equal(ds$root$NC$friendlyClassName, "NetCDF group")
})

test_that("NC objects are found by path", {
  nc <- open_ncdf(.write_hierarchy())$root$NC
  g2 <- nc$find_by_name("/g1/g2")
  expect_s3_class(g2, "NCGroup")
  expect_s3_class(g2$find_by_name("z"), "NCVariable")
  expect_equal(g2$find_by_name("..")$fullname, "/g1")
  expect_equal(g2$find_by_name("../v1")$name, "v1")
  expect_equal(g2$find_by_name("top")$name, "top")
  expect_equal(nc$find_by_name("g3")$fullname, "/g3")
  expect_equal(nc$find_by_name("g1")$find_by_name("v3")$name, "v3")
  expect_error(nc$find_by_name("/no/v"), "not found")
})

test_that("Dimensions are found by name and by id", {
  nc <- open_ncdf(.write_hierarchy())$root$NC
  g2 <- nc$find_by_name("/g1/g2")
  expect_s3_class(nc$find_by_name("x"), "NCVariable")
  z <- g2$NCdims[["z"]]
  expect_s3_class(z, "NCDimension")
  expect_equal(g2$find_dim_by_id(z$id)$name, "z")
  expect_null(nc$find_dim_by_id(z$id))
  expect_null(nc$find_dim_by_id(NULL))
  expect_equal(g2$find_dimvar_by_name("x")$name, "x")
  expect_equal(nc$find_dimvar_by_name("/g1/g2/z")$name, "z")
})

test_that("Visible dimensions include those of parent groups", {
  nc <- open_ncdf(.write_hierarchy())$root$NC
  g2 <- nc$find_by_name("/g1/g2")
  expect_equal(names(g2$dimensions("local")), "z")
  expect_setequal(sapply(g2$dimensions(), function(d) d$name), c("z", "x"))
  expect_equal(sapply(nc$dimensions(), function(d) d$name), c(x = "x"))
})

test_that("A NC group reports names, unused variables and full names", {
  nc <- open_ncdf(.write_hierarchy())$root$NC
  expect_true(nc$has_name("TOP"))
  expect_false(nc$has_name("v1"))
  expect_length(nc$unused(), 0L)
  expect_setequal(unlist(nc$fullnames()), c("/", "/g1", "/g1/g2", "/g3"))
  expect_equal(nc$find_by_name("/g1/g2")$root$name, "/")
  expect_false(nc$can_write)
  expect_false(is.null(nc$handle))
})

test_that("A NC group only accepts NC variables and dimensions", {
  nc <- open_ncdf(.write_hierarchy())$root$NC
  expect_warning(nc$append(CFGroup$new("", NULL)), "Cannot add object of class CFGroup")
  expect_error(nc$handle <- 1, "Can't assign")
  expect_error(nc$CF <- 1, "Bad CF group assignment")
  expect_error(nc$name <- "root", "Cannot change the name of the root group")
})

test_that("An unreferenced NC variable is reported as unused", {
  fn <- .write_hierarchy()
  ds <- open_ncdf(fn)
  v <- ds$root$NC$NCvars[["top"]]
  v$.__enclos_env__$private$.CFobjects <- list()
  expect_true("top" %in% names(ds$root$NC$unused()))
})
