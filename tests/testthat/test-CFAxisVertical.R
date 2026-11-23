# CFAxisVertical: regular and parametric vertical axes.

# Ocean s-coordinate file. `eta` is "3D" (x, y, time), "2D" (x, y) or "none"
# (no variable on file, so a zero term). `g` is 1 or 2.
.write_ocean_s <- function(g = 1L, eta = "3D") {
  s <- c(-0.875, -0.625, -0.375, -0.125)
  C <- c(-0.9, -0.6, -0.3, -0.1)
  h <- matrix(c(50, 60, 70, 80, 90, 100), 2, 3)
  ft <- "s: s_rho C: Cs_r eta: zeta depth: h depth_c: hc"
  vars <- list(
    list(name = "x", type = "NC_DOUBLE", dims = "x", data = c(1, 2), atts = list(units = "km")),
    list(name = "y", type = "NC_DOUBLE", dims = "y", data = c(1, 2, 3), atts = list(units = "km")),
    list(name = "time", type = "NC_DOUBLE", dims = "time", data = c(0, 1), atts = list(units = "days since 2000-01-01")),
    list(name = "s_rho", type = "NC_DOUBLE", dims = "s_rho", data = s,
         atts = list(standard_name = paste0("ocean_s_coordinate_g", g), positive = "up", formula_terms = ft)),
    list(name = "Cs_r", type = "NC_DOUBLE", dims = "s_rho", data = C),
    list(name = "h", type = "NC_DOUBLE", dims = c("x", "y"), data = h,
         atts = list(units = "m", standard_name = "sea_floor_depth_below_geoid")),
    list(name = "hc", type = "NC_DOUBLE", dims = NULL, data = 10),
    list(name = "temp", type = "NC_FLOAT", dims = c("x", "y", "s_rho", "time"), data = array(1:48, c(2, 3, 4, 2)))
  )
  if (eta == "3D")
    vars <- c(vars, list(list(name = "zeta", type = "NC_DOUBLE", dims = c("x", "y", "time"),
                              data = array(seq(0.1, 1.2, by = 0.1), c(2, 3, 2)))))
  else if (eta == "2D")
    vars <- c(vars, list(list(name = "zeta", type = "NC_DOUBLE", dims = c("x", "y"),
                              data = matrix(seq(0.1, 0.6, by = 0.1), 2, 3))))
  .write_nc(c(x = 2L, y = 3L, s_rho = 4L, time = 2L), vars)
}

# Atmosphere hybrid sigma pressure file, with "a: p0:" or "ap:" terms.
.write_hybrid <- function(ap = FALSE) {
  ft <- if (ap) "ap: hyam b: hybm ps: PS" else "a: hyam b: hybm p0: P0 ps: PS"
  .write_nc(c(lon = 2L, lat = 2L, lev = 3L, time = 2L), list(
    list(name = "lon", type = "NC_DOUBLE", dims = "lon", data = c(10, 20), atts = list(units = "degrees_east")),
    list(name = "lat", type = "NC_DOUBLE", dims = "lat", data = c(40, 50), atts = list(units = "degrees_north")),
    list(name = "time", type = "NC_DOUBLE", dims = "time", data = c(0, 1), atts = list(units = "days since 2000-01-01")),
    list(name = "lev", type = "NC_DOUBLE", dims = "lev", data = c(0.9, 0.5, 0.1),
         atts = list(standard_name = "atmosphere_hybrid_sigma_pressure_coordinate", positive = "down", formula_terms = ft)),
    list(name = "hyam", type = "NC_DOUBLE", dims = "lev", data = if (ap) c(1000, 5000, 9000) else c(0.01, 0.05, 0.09)),
    list(name = "hybm", type = "NC_DOUBLE", dims = "lev", data = c(0.9, 0.5, 0.1)),
    list(name = "P0", type = "NC_DOUBLE", dims = NULL, data = 1e5),
    list(name = "PS", type = "NC_DOUBLE", dims = c("lon", "lat", "time"), data = array(1e5 + 1:8, c(2, 2, 2)),
         atts = list(units = "Pa")),
    list(name = "ta", type = "NC_FLOAT", dims = c("lon", "lat", "lev", "time"), data = array(1:24, c(2, 2, 3, 2)))
  ))
}

test_that("A parametric vertical axis is recognized from its formula terms", {
  ax <- open_ncdf(.write_ocean_s())[["s_rho"]]
  expect_s3_class(ax, "CFAxisVertical")
  expect_true(ax$is_parametric)
  expect_equal(ax$friendlyClassName, "Vertical axis (parametric)")
  expect_equal(ax$formula_terms$term, c("s", "C", "eta", "depth", "depth_c"))
  expect_output(print(ax), "not calculated")
})

test_that("Ocean s-coordinate g1 with time-varying eta is computed", {
  ax <- open_ncdf(.write_ocean_s(1L, "3D"))[["s_rho"]]
  z <- ax$parametric_coordinates
  expect_s3_class(z, "CFVariable")
  expect_equal(ax$computed_name, "altitude")
  vals <- z$raw()
  expect_equal(dim(vals), c(2L, 3L, 4L, 2L))
  # z(n,k,j,i) = S(k,j,i) + eta(n,j,i) * (1 + S(k,j,i) / depth(j,i))
  S <- 10 * -0.375 + (70 - 10) * -0.3
  expect_equal(unname(vals[1, 2, 3, 2]), S + 0.9 * (1 + S / 70))
  expect_equal(z$attribute("units"), "m")
  expect_output(print(ax), "altitude")
})

test_that("Ocean s-coordinate g1 with constant eta is computed", {
  vals <- open_ncdf(.write_ocean_s(1L, "2D"))[["s_rho"]]$parametric_coordinates$raw()
  expect_equal(dim(vals), c(2L, 3L, 4L))
  S <- 10 * -0.375 + (70 - 10) * -0.3
  expect_equal(unname(vals[1, 2, 3]), S + 0.3 * (1 + S / 70))
})

test_that("Ocean s-coordinate g1 without eta on file uses zero", {
  ax <- open_ncdf(.write_ocean_s(1L, "none"))[["s_rho"]]
  vals <- ax$parametric_coordinates$raw()
  expect_equal(unname(vals[1, 2, 3]), 10 * -0.375 + (70 - 10) * -0.3)
})

test_that("Ocean s-coordinate g2 is computed for all forms of eta", {
  # z(n,k,j,i) = eta(n,j,i) + (eta(n,j,i) + depth(j,i)) * S(k,j,i)
  S <- (10 * -0.375 + 70 * -0.3) / (10 + 70)
  v3 <- open_ncdf(.write_ocean_s(2L, "3D"))[["s_rho"]]$parametric_coordinates$raw()
  expect_equal(unname(v3[1, 2, 3, 2]), 0.9 + (0.9 + 70) * S)
  v2 <- open_ncdf(.write_ocean_s(2L, "2D"))[["s_rho"]]$parametric_coordinates$raw()
  expect_equal(unname(v2[1, 2, 3]), 0.3 + (0.3 + 70) * S)
  v0 <- open_ncdf(.write_ocean_s(2L, "none"))[["s_rho"]]$parametric_coordinates$raw()
  expect_equal(unname(v0[1, 2, 3]), 70 * S)
})

test_that("Atmosphere hybrid sigma pressure with a and p0 is computed", {
  ax <- open_ncdf(.write_hybrid())[["lev"]]
  p <- ax$parametric_coordinates
  expect_equal(ax$computed_name, "air_pressure")
  vals <- p$raw()
  expect_equal(dim(vals), c(2L, 2L, 3L, 2L))
  # p(n,k,j,i) = a(k) * p0 + b(k) * ps(n,j,i)
  expect_equal(unname(vals[2, 1, 2, 2]), 0.05 * 1e5 + 0.5 * (1e5 + 6))
  expect_equal(p$attribute("units"), "Pa")
})

test_that("Atmosphere hybrid sigma pressure with ap is computed", {
  vals <- open_ncdf(.write_hybrid(ap = TRUE))[["lev"]]$parametric_coordinates$raw()
  expect_equal(unname(vals[2, 1, 2, 2]), 5000 + 0.5 * (1e5 + 6))
})

test_that("The units of the computed coordinates come from the standard names table", {
  skip_if_not_installed("xml2")
  sn <- CF$standard_names$.__enclos_env__$private
  old <- sn$names
  sn$names <- xml2::read_xml(paste0('<standard_name_table><version_number>1</version_number>',
                                    '<entry id="air_pressure"><canonical_units>Pa</canonical_units>',
                                    '<description>Pressure</description></entry></standard_name_table>'))
  on.exit(sn$names <- old)

  ax <- open_ncdf(.write_hybrid())[["lev"]]
  expect_null(ax$computed_units)
  invisible(ax$parametric_coordinates)
  expect_equal(ax$computed_units, "Pa")
})

test_that("A depth term with a non-standard name gives a non-standard computed name", {
  fn <- .write_ocean_s(1L, "none")
  nc <- RNetCDF::open.nc(fn, write = TRUE)
  RNetCDF::att.put.nc(nc, "h", "standard_name", "NC_CHAR", "my_depth")
  RNetCDF::close.nc(nc)
  ax <- open_ncdf(fn)[["s_rho"]]
  invisible(ax$parametric_coordinates)
  expect_equal(ax$computed_name, "non_standard_name")
  expect_equal(ax$computed_units, "unknown units")
})

test_that("A parametric axis without a known formula has no computed coordinates", {
  fn <- .write_nc(c(lev = 2L), list(
    list(name = "lev", type = "NC_DOUBLE", dims = "lev", data = c(0.9, 0.1),
         atts = list(standard_name = "atmosphere_sigma_coordinate", positive = "down",
                     formula_terms = "sigma: lev ps: PS ptop: PTOP")),
    list(name = "v", type = "NC_FLOAT", dims = "lev", data = c(1, 2))))
  ax <- open_ncdf(fn)[["lev"]]
  expect_true(ax$is_parametric)
  expect_null(ax$parametric_coordinates)
})

test_that("A copy of a parametric axis keeps the formula terms", {
  ax <- open_ncdf(.write_ocean_s())[["s_rho"]]
  cp <- ax$copy("s2", CFGroup$new("", NULL))
  expect_equal(cp$name, "s2")
  expect_true(cp$is_parametric)
  expect_equal(cp$formula_terms, ax$formula_terms)
})

test_that("Subsetting a variable subsets the parametric terms of its vertical axis", {
  ds <- open_ncdf(.write_ocean_s())
  sub <- ds[["temp"]]$subset(x = 2, time = "2000-01-01")
  ax <- sub$axes[["s_rho"]]
  expect_true(ax$is_parametric)
  vals <- ax$parametric_coordinates$raw()
  expect_equal(dim(vals), c(1L, 3L, 4L, 1L))
  # x = 2, y = 2: depth = 80, eta = 0.4
  S <- 10 * -0.375 + (80 - 10) * -0.3
  expect_equal(unname(vals[1, 2, 3, 1]), S + 0.4 * (1 + S / 80))
})

test_that("A parametric term prints and has data", {
  ax <- open_ncdf(.write_ocean_s())[["s_rho"]]
  depth <- ax$formula_terms$param[[4L]]
  expect_s3_class(depth, "CFVerticalParametricTerm")
  expect_true(depth$has_data)
  expect_output(print(depth), "Parametric formula term")
  expect_output(print(depth), "Axes:")
  hc <- ax$formula_terms$param[[5L]]
  expect_output(print(hc), "\\(none\\)")
  zero <- open_ncdf(.write_ocean_s(1L, "none"))[["s_rho"]]$formula_terms$param[[3L]]
  expect_false(zero$has_data)
  expect_equal(zero$values, 0)
})

test_that("A regular vertical axis is copied, subset and appended", {
  grp <- CFGroup$new("", NULL)
  ax <- makeVerticalAxis("lev", c(10, 20, 30), bounds = rbind(c(5, 15, 25), c(15, 25, 35)), group = grp)
  expect_equal(ax$friendlyClassName, "Vertical axis")
  expect_false(ax$is_parametric)
  expect_equal(ax$copy(group = grp)$values, c(10, 20, 30))
  expect_equal(ax$copy_with_values("lev2", grp, c(1, 2))$values, c(1, 2))
  expect_equal(ax$subset(group = grp, rng = c(2, 3))$values, c(20, 30))
  expect_equal(ax$subset(group = grp)$values, c(10, 20, 30))

  more <- makeVerticalAxis("lev", c(40, 50), bounds = rbind(c(35, 45), c(45, 55)), group = grp)
  ap <- ax$append(more, grp)
  expect_equal(ap$values, c(10, 20, 30, 40, 50))
  expect_equal(dim(ap$bounds$values), c(2L, 5L))
  expect_error(ax$append(ax, grp), "cannot be appended")
})

test_that("A vertical axis on file is copied and subset", {
  ax <- open_ncdf(.write_hybrid())[["lev"]]
  grp <- CFGroup$new("", NULL)
  cp <- ax$copy("lev2", grp)
  expect_equal(cp$name, "lev2")
  sub <- ax$subset("lev3", grp, c(2, 3))
  expect_equal(sub$name, "lev3")
  expect_equal(sub$values, c(0.5, 0.1))
})

test_that("Appending to a parametric axis drops the formula terms", {
  grp <- CFGroup$new("", NULL)
  ax <- open_ncdf(.write_hybrid())[["lev"]]$copy(group = grp)
  more <- makeVerticalAxis("lev", c(0.05, 0.01), group = grp)
  more$set_attribute("standard_name", "NC_CHAR", "atmosphere_hybrid_sigma_pressure_coordinate")
  more$set_attribute("positive", "NC_CHAR", "down")
  ap <- ax$append(more, grp)
  expect_equal(length(ap$values), 5L)
  expect_false(ap$is_parametric)
})
