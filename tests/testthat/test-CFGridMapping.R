# CFGridMapping: WKT2 strings for the CF grid mappings and the CRS components
# taken from the EPSG database.

m <- list(LENGTHUNIT = "m")

# One case per grid mapping, with typical CF attributes. Every case must give
# a WKT2 string that GDAL accepts and that contains the expected method.
projections <- list(
  albers_conical_equal_area = list(method = "Albers Equal Area", atts = list(
    latitude_of_projection_origin = 23, longitude_of_central_meridian = -96,
    standard_parallel = c(29.5, 45.5), false_easting = 0, false_northing = 0)),
  azimuthal_equidistant = list(method = "Azimuthal Equidistant", atts = list(
    latitude_of_projection_origin = 52, longitude_of_projection_origin = 10, false_easting = 100)),
  geostationary = list(method = "PROJ geos", atts = list(
    perspective_point_height = 35785831, longitude_of_projection_origin = 0,
    sweep_angle_axis = "x", false_easting = 0)),
  lambert_azimuthal_equal_area = list(method = "Lambert Azimuthal Equal Area", atts = list(
    latitude_of_projection_origin = 52, longitude_of_projection_origin = 10,
    false_easting = 4321000, false_northing = 3210000)),
  lambert_conformal_conic = list(method = "Lambert Conic Conformal (2SP)", atts = list(
    standard_parallel = c(33, 45), latitude_of_projection_origin = 40,
    longitude_of_central_meridian = -97, false_easting = 0)),
  lambert_cylindrical_equal_area = list(method = "Lambert Cylindrical Equal Area", atts = list(
    longitude_of_central_meridian = 0, false_northing = 0)),
  mercator = list(method = "Mercator (variant B)", atts = list(
    longitude_of_projection_origin = 0, standard_parallel = 0, false_easting = 0)),
  oblique_mercator = list(method = "Hotine Oblique Mercator (variant B)", atts = list(
    latitude_of_projection_origin = 4, longitude_of_projection_origin = 115,
    azimuth_of_central_line = 53.3, scale_factor_at_projection_origin = 0.99984, false_easting = 0)),
  orthographic = list(method = "Orthographic", atts = list(
    latitude_of_projection_origin = 40, longitude_of_projection_origin = -100, false_easting = 0)),
  polar_stereographic = list(method = "Polar Stereographic (variant B)", atts = list(
    straight_vertical_longitude_from_pole = -45, latitude_of_projection_origin = 90, standard_parallel = 70)),
  rotated_latitude_longitude = list(method = "PROJ ob_tran", atts = list(
    grid_north_pole_latitude = 39.25, grid_north_pole_longitude = -162)),
  sinusoidal = list(method = "PROJ sinu lon_0=0", atts = list(
    longitude_of_projection_origin = 0, false_easting = 0)),
  stereographic = list(method = "Oblique Stereographic", atts = list(
    latitude_of_projection_origin = 52, longitude_of_projection_origin = 5,
    scale_factor_at_projection_origin = 0.9999, false_easting = 155000, false_northing = 463000)),
  transverse_mercator = list(method = "Transverse Mercator", atts = list(
    longitude_of_central_meridian = 9, latitude_of_projection_origin = 0,
    scale_factor_at_central_meridian = 0.9996, false_easting = 500000, false_northing = 0)),
  vertical_perspective = list(method = "Vertical Perspective", atts = list(
    perspective_point_height = 35785831, longitude_of_projection_origin = 0, latitude_of_projection_origin = 0))
)

for (gmn in names(projections)) {
  local({
    p <- projections[[gmn]]
    test_that(paste("WKT2 for", gmn, "is valid"), {
      wkt <- .grid_mapping(gmn, p$atts)$wkt2(m)
      expect_true(grepl(p$method, wkt, fixed = TRUE))
      expect_true(.valid_wkt(wkt))
    })
  })
}

test_that("WKT2 for projections without optional parameters is valid", {
  for (gmn in setdiff(names(projections), "lambert_conformal_conic"))
    expect_true(.valid_wkt(.grid_mapping(gmn)$wkt2(m)), label = gmn)
})

test_that("Lambert conformal conic with one standard parallel is the 1SP variant", {
  wkt <- .grid_mapping("lambert_conformal_conic", list(standard_parallel = 25, latitude_of_projection_origin = 25,
                                                       longitude_of_central_meridian = 265, false_northing = 0))$wkt2(m)
  expect_true(grepl("Lambert Conic Conformal (1SP)", wkt, fixed = TRUE))
  expect_true(.valid_wkt(wkt))
})

test_that("Lambert conformal conic without standard parallel has no method", {
  wkt <- .grid_mapping("lambert_conformal_conic")$wkt2(m)
  expect_true(grepl('CONVERSION["unknown",]', wkt, fixed = TRUE))
})

test_that("Mercator with a scale factor is variant A", {
  wkt <- .grid_mapping("mercator", list(scale_factor_at_projection_origin = 0.9, false_northing = 10))$wkt2(m)
  expect_true(grepl("Mercator (variant A)", wkt, fixed = TRUE))
  expect_true(grepl('PARAMETER["False northing",10', wkt, fixed = TRUE))
  expect_true(.valid_wkt(wkt))
})

test_that("Polar stereographic with a scale factor is variant A", {
  wkt <- .grid_mapping("polar_stereographic", list(longitude_of_projection_origin = 0,
                                                   scale_factor_at_projection_origin = 0.994))$wkt2(m)
  expect_true(grepl("Polar Stereographic (variant A)", wkt, fixed = TRUE))
  expect_true(.valid_wkt(wkt))
})

test_that("Geostationary takes the sweep from the fixed angle axis", {
  expect_true(grepl("sweep=y", .grid_mapping("geostationary", list(fixed_angle_axis = "x"))$wkt2(m), fixed = TRUE))
  expect_true(grepl("sweep=x", .grid_mapping("geostationary", list(fixed_angle_axis = "y"))$wkt2(m), fixed = TRUE))
  expect_true(grepl("x_0=0 y_0=5", .grid_mapping("geostationary", list(false_northing = 5))$wkt2(m), fixed = TRUE))
})

test_that("A crs_wkt attribute is returned unchanged", {
  gm <- .grid_mapping("transverse_mercator", list(crs_wkt = "PROJCRS[\"as given\"]"))
  expect_equal(gm$wkt2(m), "PROJCRS[\"as given\"]")
})

test_that("A projected CRS name from the EPSG database gives the EPSG definition", {
  wkt <- .grid_mapping("transverse_mercator", list(projected_crs_name = "WGS 84 / UTM zone 33N"))$wkt2(m)
  expect_true(grepl("ID[\"EPSG\",32633]", wkt, fixed = TRUE))
  expect_true(.valid_wkt(wkt))
})

test_that("A compound CRS name from the EPSG database gives the EPSG definition", {
  wkt <- .grid_mapping("transverse_mercator", list(projected_crs_name = "ETRS89 / TM35FIN(N,E) + N60 height"))$wkt2(m)
  expect_true(startsWith(wkt, "COMPOUNDCRS[\"ETRS89 / TM35FIN(N,E) + N60 height\""))
  expect_true(grepl("VERTCRS[", wkt, fixed = TRUE))
  expect_true(.valid_wkt(wkt))
})

test_that("An unknown projected CRS name is used in a manual WKT2", {
  wkt <- .grid_mapping("transverse_mercator", list(projected_crs_name = "My projection"))$wkt2(m)
  expect_true(startsWith(wkt, "PROJCRS[\"My projection\""))
  expect_true(.valid_wkt(wkt))
})

test_that("A geographic CRS name selects the 2D EPSG CRS for latitude_longitude", {
  wkt <- .grid_mapping("latitude_longitude", list(geographic_crs_name = "WGS 84"))$wkt2(m)
  expect_true(endsWith(wkt, "ID[\"EPSG\",4326]]"))
  expect_true(.valid_wkt(wkt))
})

test_that("An ambiguous geographic CRS name is used as the base CRS name of a projection", {
  wkt <- .grid_mapping("transverse_mercator", list(geographic_crs_name = "WGS 84"))$wkt2(m)
  expect_true(grepl("BASEGEOGCRS[\"WGS 84\"", wkt, fixed = TRUE))
  expect_true(.valid_wkt(wkt))
})

test_that("latitude_longitude without names gives a manual geographic CRS", {
  wkt <- .grid_mapping("latitude_longitude")$wkt2(m)
  expect_true(startsWith(wkt, "GEOGCRS[\"unknown\""))
  expect_true(grepl("World Geodetic System 1984", wkt, fixed = TRUE))
  expect_true(.valid_wkt(wkt))
})

test_that("The datum is found by its CF name and by its EPSG name", {
  cf <- .grid_mapping("latitude_longitude", list(horizontal_datum_name = "Hungarian_Datum_1909"))$wkt2(m)
  expect_true(grepl("DATUM[\"Hungarian Datum 1909\"", cf, fixed = TRUE))
  epsg <- .grid_mapping("latitude_longitude", list(horizontal_datum_name = "Hungarian Datum 1909"))$wkt2(m)
  expect_equal(epsg, cf)
  expect_true(.valid_wkt(cf))
})

test_that("A datum ensemble is written as an ENSEMBLE", {
  wkt <- .grid_mapping("latitude_longitude", list(horizontal_datum_name = "World Geodetic System 1984 ensemble"))$wkt2(m)
  expect_true(grepl("ENSEMBLE[\"World Geodetic System 1984 ensemble\"", wkt, fixed = TRUE))
  expect_true(.valid_wkt(wkt))
})

test_that("An unknown datum name with an EPSG ellipsoid name gives a manual datum", {
  wkt <- .grid_mapping("latitude_longitude", list(horizontal_datum_name = "My datum",
                                                  reference_ellipsoid_name = "Airy 1830"))$wkt2(m)
  expect_true(grepl("DATUM[\"My datum\",ELLIPSOID[\"Airy 1830\"", wkt, fixed = TRUE))
  expect_true(grepl("ID[\"EPSG\",7001]", wkt, fixed = TRUE))
  expect_true(.valid_wkt(wkt))
})

test_that("The ellipsoid is found in the EPSG database by its parameters", {
  ell <- function(atts) .grid_mapping("latitude_longitude", atts)$wkt2(m)
  expect_true(grepl("ELLIPSOID[\"Airy 1830\"", ell(list(semi_major_axis = 6377563.396, inverse_flattening = 299.3249646)), fixed = TRUE))
  expect_true(grepl("ELLIPSOID[\"Clarke 1866\"", ell(list(semi_major_axis = 6378206.4, semi_minor_axis = 6356583.8)), fixed = TRUE))
  expect_true(grepl("ELLIPSOID[\"GRS 1980 Authalic Sphere\"", ell(list(semi_major_axis = 6371007)), fixed = TRUE))
  expect_true(grepl("ELLIPSOID[\"GRS 1980 Authalic Sphere\"", ell(list(earth_radius = 6371007)), fixed = TRUE))
})

test_that("An ellipsoid not in the EPSG database is written from its parameters", {
  ell <- function(atts) .grid_mapping("latitude_longitude", atts)$wkt2(m)
  expect_true(grepl("ELLIPSOID[\"unknown\",6400000,300,", ell(list(semi_major_axis = 6400000, inverse_flattening = 300)), fixed = TRUE))
  expect_true(grepl("ELLIPSOID[\"unknown\",6400000,320,", ell(list(semi_major_axis = 6400000, semi_minor_axis = 6380000)), fixed = TRUE))
  expect_true(grepl("ELLIPSOID[\"unknown\",6400000,0,", ell(list(semi_major_axis = 6400000)), fixed = TRUE))
  expect_true(grepl("ELLIPSOID[\"unknown\",6370000,0,", ell(list(earth_radius = 6370000)), fixed = TRUE))
  expect_true(.valid_wkt(ell(list(semi_major_axis = 6400000, inverse_flattening = 300))))
})

test_that("The prime meridian is found by name or by longitude in degrees", {
  pm <- function(atts) .grid_mapping("latitude_longitude", c(list(semi_major_axis = 6400000), atts))$wkt2(m)
  expect_true(grepl("PRIMEM[\"Paris\"", pm(list(prime_meridian_name = "Paris")), fixed = TRUE))
  expect_true(grepl("PRIMEM[\"Greenwich\"", pm(list(longitude_of_prime_meridian = 0)), fixed = TRUE))
  expect_false(grepl("PRIMEM", pm(list(longitude_of_prime_meridian = 12)), fixed = TRUE))
})

test_that("A geoid name makes a compound CRS with a vertical CRS", {
  info <- list(LENGTHUNIT = "m", VERTINFO = list(standard_name = "height", positive = "up", units = "m"))
  wkt <- .grid_mapping("transverse_mercator", list(geoid_name = "EGM2008 geoid"))$wkt2(info)
  expect_true(startsWith(wkt, "COMPOUNDCRS[\"unknown\",PROJCRS["))
  expect_true(grepl("VDATUM[\"EGM2008 geoid\",ID[\"EPSG\",1027]]", wkt, fixed = TRUE))

  wkt <- .grid_mapping("transverse_mercator", list(geopotential_datum_name = "My vertical datum"))$wkt2(info)
  expect_true(grepl("VDATUM[\"My vertical datum\"]", wkt, fixed = TRUE))
})

test_that("Units of measure are found by name and by alias", {
  expect_true(grepl("LENGTHUNIT[\"metre\",1]", .grid_mapping("transverse_mercator")$wkt2(list(LENGTHUNIT = "meters")), fixed = TRUE))
  expect_true(grepl("LENGTHUNIT[\"metre\",1]", .grid_mapping("transverse_mercator")$wkt2(list(LENGTHUNIT = "m")), fixed = TRUE))
  expect_true(grepl("LENGTHUNIT[\"kilometre\",1000]", .grid_mapping("transverse_mercator")$wkt2(list(LENGTHUNIT = "km")), fixed = TRUE))
  expect_true(grepl("],,ID", .grid_mapping("transverse_mercator")$wkt2(list(LENGTHUNIT = "no_such_unit")), fixed = TRUE) ||
              grepl('north,ORDER[2]],]', .grid_mapping("transverse_mercator")$wkt2(list(LENGTHUNIT = "no_such_unit")), fixed = TRUE))
})

test_that("A grid mapping needs a known grid_mapping_name", {
  grp <- CFGroup$new("", NULL)
  expect_error(CFGridMapping$new("crs", grp), "grid_mapping_name")
  expect_error(CFGridMapping$new("crs", grp, "no_such_projection"), "Unrecognized grid mapping name")
})

test_that("A grid mapping prints and describes itself", {
  gm <- .grid_mapping("mercator", list(false_easting = 0))
  expect_output(print(gm), "Grid mapping name: mercator")
  expect_equal(gm$brief(), data.frame(name = "/crs", grid_mapping = "mercator"))
  expect_equal(gm$friendlyClassName, "Grid mapping")
})

test_that("A grid mapping is read from file and gives the CRS of its variable", {
  fn <- .write_nc(c(x = 2L, y = 2L), list(
    list(name = "x", type = "NC_DOUBLE", dims = "x", data = c(5e5, 6e5),
         atts = list(standard_name = "projection_x_coordinate", units = "m")),
    list(name = "y", type = "NC_DOUBLE", dims = "y", data = c(4e6, 5e6),
         atts = list(standard_name = "projection_y_coordinate", units = "m")),
    list(name = "crs", type = "NC_INT", dims = NULL,
         atts = list(grid_mapping_name = "transverse_mercator", projected_crs_name = "WGS 84 / UTM zone 33N")),
    list(name = "tas", type = "NC_FLOAT", dims = c("x", "y"), data = array(1:4, c(2, 2)),
         atts = list(grid_mapping = "crs"))
  ))
  ds <- open_ncdf(fn)
  tas <- ds[["tas"]]
  expect_s3_class(tas$crs, "CFGridMapping")
  expect_true(grepl("UTM zone 33N", tas$crs_wkt2, fixed = TRUE))
  expect_output(print(ds[["crs"]]), "transverse_mercator")
})

test_that("A grid mapping in a subgroup prints its group", {
  fn <- .write_nc(c(x = 2L), list(
    list(name = "g1/crs", type = "NC_INT", dims = NULL, atts = list(grid_mapping_name = "latitude_longitude"))
  ))
  ds <- open_ncdf(fn)
  expect_output(print(ds[["/g1/crs"]]), "Group            : g1")
})

test_that("The CRS of a variable without grid mapping is WGS 84 when in range", {
  ds <- open_ncdf(.write_nc(c(lon = 2L, lat = 2L), list(
    list(name = "lon", type = "NC_DOUBLE", dims = "lon", data = c(10, 20), atts = list(units = "degrees_east")),
    list(name = "lat", type = "NC_DOUBLE", dims = "lat", data = c(40, 50), atts = list(units = "degrees_north")),
    list(name = "tas", type = "NC_FLOAT", dims = c("lon", "lat"), data = array(1:4, c(2, 2)))
  )))
  expect_true(endsWith(ds[["tas"]]$crs_wkt2, "ID[\"EPSG\",4326]]"))
})

test_that("The CRS of a variable without grid mapping is empty when out of range or not spatial", {
  ds <- open_ncdf(.write_nc(c(lon = 2L, lat = 2L, t = 2L), list(
    list(name = "lon", type = "NC_DOUBLE", dims = "lon", data = c(-100, 200), atts = list(units = "degrees_east")),
    list(name = "lat", type = "NC_DOUBLE", dims = "lat", data = c(40, 50), atts = list(units = "degrees_north")),
    list(name = "t", type = "NC_DOUBLE", dims = "t", data = c(1, 2)),
    list(name = "tas", type = "NC_FLOAT", dims = c("lon", "lat"), data = array(1:4, c(2, 2))),
    list(name = "pr", type = "NC_FLOAT", dims = "t", data = c(1, 2))
  )))
  expect_equal(ds[["tas"]]$crs_wkt2, "")
  expect_equal(ds[["pr"]]$crs_wkt2, "")
})

test_that("The axis information for WKT2 has the length unit and the vertical axis", {
  ds <- open_ncdf(.write_nc(c(x = 2L, z = 2L), list(
    list(name = "x", type = "NC_DOUBLE", dims = "x", data = c(1, 2),
         atts = list(standard_name = "projection_x_coordinate", units = "km")),
    list(name = "z", type = "NC_DOUBLE", dims = "z", data = c(10, 20),
         atts = list(standard_name = "height", units = "m", positive = "up", axis = "Z")),
    list(name = "tas", type = "NC_FLOAT", dims = c("x", "z"), data = array(1:4, c(2, 2)))
  )))
  info <- .wkt2_axis_info(ds[["tas"]])
  expect_equal(info$LENGTHUNIT, "km")
  expect_equal(info$VERTINFO, list(standard_name = "height", positive = "up", units = "m"))
})
