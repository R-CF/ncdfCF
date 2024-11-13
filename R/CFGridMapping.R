# Standard names for grid mappings
CRS_names <- c("albers_conical_equal_area", "azimuthal_equidistant",
               "geostationary", "lambert_azimuthal_equal_area",
               "lambert_conformal_conic", "lambert_cylindrical_equal_area",
               "latitude_longitude", "mercator", "oblique_mercator",
               "orthographic", "polar_stereographic",
               "rotated_latitude_longitude", "sinusoidal", "stereographic",
               "transverse_mercator", "vertical_perspective")

#' CF grid mapping object
#'
#' @description This class contains the details for a coordinate reference
#' system, or grid mapping in CF terms, of a data variable.
#'
#' When reporting the coordinate reference system to the caller, a character
#' string in WKT2 format is returned, following the OGC standard.
#'
#' @references https://docs.ogc.org/is/18-010r11/18-010r11.pdf
#'
#' @docType class
#' @export
CFGridMapping <- R6::R6Class("CFGridMapping",
  inherit = CFObject,
  private = list(
    # === CF grid_mapping_name METHOD & PARAMETER rendering ====================
    aea = function(angle, length) {
      # The x (abscissa) and y (ordinate) rectangular coordinates are identified by the standard_name
      # attribute values projection_x_coordinate and projection_y_coordinate respectively.
      prj <- self$attribute(c("latitude_of_projection_origin",
                              "longitude_of_central_meridian",
                              "standard_parallel", # Must be 2 values!
                              "false_easting",
                              "false_northing"))

      wkt2 <- paste0('METHOD["Albers Equal Area",', .wkt2_id(9822), ']')
      if (!is.null(prj$latitude_of_projection_origin))
        wkt2 <- paste0(wkt2, ',PARAMETER["Latitude of false origin",', prj$latitude_of_projection_origin, ',', angle, ']')
      if (!is.null(prj$longitude_of_central_meridian))
        wkt2 <- paste0(wkt2, ',PARAMETER["Longitude of false origin",', prj$longitude_of_central_meridian, ',', angle, ']')
      if (!(is.null(prj$standard_parallel) || length(prj$standard_parallel) < 2L))
        wkt2 <- paste0(wkt2, ',PARAMETER["Latitude of 1st standard parallel",', prj$standard_parallel[1L], ',', angle, ']',
                             ',PARAMETER["Latitude of 2nd standard parallel",', prj$standard_parallel[2L], ',', angle, ']')
      if (!(is.null(prj$false_easting) && is.null(prj$false_northing))) {
        if (is.null(prj$false_easting)) prj$false_easting <- 0
        if (is.null(prj$false_northing)) prj$false_northing <- 0
        wkt2 <- paste0(wkt2, ',PARAMETER["Easting at false origin",', prj$false_easting, ',', length,
                             '],PARAMETER["Northing at false origin",', prj$false_northing, ',', length, ']')
      }
      wkt2
    },

    aeqd = function(angle, length) {
      # The x (abscissa) and y (ordinate) rectangular coordinates are identified by the standard_name
      # attribute values projection_x_coordinate and projection_y_coordinate respectively.
      prj <- self$attribute(c("latitude_of_projection_origin",
                              "longitude_of_projection_origin",
                              "false_easting",
                              "false_northing"))

      wkt2 <- paste0('METHOD["Azimuthal Equidistant",', .wkt2_id(1125), ']')
      if (!is.null(prj$latitude_of_projection_origin))
        wkt2 <- paste0(wkt2, ',PARAMETER["Latitude of natural origin",', prj$latitude_of_projection_origin, ',', angle, ']')
      if (!is.null(prj$longitude_of_projection_origin))
        wkt2 <- paste0(wkt2, ',PARAMETER["Longitude of natural origin",', prj$longitude_of_projection_origin, ',', angle, ']')
      if (!(is.null(prj$false_easting) && is.null(prj$false_northing))) {
        if (is.null(prj$false_easting)) prj$false_easting <- 0
        if (is.null(prj$false_northing)) prj$false_northing <- 0
        wkt2 <- paste0(wkt2, ',PARAMETER["False easting",', prj$false_easting, ',', length,
                             '],PARAMETER["False northing",', prj$false_northing, ',', length, ']')
      }
      wkt2
    },

    geos = function() {
      # Not in the EPSG database
      prj <- self$attribute(c("longitude_of_projection_origin",
                              "perspective_point_height",
                              "sweep_angle_axis",
                              "fixed_angle_axis",
                              "false_easting",
                              "false_northing"))

      h <- if (!is.null(prj$perspective_point_height)) prj$perspective_point_height else 0
      lon0 <- if (!is.null(prj$longitude_of_projection_origin)) prj$longitude_of_projection_origin else 0
      sweep <- if (!is.null(prj$sweep_angle_axis)) prj$sweep_angle_axis
               else if (!is.null(prj$fixed_angle_axis))
                 c('x', 'y')[as.integer(prj$fixed_angle_axis == 'x') + 1L]
               else 'y'

      if (is.null(prj$false_easting) && is.null(prj$false_northing))
        fe_fn <- ''
      else {
        if (is.null(prj$false_easting)) prj$false_easting <- 0
        if (is.null(prj$false_northing)) prj$false_northing <- 0
        fe_fn <- paste0(' x_0=', prj$false_easting, ' y_0=', prj$false_northing)
      }

      paste0('METHOD["PROJ geos h=', h, ' lon_0=', lon0, ' sweep=', sweep, fe_fn, '"]')
    },

    laea = function(angle, length) {
      # The x (abscissa) and y (ordinate) rectangular coordinates are identified by the standard_name
      # attribute values projection_x_coordinate and projection_y_coordinate respectively.
      prj <- self$attribute(c("latitude_of_projection_origin",
                              "longitude_of_projection_origin",
                              "false_easting",
                              "false_northing"))

      wkt2 <- paste0('METHOD["Lambert Azimuthal Equal Area",', .wkt2_id(9820), ']')
      if (!is.null(prj$latitude_of_projection_origin))
        wkt2 <- paste0(wkt2, 'PARAMETER["Latitude of natural origin",', prj$latitude_of_projection_origin, ',', angle, ']')
      if (!is.null(prj$longitude_of_projection_origin))
        wkt2 <- paste0(wkt2, 'PARAMETER["Longitude of natural origin",', prj$longitude_of_projection_origin, ',', angle, ']')
      if (!(is.null(prj$false_easting) && is.null(prj$false_northing))) {
        if (is.null(prj$false_easting)) prj$false_easting <- 0
        if (is.null(prj$false_northing)) prj$false_northing <- 0
        wkt2 <- paste0(wkt2, ',PARAMETER["False easting",', prj$false_easting, ',', length,
                            '],PARAMETER["False northing",', prj$false_northing, ',', length, ']')
      }
      wkt2
    },

    lcc = function(angle, length) {
      # The x (abscissa) and y (ordinate) rectangular coordinates are identified by the standard_name
      # attribute values projection_x_coordinate and projection_y_coordinate respectively.
      # For EPSG:9801 "Scale factor at natural origin" is 1 and not specified in an attribute!
      prj <- self$attribute(c("latitude_of_projection_origin",
                              "longitude_of_central_meridian",
                              "standard_parallel", # 1 or 2 values
                              "false_easting",
                              "false_northing"))

      if (length(prj$standard_parallel) == 1L) {
        wkt2 <- paste0('METHOD["Lambert Conic Conformal (1SP)",', .wkt2_id(9801), ']')
        if (!is.null(prj$latitude_of_projection_origin))
          wkt2 <- paste0(wkt2, ',PARAMETER["Latitude of natural origin",', prj$latitude_of_projection_origin, ',', angle, ']')
        if (!is.null(prj$longitude_of_central_meridian))
          wkt2 <- paste0(wkt2, ',PARAMETER["Longitude of natural origin",', prj$longitude_of_central_meridian, ',', angle, ']')
        if (!is.null(prj$standard_parallel))
          wkt2 <- paste0(wkt2, ',PARAMETER["Scale factor at natural origin",1,', .wkt2_uom(9201), ']')

        if (!(is.null(prj$false_easting) && is.null(prj$false_northing))) {
          if (is.null(prj$false_easting)) prj$false_easting <- 0
          if (is.null(prj$false_northing)) prj$false_northing <- 0
          wkt2 <- paste0(wkt2, ',PARAMETER["False easting",', prj$false_easting, ',', length,
                              '],PARAMETER["False northing",', prj$false_northing, ',', length, ']')
        }
      } else {
        wkt2 <- paste0('METHOD["Lambert Conic Conformal (2SP)",', .wkt2_id(9802), ']')
        if (!is.null(prj$latitude_of_projection_origin))
          wkt2 <- paste0(wkt2, ',PARAMETER["Latitude of false origin",', prj$latitude_of_projection_origin, ',', angle, ']')
        if (!is.null(prj$longitude_of_central_meridian))
          wkt2 <- paste0(wkt2, ',PARAMETER["Longitude of false origin",', prj$longitude_of_central_meridian, ',', angle, ']')
        if (!is.null(prj$standard_parallel[1L]))
          wkt2 <- paste0(wkt2, ',PARAMETER["Latitude of 1st standard parallel",', prj$standard_parallel[1L], ',', angle, ']')
        if (!is.null(prj$standard_parallel[2L]))
          wkt2 <- paste0(wkt2, ',PARAMETER["Latitude of 2nd standard parallel",', prj$standard_parallel[2L], ',', angle, ']')
        if (!(is.null(prj$false_easting) && is.null(prj$false_northing))) {
          if (is.null(prj$false_easting)) prj$false_easting <- 0
          if (is.null(prj$false_northing)) prj$false_northing <- 0
          wkt2 <- paste0(wkt2, ',PARAMETER["Easting at false origin",', prj$false_easting, ',', length,
                              '],PARAMETER["Northing at false origin",', prj$false_northing, ',', length, ']')
        }
      }
      wkt2
    },

    lcea = function(angle, length) {
      # The x (abscissa) and y (ordinate) rectangular coordinates are identified by the standard_name
      # attribute values projection_x_coordinate and projection_y_coordinate respectively.
      # FIXME: EPSG:9834 is on a spheroid
      prj <- self$attribute(c("longitude_of_central_meridian",
                              "false_easting",
                              "false_northing"))

      wkt2 <- paste0('METHOD["Lambert Cylindrical Equal Area",', .wkt2_id(9835),
                     '],PARAMETER["Latitude of 1st standard parallel",0,', angle, ']')
      if (!is.null(prj$longitude_of_central_meridian))
        wkt2 <- paste0(wkt2, ',PARAMETER["Longitude of natural origin",', prj$longitude_of_central_meridian, ',', angle, ']')

      if (!(is.null(prj$false_easting) && is.null(prj$false_northing))){
        if (is.null(prj$false_easting)) prj$false_easting <- 0
        if (is.null(prj$false_northing)) prj$false_northing <- 0
        wkt2 <- paste0(wkt2, ',PARAMETER["False easting",', prj$false_easting, ',', length,
                            '],PARAMETER["False northing",', prj$false_northing, ',', length, ']')
      }
      wkt2
    },

    merc = function(angle, length) {
      # The x (abscissa) and y (ordinate) rectangular coordinates are identified by the standard_name
      # attribute values projection_x_coordinate and projection_y_coordinate respectively.
      prj <- self$attribute(c("longitude_of_projection_origin",
                              "scale_factor_at_projection_origin",
                              "standard_parallel",
                              "false_easting",
                              "false_northing"))

      lon0 <- if (!is.null(prj$longitude_of_projection_origin)) prj$longitude_of_projection_origin else 0
      lat0 <- if (!is.null(prj$standard_parallel)) prj$standard_parallel else 0
      scl <- if (!is.null(prj$scale_factor_at_projection_origin)) prj$scale_factor_at_projection_origin else 1

      if (is.null(prj$false_easting) && is.null(prj$false_northing))
        fe_fn <- ''
      else {
        if (is.null(prj$false_easting)) prj$false_easting <- 0
        if (is.null(prj$false_northing)) prj$false_northing <- 0
        fe_fn <- paste0(',PARAMETER["False easting",', prj$false_easting, ',', length,
                        '],PARAMETER["False northing",', prj$false_northing, ',', length, ']')
      }

      if (!is.null(prj$scale_factor_at_projection_origin)) {
        paste0('METHOD["Mercator (variant A)",', .wkt2_id(9804), ']',
               ',PARAMETER["Latitude of natural origin",', lat0, ',', angle, ']',
               ',PARAMETER["Longitude of natural origin",', lon0, ',', angle, ']',
               ',PARAMETER["Scale factor at natural origin",', scl, ',', .wkt2_uom(9201), ']',
               fe_fn)
      } else {
        paste0('Mercator (variant B)",', .wkt2_id(9805), ']',
               ',PARAMETER["Latitude of 1st standard parallel",', lat0, ',', angle, ']',
               ',PARAMETER["Longitude of natural origin",', lon0, ',', angle, ']',
               fe_fn)
      }
    },

    omerc = function(angle, length) {
      # The x (abscissa) and y (ordinate) rectangular coordinates are identified by the standard_name
      # attribute values projection_x_coordinate and projection_y_coordinate respectively.
      prj <- self$attribute(c("latitude_of_projection_origin",
                              "longitude_of_projection_origin",
                              "azimuth_of_central_line",
                              "scale_factor_at_projection_origin",
                              "false_easting",
                              "false_northing"))

      wkt2 <- paste0('METHOD["Hotine Oblique Mercator (variant B)",', .wkt2_id(9815), ']')
      if (!is.null(prj$latitude_of_projection_origin))
        wkt2 <- paste0(wkt2, ',PARAMETER["Latitude of projection center",', prj$latitude_of_projection_origin, ',', angle, ']')
      if (!is.null(prj$longitude_of_projection_origin))
        wkt2 <- paste0(wkt2, ',PARAMETER["Longitude of projection center",', prj$longitude_of_projection_origin, ',', angle, ']')
      if (!is.null(prj$azimuth_of_central_line))
        wkt2 <- paste0(wkt2, ',PARAMETER["Azimuth at projection center",', prj$azimuth_of_central_line, ',', angle, ']')
      if (!is.null(prj$scale_factor_at_projection_origin))
        wkt2 <- paste0(wkt2, ',PARAMETER["Scale factor at projection center",', prj$scale_factor_at_projection_origin, ',', .wkt2_uom(9201), ']')

      if (!(is.null(prj$false_easting) && is.null(prj$false_northing))) {
        if (is.null(prj$false_easting)) prj$false_easting <- 0
        if (is.null(prj$false_northing)) prj$false_northing <- 0
        wkt2 <- paste0(wkt2, ',PARAMETER["Easting at projection center",', prj$false_easting, ',', length,
                            '],PARAMETER["Northing at projection center",', prj$false_northing, ',', length, ']')
      }
      wkt2
    },

    ortho = function(angle, length) {
      # The x (abscissa) and y (ordinate) rectangular coordinates are identified by the standard_name
      # attribute values projection_x_coordinate and projection_y_coordinate respectively.
      prj <- self$attribute(c("latitude_of_projection_origin",
                              "longitude_of_projection_origin",
                              "false_easting",
                              "false_northing"))

      lon0 <- if (!is.null(prj$longitude_of_projection_origin)) prj$longitude_of_projection_origin else 0
      lat0 <- if (!is.null(prj$latitude_of_projection_origin)) prj$latitude_of_projection_origin else 0

      if (is.null(prj$false_easting) && is.null(prj$false_northing))
        fe_fn <- ''
      else {
        if (is.null(prj$false_easting)) prj$false_easting <- 0
        if (is.null(prj$false_northing)) prj$false_northing <- 0
        fe_fn <- paste0(',PARAMETER["False easting",', prj$false_easting, ',', length,
                        '],PARAMETER["False northing",', prj$false_northing, ',', length, ']')
      }

      paste0('METHOD["Orthographic",', .wkt2_id(9840), ']',
             ',PARAMETER["Latitude of natural origin",', lat0, ',', angle, ']',
             ',PARAMETER["Longitude of natural origin",', lon0, ',', angle, ']',
             fe_fn)
    },

    ps = function(angle, length) {
      # The x (abscissa) and y (ordinate) rectangular coordinates are identified by the standard_name
      # attribute values projection_x_coordinate and projection_y_coordinate respectively.
      prj <- self$attribute(c("longitude_of_projection_origin",
                              "latitude_of_projection_origin",
                              "straight_vertical_longitude_from_pole",
                              "scale_factor_at_projection_origin",
                              "standard_parallel",
                              "false_easting",
                              "false_northing"))

      lon0 <- if (!is.null(prj$longitude_of_projection_origin)) prj$longitude_of_projection_origin
              else if (!is.nul(prj$straight_vertical_longitude_from_pole)) prj$straight_vertical_longitude_from_pole
              else 0
      lat0 <- if (!is.null(prj$latitude_of_projection_origin)) prj$latitude_of_projection_origin else 0
      latts <- if (!is.null(prj$standard_parallel)) prj$standard_parallel else 0

      if (is.null(prj$false_easting) && is.null(prj$false_northing))
        fe_fn <- ''
      else {
        if (is.null(prj$false_easting)) prj$false_easting <- 0
        if (is.null(prj$false_northing)) prj$false_northing <- 0
        fe_fn <- paste0(',PARAMETER["False easting",', prj$false_easting, ',', length,
                        '],PARAMETER["False northing",', prj$false_northing, ',', length, ']')
      }

      if (!is.null(prj$scale_factor_at_projection_origin)) {
        paste0('METHOD["Polar Stereographic (variant A)",', .wkt2_id(9810), ']',
               ',PARAMETER["Latitude of natural origin",', lat0, ',', angle, ']',
               ',PARAMETER["Longitude of natural origin",', lon0, ',', angle, ']',
               ',PARAMETER["Scale factor at natural origin",', prj$scale_factor_at_projection_origin, ',', .wkt2_uom(9201), ']',
               fe_fn)
      } else {
        paste0('METHOD["Polar Stereographic (variant B)",', .wkt2_id(9829), ']',
               ',PARAMETER["Latitude of standard parallel",', latts, ',', angle, ']',
               ',PARAMETER["Longitude of origin",', lon0, ',', angle, ']',
               fe_fn)
      }
    },

    rot = function() {
      # The rotated latitude and longitude coordinates are identified by the standard_name attribute
      # values grid_latitude and grid_longitude respectively.
      # Not in the EPSG database
      prj <- self$attribute(c("grid_north_pole_latitude",
                              "grid_north_pole_longitude"))
      # CF argument north_pole_grid_longitude not used

      # The below is not elegant, but it fixes incomplete specification
      if (is.null(prj$grid_north_pole_latitude)) prj$grid_north_pole_latitude <- 0
      if (is.null(prj$grid_north_pole_longitude)) prj$grid_north_pole_longitude <- 0

      paste0('METHOD["PROJ ob_tran o_proj=latlon o_lat_p=', prj$grid_north_pole_latitude,
             ' o_lon_p=', prj$grid_north_pole_longitude, '"]')
    },

    sinus = function() {
      # The x (abscissa) and y (ordinate) rectangular coordinates are identified by the standard_name
      # attribute values projection_x_coordinate and projection_y_coordinate respectively.
      # Not in the EPSG database
      prj <- self$attribute(c("longitude_of_projection_origin",
                              "false_easting",
                              "false_northing"))

      lon0 <- if (!is.null(prj$longitude_of_projection_origin)) longitude_of_projection_origin else 0

      if (is.null(prj$false_easting) && is.null(prj$false_northing))
        fe_fn <- ''
      else {
        if (is.null(prj$false_easting)) prj$false_easting <- 0
        if (is.null(prj$false_northing)) prj$false_northing <- 0
        fe_fn <- paste0(' x_0=', prj$false_easting, ' y_0=', prj$false_northing)
      }

      paste0('METHOD["PROJ sinu o_lon=', lon, fe_fn, '"]')
    },

    stereo = function(angle, length) {
      # The x (abscissa) and y (ordinate) rectangular coordinates are identified by the standard_name
      # attribute values projection_x_coordinate and projection_y_coordinate respectively.
      prj <- self$attribute(c("longitude_of_projection_origin",
                              "latitude_of_projection_origin",
                              "scale_factor_at_projection_origin",
                              "false_easting",
                              "false_northing"))

      lon0 <- if (!is.null(prj$longitude_of_projection_origin)) prj$longitude_of_projection_origin else 0
      lat0 <- if (!is.null(prj$latitude_of_projection_origin)) prj$latitude_of_projection_origin else 0
      scl <- if (!is.null(prj$scale_factor_at_projection_origin)) prj$scale_factor_at_projection_origin else 1

      if (is.null(prj$false_easting) && is.null(prj$false_northing))
        fe_fn <- ''
      else {
        if (is.null(prj$false_easting)) prj$false_easting <- 0
        if (is.null(prj$false_northing)) prj$false_northing <- 0
        fe_fn <- paste0(',PARAMETER["False easting",', prj$false_easting, ',', length,
                        '],PARAMETER["False northing",', prj$false_northing, ',', length, ']')
      }

      paste0('METHOD["Oblique Stereographic",', .wkt2_id(9809), ']',
             ',PARAMETER["Latitude of natural origin",', lat0, ',', angle, ']',
             ',PARAMETER["Longitude of natural origin",', lon0, ',', angle, ']',
             ',PARAMETER["Scale factor at natural origin",', scl, ',', .wkt2_uom(9201), ']',
             fe_fn)
    },

    tmerc = function(angle, length) {
      # The x (abscissa) and y (ordinate) rectangular coordinates are identified by the standard_name
      # attribute values projection_x_coordinate and projection_y_coordinate respectively.
      # EPSG:9807
      prj <- self$attribute(c("longitude_of_central_meridian",
                              "latitude_of_projection_origin",
                              "scale_factor_at_central_meridian",
                              "false_easting",
                              "false_northing"))

      lon0 <- if (!is.null(prj$longitude_of_central_meridian)) prj$longitude_of_central_meridian else 0
      lat0 <- if (!is.null(prj$latitude_of_projection_origin)) prj$latitude_of_projection_origin else 0
      scl <- if (!is.null(prj$scale_factor_at_central_meridian)) prj$scale_factor_at_central_meridian else 1

      if (is.null(prj$false_easting) && is.null(prj$false_northing))
        fe_fn <- ''
      else {
        if (is.null(prj$false_easting)) prj$false_easting <- 0
        if (is.null(prj$false_northing)) prj$false_northing <- 0
        fe_fn <- paste0(',PARAMETER["False easting",', prj$false_easting, ',', length,
                        '],PARAMETER["False northing",', prj$false_northing, ',', length, ']')
      }

      paste0('METHOD["Transverse Mercator",', .wkt2_id(9807), ']',
             ',PARAMETER["Latitude of natural origin",', lat0, ',', angle, ']',
             ',PARAMETER["Longitude of natural origin",', lon0, ',', angle, ']',
             ',PARAMETER["Scale factor at natural origin",', scl, ',', .wkt2_uom(9201), ']',
             fe_fn)
    },

    persp = function(angle) {
      # The x (abscissa) and y (ordinate) rectangular coordinates are identified by the standard_name
      # attribute values projection_x_coordinate and projection_y_coordinate respectively.
      prj <- self$attribute(c("longitude_of_projection_origin",
                              "latitude_of_projection_origin",
                              "perspective_point_height"))
      # CF parameters false_easting and false_northing not used in EPSG

      # The first line below is not elegant, but it fixes incomplete specification
      h <- if (is.null(prj$perspective_point_height)) prj$perspective_point_height else 0
      lon0 <- if (!is.null(prj$longitude_of_projection_origin)) prj$longitude_of_projection_origin else 0
      lat0 <- if (!is.null(prj$latitude_of_projection_origin)) prj$latitude_of_projection_origin else 0

      paste0('METHOD["Vertical Perspective",', .wkt2_id(9838), ']',
             ',PARAMETER["Latitude of topocentric origin",', lat0, ',', angle, ']',
             ',PARAMETER["Longitude of topocentric origin",', lon0, ',', angle, ']',
             ',PARAMETER["Viewpoint height",', h, ',', .wkt2_uom(9001), ']')
    },

    # === CRS objects ==========================================================

    # This method creates an UOM WKT2 string from a specified UOM name. This is
    # typically called to describe the UOM of CFVariable axes for the CS section
    # of a full WKT2 string for a CFVariable or CFData instance.
    # Known aliases (by the EPSG database) are interpreted correctly, with the
    # resulting WKT2 string using the default name of the UOM. If no alias is
    # found, the method returns an empty string, otherwise the WKT2 string.
    # If argument `id` is `TRUE`, include an ID section, exclude otherwise.
    uom = function(name, id = FALSE) {
      if (name %in% c("degrees", "meters", "kilometers", "metres", "kilometres",
                      "seconds", "minutes", "hours", "days", "months", "years"))
        name <- trimws(name, whitespace = "s")
      details <- epsg_uom[epsg_uom$unit_of_meas_name == name, ]
      if (nrow(details)) .wkt2_uom(details$uom_code, id)
      else {
        details <- epsg_uom_alias[epsg_uom_alias$alias == name, ]
        if (nrow(details)) .wkt2_uom(details$object_code, id)
        else return('')
      }
    },

    # Create an ellipsoid WKT2 from attributes by finding an EPSG code by (1)
    # name or (2) other attributes, or (3) otherwise a "manual" string from
    # attributes, or (4) an empty string if no attributes are present, in that
    # order. Note that the semi_major_axis, semi_minor_axis and earth_radius are
    # required to be in meters, meaning that 9 EPSG ellipsoids cannot be
    # represented through attributes.
    ellipsoid = function() {
      atts <- self$attribute(c("reference_ellipsoid_name", "semi_major_axis", "semi_minor_axis", "inverse_flattening", "earth_radius"))

      # (1) Find an EPSG code by name, set ellipsoid name otherwise
      if (!is.null(atts$reference_ellipsoid_name)) {
        epsg_code <- epsg_ell[epsg_ell$ellipsoid_name == atts$reference_ellipsoid_name, "ellipsoid_code"]
        if (length(epsg_code)) return(.wkt2_ellipsoid(epsg_code))
        ell_name <- atts$reference_ellipsoid_name # For manual ellipsoid
      } else ell_name <- 'unknown'

      a <- atts$semi_major_axis
      b <- atts$semi_minor_axis
      invf <- atts$inverse_flattening
      r <- atts$earth_radius

      # (2) Find an EPSG code by set of attributes
      details <- if (!is.null(a)) {
        if (!is.null(invf))
          epsg_ell[isTRUE(all.equal(c(epsg_ell$semi_major_axis, epsg_ell$inv_flattening), c(a, invf))), ]
        else if (!is.null(b))
          epsg_ell[isTRUE(all.equal(c(epsg_ell$semi_major_axis, epsg_ell$semi_minor_axis), c(a, b))), ]
        else
          epsg_ell[isTRUE(all.equal(c(epsg_ell$semi_major_axis, epsg_ell$semi_minor_axis), c(a, a))), ]
      } else if (!is.null(r))
        epsg_ell[isTRUE(all.equal(epsg_ell$semi_major_axis, r)), ]
      else data.frame()

      if (nrow(details) == 1L)
        .wkt2_ellipsoid(details$ellipsoid_code)
      else {
        # (3) Manual WKT
        uom <- .wkt2_uom(9001L) # CF only supports meter uom
        if (!is.null(a)) {
          if (is.null(invf))
            infv <- if (is.null(b)) 0 else a / (a - b)
          paste0('ELLIPSOID["', ell_name, '",', a, ',', invf, ',', uom, ']')
        } else if (!is.null(r)) {
          paste0('ELLIPSOID["', ell_name, '",', r, ',0,', uom, ']')
        } else '' # (4) fail
      }
    },

    # Create a prime meridian WKT2 from attributes by finding an EPSG code by
    # name or longitude, or an empty string if no attributes are present.
    # The return value is a list with two elements: WKT2 holds the WKT2 string,
    # ANGLEUNIT holds the EPSG code of the UOM.
    prime_meridian = function() {
      atts <- self$attribute(c("prime_meridian_name", "longitude_of_prime_meridian"))
      details <- if (!is.null(atts$prime_meridian_name)) {
        epsg_pm[epsg_pm$prime_meridian_name == atts$prime_meridian_name, ]
      } else if (!is.null(atts$longitude_of_prime_meridian)) {
        epsg_pm[isTRUE(all.equal(epsg_pm$greenwich_longitude, atts$longitude_of_prime_meridian)), ]
      } else data.frame()
      if (nrow(details)) list(WKT2 = .wkt2_pm(details$prime_meridian_code), ANGLEUNIT = details$uom_code)
      else list(WKT2 = '', ANGLEUNIT = 9102)
    },

    # Return the EPSG code of the ANGLEUNIT of the prime meridian used by the
    # geodetic datum or its ensemble.
    datum_angleunit = function(epsg_code) {
      datum <- epsg_datum[epsg_datum$datum_code == epsg_code, ]
      if (datum$datum_type == "vertical")
        stop("Only call this function for a geodetic datum.")

      if (datum$datum_type == "ensemble") {
        members <- epsg_datum_ensemble[epsg_datum_ensemble$datum_ensemble_code == datum$datum_code, ]
        datum <- epsg_datum[epsg_datum$datum_code == members$member_code[1L], ]
      }

      epsg_pm[epsg_pm$prime_meridian_code == datum$prime_meridian_code[1L], "uom_code"]
    },

    # Create a geodetic datum WKT2 from attributes by finding an EPSG code by
    # name. If there is no attribute to name the datum or the name is not
    # present in the EPSG database, an ellipsoid is sought for. If an ellipsoid
    # is found, a "manual" WKT is made, possibly with a prime meridian if
    # specified. Otherwise a WGS84 datum is returned, as the default. Note that
    # this procedure extends the CF guidance, which requires that any name be
    # a valid EPSG database datum name.
    # The return value is a list with two elements: WKT2 holds the WKT2 string,
    # ANGLEUNIT holds the EPSG code of the UOM used by the datum
    datum_geo = function() {
      dtm_name <- self$attribute("horizontal_datum_name")

      # EPSG datum from attribute name
      if (nzchar(dtm_name)) {
        dtm_code <- epsg_datum[epsg_datum$cf_name == dtm_name, "datum_code"]
        if (!length(dtm_code))
          dtm_code <- epsg_datum[epsg_datum$datum_name == dtm_name, "datum_code"]
        if (length(dtm_code))
          return(list(source = "EPSG", WKT2 = .wkt2_datum_geo(dtm_code), ANGLEUNIT = private$datum_angleunit(dtm_code)))
      } else dtm_name <- 'unknown'

      # Manual
      ellipsoid <- private$ellipsoid()
      if (nzchar(ellipsoid)) {
        pm <- private$prime_meridian()
        if (nzchar(pm$WKT2)) pm$WKT2 <- paste0(',', pm$WKT2)
        list(source = "manual", WKT2 = paste0('DATUM["', dtm_name, '",', ellipsoid, ']', pm$WKT2), ANGLEUNIT = pm$ANGLEUNIT)
      } else
        list(source = "default", WKT2 = .wkt2_datum_geo(6326L), ANGLEUNIT = private$datum_angleunit(6326L))
    },

    datum_vert = function(name) {
      details <- epsg_datum[epsg_datum$datum_name == name, ]
      if (nrow(details) == 1L)
        .wkt2_datum_vert(details$datum_code)
      else paste0('VDATUM["', name, '"]')
    },

    # Coordinate system
    coordsys = function(epsg_code) {
      # FIXME: axis ordering
      .wkt2_coordsys(epsg_code)
    },

    # Either the epsg_code argument or the name argument has to be specified. If
    # both are specified, the name argument will be ignored.
    # The name argument is not unique. If zero or multiple rows are found, an
    # empty string is returned and a WKT will have to be constructed from other
    # supplied parameters, except when multiple rows are found and the
    # grid_mapping_name is "latitude_longitude" which will select the
    # "geographic 2D" version of the CRS (which should be always present).
    geo_crs = function(epsg_code = NULL, name = NULL) {
      if (is.null(epsg_code)) {
        details <- epsg_geo_crs[epsg_geo_crs$coord_ref_sys_name == name, ]
        if (!nrow(details)) return('')
        if (nrow(details) > 1L) {
          if (self$grid_mapping_name == "latitude_longitude")
            details <- details[details$coord_ref_sys_kind == "geographic 2D", ]
          else return('')
        }
        epsg_code <- details$coord_ref_sys_code
      }

      .wkt2_crs_geo(epsg_code)
    },

    # Either the epsg_code argument or the name argument has to be specified. If
    # both are specified, the name argument will be ignored.
    # The arguments may refer to a projected CRS or a compound CRS.
    #
    # A PROJCRS always has a base_crs_code, projection_conv_code and coord_sys_code
    proj_crs = function(epsg_code = NULL, name = NULL) {
      if (!is.null(epsg_code)) {
        details <- epsg_proj_crs[epsg_proj_crs$coord_ref_sys_code == epsg_code, ]
        if (nrow(details) == 0L) return(private$cmpd_crs(epsg_code, name))
      } else if (!is.null(name)) {
        details <- epsg_proj_crs[epsg_proj_crs$coord_ref_sys_name == name, ]
        if (nrow(details) == 0L)  return(private$cmpd_crs(epsg_code, name))
        if (nrow(details) > 1L) {
          warning(paste0("Multiple CRS entries found for '", name, "'. Using the first entry."))
          details <- details[1L, ]
        }
      } else return('')

      paste0('PROJCRS["', details$coord_ref_sys_name, '",',
             .wkt2_crs_base(details$base_crs_code), ',',
             .wkt2_conversion(details$projection_conv_code), ',',
             private$coordsys(details$coord_sys_code), ',',
             .wkt2_id(details$coord_ref_sys_code), ']')
    },

    vert_crs = function(epsg_code) {
      .wkt2_crs_vert(epsg_code)
    },

    # Either the epsg_code argument or the name argument has to be specified. If
    # both are specified, the name argument will be ignored.
    cmpd_crs = function(epsg_code = NULL, name = NULL) {
      if (is.null(epsg_code)) {
        details <- epsg_cmpd_crs[epsg_cmpd_crs$coord_ref_sys_name == name, ]
        if (nrow(details) == 0L)  return('')
        epsg_code <- details$coord_ref_sys_code
      }

      .wkt2_crs_compound(epsg_code)
    }
  ),

  public = list(
    #' @field grid_mapping_name The name of the grid mapping.
    grid_mapping_name = "",

    #' @description Create a new instance of this class.
    #' @param grp The group that contains the netCDF variable.
    #' @param nc_var The netCDF variable that describes this instance.
    #' @param name The formal grid mapping name from the attribute.
    initialize = function(grp, nc_var, name) {
      super$initialize(nc_var, grp)

      if(!(name %in% CRS_names))
        stop("Unsupported grid mapping: ", name)
      self$grid_mapping_name <- name
    },

    #' @description Prints a summary of the grid mapping to the console.
    print = function() {
      cat("<Grid mapping>", self$name, "\n")
      cat("Grid mapping name:", self$grid_mapping_name, "\n")
      if (self$group$name != "/")
        cat("Group            :", self$group$name, "\n")

      self$print_attributes()
    },

    #' @description Retrieve a 1-row `data.frame` with some information on this
    #'   grid mapping.
    brief = function() {
      data.frame(name = self$name, grid_mapping = self$grid_mapping_name)
    },

    #' @description Retrieve the CRS string for a specific variable.
    #' @param axis_info A list with information that describes the axes of the
    #' `CFVariable` or `CFData` instance to describe.
    #' @return A character string with the CRS in WKT2 format.
    crs = function(axis_info) {
      crs_attr <- self$attribute("crs_wkt")
      if (nzchar(crs_attr)) return(crs_attr)

      projcrs_name <- self$attribute("projected_crs_name")
      if (nzchar(projcrs_name)) {
        proj <- private$proj_crs(name = projcrs_name)
        if (nzchar(proj)) return(proj)
      } else projcrs_name <- 'unknown'

      geocrs_name <- self$attribute("geographic_crs_name")
      if (nzchar(geocrs_name)) {
        geo <- private$geo_crs(name = geocrs_name)
        if (nzchar(geo)) return(geo)
      } else geocrs_name <- 'unknown'

      # If code reaches here, any set name is not present in the EPSG
      # database so create a manual WKT from it
      datum <- private$datum_geo()
      angle <- .wkt2_uom(datum$ANGLEUNIT)

      if (self$grid_mapping_name == "latitude_longitude")
        return(paste0('GEOGCRS["', geocrs_name, '",', datum$WKT2,
                      ',CS[ellipsoidal,2],AXIS["north (Lat)",north,ORDER[1],',
                      angle, '],AXIS["east (Lon)",east,ORDER[2],', angle, ']]'))

      length <- private$uom(axis_info$LENGTHUNIT)

      # Is there a vertical axis?
      vert <- self$attribute(c("geoid_name", "geopotential_datum_name"))
      vdatum <- if (!is.null(vert$geoid_name)) vert$geoid_name
                else if (!is.null(vert$geopotential_datum_name)) vert$geopotential_datum_name
                else ''

      # Compound CRS
      wkt <- if (nzchar(vdatum)) 'COMPOUNDCRS["unknown",' else ''

      # Projected CRS
      wkt <- paste0(wkt, 'PROJCRS["', projcrs_name, '",BASEGEOGCRS["', geocrs_name,
                    '",', datum$WKT2, '],CONVERSION["unknown",')

      # Coordinate operation method
      wkt <- paste0(wkt,
             switch(self$grid_mapping_name,
               "albers_conical_equal_area" = private$aea(angle, length),
               "azimuthal_equidistant" = private$aeqd(angle, length),
               "geostationary" = private$geos(),
               "lambert_azimuthal_equal_area" = private$laea(angle, length),
               "lambert_conformal_conic" = private$lcc(angle, length),
               "lambert_cylindrical_equal_area" = private$lcea(angle, length),
               "mercator" = private$merc(angle, length),
               "oblique_mercator" = private$omerc(angle, length),
               "orthographic" = private$ortho(angle, length),
               "polar_stereographic" = private$ps(angle, length),
               "rotated_latitude_longitude" = private$rot(),
               "sinusoidal" = private$sinus(),
               "stereographic" = private$stereo(angle, length),
               "transverse_mercator" = private$tmerc(angle, length),
               "vertical_perspective" = private$persp(angle)
             ))

      # Coordinate system and close the PROJCRS
      wkt <- paste0(wkt, '],CS[Cartesian,2],AXIS["(E)",east,ORDER[1]],AXIS["(N)",north,ORDER[2]],', length, ']')

      # Compound CRS?
      if (nzchar(vdatum)) {
        vert <- axis_info$VERTINFO
        wkt <- paste0(wkt, ',VERTCRS["unknown",', private$datum_vert(vdatum),
                      ',CS[vertical,1],AXIS["', vert$standard_name, '",',
                      vert$positive, '],', private$uom(vert$units), ']]')
      }

      wkt
    }
  ),

  active = list(
    #' @field friendlyClassName (read-only) A nice description of the class.
    friendlyClassName = function(value) {
      if (missing(value))
        "Grid mapping"
    }
  )
)
