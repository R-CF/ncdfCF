#' CF grid mapping object
#'
#' @description This class contains the details for a coordinate reference
#' system, or grid mapping in CF terms, of a data variable.
#'
#' @docType class
#'
#' @export
CFGridMapping <- R6::R6Class("CFGridMapping",
  inherit = CFObject,
  private = list(
    aea = function() {
      # +proj=aea +lat_1=29.5 +lat_2=42.5
      NULL
    },

    aeqd = function() {
      NULL
    },

    geos = function() {
      NULL
    },

    laea = function() {
      NULL
    },

    lcc = function() {
      NULL
    },

    lcea = function() {
      NULL
    },

    latlon = function() {
      NULL
    },

    merc = function() {
      NULL
    },

    omerc = function() {
      NULL
    },

    ortho = function() {
      NULL
    },

    ps = function() {
      NULL
    },

    rot = function() {
      atts <- self$attribute(c("grid_north_pole_latitude",
                               "grid_north_pole_longitude",
                               "north_pole_grid_longitude"))
      have <- nzchar(atts)
      if (have[1L] && have[2L]) {
        if (have[3L]) lon0 <- paste0(" +lon_0=", atts[3L])
        else lon0 <- ""
        paste0("+proj=ob_tran +o_proj=latlon +o_lat_p=", atts[1L], " +o_lon_p=", atts[2L], lon0)
      } else ""
    },

    sinus = function() {
      NULL
    },

    stereo = function() {
      NULL
    },

    tmerc = function() {
      NULL
    },

    persp = function() {
      NULL
    }
  ),
  public = list(
    #' @field group The [NCGroup] that this grid mapping is located in.
    group = NULL,

    #' @field grid_mapping_name The formal name of the grid mapping.
    grid_mapping_name = "",

    #' @description Create a new instance of this class.
    #' @param grp The group that contains the netCDF variable.
    #' @param nc_var The netCDF variable that describes this instance.
    #' @param name The formal grid mapping name from the attribute.
    initialize = function(grp, nc_var, name) {
      super$initialize(nc_var)
      self$group <- grp

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

    #' @description Retrieve a 1-row `data.frame` with some information on this grid mapping.
    brief = function() {
      data.frame(name = self$name, grid_mapping = self$grid_mapping_name)
    },

    #' @description Retrieve the CRS string.
    #' @return A character string with the CRS. The format depends on how it is
    #' stored in the netCDF resource.
    crs = function() {
      crs_attr <- self$attribute("crs_wkt")
      if (length(crs_attr)) crs_attr
      else {
        switch(self$grid_mapping_name,
               "albers_conical_equal_area" = private$aea(),
               "azimuthal_equidistant" = private$aeqd(),
               "geostationary" = private$geos(),
               "lambert_azimuthal_equal_area" = private$laea(),
               "lambert_conformal_conic" = private$lcc(),
               "lambert_cylindrical_equal_area" = private$lcea(),
               "latitude_longitude" = private$latlon(),
               "mercator" = private$merc(),
               "oblique_mercator" = private$omerc(),
               "orthographic" = private$ortho(),
               "polar_stereographic" = private$ps(),
               "rotated_latitude_longitude" = private$rot(),
               "sinusoidal" = private$sinus(),
               "stereographic" = private$stereo(),
               "transverse_mercator" = private$tmerc(),
               "vertical_perspective" = private$persp()
              )
      }
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
