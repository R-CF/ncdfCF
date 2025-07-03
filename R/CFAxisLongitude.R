#' Longitude CF axis object
#'
#' @description This class represents a longitude axis. Its values are numeric.
#'   This class is used for axes that represent longitudes. This class adds some
#'   logic that is specific to longitudes, such as their range, orientation and
#'   their meaning. (In the near future, it will also support selecting data
#'   that crosses the 0-360 degree boundary.)
#'
#' @docType class
#' @export
CFAxisLongitude <- R6::R6Class("CFAxisLongitude",
  inherit = CFAxisNumeric,
  public = list(

    #' @description Create a new instance of this class.
    #'
    #'   Creating a new longitude axis is more easily done with the
    #'   [makeLongitudeAxis()] function.
    #' @param nc_var The netCDF variable that describes this instance.
    #' @param nc_dim The netCDF dimension that describes the dimensionality.
    #' @param values The coordinates of this axis.
    initialize = function(nc_var, nc_dim, values) {
      super$initialize(nc_var, nc_dim, "X", values)
    }
  ),
  active = list(
   #' @field friendlyClassName (read-only) A nice description of the class.
   friendlyClassName = function(value) {
     if (missing(value))
       "Longitude axis"
   }
  )
)
