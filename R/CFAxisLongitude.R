#' Longitude CF axis object
#'
#' @description This class represents a longitude axis. Its values are numeric.
#' This class is used for axes that represent longitudes.
#'
#' This class adds some logic that is specific to longitudes, such as their
#' range, orientation and their meaning. (In the near future, it will also
#' support selecting data that crosses the 0-360 degree boundary.)
#'
#' @docType class
#'
#' @name CFAxisLongitude
#' @format An [R6Class] generator object.
NULL

#' @export
CFAxisLongitude <- R6::R6Class("CFAxisLongitude",
  inherit = CFAxisNumeric,
  public = list(

    initialize = function(grp, nc_var, nc_dim, values) {
      super$initialize(grp, nc_var, nc_dim, "X", values)
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
