#' Latitude CF axis object
#'
#' @description This class represents a latitude axis. Its values are numeric.
#' This class adds some logic that is specific to latitudes, such as their
#' range, orientation and meaning.
#'
#' @docType class
#' @export
CFAxisLatitude <- R6::R6Class("CFAxisLatitude",
  inherit = CFAxisNumeric,
  public = list(

    #' @description Create a new instance of this class.
    #'
    #'   Creating a new latitude axis is more easily done with the
    #'   [makeLatitudeAxis()] function.
    #' @param nc_var The netCDF variable that describes this instance.
    #' @param nc_dim The netCDF dimension that describes the dimensionality.
    #' @param values The coordinates of this axis.
    initialize = function(nc_var, nc_dim, values) {
      super$initialize(nc_var, nc_dim, "Y", values)
    }
  ),
  active = list(
   #' @field friendlyClassName (read-only) A nice description of the class.
   friendlyClassName = function(value) {
     if (missing(value))
       "Latitude axis"
   }
  )
)
