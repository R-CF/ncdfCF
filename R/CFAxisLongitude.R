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

# ============ Helper functions

# This function checks if the supplied coordinates are within the domain of
# longitude values. The values have to be numeric and monotonic and be either in
# the range [-180, 180] or [0, 360]. Returns TRUE or FALSE.
.check_longitude_domain <- function(crds) {
  len <- length(crds)
  if (!len) TRUE
  else if (is.numeric(crds))
    switch(.monotonicity(crds) + 2L,
           (crds[len] >= -180 && crds[1L] <= 180) || (crds[len] >= 0 && crds[1L] <= 360),
           FALSE,
           (crds[1L] >= -180 && crds[len] <= 180) || (crds[1L] >= 0 && crds[len] <= 360))
  else FALSE
}
