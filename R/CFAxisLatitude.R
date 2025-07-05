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

# ============ Helper functions

# This function checks if the supplied coordinates are within the domain of
# latitude values. The values have to be numeric and monotonic and in
# the range [-90, 90]. Returns TRUE or FALSE.
.check_latitude_domain <- function(crds) {
  len <- length(crds)
  if (!len) TRUE
  else if (is.numeric(crds))
    switch(.monotonicity(crds) + 2L,
           crds[len] >= -90 && crds[1L] <= 90,
           FALSE,
           crds[1L] >= -90 && crds[len] <= 90)
  else FALSE
}
