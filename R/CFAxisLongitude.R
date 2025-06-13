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
    },

    #' @description Return an axis spanning a smaller coordinate range. This
    #'   method returns an axis which spans the range of indices given by the
    #'   `rng` argument.
    #'
    #' @param group The group to create the new axis in.
    #' @param rng The range of values from this axis to include in the returned
    #'   axis.
    #'
    #' @return A `CFAxisLongitude` instance covering the indicated range of
    #'   indices. If the value of the argument is `NULL`, return the entire
    #'   axis.
    subset = function(group, rng = NULL) {
      var <- NCVariable$new(-1L, self$name, group, "NC_DOUBLE", 1L, NULL)

      if (is.null(rng)) {
        ax <- self$clone()
        ax$group <- group
        ax
      } else {
        rng <- range(rng)
        dim <- NCDimension$new(-1L, self$name, rng[2L] - rng[1L] + 1L, FALSE)
        lon <- CFAxisLongitude$new(var, dim, private$values[rng[1L]:rng[2L]])
        bnds <- self$bounds
        if (inherits(bnds, "CFBounds")) lon$bounds <- bnds$sub_bounds(group, rng)
        private$subset_coordinates(lon, idx)
        lon$attributes <- self$attributes
        lon$set_attribute("actual_range", self$NCvar$vtype, range(lon$values))
        lon
      }
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
