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
    },

    #' @description Return an axis spanning a smaller coordinate range. This
    #'   method returns an axis which spans the range of indices given by the
    #'   `rng` argument.
    #'
    #' @param group The group to create the new axis in.
    #' @param rng The range of values from this axis to include in the returned
    #'   axis.
    #'
    #' @return A `CFAxisLatitude` instance covering the indicated range of
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
        lat <- CFAxisLatitude$new(var, dim, private$values[rng[1L]:rng[2L]])
        bnds <- self$bounds
        if (inherits(bnds, "CFBounds")) lat$bounds <- bnds$sub_bounds(group, rng)
        private$subset_coordinates(lat, idx)
        lat$attributes <- self$attributes
        lat$set_attribute("actual_range", self$NCvar$vtype, range(lat$values))
        lat
      }
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
