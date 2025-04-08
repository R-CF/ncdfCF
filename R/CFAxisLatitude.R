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
    #' @param grp The group that contains the netCDF variable.
    #' @param nc_var The netCDF variable that describes this instance.
    #' @param nc_dim The netCDF dimension that describes the dimensionality.
    #' @param values The coordinates of this axis.
    initialize = function(grp, nc_var, nc_dim, values) {
      super$initialize(grp, nc_var, nc_dim, "Y", values)
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
    #'   indices. If the `rng` argument includes only a single value, an
    #'   [CFAxisScalar] instance is returned with the value from this axis. If
    #'   the value of the argument is `NULL`, return the entire axis (possibly
    #'   as a scalar axis).
    subset = function(group, rng = NULL) {
      var <- NCVariable$new(-1L, self$name, group, "NC_DOUBLE", 1L, NULL)

      .make_scalar <- function(idx) {
        scl <- CFAxisScalar$new(group, var, "Y", idx)
        bnds <- self$bounds
        if (inherits(bnds, "CFBounds")) scl$bounds <- bnds$sub_bounds(group, idx)
        private$subset_coordinates(scl, idx)
        scl
      }

      if (is.null(rng)) {
        if (length(private$values) > 1L) {
          ax <- self$clone()
          ax$group <- group
          ax
        } else
          .make_scalar(1L)
      } else {
        if (rng[1L] == rng[2L])
          .make_scalar(private$values[rng[1L]])
        else {
          dim <- NCDimension$new(-1L, self$name, rng[2L] - rng[1L] + 1L, FALSE)
          lat <- CFAxisLatitude$new(group, var, dim, private$values[rng[1L]:rng[2L]])
          bnds <- self$bounds
          if (inherits(bnds, "CFBounds")) lat$bounds <- bnds$sub_bounds(group, rng)
          private$subset_coordinates(lat, idx)
          lat
        }
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
