#' CF discrete axis object
#'
#' @description This class represent discrete CF axes, i.e. those axes whose
#' coordinate values do not represent a physical or categorical property. The
#' coordinate values are ordinal values equal to the index into the axis.
#'
#' @docType class
#'
#' @export
CFAxisDiscrete <- R6::R6Class("CFAxisDiscrete",
  inherit = CFAxis,
  public = list(
    #' @description Create a new instance of this class.
    #' @param grp The group that contains the netCDF variable.
    #' @param nc_var The netCDF variable that describes this instance.
    #' @param nc_dim The netCDF dimension that describes the dimensionality.
    #' @param orientation The orientation (`X`, `Y`, `Z`, or `T`) or `""` if
    #' different or unknown.
    initialize = function(grp, nc_var, nc_dim, orientation) {
      super$initialize(grp, nc_var, nc_dim, orientation)
    },

    #' @description Some details of the axis
    #'
    #' @return A 1-row `data.frame` with some details of the axis.
    brief = function() {
      out <- super$brief()
      out$values <- if (self$length == 1L) "[1]"
                    else paste0("[1 ... ", self$length, "]")
      out
    },

    #' @title Find indices in the axis domain
    #'
    #' @description Given a vector of numerical values `x`, find their indices
    #' in the values of the axis. In effect, this returns index values into the
    #' axis, but outside values will be dropped.
    #'
    #' @param x Vector of numeric values to find axis indices for.
    #' @param method Ignored.
    #'
    #' @return Numeric vector of the same length as `x`. Values of `x` outside
    #' of the range of the values in the axis are returned as `0` and
    #' `.Machine$integer.max`, respectively.
    indexOf = function(x, method = "constant") {
      x[x < 1] <- 0L
      x[x > self$length] <- .Machine$integer.max
      as.integer(x)
    }
  ),
  active = list(
    #' @field friendlyClassName (read-only) A nice description of the class.
    friendlyClassName = function(value) {
      if (missing(value))
        "Discrete axis"
    },

    #' @field dimnames (read-only) The coordinates of the axis as an integer
    #' vector.
    dimnames = function(value) {
      if (missing(value))
        seq(self$length)
    }
  )
)
