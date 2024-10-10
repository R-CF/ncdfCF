#' Numeric CF axis object
#'
#' @description This class represents a numeric axis. Its values are numeric.
#' This class is used for axes with numeric values but without further
#' knowledge of their nature. More specific classes descend from this class.
#'
#' @docType class
#'
#' @export
CFAxisNumeric <- R6::R6Class("CFAxisNumeric",
  inherit = CFAxis,
  public = list(
    #' @field values The values of the axis, usually a numeric vector.
    values     = NULL,

    #' @description Create a new instance of this class.
    #' @param grp The group that contains the netCDF variable.
    #' @param nc_var The netCDF variable that describes this instance.
    #' @param nc_dim The netCDF dimension that describes the dimensionality.
    #' @param orientation The orientation (`X`, `Y`, `Z`, or `T`) or `""` if
    #' different or unknown.
    #' @param values The dimension values of this axis.
    initialize = function(grp, nc_var, nc_dim, orientation, values) {
      super$initialize(grp, nc_var, nc_dim, orientation)
      self$values <- values
    },

    #' @description Summary of the time axis
    #'
    #' Prints a summary of the time axis to the console.
    print = function() {
      super$print()

      units <- self$attribute("units")
      if (!nzchar(units)) units <- ""
      len <- length(self$values)
      if (len < 7L) {
        vals <- trimws(formatC(self$values, digits = 8L))
        cat("Values   : ", paste(vals, collapse = ", "), " ", units, "\n", sep = "")
      } else {
        vals <- trimws(formatC(c(self$values[1L:3L], self$values[(len-2):len], digits = 8L)))
        cat("Values   : ", vals[1L], ", ", vals[2L], ", ", vals[3L], " ... ", vals[4L], ", ", vals[5L], ", ", vals[6L], " ", units, "\n", sep = "")
      }

      if (!is.null(self$bounds))
        self$bounds$print()
      else cat("Bounds   : (not set)\n")

      self$print_attributes()
    },

    #' @description Retrieve a 1-row `data.frame` with some information on this axis.
    brief = function() {
      out <- super$brief()

      nv <- length(self$values)
      if (nv == 1L)
        dims <- sprintf("[%s]", gsub(" ", "", formatC(self$values[1L], digits = 8L)))
      else {
        vals <- trimws(formatC(c(self$values[1L], self$values[nv]), digits = 8L))
        dims <- sprintf("[%s ... %s]", vals[1L], vals[2L])
      }

      out$values <- dims
      out
    },

    #' @description Retrieve the range of coordinate values in the axis.
    #' @return A numeric vector with two elements with the minimum and maximum
    #' values in the axis, respectively.
    range = function() {
      range(self$values)
    },

    #' @description Retrieve the indices of supplied values on the axis. If the
    #' axis has bounds then the supplied values must fall within the bounds to
    #' be considered valid.
    #' @param x A numeric vector of values whose indices into the axis to
    #' extract.
    #' @param method Extract index values without ("constant", the default) or
    #' with ("linear") fractional parts.
    #' @return An integer vector giving the indices in `x` of valid values
    #' provided, or `integer(0)` if none of the `x` values are valid.
    indexOf = function(x, method = "constant") {
      if (length(self$bounds))
        vals <- c(self$bounds$values[1L, 1L], self$bounds$values[2L, ])
      else vals <- self$values
      idx <- stats::approx(vals, 1L:length(vals), x, method = method, yleft = 0L, yright = .Machine$integer.max)$y
      idx <- idx[!is.na(idx) & idx > 0 & idx < .Machine$integer.max]
      as.integer(idx)
    },

    #' @description Return an axis spanning a smaller dimension range.
    #'
    #'   This method returns an axis which spans the range of indices given by
    #'   the `rng` argument.
    #'
    #' @param group The group to create the new axis in.
    #' @param rng The range of values from this axis to include in the returned
    #'   axis.
    #'
    #' @return A `CFAxisNumeric` covering the indicated range of indices. If
    #'   the `rng` argument includes only a single value, an [CFAxisScalar]
    #'   instance is returned with the value from this axis. If the value of the
    #'   argument is `NULL`, return the entire axis (possibly as a scalar axis).
    sub_axis = function(group, rng = NULL) {
      var <- NCVariable$new(-1L, self$name, group, "NC_DOUBLE", 1L, NULL)

      .make_scalar <- function(idx) {
        scl <- CFAxisScalar$new(group, var, self$orientation, idx)
        bnds <- self$bounds
        if (inherits(bnds, "CFBounds")) scl$bounds <- bnds$sub_bounds(group, idx)
        scl
      }

      if (is.null(rng)) {
        if (length(self$values) > 1L) {
          ax <- self$clone()
          ax$group <- group
          ax
        } else
          .make_scalar(1L)
      } else {
        if (rng[1L] == rng[2L])
          .make_scalar(self$values[rng[1L]])
        else {
          dim <- NCDimension$new(-1L, self$name, rng[2L] - rng[1L] + 1L, FALSE)
          ax <- CFAxisNumeric$new(group, var, dim, self$orientation, self$values[rng[1L]:rng[2L]])
          bnds <- self$bounds
          if (inherits(bnds, "CFBounds")) ax$bounds <- bnds$sub_bounds(group, rng)
          ax
        }
      }
    }

  ),
  active = list(
    #' @field friendlyClassName (read-only) A nice description of the class.
    friendlyClassName = function(value) {
      if (missing(value))
        "Generic numeric axis"
    },

    #' @field dimnames (read-only) The coordinates of the axis as a numeric
    #' vector.
    dimnames = function(value) {
      if (missing(value))
        self$values
    }
  )
)
