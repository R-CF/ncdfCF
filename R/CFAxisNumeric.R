#' Numeric CF axis object
#'
#' @description This class represents a numeric axis. Its values are numeric.
#' This class is used for axes with numeric values but without further
#' knowledge of their nature. More specific classes descend from this class.
#'
#' @docType class
#' @export
CFAxisNumeric <- R6::R6Class("CFAxisNumeric",
  inherit = CFAxis,
  private = list(
    # The values of the axis, usually a numeric vector.
    values = NULL,

    # The raw values of the axis
    get_values = function() {
      private$values
    },

    # Coordinate values of the axis. This may be a label set. Double values are
    # rounded to the standard number of digits.
    get_coordinates = function() {
      crds <- super$get_coordinates()
      if (any(is.double(crds))) round(crds, CF$digits)
      else crds
    },

    dimvalues_short = function() {
      crds <- self$coordinates
      nv <- length(private$values)
      if (nv == 1L)
        paste0("[", crds[1L], "]")
      else
        paste0("[", crds[1L], " ... ", crds[nv], "]")
    }
  ),
  public = list(
    #' @description Create a new instance of this class.
    #' @param grp The group that contains the netCDF variable.
    #' @param nc_var The netCDF variable that describes this instance.
    #' @param nc_dim The netCDF dimension that describes the dimensionality.
    #' @param orientation The orientation (`X`, `Y`, `Z`, or `T`) or `""` if
    #' different or unknown.
    #' @param values The coordinates of this axis.
    initialize = function(grp, nc_var, nc_dim, orientation, values) {
      super$initialize(grp, nc_var, nc_dim, orientation)
      private$values <- values
      self$set_attribute("actual_range", nc_var$vtype, range(values))
    },

    #' @description Summary of the axis printed to the console.
    #' @param ... Arguments passed on to other functions. Of particular interest
    #' is `width = ` to indicate a maximum width of attribute columns.
    #' @return `self`, invisibly.
    print = function(...) {
      super$print()

      units <- self$attribute("units")
      if (is.na(units)) units <- ""
      if (units == "1") units <- ""

      crds <- self$coordinates
      len <- length(crds)
      if (len < 8L)
        cat("Values     : ", paste(crds, collapse = ", "), "\n", sep = "")
      else
        cat("Values     : ", crds[1L], ", ", crds[2L], ", ", crds[3L], " ... ", crds[len - 2L], ", ", crds[len - 1L], ", ", crds[len], "\n", sep = "")

      if (!is.null(self$bounds))
        self$bounds$print(...)
      else cat("Bounds     : (not set)\n")

      self$print_attributes(...)
    },

    #' @description Some details of the axis.
    #'
    #' @return A 1-row `data.frame` with some details of the axis.
    brief = function() {
      out <- super$brief()
      out$values <- private$dimvalues_short()
      out
    },

    #' @description Retrieve the range of coordinate values in the axis.
    #' @return A numeric vector with two elements with the minimum and maximum
    #' values in the axis, respectively.
    range = function() {
      range(private$values)
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
        vals <- c(self$bounds$bounds[1L, 1L], self$bounds$bounds[2L, ])
      else vals <- private$values
      idx <- stats::approx(vals, 1L:length(vals), x, method = method, yleft = 0L, yright = .Machine$integer.max)$y
      idx <- idx[!is.na(idx) & idx > 0 & idx < .Machine$integer.max]
      as.integer(idx)
    },

    #' @description Return an axis spanning a smaller coordinate range. This
    #'   method returns an axis which spans the range of indices given by the
    #'   `rng` argument.
    #'
    #' @param group The group to create the new axis in.
    #' @param rng The range of values from this axis to include in the returned
    #'   axis.
    #'
    #' @return A `CFAxisNumeric` instance covering the indicated range of
    #'   indices. If the `rng` argument includes only a single value, an
    #'   [CFAxisScalar] instance is returned with the value from this axis. If
    #'   the value of the argument is `NULL`, return the entire axis (possibly
    #'   as a scalar axis).
    subset = function(group, rng = NULL) {
      var <- NCVariable$new(-1L, self$name, group, "NC_DOUBLE", 1L, NULL)

      .make_scalar <- function(idx) {
        scl <- CFAxisScalar$new(group, var, self$orientation, idx)
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
          ax <- CFAxisNumeric$new(group, var, dim, self$orientation, private$values[rng[1L]:rng[2L]])
          bnds <- self$bounds
          if (inherits(bnds, "CFBounds")) ax$bounds <- bnds$sub_bounds(group, rng)
          private$subset_coordinates(ax, idx)
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

    #' @field dimnames (read-only) The coordinates of the axis as a vector.
    #'   These are by default the values of the axis, but it could also be a set
    #'   of auxiliary coordinates, if they have been set.
    dimnames = function(value) {
      if (missing(value)) {
        self$coordinates
      }
    }
  )
)
