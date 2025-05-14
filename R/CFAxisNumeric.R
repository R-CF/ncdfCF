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
    #' @param nc_var The netCDF variable that describes this instance.
    #' @param nc_dim The netCDF dimension that describes the dimensionality.
    #' @param orientation The orientation (`X`, `Y`, `Z`, or `T`) or `""` if
    #' different or unknown.
    #' @param values The coordinates of this axis.
    initialize = function(nc_var, nc_dim, orientation, values) {
      super$initialize(nc_var, nc_dim, orientation)
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
        cat("Coordinates: ", paste(crds, collapse = ", "), "\n", sep = "")
      else
        cat("Coordinates: ", crds[1L], ", ", crds[2L], ", ", crds[3L], " ... ", crds[len - 2L], ", ", crds[len - 1L], ", ", crds[len], "\n", sep = "")

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

    #' @description Retrieve the indices of supplied coordinates on the axis. If
    #'   the axis has bounds then the supplied coordinates must fall within the
    #'   bounds to be considered valid.
    #' @param x A numeric vector of coordinates whose indices into the axis to
    #'   extract.
    #' @param method Extract index values without ("constant", the default) or
    #'   with ("linear") fractional parts.
    #' @param rightmost.closed Whether or not to include the upper limit. This
    #'   parameter is ignored for this class, effectively it always is `TRUE`.
    #' @return A vector giving the indices in `x` of valid coordinates provided.
    #'   Values of `x` outside of the range of the coordinates in the axis are
    #'   returned as `NA`.
    indexOf = function(x, method = "constant", rightmost.closed = TRUE) {
      vals <- private$values
      if (inherits(self$bounds, "CFBounds")) {
        # Axis has bounds so get the closest coordinate first, allow for extremes
        idx <- .round(stats::approx(vals, 1L:length(vals), x, method = "linear", rule = 2)$y)
        # Test that `x` falls within the bounds of the coordinates
        bnds <- self$bounds$bounds
        valid <- (bnds[1L, idx] <= x) & (x <= bnds[2L, idx])
        idx[!valid] <- NA
      } else {
        # No bounds so get the closest value
        idx <- stats::approx(vals, 1L:length(vals), x, method = method)$y
      }

      if (method == "constant")
        as.integer(idx)
      else
        idx
    },

    #' @description Tests if the axis passed to this method is identical to
    #'   `self`.
    #' @param axis The `CFAxisNumeric` or sub-class instance to test.
    #' @return `TRUE` if the two axes are identical, `FALSE` if not.
    identical = function(axis) {
      super$identical(axis) &&
      all(.near(private$values, axis$values))
    },

    #' @description Tests if the axis passed to this method can be appended to
    #'   `self`. This means that once the values in the passed axis are appended
    #'   to the values in `self`, the resulting vector must be monotonically
    #'   increasing or decreasing.
    #' @param axis The `CFAxisNumeric` or descendant class instance to test.
    #' @return `TRUE` if the passed axis can be appended to `self`, `FALSE` if
    #'   not.
    can_append = function(axis) {
      self_len <- self$length
      axis_len <- axis$length
      if (self_len > 1L) self_incr <- private$values[2L] > private$values[1L]
      if (axis_len > 1L) axis_incr <- axis$values[2L] > axis$values[1L]
      super$can_append(axis) &&
        !any(axis$values %in% private$values)
    },

    #' @description Return an axis spanning a smaller coordinate range. This
    #'   method returns an axis which spans the range of indices given by the
    #'   `rng` argument.
    #'
    #' @param group The group to create the new axis in.
    #' @param rng The range of indices whose values from this axis to include in
    #'   the returned axis.
    #'
    #' @return A `CFAxisNumeric` instance covering the indicated range of
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
        ax <- CFAxisNumeric$new(var, dim, self$orientation, private$values[rng[1L]:rng[2L]])
        bnds <- self$bounds
        if (inherits(bnds, "CFBounds")) ax$bounds <- bnds$sub_bounds(group, rng)
        private$subset_coordinates(ax, idx)
        ax
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
