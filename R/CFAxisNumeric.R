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
  cloneable = FALSE,
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
      if (any(is.double(crds))) round(crds, CF.options$digits)
      else crds
    },

    dimvalues_short = function() {
      crds <- private$get_coordinates()
      nv <- length(private$values)
      if (nv == 1L)
        paste0("[", crds[1L], "]")
      else
        paste0("[", crds[1L], " ... ", crds[nv], "]")
    },

    # This function allows descendant classes to print more detail. This stub
    # does nothing but satisfy the call to it from print()
    print_details = function(...) {
    }
  ),
  public = list(
    #' @description Create a new instance of this class.
    #'
    #'   Creating a new axis is more easily done with the [makeAxis()] function.
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
      if (private$active_coords == 1L) {
        crds <- private$get_coordinates()
        units <- self$attribute("units")
      } else {
        crds <- private$aux[[private$active_coords - 1L]]$coordinates
        units <- private$aux[[private$active_coords - 1L]]$attribute("units")
      }
      if (is.na(units)) units <- ""
      if (units == "1") units <- ""

      len <- length(crds)
      if (len < 8L)
        cat("Coordinates: ", paste(crds, collapse = ", "), sep = "")
      else
        cat("Coordinates: ", crds[1L], ", ", crds[2L], ", ", crds[3L], " ... ", crds[len - 2L], ", ", crds[len - 1L], ", ", crds[len], sep = "")
      if (units == "") cat("\n") else cat(" (", units, ")\n", sep = "")

      if (!is.null(self$bounds))
        self$bounds$print(...)
      else cat("Bounds     : (not set)\n")

      private$print_details(...)

      self$print_attributes(...)
      invisible(self)
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

    #' @description Create a copy of this axis. The copy is completely separate
    #' from `self`, meaning that both `self` and all of its components are made
    #' from new instances. This can copy a `CFAxisNumeric` instance, but also a
    #' `CFAxisLongitude` or `CFAxisLatitude` instance.
    #' @param group The group in which to place the new axis. If the argument is
    #' missing, a new group will be created for the axis with a link to the
    #' netCDF resource of `self`, if set.
    copy = function(group) {
      if (missing(group))
        group <- makeGroup(resource = self$group$resource)

      bnds <- self$bounds
      if (inherits(bnds, "CFBounds")) bnds <- bnds$coordinates
      ax <- makeAxis(self$name, group, self$orientation, private$values, bnds, self$attributes)
      private$subset_coordinates(ax, c(1L, self$length))
      ax
    },

    #' @description Tests if the axis passed to this method is identical to
    #'   `self`.
    #' @param axis The `CFAxisNumeric` or sub-class instance to test.
    #' @return `TRUE` if the two axes are identical, `FALSE` if not.
    identical = function(axis) {
      super$identical(axis) &&
      all(.near(private$values, axis$values))
    },

    #' @description Append a vector of values at the end of the current values
    #'   of the axis.
    #' @param from An instance of `CFAxisNumeric` or any of its descendants
    #'   whose values to append to the values of `self`.
    #' @return A new `CFAxisNumeric` or descendant instance with values from
    #'   self and the `from` axis appended.
    append = function(from) {
      if (super$can_append(from) && .c_is_monotonic(private$values, from$values)) {
        bnds <- if (is.null(self$bounds) || is.null(from$bounds)) NULL
                else cbind(self$bounds, from$bounds)
        makeAxis(self$name, makeGroup(), axis$orientation, c(private$values, from$values), bnds, self$attributes)
      } else
        stop("Axis values cannot be appended.", call. = FALSE)
    },

    #' @description Return an axis spanning a smaller coordinate range. This
    #'   method returns an axis which spans the range of indices given by the
    #'   `rng` argument.
    #'
    #' @param group The group to create the new axis in.
    #' @param rng The range of indices whose values from this axis to include in
    #'   the returned axis.
    #'
    #' @return A new `CFAxisNumeric` instance covering the indicated range of
    #'   indices. If the value of the argument is `NULL`, return a copy of
    #'   `self` as the new axis.
    subset = function(group, rng = NULL) {
      if (is.null(rng))
        self$copy(group)
      else {
        rng <- range(rng)
        bnds <- self$bounds
        if (inherits(bnds, "CFBounds")) bnds <- bnds$coordinates[, rng[1L]:rng[2L]]
        ax <- makeAxis(self$name, group, self$orientation, private$values[rng[1L]:rng[2L]], bnds, self$attributes)
        private$subset_coordinates(ax, rng)
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
