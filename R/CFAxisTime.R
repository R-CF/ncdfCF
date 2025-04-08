#' @importFrom CFtime unit as_timestamp
NULL

#' Time axis object
#'
#' @description This class represents a time axis. The functionality is provided
#' by the `CFTime` class in the `CFtime` package.
#'
#' @docType class
#' @export
CFAxisTime <- R6::R6Class("CFAxisTime",
  inherit = CFAxis,
  private = list(
    # The `CFTime` instance to manage CF time.
    tm = NULL,

    get_values = function() {
      private$tm$offsets
    },

    get_coordinates = function() {
      if (private$active_coords == 1L)
        private$tm$as_timestamp()
      else
        private$aux[[private$active_coords - 1L]]$coordinates
    },

    dimvalues_short = function() {
      crds <- self$coordinates
      nv <- length(crds)
      if (!nv) # it happens...
        "(no values)"
      else if (nv == 1L)
        paste0("[", crds[1L], "]")
      else
        sprintf("[%s ... %s]", crds[1L], crds[nv])
    }
  ),
  public = list(
    #' @description Create a new instance of this class.
    #' @param grp The group that contains the netCDF variable.
    #' @param nc_var The netCDF variable that describes this instance.
    #' @param nc_dim The netCDF dimension that describes the dimensionality.
    #' @param values The `CFTime` instance that manages this axis.
    initialize = function(grp, nc_var, nc_dim, values) {
      super$initialize(grp, nc_var, nc_dim, "T")
      private$tm <- values
      self$set_attribute("units", "NC_CHAR", values$cal$definition)
      self$set_attribute("calendar", "NC_CHAR", values$cal$name)
      self$set_attribute("standard_name", "NC_CHAR", "time")
      self$set_attribute("axis", "NC_CHAR", "T")
      self$set_attribute("actual_range", nc_var$vtype, range(values$offsets))
    },

    #' @description Summary of the time axis printed to the console.
    #' @param ... Arguments passed on to other functions. Of particular interest
    #' is `width = ` to indicate a maximum width of attribute columns.
    #' @return `self`, invisibly.
    print = function(...) {
      super$print()

      time <- private$tm
      crds <- self$coordinates
      len <- length(crds)
      rng <- if (len == 1L) crds[1L]
             else paste0(crds[1L], " ... ", crds[len], " (", time$unit, ")")
      bndrng <- if (!is.null(time$get_bounds()))
        paste0(time$range(format = "", bounds = TRUE), collapse = " ... ")
      else "(not set)"
      cat("Calendar   :", time$cal$name, "\n")
      cat("Range      :", rng, "\n")
      cat("Bounds     :", bndrng, "\n")

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

    #' @description Retrieve the `CFTime` instance that manages the values of
    #' this axis.
    #' @return An instance of `CFTime`.
    time = function() {
      private$tm
    },

    #' @description Retrieve the indices of supplied values on the time axis.
    #' @param x A vector of timestamps whose indices into the time axis to
    #' extract.
    #' @param method Extract index values without ("constant", the default) or
    #' with ("linear") fractional parts.
    #' @param rightmost.closed Whether or not to include the upper limit.
    #' Default is `FALSE`.
    #' @return An integer vector giving the indices in the time axis of valid
    #' values in `x`, or `integer(0)` if none of the values are valid.
    indexOf = function(x, method = "constant", rightmost.closed = FALSE) {
      time <- private$tm
      idx <- time$indexOf(x, method)
      idx <- idx[!is.na(idx) & idx > 0 & idx < .Machine$integer.max]
      len <- length(idx)
      if (!len) return (integer(0))
      if (!rightmost.closed) # FIXME: Is this correct????
        idx[len] <- idx[len] - 1
      as.integer(idx)
    },

    #' @description Retrieve the indices of the time axis falling between two
    #'   extreme values.
    #' @param x A vector of two timestamps in between of which all indices into
    #'   the time axis to extract.
    #' @param rightmost.closed Whether or not to include the upper limit.
    #'   Default is `FALSE`.
    #' @return An integer vector giving the indices in the time axis between
    #'   values in `x`, or `integer(0)` if none of the values are valid.
    slice = function(x, rightmost.closed = FALSE) {
      time <- private$tm
      idx <- time$slice(x, rightmost.closed)
      (1L:length(time))[idx]
    },

    #' @description Return an axis spanning a smaller coordinate range. This
    #'   method returns an axis which spans the range of indices given by the
    #'   `rng` argument.
    #'
    #' @param group The group to create the new axis in.
    #' @param rng The range of values from this axis to include in the returned
    #'   axis.
    #'
    #' @return A `CFAxisTime` instance covering the indicated range of indices.
    #'   If the `rng` argument includes only a single value, an [CFAxisScalar]
    #'   instance is returned with its value being the character timestamp of
    #'   the value in this axis. If the value of the argument is `NULL`, return
    #'   the entire axis (possibly as a scalar axis).
    subset = function(group, rng = NULL) {
      var <- NCVariable$new(-1L, self$name, group, "NC_DOUBLE", 1L, NULL)
      time <- private$tm

      .make_scalar <- function(idx) {
        scl <- CFAxisScalar$new(group, var, "T", as_timestamp(time)[idx])
        bnds <- time$get_bounds()
        if (!is.null(bnds)) scl$bounds <- bnds[, idx]
        private$subset_coordinates(scl, idx)
        scl
      }

      if (is.null(rng)) {
        if (length(time) > 1L) {
          ax <- self$clone()
          ax$group <- group
          ax
        } else
          .make_scalar(1L)
      } else {
        rng <- range(rng)
        if (rng[1L] == rng[2L])
          .make_scalar(rng[1L])
        else {
          idx <- time$indexOf(seq(from = rng[1L], to = rng[2L], by = 1L))
          dim <- NCDimension$new(-1L, self$name, length(idx), FALSE)
          t <- CFAxisTime$new(group, var, dim, attr(idx, "CFTime"))
          private$subset_coordinates(t, idx)
          t
        }
      }
    },

    #' @description Write the axis to a netCDF file, including its attributes.
    #' If the calendar name is "gregorian", it will be set to the functionally
    #' identical calendar "standard" as the former is deprecated.
    #' @param nc The handle of the netCDF file opened for writing or a group in
    #'   the netCDF file. If `NULL`, write to the file or group where the axis
    #'   was read from (the file must have been opened for writing). If not
    #'   `NULL`, the handle to a netCDF file or a group therein.
    #' @return Self, invisibly.
    write = function(nc = NULL) {
      if (private$tm$cal$name == "gregorian")
        self$set_attribute("calendar", "NC_CHAR", "standard")
      super$write(nc)

      .writeTimeBounds(nc, self$name, private$tm)
    }
  ),
  active = list(
    #' @field friendlyClassName (read-only) A nice description of the class.
    friendlyClassName = function(value) {
      if (missing(value))
        "Time axis"
    },

    #' @field dimnames (read-only) The coordinates of the axis as a character
    #' vector.
    dimnames = function(value) {
      if (missing(value))
        format(private$tm)
    }
  )
)

# ==============================================================================

# Helper function to write time axis bounds. These are maintained by the CFTime
# or CFClimatology instance referenced by the axis. This is a function so that
# CFAxisScalar can also access it for scalar time axes.
# nc - Handle to the netCDF file open for writing
# name - The name of the axis to write the bounds for
# time - The CFTime or CFClimatology instance whose bounds to write
.writeTimeBounds <- function(nc, name, time) {
  bnds <- time$get_bounds()
  if (!is.null(bnds)) {
    try(RNetCDF::dim.def.nc(nc, "nv2", 2L), silent = TRUE) # FIXME: nv2 could already exist with a different length
    nm <- if (inherits(time, "CFClimatology")) "climatology_bnds" else "time_bnds"
    RNetCDF::att.put.nc(nc, name, "bounds", "NC_CHAR", nm)
    RNetCDF::var.def.nc(nc, nm, "NC_DOUBLE", c("nv2", name))
    RNetCDF::var.put.nc(nc, nm, bnds)
  }
}
