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
    #'
    #'   Creating a new time axis is more easily done with the [makeTimeAxis()]
    #'   function.
    #' @param nc_var The netCDF variable that describes this instance.
    #' @param nc_dim The netCDF dimension that describes the dimensionality.
    #' @param values The `CFTime` instance that manages this axis.
    initialize = function(nc_var, nc_dim, values) {
      super$initialize(nc_var, nc_dim, "T")
      private$tm <- values
      self$set_attribute("units", "NC_CHAR", values$cal$definition)
      self$set_attribute("calendar", "NC_CHAR", values$cal$name)
      self$set_attribute("standard_name", "NC_CHAR", "time")
      self$set_attribute("axis", "NC_CHAR", "T")
      off <- values$offsets
      if (length(off))
        self$set_attribute("actual_range", nc_var$vtype, range(off))
    },

    #' @description Summary of the time axis printed to the console.
    #' @param ... Arguments passed on to other functions. Of particular interest
    #' is `width = ` to indicate a maximum width of attribute columns.
    #' @return `self`, invisibly.
    print = function(...) {
      super$print()

      time <- private$tm
      if (private$active_coords == 1L) {
        crds <- private$tm$as_timestamp()
        units <- time$unit
      } else {
        crds <- private$aux[[private$active_coords - 1L]]$coordinates
        units <- private$aux[[private$active_coords - 1L]]$attribute("units")
      }
      len <- length(crds)
      rng <- if (len == 1L) crds[1L]
             else paste(crds[1L], "...", crds[len])
      if (!is.na(units)) rng <- paste0(rng, " (", units, ")")
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

    #' @description Tests if the axis passed to this method is identical to
    #'   `self`.
    #' @param axis The `CFAxisTime` instance to test.
    #' @return `TRUE` if the two axes are identical, `FALSE` if not.
    identical = function(axis) {
      super$identical(axis) &&
      private$tm$cal$is_equivalent(axis$time()$cal) &&
      all(.near(private$tm$offsets, axis$time()$offsets))
    },

    #' @description Append a vector of time values at the end of the current
    #'   values of the axis.
    #' @param from An instance of `CFAxisTime` whose values to append to the
    #'   values of `self`.
    #' @return A new `CFAxisTime` instance with values from self and the `from`
    #'   axis appended.
    append = function(from) {
      if (super$can_append(from) && .c_is_monotonic(private$tm$offsets, from$time()$offsets)) {
        bnds <- if (is.null(private$tm$bounds) || is.null(from$time()$bounds)) NULL
                else cbind(private$tm$bounds, from$time()$bounds)
        if (class(private$tm)[1L] == "CFClimatology")
          time <- CFtime::CFClimatology$new(private$tm$cal$definition, private$tm$cal$name, c(private$tm$offsets, from$time()$offsets), bnds)
        else {
          time <- CFtime::CFTime$new(private$tm$cal$definition, private$tm$cal$name, c(private$tm$offsets, from$time()$offsets))
          time$bounds <- bnds
        }
        axis <- makeTimeAxis(self$name, makeGroup(), time)
        axis$attributes <- self$attributes
        axis
      } else
        stop("Axis values cannot be appended.", call. = FALSE)
    },

    #' @description Retrieve the indices of supplied values on the time axis.
    #' @param x A vector of timestamps whose indices into the time axis to
    #' extract.
    #' @param method Extract index values without ("constant", the default) or
    #' with ("linear") fractional parts.
    #' @param rightmost.closed Whether or not to include the upper limit.
    #' Default is `FALSE`.
    #' @return An integer vector giving the indices in the time axis of valid
    #' values in `x`, or `NA` if the value is not valid.
    indexOf = function(x, method = "constant", rightmost.closed = FALSE) {
      time <- private$tm
      idx <- time$indexOf(x, method, rightmost.closed)
      len <- length(idx)
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
    #' @param rng The range of indices whose values from this axis to include in
    #'   the returned axis.
    #'
    #' @return A `CFAxisTime` instance covering the indicated range of indices.
    #'   If the value of the argument is `NULL`, return the entire axis.
    subset = function(group, rng = NULL) {
      var <- NCVariable$new(-1L, self$name, group, "NC_DOUBLE", 1L, NULL)
      time <- private$tm

      if (is.null(rng)) {
        ax <- self$clone()
        ax$group <- group
        ax
      } else {
        rng <- range(rng)
        idx <- time$indexOf(seq(from = rng[1L], to = rng[2L], by = 1L))
        tm <- attr(idx, "CFTime")
        dim <- NCDimension$new(-1L, self$name, length(idx), FALSE)
        t <- CFAxisTime$new(var, dim, tm)
        private$subset_coordinates(t, idx)
        t$set_attribute("actual_range", self$NCvar$vtype, range(tm$offsets))
        t
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
      time <- private$tm
      if (time$cal$name == "gregorian")
        self$set_attribute("calendar", "NC_CHAR", "standard")
      super$write(nc)

      bnds <- time$get_bounds()
      if (!is.null(bnds)) {
        try(RNetCDF::dim.def.nc(nc, "nv2", 2L), silent = TRUE) # FIXME: nv2 could already exist with a different length
        nm <- if (inherits(time, "CFClimatology")) self$attribute("climatology") else self$attribute("bounds")
        RNetCDF::var.def.nc(nc, nm, "NC_DOUBLE", c("nv2", self$name))
        RNetCDF::var.put.nc(nc, nm, bnds)
      }
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
