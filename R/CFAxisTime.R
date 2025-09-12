# #' @importFrom CFtime unit as_timestamp
# NULL

#' Time axis object
#'
#' @description This class represents a time axis. The functionality is provided
#' by the `CFTime` class in the `CFtime` package.
#'
#' @docType class
#' @export
CFAxisTime <- R6::R6Class("CFAxisTime",
  inherit = CFAxis,
  cloneable = FALSE,
  private = list(
    # The `CFTime` or `CFClimatology` instance to manage CF time.
    .tm = NULL,

    get_coordinates = function() {
      if (private$.active_coords == 1L)
        private$.tm$as_timestamp()
      else {
        crds <- private$.aux[[private$.active_coords - 1L]]$coordinates
        dim(crds) <- NULL
        crds
      }
    },

    dimvalues_short = function() {
      crds <- private$get_coordinates()
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
    #' @param var The name of the axis when creating a new axis. When reading an
    #'   axis from file, the [NCVariable] object that describes this instance.
    #' @param values Either the numeric values of this axis, or an instance of
    #'   `CFTime` or `CFClimatology` with bounds set. If these are numeric
    #'   values, argument `var` must be a `NCVariable`.
    #' @param start Optional. Integer index where to start reading axis data
    #'   from file. The index may be `NA` to start reading data from the start.
    #' @param count Optional. Number of elements to read from file. This may be
    #'   `NA` to read to the end of the data.
    #' @param attributes Optional. A `data.frame` with the attributes of the
    #'   axis. When an empty `data.frame` (default) and argument `var` is an
    #'   NCVariable instance, attributes of the axis will be taken from the
    #'   netCDF resource.
    initialize = function(var, values, start = 1L, count = NA, attributes = data.frame()) {
      if (inherits(values, "CFTime")) {
        private$.tm <- values
        values <- private$.tm$offsets
      } else {
        if (inherits(var, "NCVariable")) {
          # Make a CFTime of CFClimatology
          private$.tm <- .makeTimeObject(var, var$attribute("units"), values, NULL)
        } else
          stop("When initializing by time axis name, must provide a `CFTime` instance as argument 'values'.", call. = FALSE) # nocov
      }

      super$initialize(var, values = values, start = start, count = count, orientation = "T", attributes = attributes)
      self$set_attribute("standard_name", "NC_CHAR", "time")
      if (!inherits(var, "NCVariable")) {
        self$set_attribute("units", "NC_CHAR", private$.tm$calendar$definition)
        self$set_attribute("calendar", "NC_CHAR", private$.tm$calendar$name)
      }
    },

    #' @description Summary of the time axis printed to the console.
    #' @param ... Arguments passed on to other functions. Of particular interest
    #' is `width = ` to indicate a maximum width of attribute columns.
    print = function(...) {
      super$print()

      time <- private$.tm
      if (private$.active_coords == 1L) {
        crds <- time$as_timestamp()
        units <- time$unit
      } else {
        crds <- private$.aux[[private$.active_coords - 1L]]$coordinates
        units <- private$.aux[[private$.active_coords - 1L]]$attribute("units")
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
    },

    #' @description Some details of the axis.
    #'
    #' @return A 1-row `data.frame` with some details of the axis.
    brief = function() {
      out <- super$brief()
      out$values <- private$dimvalues_short()
      out
    },

    #' @description Tests if the axis passed to this method is identical to
    #'   `self`.
    #' @param axis The `CFAxisTime` instance to test.
    #' @return `TRUE` if the two axes are identical, `FALSE` if not.
    identical = function(axis) {
      super$identical(axis) &&
      private$.tm$cal$is_equivalent(axis$time$cal) &&
      all(.near(private$.tm$offsets, axis$time$offsets))
    },

    #' @description Create a copy of this axis. The copy is completely separate
    #' from `self`, meaning that both `self` and all of its components are made
    #' from new instances.
    #' @param name The name for the new axis. If an empty string is passed, will
    #'   use the name of this axis.
    #' @return The newly created axis.
    copy = function(name = "") {
      time <- private$.tm$copy()
      if (self$has_resource) {
        ax <- CFAxisTime$new(self$NCvar, values = time, start = private$.start_count$start,
                             count = private$.start_count$count, attributes = self$attributes)
        if (nzchar(name))
          ax$name <- name
      } else {
        if (!nzchar(name))
          name <- self$name
        ax <- CFAxisTime$new(name, values = time, attributes = self$attributes)
      }
      private$copy_properties_into(ax)
    },

    #' @description Create a copy of this axis but using the supplied values.
    #'   The attributes are copied to the new axis. Boundary values and
    #'   auxiliary coordinates are not copied.
    #'
    #'   After this operation the attributes of the newly created axes may not
    #'   be accurate, except for the "actual_range" attribute. The calling code
    #'   should set, modify or delete attributes as appropriate.
    #' @param name The name for the new axis. If an empty string is passed, will
    #'   use the name of this axis.
    #' @param values The values to the used with the copy of this axis. This can
    #'   be a `CFTime` instance, a vector of numeric values, a vector of
    #'   character timestamps in ISO8601 or UDUNITS format, or a vector of
    #'   `POSIXct` or `Date` values. If not a `CFTime` instance, the `values`
    #'   will be converted into a `CFTime` instance using the definition and
    #'   calendar of this axis.
    #' @return The newly created axis.
    copy_with_values = function(name = "", values) {
      if (!nzchar(name))
        name <- self$name

      if (!inherits(values, "CFTime")) {
        values <- try(CFtime::CFTime$new(self$attribute("units"), self$attribute("calendar"), values), silent = TRUE)
        if (inherits(values, "try-error"))
          stop("Invalid values for a 'time' axis.", call. = FALSE) # nocov
      }

      CFAxisTime$new(name, values = values, attributes = self$attributes)
    },

    #' @description Append a vector of time values at the end of the current
    #'   values of the axis.
    #' @param from An instance of `CFAxisTime` whose values to append to the
    #'   values of this axis.
    #' @return A new `CFAxisTime` instance with values from this axis and the
    #'   `from` axis appended.
    append = function(from) {
      ft <- from$time
      if (super$can_append(from) && .c_is_monotonic(private$.tm$offsets, ft$offsets)) {
        bnds <- if (is.null(private$.tm$bounds) || is.null(ft$bounds)) NULL
                else cbind(private$.tm$bounds, ft$bounds)
        if (class(private$.tm)[1L] == "CFClimatology")
          time <- CFtime::CFClimatology$new(private$.tm$cal$definition, private$.tm$cal$name, c(private$.tm$offsets, ft$offsets), bnds)
        else {
          time <- CFtime::CFTime$new(private$.tm$cal$definition, private$.tm$cal$name, c(private$.tm$offsets, ft$offsets))
          time$bounds <- bnds
        }
        ax <- CFAxisTime$new(self$name, values = time, attributes = self$attributes)

        if (!is.null(private$.bounds)) {
          new_bnds <- private$.bounds$append(from$bounds)
          if (!is.null(new_bnds))
            ax$bounds <- new_bnds
        }

        ax
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
    #' @return A vector giving the indices in the time axis of valid
    #' values in `x`, or `NA` if the value is not valid.
    indexOf = function(x, method = "constant", rightmost.closed = FALSE) {
      idx <- private$.tm$indexOf(x, method, rightmost.closed)

      if (method == "constant")
        as.integer(idx)
      else
        idx
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
      time <- private$.tm
      idx <- time$slice(x, rightmost.closed)
      range((1L:length(time))[idx])
    },

    #' @description Return an axis spanning a smaller coordinate range. This
    #'   method returns an axis which spans the range of indices given by the
    #'   `rng` argument.
    #'
    #' @param name The name for the new axis. If an empty string is passed
    #'   (default), will use the name of this axis.
    #' @param rng The range of indices whose values from this axis to include in
    #'   the returned axis. If the value of the argument is `NULL`, return a
    #'   copy of the axis.
    #' @return A new `CFAxisNumeric` instance covering the indicated range of
    #'   indices. If the value of the argument `rng` is `NULL`, return a copy of
    #'   `self` as the new axis.
    subset = function(name = "", rng = NULL) {
      if (is.null(rng))
        self$copy(name)
      else {
        rng <- as.integer(range(rng))
        idx <- private$.tm$indexOf(seq(from = rng[1L], to = rng[2L], by = 1L))
        newtm <- attr(idx, "CFTime")

        if (self$has_resource) {
          ax <- CFAxisTime$new(private$.NCvar, values = newtm, start = private$.start_count$start + rng[1L] - 1L,
                               count = rng[2L] - rng[1L] + 1L, attributes = self$attributes)
          if (nzchar(name))
            ax$name <- name
        } else {
          if (!nzchar(name))
            name <- self$name
          ax <- CFAxisTime$new(name, values = newtm, attributes = self$attributes)
        }
        private$copy_properties_into(ax, rng)
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
      time <- private$.tm
      if (time$cal$name == "gregorian")
        self$set_attribute("calendar", "NC_CHAR", "standard")
      super$write(nc)

      # bnds <- time$get_bounds()
      # if (!is.null(bnds)) {
      #   # Do we have to record did somewhere??
      #   dnm <- "nv"
      #   did <- try(RNetCDF::dim.def.nc(nc, dnm, 2L), silent = TRUE)
      #   if (inherits(did, "try-error")) {
      #     did <- RNetCDF::dim.inq.nc(nc, dnm)
      #     did <- if (did$length == 2L) did$id
      #            else {
      #              dnm <- paste(sample(letters, 8, TRUE), collapse = "") # Random name
      #              RNetCDF::dim.def.nc(nc, dnm, 2L)
      #            }
      #   }
      #   nm <- if (inherits(time, "CFClimatology")) self$attribute("climatology") else self$attribute("bounds")
      #   RNetCDF::var.def.nc(nc, nm, "NC_DOUBLE", c(dnm, self$name))
      #   RNetCDF::var.put.nc(nc, nm, bnds)
      # }
    }
  ),
  active = list(
    #' @field friendlyClassName (read-only) A nice description of the class.
    friendlyClassName = function(value) {
      if (missing(value))
        "Time axis"
    },

    #' @field time (read-only) Retrieve the `CFTime` instance that manages the
    #'   values of this axis.
    time = function(value) {
      if (missing(value))
        private$.tm
    },

    #' @field dimnames (read-only) The coordinates of the axis as a character
    #'   vector.
    dimnames = function(value) {
      if (missing(value))
        format(private$.tm)
    }
  )
)
