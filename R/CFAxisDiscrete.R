#' CF discrete axis object
#'
#' @description This class represent discrete CF axes, i.e. those axes whose
#'   coordinate values do not represent a physical property. The coordinate
#'   values are ordinal values equal to the index into the axis.
#'
#' @docType class
#' @export
CFAxisDiscrete <- R6::R6Class("CFAxisDiscrete",
  inherit = CFAxis,
  cloneable = FALSE,
  private = list(
    # Values for this axis. Since there is no underlying NCVariable for this
    # axis, this is managed internally.
    .values = NULL,

    dimvalues_short = function() {
      crds <- private$get_coordinates()
      len <- length(crds)
      if (len == 1L) paste0("[", crds[1L], "]")
      else paste0("[", crds[1L], " ... ", crds[len], "]")
    }
  ),
  public = list(
    #' @description Create a new instance of this class. The values of this axis
    #'   are always a sequence, but the sequence may start with any positive
    #'   value such that the length of this instance falls within the length of
    #'   the axis on file, if there is one.
    #'
    #'   Creating a new discrete axis is more easily done with the
    #'   [makeDiscreteAxis()] function.
    #' @param var The name of the axis when creating a new axis. When reading an
    #'   axis from file, the [NCVariable] object that describes this instance.
    #' @param start Optional. Integer value that indicates the starting value of
    #'   this axis. Defults to `1L`.
    #' @param count Number of elements in the axis.
    initialize = function(var, start = 1L, count) {
      super$initialize(var)
      private$.values <- start:(start + count - 1L)
    },

    #' @description Summary of the axis printed to the console.
    #' @param ... Arguments passed on to other functions. Of particular interest
    #' is `width = ` to indicate a maximum width of attribute columns.
    #' @return `self`, invisibly.
    print = function(...) {
      super$print()
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

    #' @description Create a copy of this axis. The copy is completely separate
    #'   from this axis, meaning that both this axis and all of its components
    #'   are made from new instances.
    #' @param name The name for the new axis. If an empty string is passed, will
    #'   use the name of this axis.
    #' @return The newly created axis.
    copy = function(name = "") {
      if (!nzchar(name))
        name <- self$name
      ax <- CFAxisDiscrete$new(name, start = private$.values[1L], count = length(private$.values))
      private$copy_properties_into(ax)
    },

    #' @description Find indices in the axis domain. Given a vector of numerical
    #'   values `x`, find their indices in the values of the axis. Outside
    #'   values will be dropped.
    #'
    #' @param x Vector of numeric values to find axis indices for.
    #' @param method Ignored.
    #' @param rightmost.closed Ignored.
    #'
    #' @return Numeric vector of the same length as `x`. Values of `x` outside
    #'   of the range of the values in the axis are returned as `NA`.
    indexOf = function(x, method = "constant", rightmost.closed = TRUE) {
      x[x < private$.values[1L] | x > private$.values[length(private$.values)]] <- NA
      as.integer(x)
    },

    #' @description Given a range of coordinate values, returns the indices into
    #'   the axis that fall within the supplied range. If the axis has auxiliary
    #'   coordinates selected then these will be used for the identification of
    #'   the indices to return.
    #' @param rng A vector whose extreme values indicate the indices of
    #'   coordinates to return. The mode of the vector has to be integer or
    #'   agree with any auxiliary coordinates selected.
    #' @return An integer vector of length 2 with the lower and higher indices
    #'   into the axis that fall within the range of coordinates in argument
    #'   `rng`. Returns `NULL` if no (boundary) values of the axis fall within
    #'   the range of coordinates.
    slice = function(rng) {
      if (private$.active_coords > 1L)
        private$.aux[[private$.active_coords - 1L]]$slice(rng)
      else {
        lo <- private$.values[1L]
        hi <- private$.values[length(private$.values)]
        rng <- range(rng)
        rng <- as.integer(c(ceiling(rng[1L]), floor(rng[2L])))
        if (rng[2L] < lo || rng[1L] > hi) NULL
        else {
          if (rng[1L] < lo) rng[1L] <- lo
          if (rng[2L] > hi) rng[2L] <- hi
          rng
        }
      }
    },

    #' @description Return an axis spanning a smaller coordinate range. This
    #'   method returns an axis which spans the range of indices given by the
    #'   `rng` argument.
    #' @param name The name for the new axis. If an empty string is passed, will
    #'   use the name of this axis.
    #' @param rng The range of indices whose values from this axis to include in
    #'   the returned axis. If the value of the argument is `NULL`, return a
    #'   copy of the axis.
    #' @return A new `CFAxisDiscrete` instance covering the indicated range of
    #'   indices. If the value of the argument is `NULL`, return a copy of
    #'   `self` as the new axis.
    subset = function(name = "", rng = NULL) {
      if (is.null(rng))
        self$copy(name)
      else {
        if (!nzchar(name))
          name <- self$name
        ax <- CFAxisDiscrete$new(name, start = rng[1L], count = rng[2L] - rng[1L] + 1L)
        private$copy_properties_into(ax, rng)
      }
    },

    #' @description Append a vector of values at the end of the current values
    #'   of the axis.
    #' @param from An instance of `CFAxisDiscrete` whose length to add to this
    #'   axis.
    #' @return A new `CFAxisDiscrete` instance with a length that is the sum of
    #'   the lengths of this axis and the `from` axis.
    append = function(from) {
      if (super$can_append(from)) {
        CFAxisDiscrete$new(self$name, start = private$.values[1L], count = self$length + from$length)
      } else
        stop("Axis values cannot be appended.", call. = FALSE)
    },

    #' @description Write the axis to a netCDF file, including its attributes,
    #' but only if it has an associated NC variable in the file.
    #' @param nc The handle of the netCDF file opened for writing or a group in
    #'   the netCDF file. If `NULL`, write to the file or group where the axis
    #'   was read from (the file must have been opened for writing). If not
    #'   `NULL`, the handle to a netCDF file or a group therein.
    #' @return Self, invisibly.
    write = function(nc = NULL) {
      h <- if (inherits(nc, "NetCDF")) nc else self$NCvar$handle

      # Write the dimension for the axis. Error will be thrown when trying to
      # write a dimension that's already defined, such as when a dimension is
      # shared between multiple objects. In that case, get the id.
      did <- try(RNetCDF::dim.def.nc(h, self$name, self$length), silent = TRUE)
      if (inherits(did, "try-error"))
        did <- RNetCDF::dim.inq.nc(h, self$name)$id

      # No values or attributes to write.

      # Write labels.
      lapply(private$.aux, function(x) x$write(nc, did))

      invisible(self)
    }
  ),
  active = list(
    #' @field friendlyClassName (read-only) A nice description of the class.
    friendlyClassName = function(value) {
      if (missing(value))
        "Discrete axis"
    },

    #' @field dimnames (read-only) The coordinates of the axis as an integer
    #' vector, or labels for every axis element if they have been set.
    dimnames = function(value) {
      if (missing(value))
        private$get_coordinates()
    }
  )
)
