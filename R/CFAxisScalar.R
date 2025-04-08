# Scalar axes MUST come after dimensional axes in the axis list of CFVariables

#' Scalar CF axis object
#'
#' @description This class represents a scalar axis. Its single value can be of
#' any type. It is typically used as an auxiliary axis to record some parameter
#' of interest such as the single time associated with a spatial grid with
#' longitude, latitude and vertical axes.
#'
#' @docType class
#' @export
CFAxisScalar <- R6::R6Class("CFAxisScalar",
  inherit = CFAxis,
  private = list(
    # The value of the axis. This could be a composite value, such
    # as a `CFTime` instance.
    value = NULL,

    get_values = function() {
      if (inherits(private$value, "CFTime")) private$value$offsets
      else private$value
    },

    get_coordinates = function() {
      if (private$active_coords == 1L) {
        if (inherits(private$value, "CFTime")) private$value$as_timestamp()
        else private$value
      } else
        private$aux[[private$active_coords - 1L]]$coordinates
    },

    dimvalues_short = function() {
      paste0("[", private$get_coordinates(), "]")
    }
  ),
  public = list(
    #' @description Create an instance of this class.
    #' @param grp The group that contains the netCDF variable.
    #' @param nc_var The netCDF variable that describes this instance.
    #' @param orientation The orientation of this axis, or "" if not known.
    #' @param value The value of this axis, possibly a compound type like
    #' `CFTime` or `CFClimatology`.
    initialize = function(grp, nc_var, orientation, value) {
      dim <- NCDimension$new(-1L, nc_var$name, 1L, FALSE)
      super$initialize(grp, nc_var, dim, orientation)
      private$value <- value
      if (inherits(value, "CFTime"))
        self$set_attribute("actual_range", nc_var$vtype, c(value$offsets, value$offsets))
      else
        self$set_attribute("actual_range", nc_var$vtype, c(value, value))
    },

    #' @description Summary of the scalar axis printed to the console.
    #' @param ... Arguments passed on to other functions. Of particular interest
    #' is `width = ` to indicate a maximum width of attribute columns.
    #' @return `self`, invisibly.
    print = function(...) {
      cat("<", self$friendlyClassName, "> ", self$name, "\n", sep = "")
      if (self$group$name != "/")
        cat("Group    :", self$group$fullname, "\n")

      longname <- self$attribute("long_name")
      if (!is.na(longname) && longname != self$name)
        cat("Long name:", longname, "\n")

      cat("Axis     :", self$orientation, "\n")

      if (inherits(private$value, "CFTime")) {
        cat("Value    :", as_timestamp(private$value), "\n")
        bnds <- private$value$get_bounds("timestamp")
        if (is.null(bnds)) cat("Bounds   : (not set)\n")
        else cat("Bounds   : ", bnds[1L, 1L], ", ", bnds[2L, 1L], "\n", sep = "")
      } else {
        units <- self$attribute("units")
        if (is.na(units)) units <- ""
        cat("Value    : ", private$value, " ", units, "\n", sep = "")
        if (inherits(self$bounds, "CFBounds"))
          self$bounds$print(...)
        else cat("Bounds   : (not set)\n")
      }

      self$print_attributes(...)
    },

    #' @description Some details of the axis.
    #'
    #' @return A 1-row `data.frame` with some details of the axis.
    brief = function() {
      longname <- self$attribute("long_name")
      if (is.na(longname) || longname == self$name) longname <- ""
      units <- self$attribute("units")
      if (is.na(units)) units <- ""

      data.frame(id = "", axis = self$orientation, group = self$group$fullname,
                 name = self$name, long_name = longname, length = 1L,
                 unlim = "", values = private$dimvalues_short(), unit = units)
    },

    #' @description Retrieve the `CFTime` instance that manages the time value
    #' if this scalar axis represents time.
    #' @return An instance of `CFTime`, or `NULL` if this axis does not
    #' represent time.
    time = function() {
      if (inherits(private$value, "CFTime")) private$value
      else NULL
    },

    #' @description Return the axis. This method returns a clone of this axis,
    #' given that a scalar axis cannot be subset.
    #'
    #' @param group The group to create the new axis in.
    #' @param rng Ignored.
    #'
    #' @return A `CFAxisScalar` cloned from this axis.
    subset = function(group, rng = NULL) {
      ax <- self$clone()
      ax$group <- group
      ax
    },

    #' @description Write the axis to a netCDF file, including its attributes.
    #' @param nc The handle of the netCDF file opened for writing or a group in
    #'   the netCDF file. If `NULL`, write to the file or group where the axis
    #'   was read from (the file must have been opened for writing). If not
    #'   `NULL`, the handle to a netCDF file or a group therein.
    #' @return Self, invisibly.
    write = function(nc = NULL) {
      super$write(nc)
      if (inherits(private$value, "CFTime"))
        .writeTimeBounds(nc, self$name, private$value)
    }
  ),
  active = list(
    #' @field friendlyClassName (read-only) A nice description of the class.
    friendlyClassName = function(value) {
      if (missing(value))
        "Scalar axis"
    },

    #' @field dimnames (read-only) The coordinate of the axis.
    dimnames = function(value) {
      if (missing(value))
        if (inherits(private$value, "CFTime")) as_timestamp(private$value)
        else private$value
    }
  )
)
