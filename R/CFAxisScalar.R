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
    get_values = function() {
      if (inherits(self$values, "CFTime")) self$values$offsets
      else self$values
    },

    dimvalues_short = function() {
      v <- if (inherits(self$values, "CFTime")) as_timestamp(self$values)
           else self$values
      paste0("[", v, "]")
    }
  ),
  public = list(
    #' @field values The value of the axis. This could be a composite value, such
    #' as a `CFTime` instance.
    values = NULL,

    #' @description Create an instance of this class.
    #' @param grp The group that contains the netCDF variable.
    #' @param nc_var The netCDF variable that describes this instance.
    #' @param orientation The orientation of this axis, or "" if not known.
    #' @param value The value of this axis, possibly a compound type like
    #' `CFTime`.
    initialize = function(grp, nc_var, orientation, value) {
      dim <- NCDimension$new(-1L, nc_var$name, 1L, FALSE)
      super$initialize(grp, nc_var, dim, orientation)
      self$values <- value
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

      if (inherits(self$values, "CFTime")) {
        cat("Value    :", as_timestamp(self$values), "\n")
        bnds <- self$values$get_bounds("timestamp")
        if (is.null(bnds)) cat("Bounds   : (not set)\n")
        else cat("Bounds   : ", bnds[1L, 1L], ", ", bnds[2L, 1L], "\n", sep = "")
      } else {
        units <- self$attribute("units")
        if (is.na(units)) units <- ""
        cat("Value    : ", self$values, " ", units, "\n", sep = "")
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
      if (inherits(self$values, "CFTime")) self$values
      else NULL
    },

    #' @description Return the axis. This method returns a clone of this axis,
    #' given that a scalar axis cannot be subset.
    #'
    #' @param group The group to create the new axis in.
    #' @param rng Ignored.
    #'
    #' @return A `CFAxisScalar` cloned from this axis.
    sub_axis = function(group, rng = NULL) {
      ax <- self$clone()
      ax$group <- group
      ax
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
        if (inherits(self$values, "CFTime")) as_timestamp(self$values)
        else self$values
    }
  )
)
