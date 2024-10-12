# Scalar axes MUST come after dimensional axes in the axis list of CFVariables

#' Scalar CF axis object
#'
#' @description This class represents a scalar axis. Its single value can be of
#' any type. It is typically used as an auxiliary axis to record some parameter
#' of interest such as the single time associated with a spatial grid with
#' longitude, latitude and vertical axes.
#'
#' @docType class
#'
CFAxisScalar <- R6::R6Class("CFAxisScalar",
  inherit = CFAxis,
  public = list(
    #' @field value The value of the axis.
    value = NULL,

    #' @description Create an instance of this class.
    #' @param grp The group that contains the netCDF variable.
    #' @param nc_var The netCDF variable that describes this instance.
    #' @param orientation The orientation of this axis, or "" if not known.
    #' @param value The value of this axis.
    initialize = function(grp, nc_var, orientation, value) {
      dim <- NCDimension$new(-1L, nc_var$name, 1L, FALSE)
      super$initialize(grp, nc_var, dim, orientation)
      self$value <- value
    },

    #' @description Summary of the scalar axis
    #'
    #' Prints a summary of the scalar axis to the console.
    print = function() {
      cat("<", self$friendlyClassName, "> ", self$name, "\n", sep = "")
      if (self$group$name != "/")
        cat("Group    :", self$group$fullname, "\n")

      longname <- self$attribute("long_name")
      if (nzchar(longname) && longname != self$name)
        cat("Long name:", longname, "\n")

      cat("Axis     :", self$orientation, "\n")

      units <- self$attribute("units")
      if (!nzchar(units)) units <- ""
      cat("Value    : ", self$value, " ", units, "\n", sep = "")
      if (inherits(self$bounds, "CFBounds"))
        self$bounds$print()
      else cat("Bounds   : (not set)\n")

      self$print_attributes()
    },

    #' @description Retrieve a 1-row data.frame with some information on this axis.
    brief = function() {
      longname <- self$attribute("long_name")
      if (!nzchar(longname) || longname == self$name) longname <- ""
      units <- self$attribute("units")
      if (!nzchar(units)) units <- ""

      data.frame(id = "", axis = self$orientation, group = self$group$fullname,
                 name = self$name, long_name = longname, length = 1L,
                 unlim = "", values = paste0("[", self$value, "]"), unit = units)
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
        self$value
    }
  )
)
