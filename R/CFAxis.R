#' CF axis object
#'
#' @description This class is a basic ancestor to all classes that represent CF
#'   axes. More useful classes use this class as ancestor.
#'
#' @details The fields in this class are common among all CF axis objects.
#'
#' @docType class
#'
#' @export
CFAxis <- R6::R6Class("CFAxis",
  inherit = CFObject,
  public = list(
    #' @field group The [NCGroup] that this axis is located in.
    group       = NULL,

    #' @field NCdim The [NCDimension] that stores the netCDF dimension details.
    #' This is `NULL` for [CFAxisScalar] instances.
    NCdim       = NULL,

    #' @field orientation A character "X", "Y", "Z" or "T" to indicate the
    #' orientation of the axis, or an empty string if not known or different.
    orientation = "",

    #' @field bounds The boundary values of this axis, if set.
    bounds      = NULL,

    #' Create a basic CF axis object
    #'
    #' Create a new CF axis instance from a dimension and a variable in a netCDF
    #' resource. This method is called upon opening a netCDF resource by the
    #' `initialize()` method of a descendant class suitable for the type of
    #' axis.
    #'
    #' @param grp The [NCGroup] that this axis is located in.
    #' @param nc_var The [NCVariable] instance upon which this CF axis is
    #'   based.
    #' @param nc_dim The [NCDimension] instance upon which this CF axis is
    #'   based.
    #' @param orientation The orientation of the axis: "X", "Y", "Z" "T", or
    #' "" when not known or relevant.
    #' @return A basic `CFAxis` object.
    initialize = function(grp, nc_var, nc_dim, orientation) {
      super$initialize(nc_var)
      self$group <- grp
      self$NCdim <- nc_dim
      self$orientation <- orientation

      nc_var$CF <- self
    },

    #' @description Summary of the axis
    #'
    #' Prints a summary of the axis to the console. This method is typically
    #' called by the `print()` method of descendant classes.
    print = function() {
      cat("<", self$friendlyClassName, "> [", self$dimid, "] ", self$name, "\n", sep = "")
      if (self$group$name != "/")
        cat("Group    :", self$group$fullname, "\n")

      longname <- self$attribute("long_name")
      if (nzchar(longname) && longname != self$name)
        cat("Long name:", longname, "\n")

      cat("Length   :", self$NCdim$length)
      if (self$NCdim$unlim) cat(" (unlimited)\n") else cat("\n")
      cat("Axis     :", self$orientation, "\n")
    },

    #' @description Some details of the axis
    #'
    #' @return A 1-row `data.frame` with some details of the axis.
    brief = function() {
      longname <- self$attribute("long_name")
      if (!nzchar(longname) || longname == self$name) longname <- ""
      unlim <- if (self$NCdim$unlim) "U" else ""
      units <- self$attribute("units")
      if (!nzchar(units)) units <- ""

      data.frame(id = self$dimid, axis = self$orientation, group = self$group$fullname,
                 name = self$name, long_name = longname, length = self$NCdim$length,
                 unlim = unlim, values = "", unit = units)
    },

    #' @description Very concise information on the axis
    #'
    #' The information returned by this function is very concise and most useful
    #' when combined with similar information from other axes.
    #'
    #' @return Character string with very basic axis information.
    shard = function() {
      self$NCdim$shard()
    },

    #' @description Return the `CFtime` instance that represents time
    #'
    #' This method is only useful for `CFAxisTime` instances. This stub is here
    #' to make the call to this method succeed with no result for the other axis
    #' descendants.
    #'
    #' @return `NULL`
    time = function() {
      NULL
    },

    #' @description Return an axis spanning a smaller dimension range
    #'
    #' This method is "virtual" in the sense that it does not do anything other
    #' than return `NULL`. This stub is here to make the call to this method
    #' succeed with no result for the other axis descendants that do not
    #' implement this method.
    #'
    #' @param group The group to create the new axis in.
    #' @param rng The range of values from this axis to include in the returned
    #' axis. If the value of the argument is `NULL`, return the entire axis
    #' (possibly as a scalar axis).
    #'
    #' @return `NULL`
    sub_axis = function(group, rng = NULL) {
      NULL
    },

    #' @title Find indices in the axis domain
    #'
    #' @description Given a vector of numerical, timestamp or categorical values
    #' `x`, find their indices in the values of the axis. With
    #' `method = "constant"` this returns the index of the value lower than the
    #' supplied values in `x`. With `method = "linear"` the return value
    #' includes any fractional part.
    #'
    #' If bounds are set on the numerical or time axis, the indices are taken
    #' from those bounds. Returned indices may fall in between bounds if the
    #' latter are not contiguous, with the exception of the extreme values in
    #' `x`.
    #'
    #' @param x Vector of numeric, timestamp or categorial values to find axis
    #'   indices for. The timestamps can be either character, POSIXct or Date
    #'   vectors. The type of the vector has to correspond to the type of the
    #'   axis.
    #' @param method Single character value of "constant" or "linear".
    #'
    #' @return Numeric vector of the same length as `x`. If `method = "constant"`,
    #'   return the index value for each match. If `method = "linear"`, return
    #'   the index value with any fractional value. Values of `x` outside of the
    #'   range of the values in the axis are returned as `0` and `.Machine$integer.max`,
    #'    respectively.
    indexOf = function(x, method = "constant") {
      stop("`indexOf()` must be implemented by descendant CFAxis class.")
    }
  ),

  active = list(
    #' @field friendlyClassName (read-only) A nice description of the class.
    friendlyClassName = function(value) {
      if (missing(value))
        "Generic CF axis"
    },

    #' @field dimid The netCDF dimension id of this axis.
    dimid = function(value) {
      if (missing(value))
        self$NCdim$id
    },

    #' @field length The declared length of this axis.
    length = function(value) {
      if (missing(value))
        self$NCdim$length
    }
  )
)

# Public S3 methods ------------------------------------------------------------

#' Axis length
#'
#' This method returns the lengths of the axes of a variable or axis.
#'
#' @param x The `CFVariable` or a descendant of `CFAxis`.
#'
#' @return Vector of dimension lengths.
#' @export
#'
#' @examples
#' fn <- system.file("extdata", "ERA5land_Rwanda_20160101.nc", package = "ncdfCF")
#' ds <- open_ncdf(fn)
#' t2m <- ds[["t2m"]]
#' dim(t2m)
dim.CFAxis <- function(x) {
  x$length
}

#' @export
dimnames.CFAxis <- function(x) {
  x$dimnames
}

