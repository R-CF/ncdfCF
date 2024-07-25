CFAxis <- R6::R6Class("CFAxis",
  inherit = CFObject,
  public = list(
    group       = NULL,
    NCdim       = NULL,
    orientation = "",
    bounds      = NULL,

    initialize = function(grp, nc_var, nc_dim) {
      super$initialize(nc_var)
      self$group <- grp
      self$NCdim <- nc_dim

      nc_var$CF <- self
    },

    print = function() {
      cat("<Axis> [", self$dimid, "] ", self$name, "\n", sep = "")
      if (self$group$name != "/")
        cat("Group    :", self$group$fullname, "\n")

      longname <- self$attribute("long_name")
      if (length(longname) && longname != self$name)
        cat("Long name:", longname, "\n")

      cat("Length   :", self$NCdim$length)
      if (self$NCdim$unlim) cat(" (unlimited)\n") else cat("\n")
      cat("Axis     :", self$orientation, "\n")
    },

    brief = function() {
      longname <- self$attribute("long_name")
      if (!length(longname) || longname == self$name) longname <- ""
      unlim <- if (self$NCdim$unlim) "U" else ""

      data.frame(id = self$dimid, axis = self$orientation, name = self$name, long_name = longname,
                 length = self$NCdim$length, values = "", unlim = unlim, bounds = "")
    },

    shard = function() {
      self$NCdim$shard()
    },

    time = function() {
      NULL
    },

    indexOf = function(x, method = "constant") {
      stop("`indexOf()` must be implemented by descendant CFAxis class.")
    }
  ),

  active = list(
    dimid = function(value) {
      if (missing(value))
        self$NCdim$id
    },

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
#' @returns Vector of dimension lengths.
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

#' @name indexOf
#' @title Find indices in the axis domain
#'
#' @description
#' Given a vector of numerical, timestamp or categorical values `x`, find their
#' indices in the values of axis `y`. With `method = "constant"` this
#' returns the index of the value lower than the supplied values in `x`. With
#' `method = "linear"` the return value includes any fractional part.
#'
#' If bounds are set on the numerical or time axis, the indices are taken
#' from those bounds. Returned indices may fall in between bounds if the latter
#' are not contiguous, with the exception of the extreme values in `x`.
#'
#' @param x Vector of numeric, timestamp or categorial values to find axis
#' indices for. The timestamps can be either character, POSIXct or Date vectors.
#' The type of the vector has to correspond to the type of `y`.
#' @param y An instance of `CFAxisNumeric`, `CFAxisTime` or `CFAxisCharacter`.
#' @param method Single character value of "constant" or "linear".
#'
#' @returns Numeric vector of the same length as `x`. If `method = "constant"`,
#'   return the index value for each match. If `method = "linear"`, return the
#'   index value with any fractional value. Values of `x` outside of the range
#'   of the values in `y` are returned as `0` and `.Machine$integer.max`,
#'   respectively.
#' @examples
#' fn <- system.file("extdata",
#'                   "pr_day_EC-Earth3-CC_ssp245_r1i1p1f1_gr_20240101-20241231_vncdfCF.nc",
#'                   package = "ncdfCF")
#' ds <- open_ncdf(fn)
#' lon <- ds[["lon"]]
#' lon$indexOf(c(8.5, 8.9, 9.3, 9.7, 10.1))
#' lon$indexOf(c(8.5, 8.9, 9.3, 9.7, 10.1), "linear")
#'
#' time <- ds[["time"]]
#' time$indexOf(c("2024-03-01", "2024-03-02"))
NULL
