#' @import methods
#' @import R6
#' @include NCObject.R
NULL

CFObject <- R6::R6Class("CFObject",
  public = list(
    NCvar = NCObject,

    initialize = function(nc_var) {
      self$NCvar <- nc_var
    },

    attribute = function(att, field = "value") {
      self$NCvar$attribute(att, field)
    },

    print_attributes = function(width = 50) {
      if (nrow(self$NCvar$attributes)) {
        cat("\nAttributes:\n")
        print(.slim.data.frame(self$NCvar$attributes, width), right = FALSE, row.names = FALSE)
      }
    }
  ),

  active = list(
    id = function(value) {
      if (missing(value))
        self$NCvar$id
    },

    name = function(value) {
      if (missing(value))
        self$NCvar$name
    },

    attributes = function(value) {
      if (missing(value))
        self$NCvar$attributes
    }
  )
)

#' @name dimnames
#' @title Names or dimension values of an `CFObject` instance
#'
#' @description Retrieve the variable or dimension names of an `ncdfCF` object.
#' The `names()` function gives the names of the variables in the data set,
#' prepended with the path to the group if the resource uses groups.
#' The return value of the `dimnames()` function differs depending on the type
#' of object:
#' * `CFDataset`, `CFVariable`: The dimnames are returned as a vector of the
#' names of the axes of the data set or variable, prepended with the path to the
#' group if the resource uses groups. Note that this differs markedly from the
#' `base::dimnames()` functionality.
#' * `CFAxisNumeric`: The values of the elements along the axis as a
#' numeric vector.
#' * `CFAxisCharacter`: The values of the elements along the axis as
#' a character vector.
#' * `CFAxisTime`: The values of the elements along the axis as a
#' character vector containing timestamps in ISO8601 format. This could be dates
#' or date-times if time information is available in the axis.
#' * `CFAxisDiscrete`: The index values of the axis, from 1 to the
#' length of the axis.
#'
#' @param x An `CFObject` whose axis names to retrieve. This could be
#' `CFDataset`, `CFVariable`, or a class descending from `CFAxis`.
#'
#' @returns A vector as described in the Description section.
#' @examples
#' fn <- system.file("extdata",
#'   "pr_day_EC-Earth3-CC_ssp245_r1i1p1f1_gr_20240101-20241231_vncdfCF.nc",
#'   package = "ncdfCF")
#' ds <- open_ncdf(fn)
#'
#' # CFDataset
#' dimnames(ds)
#'
#' # CFVariable
#' pr <- ds[["pr"]]
#' dimnames(pr)
#'
#' # CFAxisNumeric
#' lon <- ds[["lon"]]
#' dimnames(lon)
#'
#' # CFAxisTime
#' t <- ds[["time"]]
#' dimnames(t)
NULL

