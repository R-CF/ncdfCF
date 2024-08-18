#' @import methods
#' @import R6
#' @include NCObject.R
NULL

#' CF base object
#'
#' @description This class is a basic ancestor to all classes that represent CF
#'   objects, specifically data variables and axes. More useful classes use this
#'   class as ancestor.
#'
#' @details The fields in this class are common among all CF objects.
#'
#' @docType class
#'
#' @name CFObject
#' @format An \code{\link{R6Class}} generator object.
NULL

CFObject <- R6::R6Class("CFObject",
  public = list(
    #' @field NCvar The [NCVariable] instance that this CF object represents.
    NCvar = NULL,

    #' Create a basic CF object
    #'
    #' Create a new CF object instance from a variable in a netCDF resource.
    #' This method is called upon opening a netCDF resource.
    #'
    #' @param nc_var The [NCVariable] instance upon which this CF object is
    #'   based.
    #' @returns A basic CF object.
    initialize = function(nc_var) {
      self$NCvar <- nc_var
    },

    #' Attributes of a CF object
    #'
    #' This method returns CF object attributes.
    #'
    #' @param att Vector of character strings of attributes to return.
    #' @param field The field of the `data.frame` to return values from. This
    #' must be "value" (default), "type" or "length".
    #' @returns A vector of values from the `data.frame`, named with the `att`
    #' value.
    attribute = function(att, field = "value") {
      self$NCvar$attribute(att, field)
    },

    #' Print the attributes of the CF object
    #'
    #' This function prints the attributes of the CF object to the console,
    #'
    #' @param width The maximum width of each column in the `data.frame` when
    #' printed to the console.
    print_attributes = function(width = 50) {
      if (nrow(self$NCvar$attributes)) {
        cat("\nAttributes:\n")
        print(.slim.data.frame(self$NCvar$attributes, width), right = FALSE, row.names = FALSE)
      }
    }
  ),

  active = list(
    #' @field friendlyClassName (read-only) A nice description of the class.
    friendlyClassName = function(value) {
      if (missing(value))
        "Generic CF object"
    },

    #' @field id (read-only) The identifier of the CF object.
    id = function(value) {
      if (missing(value))
        self$NCvar$id
    },

    #' @field name (read-only) The name of the CF object.
    name = function(value) {
      if (missing(value))
        self$NCvar$name
    },

    #' @field attributes (read-only) A `data.frame` with the attributes of the CF object.
    attributes = function(value) {
      if (missing(value))
        self$NCvar$attributes
    }
  )
)

#' @name dimnames
#' @title Names or dimension values of an CF object
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
#' * `CFAxisNumeric`, `CFAxisLongitude`, `CFAxisLatitude`, `CFAxisVertical`: The
#' values of the elements along the axis as a numeric vector.
#' * `CFAxisTime`: The values of the elements along the axis as a
#' character vector containing timestamps in ISO8601 format. This could be dates
#' or date-times if time information is available in the axis.
#' * `CFAxisScalar`: The value of the scalar.
#' * `CFAxisCharacter`: The values of the elements along the axis as
#' a character vector.
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

