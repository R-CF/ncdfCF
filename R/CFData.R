#' Data extracted from a CF data variable
#'
#' @description This class holds the data that is extracted from a [CFVariable],
#' using the `subset()` class method. The instance of this class will
#' additionally have the axes and other relevant information such as its
#' attributes (as well as those of the axes) and the coordinate reference system.
#'
#' The class has a number of utility functions to extract the data in specific
#' formats:
#' * `raw()`: The data without any further processing. The axes are
#' as they are stored in the netCDF resource; there is thus no guarantee as to
#' how the data is organized in the array. Dimnames will be set.
#' * `array()`: An array of the data which is organized as a standard R array.
#' Dimnames will be set.
#' * `stars()`: The data is returned as a `stars` object, with all relevant
#' metadata set. Package `stars` must be installed for this to work.
#' * `rast()`: The data is returned as a `SpatRaster` object, with all relevant
#' metadata set. Package `terra` must be installed for this to work.
#' * `data.table()`: The data is returned as a `data.table`, with the first
#' columns corresponding to the dimension values of the axes of the data,
#' followed by a column "value". Package `data.table` must be installed for this
#' to work.
#'
#' In general, the metadata from the netCDF resource will be lost when exporting
#' to a different format insofar as those metadata are not recognized by the
#' different format.
#'
#' @docType class
#'
#' @export
CFData <- R6::R6Class("CFData",
  inherit = CFObject,
  private = list(
    set_dimnames = function() {
      # FIXME
    }
  ),
  public = list(
    #' @field value The data of this object. The structure of the data depends
    #' on the method that produced it. Typical structures are an array or a
    #' `data.table`.
    value = NULL,

    #' @field axes List of instances of classes descending from [CFAxis] that
    #'   are the axes of the data object.
    axes  = list(),

    #' @description Create an instance of this class.
    #' @param name The name of the object.
    #' @param group The group that this data should live in. This is usually an
    #' in-memory group, but it could be a regular group if the data is prepared
    #' for writing into a new netCDF file.
    #' @param value The data of this object. The structure of the data depends
    #'   on the method that produced it.
    #' @param axes A `list` of [CFAxis] descendant instances that describe the
    #'   axes of the argument `value`.
    #' @param attributes A `data.frame` with the attributes associated with the
    #'   data in argument `value`.
    initialize = function(name, group, value, axes, attributes) {
      var <- NCVariable$new(-1L, name, group, "NC_FLOAT", 0L, NULL)
      var$attributes <- attributes
      super$initialize(var)

      self$value <- value
      self$axes <- axes
    },

    #' @description Retrieve the data in the object exactly as it was produced
    #' by the operation on `CFVariable`. This is usually an `array` with the
    #' contents along axes varying.
    raw = function() {
      private$set_dimnames()
      self$value
    }
  )
)
