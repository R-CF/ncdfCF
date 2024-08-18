#' Data extracted from a CF data variable
#'
#' @description This class holds the data that is extracted from a [CFVariable],
#' using the `subset()` class method. The instance of this class will
#' additionally have the axes and other relevant information such as the unit of
#' the data and the coordinate reference system.
#'
#' The class has a number of utility functions to extract the data in specific
#' formats:
#' * `raw()`: The array of the data without any further processing. The axes are
#' as they are stored in the netCDF resource; there is thus no guarantee as to
#' how the data is organized in the array. Dimnames will be set.
#' * `array()`: An array of the data which is organized as a standard R array.
#' Dimnames will be set.
#' * `stars()`: The data is returned as a `stars` object, with all relevant
#' metadata set. Package `stars` must be installed for this to work.
#' * `rast()`: The data is returned as a `SpatRaster` object, with all relevant
#' metadata set. Package `terra` must be installed for this to work.
#' * `data.table`: The data is returned as a `data.table`, with the first
#' columns corresponding to the dimension values of the axes of the data,
#' followed by a column "value". Package `data.table` must be installed for this
#' to work.
#'
#' @name CFData
#' @docType class
#'
#' @format An [R6Class] generator object.
NULL

#' @export
CFData <- R6::R6Class("CFData",
  public = list(

  )
)
