#' CF auxiliary longitude-latitude variable
#'
#' @description This class represents the longitude and latitude variables that
#' compose auxiliary coordinate variable axes for X-Y grids that are not
#' longitude-latitude.
#'
#' @details The class provides access to the data arrays for longitude and
#' latitude from the netCDF resource, as well as all the details that have been
#' associated with both axes. Additionally, this class can generate the index
#' to extract values on a long-lat grid of the associated X-Y grid data variable
#' using a user-selectable extent and resolution.
#'
#' @docType class
#'
#' @name CFAuxiliaryLongLat
#' @format An \code{\link{R6Class}} generator object.
NULL

#' @export
CFAuxiliaryLongLat <- R6::R6Class("CFAuxiliaryLongLat",
  inherit = CFObject,
  public = list(
    #' @field varLong The [NCVariable] instance of the longitude values.
    varLong = NULL,

    #' @field varLat The [NCVariable] instance of the latitude values.
    varLat = NULL,

    #' @field axisLong The [CFAxis] instance of the longitude.
    axisLong = NULL,

    #' @field axisLat The [CFAxis] instance of the latitude.
    axisLat = NULL,

    initialize = function(varLong, varLat) {
      self$varLong <- varLong
      self$varLat <- varLat

      varLong$CF <- self
      varLat$CF <- self
    }
  ),
  active = list(
    #' @field friendlyClassName (read-only) A nice description of the class.
    friendlyClassName = function(value) {
      if (missing(value))
        "Auxiliary longitude-latitude grid"
    },

    #' @field name The name of the auxiliary lon-lat grid.
    name = function(value) {
      paste(self$varLong$name, self$varLat$name, sep = "_")
    }
  )
)
