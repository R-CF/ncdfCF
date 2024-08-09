#' Latitude CF axis object
#'
#' @description This class represents a latitude axis. Its values are numeric.
#' This class is used for axes that represent latitudes.
#'
#' This class adds some logic that is specific to latitudes, such as their
#' range, orientation and their meaning.
#'
#' @docType class
#'
#' @name CFAxisLatitude
#' @format An [R6Class] generator object.
NULL

#' @export
CFAxisLatitude <- R6::R6Class("CFAxisLatitude",
  inherit = CFAxisNumeric,
  public = list(

    initialize = function(grp, nc_var, nc_dim, values) {
      super$initialize(grp, nc_var, nc_dim, "Y", values)
    }
  ),
  active = list(
   #' @field friendlyClassName (read-only) A nice description of the class.
   friendlyClassName = function(value) {
     if (missing(value))
       "Latitude axis"
   }
  )
)
