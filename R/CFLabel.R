#' CF label object
#'
#' @description This class represent CF labels, i.e. an NC variable of character
#' type that provides a textual label for a discrete or general numeric axis.
#' See also [CFAxisCharacter], which is an axis with character labels.
#'
#' @docType class
#' @export
CFLabel <- R6::R6Class("CFLabel",
  inherit = CFObject,
  public = list(
    #' @field NCdim The [NCDimension] that stores the netCDF dimension details.
    NCdim       = NULL,

    #' @field values The label values, a character vector.
    values     = NULL,

    #' @description Create a new instance of this class.
    #' @param grp The group that contains the netCDF variable.
    #' @param nc_var The netCDF variable that describes this instance.
    #' @param nc_dim The netCDF dimension that describes the dimensionality.
    #' @param values Character vector of the label values.
    initialize = function(grp, nc_var, nc_dim, values) {
      super$initialize(nc_var, grp)
      self$NCdim <- nc_dim
      self$values <- trimws(values)

      nc_var$CF <- self
    }
  ),
  active = list(
    #' @field friendlyClassName (read-only) A nice description of the class.
    friendlyClassName = function(value) {
      if (missing(value))
        "Axis labels"
    },

    #' @field length (read-only) The number of labels.
    length = function(value) {
      if (missing(value))
        self$NCdim$length
    },

    #' @field dimid (read-only) The netCDF dimension id of this label.
    dimid = function(value) {
      if (missing(value))
        self$NCdim$id
    }
  )
)
