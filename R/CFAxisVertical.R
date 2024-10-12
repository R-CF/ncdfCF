#' Parametric vertical CF axis object
#'
#' @description This class represents a parametric vertical axis. It is defined
#' through an index value that is contained in the axis, with additional
#' [NCVariable]s that hold ancillary data with which to calculate dimensional
#' axis values. It is used in atmosphere and ocean data sets. Non-parametric
#' vertical axes are stored in an [CFAxisNumeric].
#'
#' @references https://cfconventions.org/Data/cf-conventions/cf-conventions-1.11/cf-conventions.html#parametric-vertical-coordinate
#'
#' @docType class
#'
CFAxisVertical <- R6::R6Class("CFAxisVertical",
  inherit = CFAxisNumeric,
  private = list(
    # A `data.frame` with columns `terms`, `variable` and `NCvar` containing the
    # terms of the formula to calculate the axis values. Column `NCvar` has the
    # references to the [NCVariable]s that hold the data for the terms.
    terms = NULL,

    # The computed values of the parametric axis.
    computed_values = NULL,

    # This function computes the actual dimensional axis values from the terms.
    compute = function() {
      # Not yet implemented
    }
  ),
  public = list(
    #' @field parameter_name The 'standard_name' attribute of the [NCVariable]
    #' that identifies the parametric form of this axis.
    parameter_name = "",

    #' @field computed_name The standard name for the computed values of the axis.
    computed_name = "",

    #' @field computed_units The unit of the computed values of the axis.
    computed_units = "",

    #' @description Create a new instance of this class.
    #' @param grp The group that contains the netCDF variable.
    #' @param nc_var The netCDF variable that describes this instance.
    #' @param nc_dim The netCDF dimension that describes the dimensionality.
    #' @param values The dimension values of this axis.
    #' @param standard_name Character string with the "standard_name" that
    #' defines the meaning, and processing of coordinates, of this axis.
    initialize = function(grp, nc_var, nc_dim, values, standard_name) {
      super$initialize(grp, nc_var, nc_dim, "Z", values)
      self$parameter_name <- standard_name
    }
  ),
  active = list(
    #' @field friendlyClassName (read-only) A nice description of the class.
    friendlyClassName = function(value) {
      if (missing(value))
        "Parametric vertical axis"
    },

    #' @field formula_terms (read-only) A `data.frame` with the "formula_terms"
    #' to calculate the parametric axis values.
    formula_terms = function(value) {
      if (missing(value))
        private$terms
      else {
        private$terms <- value
        private$compute()
      }
    },

    #' @field dimnames (read-only) The coordinates of the axis.
    dimnames = function(value) {
      if (missing(value))
        private$computed_values
    }
  )
)
