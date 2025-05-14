#' CF character axis object
#'
#' @description This class represent CF axes that use categorical character
#' labels as coordinate values. Note that this is different from a [CFLabel],
#' which is associated with an axis but not an axis itself.
#'
#' This is an extension to the CF Metadata Conventions. As per CF, axes are
#' required to have numerical values, which is relaxed here.
#'
#' @docType class
#' @export
CFAxisCharacter <- R6::R6Class("CFAxisCharacter",
  inherit = CFAxis,
  private = list(
    # The character labels of this axis.
    values = NULL,

    get_values = function() {
      private$values
    },

    dimvalues_short = function() {
      if (self$length) sprintf("[%s]", paste0(private$values, collapse = ", "))
      else "(no values)"
    }
  ),
  public = list(
    #' @description Create a new instance of this class.
    #' @param nc_var The netCDF variable that describes this instance.
    #' @param nc_dim The netCDF dimension that describes the dimensionality.
    #' @param orientation The orientation (`X`, `Y`, `Z`, or `T`) or `""` if
    #' different or unknown.
    #' @param values The character coordinates of this axis.
    initialize = function(nc_var, nc_dim, orientation, values) {
      super$initialize(nc_var, nc_dim, orientation)
      private$values <- values
      self$set_attribute("actual_range", nc_var$vtype, range(values))
    },

    #' @description Some details of the axis.
    #'
    #' @return A 1-row `data.frame` with some details of the axis.
    brief = function() {
      out <- super$brief()
      out$values <- private$dimvalues_short()
      out
    },

    #' @description Tests if the axis passed to this method is identical to
    #'   `self`.
    #' @param axis The `CFAxisCharacter` instance to test.
    #' @return `TRUE` if the two axes are identical, `FALSE` if not.
    identical = function(axis) {
      super$identical(axis) &&
      all(private$values == axis$values)
    },

    #' @description Tests if the axis passed to this method can be appended to
    #'   `self`. This means that all values in the passed axis must be different
    #'   from the values in `self`.
    #' @param axis The `CFAxisCharacter` instance to test.
    #' @return `TRUE` if the passed axis can be appended to `self`, `FALSE` if
    #'   not.
    can_append = function(axis) {
      super$can_append(axis) &&
      !any(axis$values %in% private$values)
    },

    #' @description Find indices in the axis domain. Given a vector of character
    #'   strings `x`, find their indices in the coordinates of the axis.
    #'
    #' @param x Vector of character strings to find axis indices for.
    #' @param method Ignored.
    #' @param rightmost.closed Ignored.
    #'
    #' @return Numeric vector of the same length as `x`. Values of `x` that are
    #'   not equal to a coordinate of the axis are returned as `NA`.
    indexOf = function(x, method = "constant", rightmost.closed = TRUE) {
      match(x, private$values)
    }
  ),
  active = list(
    #' @field friendlyClassName (read-only) A nice description of the class.
    friendlyClassName = function(value) {
      if (missing(value)) "Character axis"
    },

    #' @field dimnames (read-only) The coordinates of the axis as a character
    #' vector.
    dimnames = function(value) {
    if (missing(value))
      private$values
    }
  )
)
