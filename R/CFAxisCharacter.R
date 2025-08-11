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
  cloneable = FALSE,
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
    #'
    #'   Creating a new character axis is more easily done with the [makeAxis()]
    #'   function.
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

    #' @description Create a copy of this axis. The copy is completely separate
    #' from `self`, meaning that both `self` and all of its components are made
    #' from new instances.
    #' @param group The group in which to place the new axis. If the argument is
    #' missing, a new group will be created for the axis with a link to the
    #' netCDF resource of `self`, if set.
    copy = function(group) {
      if (missing(group))
        group <- makeGroup(resource = self$group$resource)

      ax <- makeCharacterAxis(self$name, group, private$values, self$attributes)
      private$subset_coordinates(ax, c(1L, self$length))
      ax
    },

    #' @description Tests if the axis passed to this method is identical to
    #'   `self`.
    #' @param axis The `CFAxisCharacter` instance to test.
    #' @return `TRUE` if the two axes are identical, `FALSE` if not.
    identical = function(axis) {
      super$identical(axis) &&
      all(private$values == axis$values)
    },

    #' @description Append a vector of values at the end of the current values
    #'   of the axis.
    #' @param from An instance of `CFAxisCharacter` whose values to append to
    #'   the values of `self`.
    #' @return A new `CFAxisCharacter` instance with values from `self` and the
    #'   `from` axis appended.
    append = function(from) {
      if (super$can_append(from) && !any(from$values %in% private$values)) {
        makeCharacterAxis(self$name, makeGroup(), c(private$values, from$values), self$attributes)
      } else
        stop("Axis values are not unique after appending.", call. = FALSE)
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
