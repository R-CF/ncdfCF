#' Numeric CF axis object
#'
#' @description This class represents a numeric axis. Its values are numeric.
#' This class is used for axes with numeric values but without further
#' knowledge of their nature. More specific classes descend from this class.
#'
#' @docType class
#'
#' @name CFAxisNumeric
#' @format An [R6Class] generator object.
NULL

#' @export
CFAxisNumeric <- R6::R6Class("CFAxisNumeric",
  inherit = CFAxis,
  public = list(
    #' @field values The values of the axis, usually a numeric vector.
    values     = NULL,

    initialize = function(grp, nc_var, nc_dim, orientation, values) {
      super$initialize(grp, nc_var, nc_dim, orientation)
      self$values <- values
    },

    print = function() {
      super$print()

      units <- self$attribute("units")
      if (!length(units)) units <- ""
      len <- length(self$values)
      if (len < 7L) {
        vals <- trimws(formatC(self$values, digits = 8L))
        cat("Values   : ", paste(vals, collapse = ", "), " ", units, "\n", sep = "")
      } else {
        vals <- trimws(formatC(c(self$values[1L:3L], self$values[(len-2):len], digits = 8L)))
        cat("Values   : ", vals[1L], ", ", vals[2L], ", ", vals[3L], " ... ", vals[4L], ", ", vals[5L], ", ", vals[6L], " ", units, "\n", sep = "")
      }

      if (!is.null(self$bounds))
        self$bounds$print()
      else cat("Bounds   : (not set)\n")

      self$print_attributes()
    },

    brief = function() {
      out <- super$brief()

      nv <- length(self$values)
      if (nv == 1L)
        dims <- sprintf("[%s]", gsub(" ", "", formatC(self$values[1L], digits = 8L)))
      else {
        vals <- trimws(formatC(c(self$values[1L], self$values[nv]), digits = 8L))
        dims <- sprintf("[%s ... %s]", vals[1L], vals[2L])
      }

      out$values <- dims
      out
    },

    indexOf = function(x, method = "linear") {
      if (length(self$bounds))
        vals <- c(self$bounds$values[1L, 1L], self$bounds$values[2L, ])
      else vals <- self$values
      stats::approx(vals, 1L:length(vals), x, method = method, yleft = 0L, yright = .Machine$integer.max)$y
    }
  ),
  active = list(
    #' @field friendlyClassName (read-only) A nice description of the class.
    friendlyClassName = function(value) {
      if (missing(value))
        "Generic numeric axis"
    }
  )
)

# Public S3 methods ------------------------------------------------------------

#' @export
dimnames.CFAxisNumeric <- function(x) {
  x$values
}
