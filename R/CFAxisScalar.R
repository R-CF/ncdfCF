#' Scalar CF axis object
#'
#' @description This class represents a scalar axis. Its single value can be of
#' any type. It is typically used as an auxiliary axis to record some parameter
#' of interest such as the single time associated with a spatial grid with
#' longitude, latitude and vertical axes.
#'
#' @docType class
#'
#' @name CFAxisScalar
#' @format An [R6Class] generator object.
NULL

#' @export
CFAxisScalar <- R6::R6Class("CFAxisScalar",
  inherit = CFAxis,
  public = list(
    #' @field value The value of the axis.
    value = NULL,

    initialize = function(grp, nc_var, orientation, value) {
      super$initialize(grp, nc_var, NULL, orientation)
      self$value <- value
    },

    print = function() {
      super$print()

      units <- self$attribute("units")
      if (!length(units)) units <- ""
      cat("Value    : ", self$value, " ", units, "\n", sep = "")
      if (!is.null(self$bounds))
        self$bounds$print()
      else cat("Bounds   : (not set)\n")

      self$print_attributes()
    },

    brief = function() {
      out <- super$brief()
      dims <- paste0("[", self$value, "]\n")
      if (is.null(self$bounds)) bnds <- ""
      else {
        rng <- self$bounds$range()
        if (is.null(rng))
          bnds <- ""
        else {
          vals <- trimws(formatC(rng, digits = 8L))
          bnds <- sprintf("[%s ... %s]", vals[1L], vals[2L])
        }
      }

      out$values <- dims
      out$bounds <- bnds
      out
    }
  ),
  active = list(
    #' @field friendlyClassName (read-only) A nice description of the class.
    friendlyClassName = function(value) {
      if (missing(value))
        "Scalar axis"
      }
  )
)
