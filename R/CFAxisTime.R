#' @importMethodsFrom CFtime bounds range format indexOf
#' @importFrom CFtime unit as_timestamp
NULL

#' Time axis object
#'
#' @description This class represents a time axis. The functionality is provided
#' by the `CFtime` package.
#'
#' @docType class
#'
#' @export
CFAxisTime <- R6::R6Class("CFAxisTime",
  inherit = CFAxis,
  public = list(
    #' @field values The CFtime instance to manage CF time.
    values     = NULL,

    #' @description Create a new instance of this class
    #' @param grp The group that contains the netCDF variable.
    #' @param nc_var The netCDF variable that describes this instance.
    #' @param nc_dim The netCDF dimension that describes the dimensionality.
    #' @param values The CFtime instance that manages this axis.
    initialize = function(grp, nc_var, nc_dim, values) {
      super$initialize(grp, nc_var, nc_dim, "T")
      self$values <- values
    },

    #' @description Summary of the time axis
    #'
    #' Prints a summary of the time axis to the console.
    print = function() {
      super$print()

      len <- length(self$values)
      rng <- paste0(range(self$values), collapse = " ... ")
      un <- paste0("(", unit(self$values), ")")
      bndrng <- if (!is.null(bounds(self$values)))
        paste0(range(self$values, format = "", bounds = TRUE), collapse = " ... ")
      else "(not set)"
      cat("Range    :", rng, un, "\n")
      cat("Bounds   :", bndrng, "\n")

      self$print_attributes()
    },

    #' @description Retrieve a 1-row data.frame with some information on this axis.
    brief = function() {
      out <- super$brief()

      time <- self$values
      nv <- length(time)
      if (!nv) { # it happens...
        vals <- "(no values)"
      } else {
        if (nv == 1L) vals <- paste0("[", format(time), "]")
        else {
          rng <- range(time, format = "", bounds = FALSE)
          vals <- sprintf("[%s ... %s]", rng[1L], rng[2L])
        }
      }

      out$values <- vals
      out
    },

    #' @description Retrieve the CFtime instance that manages this axis.
    time = function() {
      self$values
    },

    #' @description Retrieve the indices of supplied values on the time axis.
    #' @param x A vector of timestamps whose indicaes into the time axis to
    #' extract.
    #' @param method Extract index values without ("constant", the default) or
    #' with ("linear") fractional parts.
    #' @param rightmost.closed Whether or not to include the upper limit.
    #' Default is `FALSE`.
    indexOf = function(x, method = "constant", rightmost.closed = FALSE) {
      idx <- indexOf(x, self$values, method)
      idx <- idx[!is.na(idx) & idx > 0 & idx < .Machine$integer.max]
      if (!rightmost.closed)
        idx[length(idx)] <- idx[length(idx)] - 1
      as.integer(idx)
    },

    #' @description Return an axis spanning a smaller dimension range
    #'
    #' This method returns an axis which spans the range of indices given by the
    #' `rng` argument.
    #'
    #' @param group The group to create the new axis in.
    #' @param rng The range of values from this axis to include in the returned
    #' axis.
    #'
    #' @returns A `CFAxisTime` covering the indicated range of indices. If the
    #' `rng` argument includes only a single value, an [CFAxisScalar] instance
    #' is returned with its value being the character timestamp of the value in
    #' this axis.
    sub_axis = function(group, rng) {
      var <- NCVariable$new(-1L, self$name, group, "NC_DOUBLE", 1L, NULL)
      if (rng[1L] == rng[2L]) {
        scl <- CFAxisScalar$new(group, var, "T", as_timestamp(self$values)[rng[1L]])
        bnds <- bounds(self$values)
        if (!is.null(bnds)) scl$bounds <- bnds[, rng[1L]]
        scl
      } else {
        idx <- indexOf(seq(from = rng[1L], to = rng[2L], by = 1L), self$values)
        dim <- NCDimension$new(-1L, self$name, length(idx), FALSE)
        CFAxisTime$new(group, var, dim, attr(idx, "CFtime"))
      }
    }
  ),
  active = list(
    #' @field friendlyClassName (read-only) A nice description of the class.
    friendlyClassName = function(value) {
      if (missing(value))
        "Time axis"
    }
  )
)

# Public S3 methods ------------------------------------------------------------

#' @export
dimnames.CFAxisTime <- function(x) {
  format(x$values)
}
