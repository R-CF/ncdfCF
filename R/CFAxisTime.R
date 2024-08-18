#' @importMethodsFrom CFtime bounds range format indexOf
#' @importFrom CFtime slab
#' @importFrom CFtime unit
NULL

CFAxisTime <- R6::R6Class("CFAxisTime",
  inherit = CFAxis,
  public = list(
    values     = NULL,

    initialize = function(grp, nc_var, nc_dim, values) {
      super$initialize(grp, nc_var, nc_dim, "T")
      self$values <- values
    },

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

    time = function() {
      self$values
    },

    indexOf = function(x, method = "constant", rightmost.closed = FALSE) {
      tf <- slab(self$values, x, rightmost.closed)
      idx <- which(tf)
      attr(idx, "CFtime") <- attr(tf, "CFtime")
      idx
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
