#' @importMethodsFrom CFtime bounds range format indexOf
#' @importFrom CFtime unit
NULL

CFAxisTime <- R6::R6Class("CFAxisTime",
  inherit = CFAxis,
  public = list(
    values     = NULL,

    initialize = function(grp, nc_var, nc_dim, values) {
      super$initialize(grp, nc_var, nc_dim)
      self$values <- values
      self$orientation <- "T"
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
        dims <- "(no values)"
        bnds <- ""
      } else {
        if (nv == 1L) dims <- paste0("[", format(time), "]")
        else {
          rng <- range(time, format = "", bounds = FALSE)
          dims <- sprintf("[%s ... %s]", rng[1L], rng[2L])
        }
        if (!is.null(bounds(time))) {
          bndrng <- range(time, format = "", bounds = TRUE)
          bnds <- sprintf("[%s ... %s]", bndrng[1L], bndrng[2L])
        } else bnds <- ""
      }

      out$values <- dims
      out$bounds <- bnds
      out
    },

    time = function() {
      self$values
    },

    indexOf = function(x, method = "constant") {
      indexOf(x, self$values, method)
    }
  )
)

# Public S3 methods ------------------------------------------------------------

#' @export
dimnames.CFAxisTime <- function(x) {
  format(x$values)
}
