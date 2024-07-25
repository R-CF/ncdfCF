CFAxisDiscrete <- R6::R6Class("CFAxisDiscrete",
  inherit = CFAxis,
  public = list(
    initialize = function(grp, nc_var, nc_dim) {
      super$initialize(grp, nc_var, nc_dim)
    },

    brief = function() {
      out <- super$brief()
      out$values <- if (self$length == 1L) "[1]"
                    else paste0("[1 ... ", self$length, "]")
      out
    },

    indexOf = function(x, method = "constant") {
      x[x < 1] <- 0L
      x[x > self$length] <- .Machine$integer.max
      as.integer(x)
    }
  )
)

# Public S3 methods ------------------------------------------------------------

#' @export
dimnames.CFAxisDiscrete <- function(x) {
  seq(x$length)
}
