CFAxisDiscrete <- R6::R6Class("CFAxisDiscrete",
  inherit = CFAxis,
  public = list(
    initialize = function(grp, nc_var, nc_dim, orientation) {
      super$initialize(grp, nc_var, nc_dim, orientation)
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
  ),
  active = list(
    #' @field friendlyClassName (read-only) A nice description of the class.
    friendlyClassName = function(value) {
      if (missing(value))
        "Discrete axis"
    },

    #' @field dimnames (read-only) The coordinates of the axis as an integer
    #' vector.
    dimnames = function(value) {
      if (missing(value))
        seq(self$length)
    }
  )
)
