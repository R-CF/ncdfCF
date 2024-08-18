CFBounds <- R6::R6Class("CFBounds",
  inherit = CFObject,
  private = list(
    dims = NULL # The length of self$values in all dimensions
  ),
  public = list(
    values = NULL,

    initialize = function(nc_var, values) {
      super$initialize(nc_var)
      self$values <- values
      private$dims <- as.integer(dim(values))

      nc_var$CF <- self
    },

    print = function() {
      if (is.null(self$values))
        cat("Bounds   : (no values)\n")
      else {
        if (private$dims[1L] == 2L) {
          len <- dim(self$values)[2L]
          vals <- trimws(formatC(c(self$values[1L, 1L:3L], self$values[1L, (len-2L):len],
                                   self$values[2L, 1L:3L], self$values[2L, (len-2L):len]), digits = 8L))
          cat("Bounds   : ", vals[1L], ", ", vals[2L], ", ", vals[3L], " ... ", vals[4L], ", ", vals[5L], ", ", vals[6L], "\n", sep = "")
          cat("         : ", vals[7L], ", ", vals[8L], ", ", vals[9L], " ... ", vals[10L], ", ", vals[11L], ", ", vals[12L], "\n", sep = "")
        } else {
          # FIXME
          cat("Bounds   : (can't print multi-dimensional bounds just yet...)\n")
        }
      }
    },

    range = function() {
      if (is.null(self$values))
        NULL
      else {
        if (private$dims[1L] == 2L)
          c(self$values[1L, 1L], self$values[2L, private$dims[2L]])
        else if (private$dims[1L] == 4L)
          range(self$values)
        else c(0, 0) # FIXME
      }
    }
  ),
  active = list(
    #' @field friendlyClassName (read-only) A nice description of the class.
    friendlyClassName = function(value) {
      if (missing(value))
        "Bounds object"
    }
  )
)
