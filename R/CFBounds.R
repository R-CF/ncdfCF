CFBounds <- R6::R6Class("CFBounds",
  inherit = CFObject,
  public = list(
    values = NULL,

    initialize = function(nc_var) {
      super$initialize(nc_var)

      nc_var$CF <- self
    },

    print = function() {
      if (is.null(self$values))
        cat("Bounds   : (no values)\n")
      else {
        len <- dim(self$values)[2L]
        vals <- trimws(formatC(c(self$values[1L, 1L:3L], self$values[1L, (len-2):len],
                                 self$values[2L, 1L:3L], self$values[2L, (len-2):len]), digits = 8L))
        cat("Bounds   : ", vals[1L], ", ", vals[2L], ", ", vals[3L], " ... ", vals[4L], ", ", vals[5L], ", ", vals[6L], "\n", sep = "")
        cat("         : ", vals[7L], ", ", vals[8L], ", ", vals[9L], " ... ", vals[10L], ", ", vals[11L], ", ", vals[12L], "\n", sep = "")
      }
    },

    range = function() {
      if (is.null(self$values))
        NULL
      else {
        d <- dim(self$values)[2L]
        c(self$values[1L, 1L], self$values[2L, d])
      }
    }
  )
)
