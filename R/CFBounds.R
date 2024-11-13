#' CF bounds variable
#'
#' @description This class represents the bounds of an axis or an auxiliary
#'   longitude-latitude grid.
#'
#'   The class manages the bounds information for an axis (2 vertices per
#'   element) or an auxiliary longitude-latitude grid (4 vertices per element).
#'
#' @docType class
#'
#' @export
CFBounds <- R6::R6Class("CFBounds",
  inherit = CFObject,
  private = list(
    dims = NULL # The length of self$values in all dimensions
  ),
  public = list(
    #' @field values A matrix with the bounds values.
    values = NULL,

    #' @description Create an instance of this class.
    #' @param nc_var The NC variable that describes this instance.
    #' @param values A matrix with the bounds values.
    initialize = function(nc_var, values) {
      super$initialize(nc_var, nc_var$group)
      self$values <- values
      private$dims <- as.integer(dim(values))

      nc_var$CF <- self
    },

    #' @description Print a summary of the object to the console.
    print = function() {
      if (is.null(self$values))
        cat("Bounds   : (no values)\n")
      else {
        if (private$dims[1L] == 2L) {
          len <- dim(self$values)[2L]
          if (len < 7L) {
            from_vals <- trimws(formatC(self$values[1L, ], digits = 8L))
            to_vals   <- trimws(formatC(self$values[2L, ], digits = 8L))
            cat("Bounds   :", paste(from_vals, collapse = ", "), "\n")
            cat("         :", paste(to_vals, collapse = ", "), "\n")
          } else {
            vals <- trimws(formatC(c(self$values[1L, 1L:3L], self$values[1L, (len-2L):len],
                                     self$values[2L, 1L:3L], self$values[2L, (len-2L):len]), digits = 8L))
            cat("Bounds   : ", vals[1L], ", ", vals[2L], ", ", vals[3L], " ... ", vals[4L], ", ", vals[5L], ", ", vals[6L], "\n", sep = "")
            cat("         : ", vals[7L], ", ", vals[8L], ", ", vals[9L], " ... ", vals[10L], ", ", vals[11L], ", ", vals[12L], "\n", sep = "")
          }
        } else {
          # FIXME
          cat("Bounds   : (can't print multi-dimensional bounds just yet...)\n")
        }
      }
    },

    #' @description Retrieve the lowest and highest value in the bounds.
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
    },

    #' @description Return bounds spanning a smaller dimension range.
    #'
    #'   This method returns bounds which spans the range of indices given by
    #'   the `rng` argument.
    #'
    #' @param group The group to create the new bounds in.
    #' @param rng The range of values from this bounds object to include in the
    #'   returned object.
    #'
    #' @return A `CFBounds` instance covering the indicated range of indices.
    sub_bounds = function(group, rng) {
      var <- NCVariable$new(-1L, self$name, group, "NC_DOUBLE", 2L, NULL)
      CFBounds$new(var, self$values[, rng[1L]:rng[2L], drop = FALSE])
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
