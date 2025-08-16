#' CF bounds variable
#'
#' @description This class represents the boundaries of an axis or an auxiliary
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
    # Start and count vectors for reading boundary data from file. The vectors
    # must have length 2 or be `NA`.
    .start = NA,
    .count = NA,

    # A matrix with the boundary values.
    .values = NULL,

    load = function() {
      if (!is.null(private$.values)) return()

      private$.values <- self$NCvar$get_data(private$.start, private$.count)
      # FIXME
      if (!is.null(self$NCvar))
        self$set_attribute("actual_range", "NC_DOUBLE", range(private$.values))
    }
  ),
  public = list(
    #' @description Create an instance of this class.
    #' @param var The name of the boundary variable when creating a new boundary
    #'   variable. When reading a boundary variable from file, the [NCVariable]
    #'   object that describes this instance.
    #' @param values Optional. The values of the boundary variable. This must be
    #'   a numeric matrix whose first dimension has a length equal to the number
    #'   of vertices for each boundary, and the second dimension is as long as
    #'   the CFObject instances that use these boundary values. Ignored when
    #'   argument `var` is a NCVariable object.
    #' @param start Optional. Vector of indices where to start reading boundary
    #'   data along the dimensions of the data. The vector must be `NA` to read
    #'   all data, otherwise it must have `length = 2`.
    #' @param count Optional. Vector of number of elements to read along each
    #'   dimension of the boundary data. The vector must be `NA` to read to the
    #'   end of each dimension, otherwise it must have `length = 2`.
    initialize = function(var, values = NA, start = NA, count = NA) {
      super$initialize(var)
      private$.start <- start
      private$.count <- count

      if (is.character(var)) {
        private$.values <- values
        if (!is.null(self$NCvar)) # FIXME
          self$set_attribute("actual_range", "NC_DOUBLE", range(private$.values))
      } else
        self$NCvar$CF <- self
    },

    #' @description Print a summary of the object to the console.
    #' @param ... Arguments passed on to other functions. Of particular interest
    #' is `width = ` to indicate a maximum width of attribute columns.
    print = function(...) {
      private$load()
      if (is.null(private$.values))
        cat("Bounds     : (no values)\n")
      else {
        dims <- dim(private$.values)
        if (dims[1L] == 2L) {
          len <- dims[2L]
          if (len < 8L) {
            from_vals <- trimws(formatC(private$.values[1L, ], digits = 8L))
            to_vals   <- trimws(formatC(private$.values[2L, ], digits = 8L))
            cat("Bounds     :", paste(from_vals, collapse = ", "), "\n")
            cat("           :", paste(to_vals, collapse = ", "), "\n")
          } else {
            vals <- trimws(formatC(c(private$.values[1L, 1L:3L], private$.values[1L, (len-2L):len],
                                     private$.values[2L, 1L:3L], private$.values[2L, (len-2L):len]), digits = 8L))
            cat("Bounds     : ", vals[1L], ", ", vals[2L], ", ", vals[3L], " ... ", vals[4L], ", ", vals[5L], ", ", vals[6L], "\n", sep = "")
            cat("           : ", vals[7L], ", ", vals[8L], ", ", vals[9L], " ... ", vals[10L], ", ", vals[11L], ", ", vals[12L], "\n", sep = "")
          }
        } else {
          # FIXME
          cat("Bounds     : (can't print multi-dimensional bounds just yet...)\n")
        }
      }
    },

    #' @description Retrieve the lowest and highest value in the bounds.
    range = function() {
      private$load()
      if (is.null(private$.values))
        NULL
      else {
        rng <- self$attribute("actual_range")
        if (any(is.na(rng)))
          range(private$.values)
        else
          rng
      }
    },

    #' @description Return a boundary variable spanning a smaller coordinate
    #'   range.
    #'
    #'   This method returns boundary values which span the range of indices
    #'   given by the `rng` argument. This method currently only works for
    #'   boundary values along axes, i.e. 1D coordinate values with boundary
    #'   values over two vertices.
    #'
    #' @param rng The range of values from this bounds object to include in the
    #'   returned object.
    #'
    #' @return A `CFBounds` instance covering the indicated range of indices.
    sub_bounds = function(rng) {
      if (is.null(self$NCvar))
        CFBounds$new(self$name, values = private$.values[, rng[1L]:rng[2L]])
      else
        CFBounds$new(self$NCvar, start = c(1L, rng[1L]), count = c(2L, rng[2L] - rng[1L] + 1L))
    },

    #' @description Write the bounds variable to a netCDF file. This method
    #'   should not be called directly; instead, `CFArray::save()` will call this
    #'   method automatically.
    #' @param h The handle to a netCDF file open for writing.
    #' @param object_name The name of the object that uses these bounds, usually
    #' an axis but could also be an auxiliary CV or a parametric Z axis.
    write = function(h, object_name) {
      private$load()
      dim <- self$NCvar$dimension(id)
      dim$write(h)
      self$id <- RNetCDF::var.def.nc(h, self$name, self$NCvar$data_type, c(dim$name, object_name))
      self$write_attributes(h, self$name)
      RNetCDF::var.put.nc(h, self$name, private$.values)
    }
  ),
  active = list(
    #' @field friendlyClassName (read-only) A nice description of the class.
    friendlyClassName = function(value) {
      if (missing(value))
        "Boundary values object"
    },

    #' @field valuesx (read-only) Retrieve the boundary values.
    values = function(value) {
      if (missing(value)) {
        private$load()
        private$.values
      }
    }
  )
)
