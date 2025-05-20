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
    values = NULL, # A matrix with the bounds values.

    dims = NULL    # The length of private$values in all dimensions
  ),
  public = list(
    #' @field NCdim The [NCDimension] that stores the netCDF dimension details
    #' of the bounds dimension (as opposed to the dimension of the associated
    #' axis).
    NCdim = NULL,

    #' @description Create an instance of this class.
    #' @param nc_var The NC variable that describes this instance.
    #' @param nc_dim The NC dimension that defines the vertices of the bounds.
    #' @param values A matrix with the bounds values.
    initialize = function(nc_var, nc_dim, values) {
      super$initialize(nc_var)
      self$NCdim <- nc_dim
      private$values <- values
      private$dims <- as.integer(dim(values))
      self$set_attribute("actual_range", "NC_DOUBLE", range(values))

      nc_var$CF <- self
    },

    #' @description Print a summary of the object to the console.
    #' @param ... Arguments passed on to other functions. Of particular interest
    #' is `width = ` to indicate a maximum width of attribute columns.
    print = function(...) {
      if (is.null(private$values))
        cat("Bounds     : (no values)\n")
      else {
        if (private$dims[1L] == 2L) {
          len <- dim(private$values)[2L]
          if (len < 8L) {
            from_vals <- trimws(formatC(private$values[1L, ], digits = 8L))
            to_vals   <- trimws(formatC(private$values[2L, ], digits = 8L))
            cat("Bounds     :", paste(from_vals, collapse = ", "), "\n")
            cat("           :", paste(to_vals, collapse = ", "), "\n")
          } else {
            vals <- trimws(formatC(c(private$values[1L, 1L:3L], private$values[1L, (len-2L):len],
                                     private$values[2L, 1L:3L], private$values[2L, (len-2L):len]), digits = 8L))
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
      if (is.null(private$values))
        NULL
      else {
        rng <- self$attribute("actual_range")
        if (any(is.na(rng)))
          range(private$values)
        else
          rng
      }
    },

    #' @description Return bounds spanning a smaller coordinate range.
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
      CFBounds$new(var, self$NCdim, private$values[, rng[1L]:rng[2L], drop = FALSE])
    },

    #' @description Write the bounds variable to a netCDF file. This method
    #'   should not be called directly; instead, `CFArray::save()` will call this
    #'   method automatically.
    #' @param h The handle to a netCDF file open for writing.
    #' @param object_name The name of the object that uses these bounds, usually
    #' an axis but could also be an auxiliary CV or a parametric Z axis.
    write = function(h, object_name) {
      self$NCdim$write(h)
      RNetCDF::var.def.nc(h, self$name, self$NCvar$vtype, c(self$NCdim$name, object_name))
      self$write_attributes(h, self$name)
      RNetCDF::var.put.nc(h, self$name, private$values)
    }
  ),
  active = list(
    #' @field friendlyClassName (read-only) A nice description of the class.
    friendlyClassName = function(value) {
      if (missing(value))
        "Bounds object"
    },

    #' @field coordinates (read-only) Retrieve the boundary values.
    coordinates = function(value) {
      if (missing(value))
        private$values
    }
  )
)
