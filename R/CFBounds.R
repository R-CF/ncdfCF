#' CF boundary variable
#'
#' @description This class represents the boundaries of an axis or an auxiliary
#'   longitude-latitude grid.
#'
#'   The class manages the boundary information for an axis (2 vertices per
#'   element) or an auxiliary longitude-latitude grid (4 vertices per element).
#'
#' @docType class
#'
#' @export
CFBounds <- R6::R6Class("CFBounds",
  inherit = CFObject,
  private = list(
    # The number of dimensions that the owning object has.
    .owner_dims = 1L
  ),
  public = list(
    #' @description Create an instance of this class.
    #' @param var The name of the boundary variable when creating a new boundary
    #'   variable. When reading a boundary variable from file, the [NCVariable]
    #'   object that describes this instance.
    #' @param values Optional. The values of the boundary variable. This must be
    #'   a numeric matrix whose first dimension has a length equal to the number
    #'   of vertices for each boundary, and the second dimension is as long as
    #'   the `CFObject` instances that use these boundary values. Ignored when
    #'   argument `var` is a `NCVariable` object.
    #' @param start Optional. Vector of indices where to start reading boundary
    #'   data along the dimensions of the data. The vector must be `NA` to read
    #'   all data, otherwise it must have a length equal to the dimensionality
    #'   of the owning object + 1.
    #' @param count Optional. Vector of number of elements to read along each
    #'   dimension of the boundary data. The vector must be `NA` to read to the
    #'   end of each dimension, otherwise it must have a length equal to the
    #'   dimensionality of the owning object + 1.
    #' @param attributes Optional. A `data.frame` with the attributes of the
    #'   boundary object. When an empty `data.frame` (default) and argument
    #'   `var` is an `NCVariable` instance, attributes of the bounds object will
    #'   be taken from the netCDF resource.
    #' @param owner_dims Optional, the number of dimensions of the object that
    #'   these boundary values pertain to. Default is 1.
    #' @return A new instance of this class.
    initialize = function(var, values, start = NA, count = NA, attributes = data.frame(), owner_dims = 1L) {
      private$.owner_dims <- owner_dims
      super$initialize(var, values, start, count, attributes)

      if (length(private$.start_count$count) > owner_dims + 1L) {
        private$.start_count$count[owner_dims + 2L] <- 1L
        private$read_data()
        private$.values <- drop(private$.values)
      }
    },

    #' @description Print a summary of the object to the console.
    #' @param attributes Default `TRUE`, flag to indicate if the attributes of
    #' the boundary values should be printed.
    #' @param ... Arguments passed on to other functions. Of particular interest
    #' is `width = ` to indicate a maximum width of attribute columns.
    print = function(attributes = TRUE, ...) {
      v <- self$values
      if (is.null(v))
        cat("Bounds     : (no values)\n")
      else {
        dims <- dim(v)
        if (dims[1L] == 2L) {
          len <- dims[2L]
          if (len < 8L) {
            from_vals <- trimws(formatC(v[1L, ], digits = 8L))
            to_vals   <- trimws(formatC(v[2L, ], digits = 8L))
            cat("Bounds     :", paste(from_vals, collapse = ", "), "\n")
            cat("           :", paste(to_vals, collapse = ", "), "\n")
          } else {
            vals <- trimws(formatC(c(v[1L, 1L:3L], v[1L, (len-2L):len],
                                     v[2L, 1L:3L], v[2L, (len-2L):len]), digits = 8L))
            cat("Bounds     : ", vals[1L], ", ", vals[2L], ", ", vals[3L], " ... ", vals[4L], ", ", vals[5L], ", ", vals[6L], "\n", sep = "")
            cat("           : ", vals[7L], ", ", vals[8L], ", ", vals[9L], " ... ", vals[10L], ", ", vals[11L], ", ", vals[12L], "\n", sep = "")
          }
        } else {
          # FIXME
          cat("Bounds     : (can't print multi-dimensional bounds just yet...)\n")
        }
      }

      if (attributes)
        self$print_attributes()
    },

    #' @description Retrieve the lowest and highest value in the bounds.
    range = function() {
      if (is.null(self$values)) NULL
      else self$attribute("actual_range")
    },

    #' @description Create a copy of this bounds object The copy is completely
    #'   separate from `self`, meaning that both `self` and all of its
    #'   components are made from new instances.
    #' @param name The name for the new bounds object. If an empty string is
    #'   passed, will use the name of this bounds object.
    #' @return The newly created bounds object.
    copy = function(name = "") {
      if (self$has_resource) {
        b <- CFBounds$new(private$.NCvar, start = private$.start_count$start,
                          count = private$.start_count$count, attributes = self$attributes)
        if (nzchar(name))
          b$name <- name
      } else {
        if (!nzchar(name))
          name <- self$name
        b <- CFBounds$new(name, values = self$values, attributes = self$attributes)
      }
      b
    },

    # FIXME: Make new format, cater for multiple dimensions

    #' @description Return a boundary variable spanning a smaller coordinate
    #'   range. This currently only applies to 1-D axes.
    #'
    #'   This method returns boundary values which span the range of indices
    #'   given by the `rng` argument.
    #'
    #' @param rng The range of values from this bounds object to include in the
    #'   returned object.
    #'
    #' @return A `CFBounds` instance covering the indicated range of indices.
    subset = function(rng) {
      vals <- if (is.null(self$values)) NULL
              else vals <- self$values[, rng[1L]:rng[2L]]

      if (is.null(private$.NCvar))
        CFBounds$new(private$.name, values = vals, attributes = self$attributes)
      else {
        # The rng argument applies to anything but the first dimension (= # of vertices)
        # of the values. Apply any subsets in self before making the new boundary variable.
        # Currently only for 1-D axes.
        sc <- private$.start_count
        start <- c(1L, sc$start[2L] + rng[1L] - 1L)
        count <- c(sc$count[1L], rng[2L] - rng[1L] + 1L)

        # Add 1L to start and count to read first layer of any additional dimensions that shouldn't be there,
        # if there are such additional dimensions.
        if ((nd <- private$.NCvar$ndims) > length(start)) {
          ones <- rep(1L, nd - length(start))
          start <- c(start, ones)
          count <- c(count, ones)
          vals <- NULL
        }

        CFBounds$new(private$.NCvar, values = vals, start = start, count = count, attributes = self$attributes)
      }
    },

    #' @description Append boundary values at the end of the current values of
    #'   the boundary variable.
    #' @param from An instance of `CFBounds` whose values to append to the
    #'   values of this boundary variable.
    #' @return A new `CFBounds` instance with values from this boundary variable
    #'   and the `from` boundary variable appended. If argument `from` is
    #'   `NULL`, return `NULL`.
    append = function(from) {
      if (is.null(from))
        NULL
      else
        CFBounds$new(self$name, values = cbind(self$values, from$values), attributes = self$attributes)
    },

    #' @description Write the boundary variable to a netCDF file. This method
    #'   should not be called directly; instead, `CFVariable::save()` will call
    #'   this method automatically.
    #' @param h The handle to a netCDF file open for writing.
    #' @param object_name The name of the object that uses these boundary
    #'   values, usually an axis but could also be an auxiliary CV or a
    #'   parametric Z axis.
    write = function(h, object_name) {
      v <- self$values
      nv <- dim(v)[1L]

      # Write the vertex dimension for the axis. Error will be thrown when
      # trying to write a dimension that's already defined, such as when a
      # vertex dimension is shared between multiple objects. In that case, get
      # the id.
      nm <- paste0("nv", nv)
      did <- try(RNetCDF::dim.def.nc(h, nm, nv), silent = TRUE)
      if (inherits(did, "try-error"))
        did <- RNetCDF::dim.inq.nc(h, nv)$id

      private$.id <- RNetCDF::var.def.nc(h, self$name, private$.data_type, c(nm, object_name))
      self$write_attributes(h, self$name)
      RNetCDF::var.put.nc(h, self$name, v)
    }
  ),
  active = list(
    #' @field friendlyClassName (read-only) A nice description of the class.
    friendlyClassName = function(value) {
      if (missing(value))
        "Boundary values object"
    },

    #' @field length (read-only) The length of the second dimension of the data, i.e. the
    #' number of boundary values.
    length = function(value) {
      if (missing(value)) {
        if (!is.null(self$values))
          dim(self$values)[2L]
        else self$dim(2L)
      }
    },

    #' @field vertices (read-only) The length of the first dimension of the data, i.e. the
    #' number of vertices that make up a boundary.
    vertices = function(value) {
      if (missing(value)) {
        if (!is.null(self$values))
          dim(self$values)[1L]
        else self$dim(1L)
      }
    },

    #' @field values Set or retrieve the boundary values of this object. Upon
    #'   retrieval, values are read from the netCDF resource, if there is one,
    #'   upon first access and cached thereafter. Upon setting values, if there
    #'   is a linked netCDF resource, this object will be detached from it.
    values = function(value) {
      if (missing(value)) {
        private$read_data()
      } else {
        private$set_values(value)
        self$detach()
      }
    }
  )
)
