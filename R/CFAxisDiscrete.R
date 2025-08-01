#' CF discrete axis object
#'
#' @description This class represent discrete CF axes, i.e. those axes whose
#'   coordinate values do not represent a physical property. The coordinate
#'   values are ordinal values equal to the index into the axis.
#'
#' @docType class
#' @export
CFAxisDiscrete <- R6::R6Class("CFAxisDiscrete",
  inherit = CFAxis,
  private = list(
    # Flag if this axis is constructed from a bare dimension, i.e. there is no
    # NC variable associated with this axis in the netCDF file.
    dim_only = FALSE,

    # The values of the axis: just an integer sequence
    get_values = function() {
      seq(self$length)
    },

    # The coordinate values of the axis are just an integer sequence, unless
    # labels have been set for the axis.
    get_coordinates = function() {
      if (private$active_coords == 1L)
        seq(self$length)
      else
        private$aux[[private$active_coords - 1L]]$coordinates
    },

    dimvalues_short = function() {
      crds <- private$get_coordinates()
      len <- self$length
      if (len == 1L) paste0("[", crds[1L], "]")
      else paste0("[", crds[1L], " ... ", crds[len], "]")
    }
  ),
  public = list(
    #' @description Create a new instance of this class.
    #'
    #'   Creating a new discrete axis is more easily done with the
    #'   [makeDiscreteAxis()] function.
    #' @param nc_var The netCDF variable that describes this instance.
    #' @param nc_dim The netCDF dimension that describes the dimensionality.
    #' @param orientation The orientation (`X`, `Y`, `Z`, or `T`) or `""` if
    #' different or unknown.
    #' @param dim_only Flag if this axis only has a dimension on file but no
    #' NC variable.
    initialize = function(nc_var, nc_dim, orientation, dim_only = FALSE) {
      super$initialize(nc_var, nc_dim, orientation)
      self$set_attribute("actual_range", "NC_INT", c(1L,self$length))
      private$dim_only <- dim_only
    },

    #' @description Summary of the axis printed to the console.
    #' @param ... Arguments passed on to other functions. Of particular interest
    #' is `width = ` to indicate a maximum width of attribute columns.
    #' @return `self`, invisibly.
    print = function(...) {
      super$print()
      self$print_attributes(...)
    },

    #' @description Some details of the axis.
    #'
    #' @return A 1-row `data.frame` with some details of the axis.
    brief = function() {
      out <- super$brief()
      out$values <- private$dimvalues_short()
      out
    },

    #' @description Append a vector of values at the end of the current values
    #'   of the axis. In a discrete axis the values are always a simple sequence
    #'   so the appended values extend the sequence, rather than using the
    #'   values from axis `from`.
    #' @param from An instance of `CFAxisDiscrete` whose length to add to the
    #'   length of `self`.
    #' @return A new `CFAxisDiscrete` with the combined length of `self` and the
    #'   `from` axis.
    append = function(from) {
      if (super$can_append(from)) {
        axis <- makeDiscreteAxis(self$name, makeGroup(), private$length + from$length)
        axis$attributes <- self$attributes
        axis
      } else
        stop("Axis values cannot be appended.", call. = FALSE)
    },

    #' @description Find indices in the axis domain. Given a vector of numerical
    #'   values `x`, find their indices in the values of the axis. In effect,
    #'   this returns index values into the axis, but outside values will be
    #'   dropped.
    #'
    #' @param x Vector of numeric values to find axis indices for.
    #' @param method Ignored.
    #' @param rightmost.closed Ignored.
    #'
    #' @return Numeric vector of the same length as `x`. Values of `x` outside
    #'   of the range of the values in the axis are returned as `NA`.
    indexOf = function(x, method = "constant", rightmost.closed = TRUE) {
      x[x < 1 | x > self$length] <- NA
      as.integer(x)
    },

    #' @description Return an axis spanning a smaller coordinate range. This
    #'   method returns an axis which spans the range of indices given by the
    #'   `rng` argument.
    #'
    #' @param group The group to create the new axis in.
    #' @param rng The range of indices from this axis to include in the returned
    #'   axis.
    #'
    #' @return A `CFAxisDiscrete` instance covering the indicated range of
    #'   indices. If the value of the argument is `NULL`, return the entire
    #'   axis.
    subset = function(group, rng = NULL) {
      if (is.null(rng)) {
        ax <- self$clone()
        ax$group <- group
      } else {
        var <- NCVariable$new(CF$newVarId(), self$name, group, "NC_DOUBLE", 1L, -1L)
        dim <- NCDimension$new(CF$newDimId(), self$name, rng[2L] - rng[1L] + 1L, FALSE)
        ax <- CFAxisDiscrete$new(var, dim, self$orientation)
        private$subset_coordinates(ax, rng)
      }
      ax
    },

    #' @description Write the axis to a netCDF file, including its attributes,
    #' but only if it has an associated NC variable in the file.
    #' @param nc The handle of the netCDF file opened for writing or a group in
    #'   the netCDF file. If `NULL`, write to the file or group where the axis
    #'   was read from (the file must have been opened for writing). If not
    #'   `NULL`, the handle to a netCDF file or a group therein.
    #' @return Self, invisibly.
    write = function(nc = NULL) {
      if (private$dim_only) {
        # Write the dimension and any labels. No values or attributes to write.
        h <- if (inherits(nc, "NetCDF")) nc else self$group$handle
        self$NCdim$write(h)
        lapply(private$aux, function(x) x$write(nc))
      } else
        super$write(nc)
      invisible(self)
    }
  ),
  active = list(
    #' @field friendlyClassName (read-only) A nice description of the class.
    friendlyClassName = function(value) {
      if (missing(value))
        "Discrete axis"
    },

    #' @field dimnames (read-only) The coordinates of the axis as an integer
    #' vector, or labels for every axis element if they have been set.
    dimnames = function(value) {
      if (missing(value))
        private$get_coordinates()
    }
  )
)
