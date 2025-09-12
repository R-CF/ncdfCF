#' Parametric formula term for a vertical CF axis object
#'
#' @description This class represents a formula term for a parametric vertical
#' axis.
#'
#' @docType class
CFVerticalParametricTerm <- R6::R6Class("CFVerticalParametricTerm",
  inherit = CFVariable,
  private = list(
    # Flag to indicate that the instance has no data
    .nodata = TRUE
  ),
  public = list(
    #' @description Create an instance of this class.
    #' @param var The [NCVariable] instance upon which this CF variable is based
    #'   when read from a netCDF resource, or the name for the new CF variable
    #'   to be created.
    #' @param axes A `list` of [CFAxis] descendant instances that describe the
    #'   axes of the data object.
    #' @param values Optional. The values of the variable in an array.
    #' @param start Optional. Vector of indices where to start reading data
    #'   along the dimensions of the array on file. The vector must be `NA` to
    #'   read all data, otherwise it must have agree with the dimensions of the
    #'   array on file. Ignored when argument `var` is not an `NCVariable`
    #'   instance.
    #' @param count Optional. Vector of number of elements to read along each
    #'   dimension of the array on file. The vector must be `NA` to read to the
    #'   end of each dimension, otherwise its value must agree with the
    #'   corresponding `start` value and the dimension of the array on file.
    #'   Ignored when argument `var` is not an `NCVariable` instance.
    #' @param attributes Optional. A `data.frame` with the attributes of the
    #'   object. When argument `var` is an `NCVariable` instance and this
    #'   argument is an empty `data.frame` (default), arguments will be read
    #'   from the resource.
    #' @return An instance of this class.
    initialize = function(var, axes, values = values, start = NA, count = NA, attributes = data.frame()) {
      super$initialize(var, axes = axes, values = values, start = start, count = count, attributes = attributes)
      if (inherits(var, "NCVariable"))
        private$.nodata <- FALSE
    },

    #' @description  Prints a summary of the parametric formula term to the console.
    #' @param ... Arguments passed on to other functions. Of particular interest
    #' is `width = ` to indicate a maximum width of attribute columns.
    #' @return `self`, invisibly.
    print = function(...) {
      cat("<Parametric formula term>", self$name, "\n")

      longname <- self$attribute("long_name")
      if (!is.na(longname) && longname != self$name)
        cat("Long name:", longname, "\n")

      cat("\nAxes:\n")
      axes <- do.call(rbind, lapply(self$axes, function(a) a$brief()))
      axes <- lapply(axes, function(c) if (all(c == "")) NULL else c)
      if (length(axes)) {
        axes <- as.data.frame(axes[lengths(axes) > 0L])
        print(.slim.data.frame(axes, ...), right = FALSE, row.names = FALSE)
      } else cat(" (none)\n")

      self$print_attributes(...)
    },

    #' @description Subset the indices to read a smaller portion of the data
    #'   from the netCDF file. The passed indices should be named after the axes
    #'   that they refer to. There may be more indices than axes and they may be
    #'   in a different order than the axes of the term.
    #' @param original_axis_names Character vector of names of the axes prior to
    #'   a modifying operation in the owning data variable.
    #' @param new_axes List of `CFAxis` instances to use for the subsetting.
    #' @param start The indices to start reading data from the file, as an
    #'   integer vector at least as long as the number of axis for the term.
    #' @param count The number of values to read from the file, as an integer
    #'   vector at least as long as the number of axis for the term.
    #' @param aux Optional. List with the parameters for an auxiliary grid
    #'   transformation. Default is `NULL`.
    #' @param ZT_dim Optional. Dimensions of the non-grid axes when an auxiliary
    #'   grid transformation is specified.
    #' @return The new parametric term object.
    subset = function(original_axis_names, new_axes, start, count, aux = NULL, ZT_dim = NULL) {
      if (!length(private$.axes))
        return(CFVerticalParametricTerm$new(private$.NCvar, axes = list(), attributes = self$attributes))

      ord <- match(names(private$.axes), original_axis_names)
      new_axis_names <- sapply(new_axes, function(ax) ax$name)
      aux_axes <- which(match(new_axis_names, original_axis_names, nomatch = 0L) == 0L)
      if (is.null(aux) || all(!(ord %in% aux_axes))) {
        # Regular axis subsetting
        # Assuming we have an NCvar here
        CFVerticalParametricTerm$new(private$.NCvar, axes = new_axes[ord], start = start[ord],
                                     count = count[ord], attributes = self$attributes)
      } else {
        # Auxiliary grid warping, but only if this term includes the affected axes
        ZT_dim <- ZT_dim[ord[-(1L:2L)] - 2L] # Dimensions of all axes other than X and Y
        d <- private$read_chunk(start[ord], count[ord])
        dim(d) <- c(aux$X[2L] * aux$Y[2L], prod(ZT_dim))
        d <- d[aux$index, ]
        dim(d) <- c(aux$box, ZT_dim)
        CFVerticalParametricTerm$new(self$name, axes = new_axes[ord], values = d,
                                     attributes = self$attributes)
      }
    }
  ),
  active = list(
    #' @field has_data Logical flag that indicates of the instance has an
    #' associated data variable. If not, the instance will report `0` as its
    #' data.
    has_data = function(value) {
      if (missing(value))
        !private$.nodata
    },

    #' @field values (read-only) The values of the parametric term. Depending on
    #'   the definition of the term, this could be a large array or a simple
    #'   scalar. Specifically, if the term is defined but no data is included in
    #'   the netCDF resource, this method will return `0`, as per the CF
    #'   Metadata Conventions.
    values = function(value) {
      if (missing(value)) {
        if (private$.nodata) 0
        else private$read_data()
      }
    }
  )
)
