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
    .nodata = TRUE,

    # Start and count indices for reading the data from file
    .start = NA,
    .count = NA
  ),
  public = list(
    #' @description Create an instance of this class.
    #' @param var The NC variable that describes this data object.
    #' @param axes A `list` of [CFAxis] descendant instances that describe the
    #'   axes of the data object.
    #' @return An instance of this class.
    initialize = function(var, axes) {
      if (inherits(var, "NCVariable")) {
        super$initialize(var, axes)
        private$.nodata <- FALSE
      }
    },

    #' @description  Prints a summary of the parametric formula term to the console.
    #' @param ... Arguments passed on to other functions. Of particular interest
    #' is `width = ` to indicate a maximum width of attribute columns.
    #' @return `self`, invisibly.
    print = function(...) {
      cat("<Parametric formula term>", self$name, "\n")
      if (self$group$name != "/")
        cat("Group    :", self$group$name, "\n")

      longname <- self$attribute("long_name")
      if (!is.na(longname) && longname != self$name)
        cat("Long name:", longname, "\n")

      cat("\nAxes:\n")
      axes <- do.call(rbind, lapply(self$axes, function(a) a$brief()))
      axes <- lapply(axes, function(c) if (all(c == "")) NULL else c)
      if (length(axes)) {
        if (all(axes$group == "/")) axes$group <- NULL
        axes <- as.data.frame(axes[lengths(axes) > 0L])
        print(.slim.data.frame(axes, ...), right = FALSE, row.names = FALSE)
      } else cat(" (none)\n")

      self$print_attributes(...)
    },

    #' @description Subset the indices to read a smaller portion of the data
    #'   from the netCDF file. The passed indices should be named after the axes
    #'   that they refer to. There may be more indices than axes and they may be
    #'   in a different order than the axes of the term.
    #' @param start The indices to start reading data from the file, as an
    #'   integer vector at least as long as the number of axis for the term.
    #' @param count The number of values to read from the file, as an integer vector at
    #'   least as long as the number of axis for the term.
    #' @return Self, invisibly.
    subset = function(start, count) {

      invisible(self)
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
        else self$NCvar$get_data(start = private$.start_count$start,
                                 count = private$.start_count$count)
      }
    }
  )
)
