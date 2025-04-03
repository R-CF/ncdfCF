#' CF cell measure variable
#'
#' @description This class represents a CF cell measure variable, the object
#'   that indicates the area or volume of every grid cell in referencing data
#'   variables.
#'
#'   If a cell measure variable is external to the current file, an instance
#'   will still be created for it, but the user must link the external file to
#'   this instance before it can be used in analysis.
#'
#' @docType class
CFCellMeasure <- R6::R6Class("CFCellMeasure",
  private = list(
    # The actual data object for the class. This is an instance of CFVariable
    # but it is only present when the cell measure variable is internal to the
    # file or after an external file has been linked.
    var = NULL,

    # The [CFDataset] from the external variable, if linked.
    ext = NULL,

    # A list of the [CFVariable] instances linking to this instance.
    variables = list(),

    # A list of the axes that this cell measure variable uses, from the internal
    # variable or from an external file after it is linked.
    axes = list(),

    # This method checks that a CFVariable, v, has (a subset of) axes that are
    # compatible with this cell measure variable. If not, an error is thrown.
    compatible = function(v) {
      lapply(private$axes, function(ax) {
        tst <- v$axes[[ax$name]]
        if (is.null(tst))
          stop("Incompatible sets of axes between data variable and cell measure variable.", call. = FALSE)
        if (ax$length != tst$length || !all(abs(ax$values - tst$values) < CF$eps))
          stop("Axis coordinates not identical between data variable and cell measure variable.", call. = FALSE)
      })
      invisible(self)
    }
  ),
  public = list(
    #' @field group The [NCGroup] that this object is located in.
    group = NULL,

    #' @field measure The measure of this instance. Either "area" or "volume".
    measure = "",

    #' @field name The name of this instance, which must refer to a NC variable
    #' or an external variable.
    name = "",

    #' @description Create an instance of this class.
    #'
    #' @param grp The group that this CF cell measure variable lives in.
    #' @param measure The measure of this object. Must be either of "area" or
    #' "volume".
    #' @param name The name of the cell measure variable. May be omitted if
    #' argument `nc_var` is specified.
    #' @param nc_var The netCDF variable that defines this CF cell measure
    #'   object. `NULL` for an external variable.
    #' @param axes List of [CFAxis] instances that describe the dimensions of
    #'   the cell measure object. `NULL` for an external variable.
    #' @return An instance of this class.
    initialize = function(grp, measure, name = NULL, nc_var = NULL, axes = NULL) {
      self$group <- grp
      if (measure %in% c("area", "volume"))
        self$measure <- measure
      else
        stop("Invalid 'measure' for cell measure variable.", call. = FALSE)

      if (!(is.null(nc_var) || is.null(axes))) {
        private$var <- CFVariable$new(grp, nc_var, axes)
        self$name <- nc_var$name
        private$axes <- axes
      } else {
        if (is.null(name))
          stop("Name of external variable must be specified.", call. = FALSE)
        self$name <- name
      }
    },

    #' @description Print a summary of the cell measure variable to the console.
    #' @param ... Arguments passed on to other functions. Of particular interest
    #' is `width = ` to indicate a maximum width of attribute columns.
    print = function(...) {
      cat("<Cell measure>", self$name, "\n")
      if (self$group$name != "/")
        cat("Group    :", self$group$fullname, "\n")
      cat("Name     :", self$name, "\n")
      cat("Measure  :", self$measure, "\n")

      if (is.null(private$var))
        cat("Data     : external (not linked)\n")
      else {
        if (!is.null(private$ext))
          cat("Location :", private$ext$uri, "\n")
        else if (private$var$group$name != "/")
          cat("Location :", private$var$group$name, "\n")

        longname <- private$var$attribute("long_name")
        if (!is.na(longname) && longname != self$name)
          cat("Long name:", longname, "\n")

        if (length(private$variables))
          cat("Linked to:", names(private$variables), "\n")

        cat("\nAxes:\n")
        axes <- do.call(rbind, lapply(private$var$axes, function(a) a$brief()))
        axes <- lapply(axes, function(c) if (all(c == "")) NULL else c)
        if (all(axes$group == "/")) axes$group <- NULL
        axes <- as.data.frame(axes[lengths(axes) > 0L])
        print(.slim.data.frame(axes, ...), right = FALSE, row.names = FALSE)

        private$var$print_attributes(...)
      }
    },

    #' @description Retrieve the values of the cell measure variable.
    #' @return The values of the cell measure as a [CFArray] instance.
    data = function() {
      if (is.null(private$var)) NULL
      else private$var$data()
    },

    #' @description Register a [CFVariable] which is using this cell measure
    #'   variable. A check is performed on the compatibility between the data
    #'   variable and this cell measure variable.
    #' @param var A `CFVariable` instance to link to this instance.
    #' @return Self, invisibly.
    register = function(var) {
      private$compatible(var)
      private$variables <- append(private$variables, var)
      names(private$variables) <- c(names(private$variables), var$name)
    },

    #' @description Link the cell measure variable to an external netCDF
    #'   resource. The resource will be opened and the appropriate data variable
    #'   will be linked to this instance. If the axes or other properties of the
    #'   external resource are not compatible with this instance, an error will
    #'   be raised.
    #' @param resource The name of the netCDF resource to open, either a local
    #'   file name or a remote URI.
    #' @return Self, invisibly.
    link = function(resource) {
      ds <- open_ncdf(resource)
      if (inherits(ds, "CFDataset")) {
        var <- ds[[self$name]]
        if (inherits(var, "CFVariable")) {
          private$axes <- var$axes
          lapply(private$variables, private$compatible) # May throw an error
          private$ext <- ds
          private$var <- var
          return(invisible(self))
        }
      }
      stop("Invalid resource for cell measure variable.", call. = FALSE)
    }
  )
)
