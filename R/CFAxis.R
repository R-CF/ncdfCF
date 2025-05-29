#' CF axis object
#'
#' @description This class is a basic ancestor to all classes that represent CF
#'   axes. More useful classes use this class as ancestor.
#'
#'   This super-class does manage the "coordinates" of the axis, i.e. the values
#'   along the axis. This could be the values of the axis as stored on file, but
#'   it can also be the values from an auxiliary coordinate set, in the form of
#'   a [CFLabel] instance. The coordinate set to use in display, selection and
#'   processing is selectable through methods and fields in this class.
#'
#' @docType class
CFAxis <- R6::R6Class("CFAxis",
  inherit = CFObject,
  private = list(
    # A list of auxiliary coordinate instances, if any are defined for the axis.
    aux = list(),

    # The active coordinates set. Either an integer or a name. If there are no
    # auxiliary coordinates or when underlying axis coordinates should be used,
    # it should be 1L.
    active_coords = 1L,

    # Get the values of the active coordinate set. In most cases that is just
    # the values but it could be a label set. Most sub-classes override or
    # extend this method.
    get_coordinates = function() {
      if (private$active_coords == 1L)
        private$get_values()
      else
        private$aux[[private$active_coords - 1L]]$coordinates
    },

    # Copy a subset of all the auxiliary coordinates to another axis. Argument
    # ax will receive the auxiliary coordinates subsetted by argument rng.
    subset_coordinates = function(ax, rng) {
      if (length(private$aux)) {
        grp <- makeGroup(-1L, "/", "/")
        lapply(private$aux, function(x) ax$auxiliary <- x$subset(grp, rng))
      }
    }
  ),
  public = list(
    #' @field NCdim The [NCDimension] that stores the netCDF dimension details.
    NCdim = NULL,

    #' @field orientation A character "X", "Y", "Z" or "T" to indicate the
    #' orientation of the axis, or an empty string if not known or different.
    orientation = "",

    #' @field bounds The boundary values of this axis, if set.
    bounds = NULL,

    #' @description Create a new CF axis instance from a dimension and a
    #'   variable in a netCDF resource. This method is called upon opening a
    #'   netCDF resource by the `initialize()` method of a descendant class
    #'   suitable for the type of axis.
    #'
    #'   Creating a new axis is more easily done with the [makeAxis()] function.
    #' @param nc_var The [NCVariable] instance upon which this CF axis is based.
    #' @param nc_dim The [NCDimension] instance upon which this CF axis is
    #'   based.
    #' @param orientation The orientation of the axis: "X", "Y", "Z" "T", or ""
    #'   when not known or relevant.
    #' @return A basic `CFAxis` object.
    initialize = function(nc_var, nc_dim, orientation) {
      super$initialize(nc_var)
      self$NCdim <- nc_dim
      self$orientation <- orientation
      self$set_attribute("axis", "NC_CHAR", orientation)
      self$delete_attribute("_FillValue")

      nc_var$CF <- self
    },

    #' @description  Prints a summary of the axis to the console. This method is
    #'   typically called by the `print()` method of descendant classes.
    #' @param ... Arguments passed on to other functions. Of particular interest
    #' is `width = ` to indicate a maximum width of attribute columns.
    #' @return `self`, invisibly.
    print = function(...) {
      cat("<", self$friendlyClassName, "> [", self$dimid, "] ", self$name, "\n", sep = "")
      if (self$group$name != "/")
        cat("Group      :", self$group$fullname, "\n")

      longname <- self$attribute("long_name")
      if (!is.na(longname) && longname != self$name)
        cat("Long name  :", longname, "\n")

      cat("Length     :", self$NCdim$length)
      if (self$NCdim$unlim) cat(" (unlimited)\n") else cat("\n")
      cat("Axis       :", self$orientation, "\n")
      if (length(private$aux))
        cat("Label sets :", paste(self$coordinate_names, collapse = ", "), "\n")
    },

    #' @description Some details of the axis.
    #' @return A 1-row `data.frame` with some details of the axis.
    brief = function() {
      longname <- self$attribute("long_name")
      if (is.na(longname) || longname == self$name) longname <- ""
      unlim <- if (self$NCdim$unlim) "U" else ""
      units <- self$attribute("units")
      if (is.na(units)) units <- ""
      if (units == "1") units <- ""

      data.frame(axis = self$orientation, group = self$group$fullname,
                 name = self$name, long_name = longname, length = self$NCdim$length,
                 unlim = unlim, values = "", unit = units)
    },

    #' @description Very concise information on the axis. The information
    #'   returned by this function is very concise and most useful when combined
    #'   with similar information from other axes.
    #' @return Character string with very basic axis information.
    shard = function() {
      self$NCdim$shard()
    },

    #' @description Retrieve interesting details of the axis.
    #' @param with_groups Should group information be included? The safe option
    #' is `TRUE` (default) when the netCDF resource has groups because names may
    #' be duplicated among objects in different groups.
    #' @return A 1-row `data.frame` with details of the axis.
    peek = function(with_groups = TRUE) {
      out <- data.frame(class = class(self)[1L], id = self$id, axis = self$orientation)
      if (with_groups) out$group <- self$group$fullname
      out$name <- self$name
      out$long_name <- self$attribute("long_name")
      out$standard_name <- self$attribute("standard_name")
      out$units <- self$attribute("units")
      out$length <- self$NCdim$length
      out$unlimited <- self$NCdim$unlim
      out$values <- private$dimvalues_short()
      out$has_bounds <- inherits(self$bounds, "CFBounds")
      out$coordinate_sets <- length(private$aux) + 1L
      out
    },

    #' @description Return the `CFTime` instance that represents time. This
    #'   method is only useful for `CFAxisTime` instances having time
    #'   information. This stub is here to make the call to this method succeed
    #'   with no result for the other descendant classes.
    #' @return `NULL`
    time = function() {
      NULL
    },

    #' @description Tests if the axis passed to this method is identical to
    #'   `self`. This only tests for generic properties - class, length and name
    #'   - with further assessment done in sub-classes.
    #' @param axis The `CFAxis` instance to test.
    #' @return `TRUE` if the two axes are identical, `FALSE` if not.
    identical = function(axis) {
      all(class(self) == class(axis)) &&
      self$length == axis$length &&
      self$name == axis$name
    },

    #' @description Tests if the axis passed to this method can be appended to
    #'   `self`. This only tests for generic properties - class, mode of the
    #'   values and name - with further assessment done in sub-classes.
    #' @param axis The `CFAxis` descendant instance to test.
    #' @return `TRUE` if the passed axis can be appended to `self`, `FALSE` if
    #'   not.
    can_append = function(axis) {
      all(class(self) == class(axis)) &&
      mode(self$values) == mode(axis$values) &&
      self$name == axis$name
    },

    #' @description Return an axis spanning a smaller coordinate range. This
    #'   method is "virtual" in the sense that it does not do anything other
    #'   than return `NULL`. This stub is here to make the call to this method
    #'   succeed with no result for the  `CFAxis` descendants that do not
    #'   implement this method.
    #' @param group The group to create the new axis in.
    #' @param rng The range of indices whose values from this axis to include in
    #'   the returned axis. If the value of the argument is `NULL`, return the
    #'   entire axis.
    #' @return `NULL`
    subset = function(group, rng = NULL) {
      NULL
    },

    #' @description Find indices in the axis domain. Given a vector of
    #'   numerical, timestamp or categorical coordinates `x`, find their indices
    #'   in the coordinates of the axis.
    #'
    #'   This is a virtual method. For more detail, see the corresponding method
    #'   in descendant classes.
    #' @param x Vector of numeric, timestamp or categorial coordinates to find
    #'   axis indices for. The timestamps can be either character, POSIXct or
    #'   Date vectors. The type of the vector has to correspond to the type of
    #'   the axis values.
    #' @param method Single character value of "constant" or "linear".
    #' @param rightmost.closed Whether or not to include the upper limit.
    #' Default is `TRUE`.
    #' @return Numeric vector of the same length as `x`.
    indexOf = function(x, method = "constant", rightmost.closed = TRUE) {
      stop("`indexOf()` must be implemented by descendant CFAxis class.")
    },

    #' @description Write the axis to a netCDF file, including its attributes.
    #' @param nc The handle of the netCDF file opened for writing or a group in
    #'   the netCDF file. If `NULL`, write to the file or group where the axis
    #'   was read from (the file must have been opened for writing). If not
    #'   `NULL`, the handle to a netCDF file or a group therein.
    #' @return Self, invisibly.
    write = function(nc = NULL) {
      h <- if (inherits(nc, "NetCDF")) nc else self$group$handle

      if (self$length == 1L) {
        RNetCDF::var.def.nc(h, self$name, self$NCvar$vtype, NA)
      } else {
        self$NCdim$write(h)
        RNetCDF::var.def.nc(h, self$name, self$NCvar$vtype, self$name)
      }

      if (self$orientation %in% c("X", "Y", "Z", "T"))
        self$set_attribute("axis", "NC_CHAR", self$orientation)
      self$write_attributes(h, self$name)

      RNetCDF::var.put.nc(h, self$name, private$get_values())

      if (!is.null(self$bounds))
        self$bounds$write(h, self$name)

      lapply(private$aux, function(l) l$write(nc))

      invisible(self)
    }
  ),

  active = list(
    #' @field friendlyClassName (read-only) A nice description of the class.
    friendlyClassName = function(value) {
      if (missing(value))
        "Generic CF axis"
    },

    #' @field dimid (read-only) The netCDF dimension id of this axis.
    dimid = function(value) {
      if (missing(value)) {
        if (inherits(self$NCdim, "NCDimension")) self$NCdim$id
        else -1L
      }
    },

    #' @field length (read-only) The declared length of this axis.
    length = function(value) {
      if (missing(value))
        self$NCdim$length
    },

    #' @field values (read-only) Retrieve the raw values of the axis. In general
    #'   you should use the `coordinates` field rather than this one.
    values = function(value) {
      if (missing(value))
        private$get_values()
    },

    #' @field coordinates (read-only) Retrieve the coordinate values of the
    #' active coordinate set from the axis.
    coordinates = function(value) {
      if (missing(value))
        private$get_coordinates()
    },

    #' @field auxiliary Set or retrieve auxiliary coordinates for the axis. On
    #'   assignment, the value must be an instance of [CFLabel] or a [CFAxis]
    #'   descendant, which is added to the end of the list of coordinate sets.
    #'   On retrieval, the active `CFLabel` or `CFAxis` instance or `NULL` when
    #'   the active coordinate set is the primary axis coordinates.
    auxiliary = function(value) {
      if (missing(value)) {
        if (private$active_coords == 1L) NULL
        else private$aux[[private$active_coords - 1L]]
      } else {
        if ((inherits(value, "CFLabel") || inherits(value, "CFAxis")) &&
            value$length == self$length && !(value$name %in% names(private$aux))) {
          private$aux <- append(private$aux, value)
          names(private$aux) <- sapply(private$aux, function(l) l$name)
        }
      }
    },

    #' @field coordinate_names Retrieve the names of the coordinate sets
    #'   defined for the axis, as a character vector. The first element in the
    #'   vector is the name of the axis and it refers to the values of the
    #'   coordinates as stored in the netCDF file. Following elements refer to
    #'   auxiliary coordinates.
    coordinate_names = function(value) {
      if (missing(value))
        c(self$name, names(private$aux))
    },

    #' @field active_coordinates Set or retrieve the name of the coordinate set
    #'   to use with the axis for printing to the console as well as for
    #'   processing methods such as `subset()`.
    active_coordinates = function(value) {
      if (missing(value))
        self$coordinate_names[private$active_coords]
      else {
        ndx <- match(value, self$coordinate_names, nomatch = 0L)
        if (ndx > 0L)
          private$active_coords <- ndx
      }
    },

    #' @field unlimited (read-only) Logical to indicate if the axis has an
    #'   unlimited dimension.
    unlimited = function(value) {
      if (missing(value))
        self$NCdim$unlim
    }
  )
)

# Public S3 methods ------------------------------------------------------------

#' Axis length
#'
#' This method returns the lengths of the axes of a variable or axis.
#'
#' @param x The `CFVariable` or a descendant of `CFAxis`.
#' @return Vector of axis lengths.
#' @export
#' @examples
#' fn <- system.file("extdata", "ERA5land_Rwanda_20160101.nc", package = "ncdfCF")
#' ds <- open_ncdf(fn)
#' t2m <- ds[["t2m"]]
#' dim(t2m)
dim.CFAxis <- function(x) {
  x$length
}

#' @export
dimnames.CFAxis <- function(x) {
  x$dimnames
}

#' Compact display of an axis.
#' @param object A `CFAxis` instance or any descendant.
#' @param ... Ignored.
#' @export
str.CFAxis <- function(object, ...) {
  cat(paste0("<", object$friendlyClassName, "> ", object$name, "\n"))
}
