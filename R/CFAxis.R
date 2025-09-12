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
  cloneable = FALSE,
  private = list(
    # A character "X", "Y", "Z" or "T" to indicate the
    # orientation of the axis, or an empty string if not known or different.
    .orient = "",

    # A flag to indicate if the axis is unlimited. Always FALSE for in-memory
    # objects upon creation but can be changed. For netCDF files, follow the
    # setting.
    .unlimited = FALSE,

    # The dimension id of the axis. This is set upon initialization.
    .dimid = -1L,

    # The CFBounds instance of this axis, if set, NULL otherwise.
    .bounds = NULL,

    # A list of auxiliary coordinate instances, if any are defined for the axis.
    .aux = list(),

    # The active coordinates set. Either an integer or a name. If there are no
    # auxiliary coordinates or when underlying axis coordinates should be used,
    # it should be 1L.
    .active_coords = 1L,

    # Get the names of the auxiliary coordinate instances.
    aux_names = function() {
      sapply(private$.aux, function(aux) aux$name)
    },

    # Get the values of the active coordinate set. In most cases that is just
    # the values but it could be a label set. Most sub-classes override or
    # extend this method.
    get_coordinates = function() {
      crds <- if (private$.active_coords == 1L) {
        private$.values
      } else
        private$.aux[[private$.active_coords - 1L]]$coordinates
      dim(crds) <- NULL
      crds
    },

    # Copy a subset of all the auxiliary coordinates to another axis. Argument
    # ax will receive the auxiliary coordinates subsetted by argument rng.
    subset_coordinates = function(ax, rng) {
      if (length(private$.aux)) {
        lapply(private$.aux, function(x) ax$auxiliary <- x$subset(x$name, rng))
      }
    },

    # When copying, the descendant class makes the new instance, then calls this
    # method to copy details common to all axes. Argument ax is the new copy of
    # this axis, rng is the range of coordinates the new copy uses, use all
    # coordinates when NULL. Returns the new axis.
    copy_properties_into = function(ax, rng = NULL) {
      ax$orientation <- private$.orient
      ax$unlimited <- private$.unlimited

      if (inherits(private$.bounds, "CFBounds"))
        ax$bounds <- private$.bounds$copy()

      private$subset_coordinates(ax, rng)
      ax$active_coordinates <- self$active_coordinates

      ax
    }
  ),
  public = list(
    #' @description Create a new CF axis instance from a dimension and a
    #'   variable in a netCDF resource. This method is called upon opening a
    #'   netCDF resource by the `initialize()` method of a descendant class
    #'   suitable for the type of axis.
    #'
    #'   Creating a new axis is more easily done with the [makeAxis()] function.
    #' @param var The name of the axis when creating a new axis. When reading an
    #'   axis from file, the [NCVariable] object that describes this instance.
    #' @param values Optional. The values of the axis in a vector. Ignored when
    #'   argument `var` is a `NCVariable` object.
    #' @param start Optional. Integer index where to start reading axis data
    #'   from file. The index may be `NA` to start reading data from the start.
    #' @param count Optional. Number of elements to read from file. This may be
    #'   `NA` to read to the end of the data.
    #' @param orientation Optional. The orientation of the axis: "X", "Y", "Z"
    #'   "T", or "" (default) when not known or relevant.
    #' @param attributes Optional. A `data.frame` with the attributes of the
    #'   axis. When an empty `data.frame` (default) and argument `var` is an
    #'   `NCVariable` instance, attributes of the axis will be taken from the
    #'   netCDF resource.
    #' @return A basic `CFAxis` object.
    initialize = function(var, values, start = 1L, count = NA, orientation = "", attributes = data.frame()) {
      orientation <- orientation[1L]
      if (!(orientation %in% c("X", "Y", "Z", "T", "")))
        stop("Invalid orientation for the axis.", call. = FALSE) # nocov

      super$initialize(var, values = values, start = start, count = count, attributes = attributes)
      if (inherits(var, "NCVariable")) {
        private$.unlimited <- if (private$.NCvar$ndims) private$.NCvar$dimension(1L)$unlim else FALSE
        private$.dimid <- private$.NCvar$dimids[1L]
      }

      private$.orient <- orientation
      if (orientation != "")
        self$set_attribute("axis", "NC_CHAR", orientation)

      if (missing(values) || is.null(values))
        private$read_data()

      self$delete_attribute("_FillValue")
    },

    #' @description  Prints a summary of the axis to the console. This method is
    #'   typically called by the `print()` method of descendant classes.
    #' @param ... Arguments passed on to other functions. Of particular interest
    #' is `width = ` to indicate a maximum width of attribute columns.
    #' @return `self`, invisibly.
    print = function(...) {
      cat("<", self$friendlyClassName, "> [", private$.id, "] ", private$.name, "\n", sep = "")

      fullname <- self$fullname
      if (fullname != self$name)
        cat("Path name:", fullname, "\n")

      longname <- self$attribute("long_name")
      if (!is.na(longname) && longname != self$name)
        cat("Long name  :", longname, "\n")

      cat("Length     :", self$length)
      if (self$unlimited) cat(" (unlimited)\n") else cat("\n")
      cat("Axis       :", private$.orient, "\n")
      if (length(private$.aux))
        cat("Label sets :", paste(self$coordinate_names, collapse = ", "), "\n")
    },

    #' @description Some details of the axis.
    #' @return A 1-row `data.frame` with some details of the axis.
    brief = function() {
      longname <- self$attribute("long_name")
      if (is.na(longname) || longname == self$name) longname <- ""
      len <- if (self$unlimited) paste0(self$length, "-U") else self$length
      units <- self$attribute("units")
      if (is.na(units)) units <- ""
      else if (units == "1") units <- ""

      data.frame(axis = private$.orient,  name = self$fullname, long_name = longname,
                 length = len, values = "", unit = units)
    },

    #' @description Very concise information on the axis. The information
    #'   returned by this function is very concise and most useful when combined
    #'   with similar information from other axes.
    #' @return Character string with very basic axis information.
    shard = function() {
      unlim <- if (self$unlimited) "-U" else ""
      crds <- self$coordinate_range
      paste0("[", self$name, " (", self$length, unlim, "): ", crds[1L], " ... ", crds[2L], "]")
    },

    #' @description Retrieve interesting details of the axis.
    #' @return A 1-row `data.frame` with details of the axis.
    peek = function() {
      out <- data.frame(class = class(self)[1L], id = self$id, axis = private$.orient)
      out$name <- self$name
      out$long_name <- self$attribute("long_name")
      out$standard_name <- self$attribute("standard_name")
      out$units <- self$attribute("units")
      out$length <- self$length
      out$unlimited <- self$unlimited
      out$values <- private$dimvalues_short()
      out$has_bounds <- inherits(private$.bounds, "CFBounds")
      out$coordinate_sets <- length(private$.aux) + 1L
      out
    },

    #' @description Detach the axis from its underlying netCDF resource,
    #'   including any dependent CF objects.
    #' @return Self, invisibly.
    detach = function() {
      if (!is.null(private$.bounds))
        private$.bounds$detach()
      lapply(private$.aux, function(x) x$detach())
      super$detach()
      invisible(self)
    },

    #' @description Copy the parametric terms of a vertical axis. This method is
    #'   only useful for `CFAxisVertical` instances having a parametric
    #'   formulation. This stub is here to make the call to this method succeed
    #'   with no result for the other descendant classes.
    #' @param from A CFAxisVertical instance that will receive references to the
    #' parametric terms.
    #' @param original_axes List of `CFAxis` instances from the CF object that
    #'   these parametric terms are copied from.
    #' @param new_axes List of `CFAxis` instances to use with the formula term
    #'   objects.
    #' @return `NULL`
    copy_terms = function(from, original_axes, new_axes) {
      NULL
    },

    #' @description Configure the function terms of a parametric vertical axis.
    #'   This method is only useful for `CFAxisVertical` instances having a
    #'   parametric formulation. This stub is here to make the call to this
    #'   method succeed with no result for the other descendant classes.
    #' @param axes List of `CFAxis` instances.
    #' @return `NULL`
    configure_terms = function(axes) {
      NULL
    },

    #' @description Tests if the axis passed to this method is identical to
    #'   `self`. This only tests for generic properties - class, length and name - with
    #'   further assessment done in sub-classes.
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
      mode(private$.values) == mode(axis$values) &&
      self$name == axis$name
    },

    #' @description Create a copy of this axis. This method is "virtual" in the
    #'   sense that it does not do anything other than return `NULL`. This stub
    #'   is here to make the call to this method succeed with no result for the
    #'   `CFAxis` descendants that do not implement this method.
    #' @param name The name for the new axis. If an empty string is passed, will
    #'   use the name of this axis.
    #' @return `NULL`
    copy = function(name = "") {
      NULL
    },

    #' @description Create a copy of this axis but using the supplied values.
    #'   This method is "virtual" in the sense that it does not do anything
    #'   other than return `NULL`. This stub is here to make the call to this
    #'   method succeed with no result for the `CFAxis` descendants that do not
    #'   implement this method.
    #' @param name The name for the new axis. If an empty string is passed, will
    #'   use the name of this axis.
    #' @param values The values to the used with the copy of this axis.
    #' @return `NULL`
    copy_with_values = function(name = "", values) {
      NULL
    },

    #' @description Return an axis spanning a smaller coordinate range. This
    #'   method is "virtual" in the sense that it does not do anything other
    #'   than return `self`. This stub is here to make the call to this method
    #'   succeed with no result for the  `CFAxis` descendants that do not
    #'   implement this method.
    #' @param name The name for the new axis if the `rng` argument is provided.
    #' @param rng The range of indices whose values from this axis to include in
    #'   the returned axis. If the value of the argument is `NULL`, return a
    #'   copy of the axis.
    #' @return `NULL`
    subset = function(name = "", rng = NULL) {
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
    #'   was read from (the file must have been opened for writing).
    #' @return Self, invisibly.
    write = function(nc = NULL) {
      h <- if (inherits(nc, "NetCDF")) nc else private$.NCvar$handle

      private$.id <- if (self$length == 1L && !private$.unlimited)
        RNetCDF::var.def.nc(h, self$name, private$.data_type, NA)
      else {
        # Write the dimension for the axis. Error will be thrown when trying to
        # write a dimension that's already defined, such as when a dimension is
        # shared between multiple objects. In that case, get the id.
        did <- try(if (private$.unlimited)
          RNetCDF::dim.def.nc(h, self$name, unlim = TRUE)
          else
            RNetCDF::dim.def.nc(h, self$name, self$length), silent = TRUE)
        if (inherits(did, "try-error"))
          did <- RNetCDF::dim.inq.nc(h, self$name)$id

        RNetCDF::var.def.nc(h, self$name, private$.data_type, did)
      }

      if (private$.orient %in% c("X", "Y", "Z", "T"))
        self$set_attribute("axis", "NC_CHAR", private$.orient)
      self$write_attributes(h, self$name)

      RNetCDF::var.put.nc(h, self$name, private$.values)

      if (!is.null(private$.bounds))
        private$.bounds$write(h, self$name)

      lapply(private$.aux, function(l) l$write(nc, did))

      invisible(self)
    }
  ),
  active = list(
    #' @field friendlyClassName (read-only) A nice description of the class.
    friendlyClassName = function(value) {
      if (missing(value))
        "Generic CF axis"
    },

    #' @field dimid The netCDF dimension id of this axis. Setting this value to
    #' anything other than the correct value will lead to disaster.
    dimid = function(value) {
      if (missing(value))
        private$.dimid
      else
        private$.dimid <- value
    },

    #' @field length (read-only) The declared length of this axis.
    length = function(value) {
      if (missing(value))
        length(private$.values)
    },

    #' @field orientation Set or retrieve the orientation of the axis, a single
    #'   character with a value of "X", "Y", "Z", "T". Setting the orientation
    #'   of the axis should only be done when the current orientation is
    #'   unknown. Setting a wrong value may give unexpected errors in processing
    #'   of data variables.
    orientation = function(value) {
      if (missing(value))
        private$.orient
      else {
        if (!is.character(value) || length(value) != 1L || !(value %in% c("X", "Y", "Z", "T", "")))
          stop("Axis orientation must be a single character 'X', 'Y', 'Z' or 'T'.", call. = FALSE) # nocov
        private$.orient <- value
      }
    },

    #' @field values (read-only) Retrieve the raw values of the axis. In general
    #'   you should use the `coordinates` field rather than this one.
    values = function(value) {
      if (missing(value))
        private$read_data()
    },

    #' @field coordinates (read-only) Retrieve the coordinate values of the
    #' active coordinate set from the axis.
    coordinates = function(value) {
      if (missing(value))
        private$get_coordinates()
      # FIXME: Must be able to add coordinates too, f.i. in an unlimited axis.
    },

    #' @field bounds Set or retrieve the bounds of this axis as a [CFBounds]
    #'   object. When setting the bounds, the bounds values must agree with the
    #'   coordinates of this axis.
    bounds = function(value) {
      if (missing(value))
        private$.bounds
      else if (inherits(value, "CFBounds")) {
        # FIXME: Check the bounds values
        private$.bounds <- value
        self$set_attribute("bounds", "NC_CHAR", value$name)
      } else if (!is.null(value))
        stop("Must assign a `CFBounds` object to the axis.", call. = FALSE) # nocov
    },

    #' @field auxiliary Set or retrieve auxiliary coordinates for the axis. On
    #'   assignment, the value must be an instance of [CFLabel] or a [CFAxis]
    #'   descendant, which is added to the end of the list of coordinate sets.
    #'   On retrieval, the active `CFLabel` or `CFAxis` instance or `NULL` when
    #'   the active coordinate set is the primary axis coordinates.
    auxiliary = function(value) {
      if (missing(value)) {
        if (private$.active_coords == 1L) NULL
        else private$.aux[[private$.active_coords - 1L]]
      } else {
        if ((inherits(value, "CFLabel") || inherits(value, "CFAxis")) &&
            value$length == self$length && !(value$name %in% private$aux_names())) {
          private$.aux <- append(private$.aux, setNames(list(value), value$name))
        }
      }
    },

    #' @field coordinate_names (read-only) Retrieve the names of the coordinate
    #'   sets defined for the axis, as a character vector. The first element in
    #'   the vector is the name of the axis and it refers to the values of the
    #'   coordinates as stored in the netCDF file. Following elements refer to
    #'   auxiliary coordinates.
    coordinate_names = function(value) {
      if (missing(value))
        c(self$name, names(private$.aux))
    },

    #' @field coordinate_range (read-only) Retrieve the range of the coordinates
    #' of the axis as a vector of two values. The mode of the result depends on
    #' the sub-type of the axis.
    coordinate_range = function(value) {
      if (missing(value)) {
        crds <- self$coordinates
        if (is.null(crds)) NULL
        else c(crds[1L], crds[length(crds)])
      }
    },

    #' @field active_coordinates Set or retrieve the name of the coordinate set
    #'   to use with the axis for printing to the console as well as for
    #'   processing methods such as `subset()`.
    active_coordinates = function(value) {
      if (missing(value))
        self$coordinate_names[private$.active_coords]
      else {
        ndx <- match(value, self$coordinate_names, nomatch = 0L)
        if (ndx > 0L)
          private$.active_coords <- ndx
      }
    },

    #' @field unlimited Logical to indicate if the axis is unlimited.
    unlimited = function(value) {
      if (missing(value))
        private$.unlimited
      else if (is.logical(value[1L])) {
        private$.unlimited <- value[1L]
      } else
        stop("Must use single logical value to set or unset unlimited axis.", call. = FALSE) # nocov
    },

    #' @field time (read-only) Retrieve the `CFTime` object associated with the
    #' axis. Always returns `NULL` but `CFAxisTime` overrides this field.
    time = function(value) {
      if (missing(value))
        NULL
    },

    #' @field is_parametric (read-only) Logical flag that indicates if the axis
    #'   has dimensional coordinates. Always `FALSE` for all axes except for
    #'   [CFAxisVertical] which overrides this method.
    is_parametric = function(value) {
      if (missing(value))
        FALSE
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
  cat(paste0("<", object$friendlyClassName, "> ", object$shard(), "\n"))
}
