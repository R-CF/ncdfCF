#' Create a group in memory to hold CF objects
#'
#' With this function a group is created in memory, i.e. not associated with a
#' netCDF resource on file. This can be used to prepare new CF objects before
#' writing them to file. Several operations on a [CFVariable] or [CFArray]
#' instance will create new virtual groups.
#'
#' @param name The name of the group, default "/".
#' @param parent Optionally, a parent group to which the new group will be added
#' as a child.
#' @param resource Optionally, the netCDF resource that holds the data that the
#' objects in this group use.
#'
#' @return A `NCGroup` instance.
#' @export
makeGroup <- function(name = "/", parent = NULL, resource = NULL) {
  if (name != "/" && !.is_valid_name(name))
    stop("Name for group is not valid", call. = FALSE)
  NCGroup$new(CF$newGroupId(), name, parent, resource)
}

#' Create an axis
#'
#' With this method you can create an axis to use with new [CFArray] instances.
#' Depending on the `orientation` argument and the type of the `values` argument
#' an instance of a class descending from [CFAxis] will be returned.
#'
#' There are several restrictions on the combination of `orientation` and
#' `values` arguments. Longitude, latitude and depth axes (`orientation` of "X",
#' "Y" or "Z") must have numeric `values`. For a time axis (`orientation` of
#' "T") the `values` argument must be an instance of `CFTime` or
#' `CFClimatology`.
#'
#' @param name Name of the axis.
#' @param group Group to place the axis in.
#' @param orientation The orientation of the axis. Must be one of "X", "Y", "Z",
#'   or "T" for longitude, latitude, height or depth, and time axes,
#'   respectively. For any other axis, indicate an empty string ""
#' @param values The coordinate values. In the case of an axis with `orientation
#'   = "T"` this must be a `CFTime` instance.
#' @param bounds The boundary values of the coordinates, or `NULL` if not
#'   available.
#' @param attributes `data.frame` with the attributes of the axis to create.
#' Depending on which axis is created one or more attributes may be added or
#' amended.
#'
#' @seealso [makeLongitudeAxis()], [makeLatitudeAxis()], [makeTimeAxis()],
#'   [makeDiscreteAxis()]
#' @return An instance of a class descending from [CFAxis].
#' @export
makeAxis <- function(name, group, orientation, values, bounds = NULL, attributes = NULL) {
  if (orientation == "X") makeLongitudeAxis(name, group, values, bounds, attributes)
  else if (orientation == "Y") makeLatitudeAxis(name, group, values, bounds, attributes)
  else if (orientation == "Z") makeVerticalAxis(name, group, values, bounds, attributes)
  else if (orientation == "T") makeTimeAxis(name, group, values, attributes)
  else if (orientation == "") {
    if (is.numeric(values)) {
      # Check if we have a potential longitude or latitude axis
      if (tolower(name) %in% c("longitude", "lon") && .check_longitude_domain(values))
        makeLongitudeAxis(name, group, values, bounds, attributes)
      else if (tolower(name) %in% c("latitude", "lat") && .check_latitude_domain(values))
        makeLatitudeAxis(name, group, values, bounds, attributes)
      else {
        # Make a generic axis
        var <- NCVariable$new(CF$newVarId(), name, group, "NC_DOUBLE", 1L, NULL)
        dim <- NCDimension$new(CF$newDimId(), name, length(values), FALSE, group)
        axis <- CFAxisNumeric$new(var, dim, "", values)
        axis$attributes <- attributes

        axis$set_attribute("actual_range", "NC_DOUBLE", range(values))
        if (!is.null(bounds)) {
          nm <- paste0(name, "_bnds")
          axis$bounds <- CFBounds$new(nm, bounds)
          axis$set_attribute("bounds", "NC_CHAR", nm)
        }
        axis
      }
    } else if (is.character(values)) {
      var <- NCVariable$new(CF$newVarId(), name, group, "NC_STRING", 1L, NULL)
      dim <- NCDimension$new(CF$newDimId(), name, length(values), FALSE, group)
      axis <- CFAxisCharacter$new(var, dim, "", values)
      axis$attributes <- attributes
    }
  } else stop("Bad `orientation` value for axis creation.", call. = FALSE)
}

#' Create a longitude axis
#'
#' With this method you can create a longitude axis to use with new [CFArray]
#' instances.
#'
#' @param name Name of the axis.
#' @param group Group to place the axis in.
#' @param values The coordinate values.
#' @param bounds The bounds of the coordinate values, or `NULL` if not
#'   available.
#' @param attributes `data.frame` with the attributes of the axis to create.
#'   Attributes "standard_name", "units", "actual_range" and "axis" will be set
#'   or updated.
#'
#' @return A [CFAxisLongitude] instance.
#' @export
makeLongitudeAxis <- function(name, group, values, bounds = NULL, attributes = NULL) {
  if (!.is_valid_name(name))
    stop("Name for axis is not valid", call. = FALSE)
  # FIXME: Check domain
  # FIXME: Arguments should not be NULL

  var <- NCVariable$new(CF$newVarId(), name, group, "NC_DOUBLE", 1L, NULL)
  dim <- NCDimension$new(CF$newDimId(), name, length(values), FALSE, group)
  axis <- CFAxisLongitude$new(var, dim, values)
  axis$attributes <- attributes

  if (is.na(axis$attribute("standard_name"))) {
    axis$set_attribute("standard_name", "NC_CHAR", "longitude")
    axis$set_attribute("units", "NC_CHAR", "degrees_east")
  }
  axis$set_attribute("actual_range", "NC_DOUBLE", range(values))
  axis$set_attribute("axis", "NC_CHAR", "X")
  if (!is.null(bounds)) {
    nm <- paste0(name, "_bnds")
    axis$bounds <- CFBounds$new(nm, bounds)
    axis$set_attribute("bounds", "NC_CHAR", nm)
  }
  axis
}

#' Create a latitude axis
#'
#' With this method you can create a latitude axis to use with new [CFArray]
#' instances.
#'
#' @param name Name of the axis.
#' @param group Group to place the axis in.
#' @param values The coordinate values.
#' @param bounds The bounds of the coordinate values, or `NULL` if not available.
#' @param attributes `data.frame` with the attributes of the axis to create.
#'   Attributes "standard_name", "units", "actual_range" and "axis" will be set
#'   or updated.
#'
#' @return A [CFAxisLatitude] instance.
#' @export
makeLatitudeAxis <- function(name, group, values, bounds, attributes = NULL) {
  if (!.is_valid_name(name))
    stop("Name for axis is not valid", call. = FALSE)
  # FIXME: Check domain

  var <- NCVariable$new(CF$newVarId(), name, group, "NC_DOUBLE", 1L, NULL)
  dim <- NCDimension$new(CF$newDimId(), name, length(values), FALSE, group)
  axis <- CFAxisLatitude$new(var, dim, values)
  axis$attributes <- attributes

  if (is.na(axis$attribute("standard_name"))) {
    axis$set_attribute("standard_name", "NC_CHAR", "latitude")
    axis$set_attribute("units", "NC_CHAR", "degrees_north")
  }
  axis$set_attribute("axis", "NC_CHAR", "Y")
  axis$set_attribute("actual_range", "NC_DOUBLE", range(values))
  if (!is.null(bounds)) {
    nm <- paste0(name, "_bnds")
    axis$bounds <- CFBounds$new(nm, bounds)
    axis$set_attribute("bounds", "NC_CHAR", nm)
  }
  axis
}

#' Create a vertical axis
#'
#' With this method you can create a vertical axis to use with new [CFArray]
#' instances. Note that you should set the "positive" attribute after creating
#' the axis to indicate if values are increasing going upwards (positive = "up")
#' or downwards (positive = "down").
#'
#' @param name Name of the axis.
#' @param group Group to place the axis in.
#' @param values The coordinate values.
#' @param bounds The bounds of the coordinate values, or `NULL` if not
#'   available.
#' @param attributes `data.frame` with the attributes of the axis to create.
#'   Attributes "actual_range" and "axis" will be set or updated.
#'
#' @return A [CFAxisVertical] instance.
#' @export
makeVerticalAxis <- function(name, group, values, bounds, attributes = NULL) {
  if (!.is_valid_name(name))
    stop("Name for axis is not valid", call. = FALSE)
  # FIXME: Check domain

  var <- NCVariable$new(CF$newVarId(), name, group, "NC_DOUBLE", 1L, NULL)
  dim <- NCDimension$new(CF$newDimId(), name, length(values), FALSE, group)
  axis <- CFAxisVertical$new(var, dim, values, "depth")
  axis$attributes <- attributes

  axis$set_attribute("axis", "NC_CHAR", "Z")
  axis$set_attribute("actual_range", "NC_DOUBLE", range(values))
  if (!is.null(bounds)) {
    nm <- paste0(name, "_bnds")
    axis$bounds <- CFBounds$new(nm, bounds)
    axis$set_attribute("bounds", "NC_CHAR", nm)
  }
  axis
}

#' Create a time axis
#'
#' With this method you can create a time axis to use with new [CFArray]
#' instances.
#'
#' @param name Name of the axis.
#' @param group Group to place the axis in.
#' @param values A `CFTime` instance with time values and optionally bounds set.
#' @param attributes `data.frame` with the attributes of the axis to create.
#'   Attributes "standard_name", "units", "calendar", actual_range" and "axis"
#'   will be set or updated.
#'
#' @return A [CFAxisTime] instance.
#' @export
makeTimeAxis <- function(name, group, values, attributes = NULL) {
  if (!.is_valid_name(name))
    stop("Name for axis is not valid", call. = FALSE)

  var <- NCVariable$new(CF$newVarId(), name, group, "NC_DOUBLE", 1L, NULL)
  dim <- NCDimension$new(CF$newDimId(), name, length(values), FALSE, group)
  axis <- CFAxisTime$new(var, dim, values)
  axis$attributes <- attributes

  if (is.na(axis$attribute("standard_name")))
    axis$set_attribute("standard_name", "NC_CHAR", "time")
  axis$set_attribute("units", "NC_CHAR", values$cal$definition)
  axis$set_attribute("calendar", "NC_CHAR", values$cal$name)
  axis$set_attribute("axis", "NC_CHAR", "T")
  axis$set_attribute("actual_range", "NC_DOUBLE", range(values$offsets))
  if (!is.null(values$bounds)) {
    nm <- paste0(name, "_bnds")
    axis$bounds <- CFBounds$new(nm, values$get_bounds())
    axis$set_attribute("bounds", "NC_CHAR", nm)
  }
  axis
}

#' Create a discrete axis
#'
#' With this method you can create a discrete axis to use with new [CFArray]
#' instances.
#'
#' @param name Name of the axis.
#' @param group Group to place the axis in.
#' @param length The length of the axis.
#' @param attributes `data.frame` with the attributes of the axis to create.
#'   Attribute "actual_range" will be set or updated.
#'
#' @return A [CFAxisDiscrete] instance. The values will be a sequence of size
#'   `length`.
#' @export
makeDiscreteAxis <- function(name, group, length, attributes = NULL) {
  if (!.is_valid_name(name))
    stop("Name for axis is not valid", call. = FALSE)

  length <- as.integer(length)
  var <- NCVariable$new(CF$newVarId(), name, group, "NC_INT", 1L, NULL)
  dim <- NCDimension$new(CF$newDimId(), name, length, FALSE, group)
  axis <- CFAxisDiscrete$new(var, dim, "")
  axis$attributes <- attributes

  axis$set_attribute("actual_range", "NC_INT", c(1L, length))
  axis
}

#' Create a character axis
#'
#' With this method you can create a character axis to use with new [CFArray]
#' instances.
#'
#' @param name Name of the axis.
#' @param group Group to place the axis in.
#' @param values The character coordinate values of the axis.
#' @param attributes `data.frame` with the attributes of the axis to create.
#'
#' @return A [CFAxisCharacter] instance.
#' @export
makeCharacterAxis <- function(name, group, values, attributes = NULL) {
  if (!.is_valid_name(name))
    stop("Name for axis is not valid", call. = FALSE)

  var <- NCVariable$new(CF$newVarId(), name, group, "NC_STRING", 1L, NULL) # FIXME: Make this NC_CHAR for netcdf3 support -> extra dim!
  dim <- NCDimension$new(CF$newDimId(), name, length(values), FALSE, group)
  axis <- CFAxisCharacter$new(var, dim, "", values)
  axis$attributes <- attributes

  axis
}

#' Create a CFArray instance from an R object
#'
#' With this function you can convert an R object into a [CFArray]. This can be
#' an array, matrix or vector of type `logical`, `integer`, `numeric` or
#' `character.`
#'
#' Dimnames on the R object will be converted to instances of a [CFAxis]
#' descendant class, depending on their values. If the dimnames along a
#' dimension of the R object can be converted to `numeric`, then it will be an
#' instance of [CFAxisNumeric]. If the dimnames are `character`, a first attempt
#' is made to create a [CFAxisTime] (i.e. the dimnames have to represent
#' timestamps), failing that a [CFAxisCharacter] will be created. If no dimnames
#' are set, an instance of [CFAxisDiscrete] is generated.
#'
#' The axes of the `CFArray` are oriented as in the R object. Note that this is
#' different from standard practice in the netCDF community and the portability
#' of saved datasets is thus limited. You can improve this situation by setting
#' the orientation of the axes and by adding attributes.
#'
#' After creation of the `CFArray`, it is recommended to set other properties,
#' such as attributes or a coordinate reference system.
#'
#' @param name The name of the CFArray to create.
#' @param values The data of this object. This can be an array, matrix or vector
#'   of type `logical`, `integer`, `numeric` or `character.`
#' @return An instance of class [CFArray].
#' @export
as_CFArray <- function(name, values) {
  # Check the properties of "values" and create axes from dimnames
  if (is.logical(values)) values <- as.integer(values)
  dt <- typeof(values)
  if (!(dt %in% c("integer", "double", "character")))
    stop("Argument 'values' is of an unsupported type", call. = FALSE)

  # Output data type
  dt <- switch(dt,
               "integer"   = "NC_INT",
               "double"    = "NC_DOUBLE",
               "character" = "NC_STRING")

  # Group for all the objects to be created
  grp <- makeGroup()

  # Helper function - "nm" is name for the axis, "len" is length of the dimension, "vals" is coordinate values
  .makeArrayAxis <- function(nm, len, vals) {
    if (is.null(vals))              # vals has no names so make discrete axis
      return(makeDiscreteAxis(nm, grp, len))

    crds <- suppressWarnings(as.numeric(vals))
    if (any(is.na(crds))) {         # Not numeric so time or character
      t <- try(CFtime::CFTime$new("days since 1970-01-01T00:00:00", "standard", vals), silent = TRUE)
      if (inherits(t, "try-error")) # Not time
        makeCharacterAxis(nm, grp, vals)
      else makeTimeAxis(nm, grp, t)
    } else makeAxis(nm, grp, "", crds)
  }

  dims <- dim(values)
  if (is.null(dims))  # values is a vector
    axes <- list(axis1 = .makeArrayAxis("axis_1", length(values), names(values)))
  else {
    dn <- dimnames(values)
    names <- names(dn)
    if (is.null(names))
      names <- sapply(seq_along(dims), function(x) paste0("axis_", x))
    else
      names <- sapply(seq_along(dims), function(x) if (nzchar(names[x])) names[x] else paste0("axis_", x))

    axes <- lapply(seq_along(dims), function(x) .makeArrayAxis(names[x], dims[x], dn[[x]]))
  }
  axes <- axes[lengths(axes) > 0L]
  if (length(axes))
    names(axes) <- sapply(axes, function(ax) ax$name)

  CFArray$new(name, grp, values, dt, axes, NULL, NULL)
}

