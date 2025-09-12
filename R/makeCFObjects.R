#' Create an axis
#'
#' With this method you can create an axis to use with new [CFVariable]
#' instances. Depending on the `orientation` argument and the type of the
#' `values` argument an instance of a class descending from [CFAxis] will be
#' returned.
#'
#' There are several restrictions on the combination of `orientation` and
#' `values` arguments. Longitude, latitude and depth axes (`orientation` of "X",
#' "Y" or "Z") must have numeric `values`. For a time axis (`orientation` of
#' "T") the `values` argument must be an instance of `CFTime` or
#' `CFClimatology`.
#'
#' @param name Name of the axis.
#' @param orientation The orientation of the axis. Must be one of "X", "Y", "Z",
#'   or "T" for longitude, latitude, height or depth, and time axes,
#'   respectively. For any other axis, indicate an empty string ""
#' @param values The coordinate values. In the case of an axis with `orientation
#'   = "T"` this must be a `CFTime` or `CFClimatology` instance.
#' @param bounds The boundary values of the coordinates, or `NULL` if not
#'   available.
#' @param attributes `data.frame` with the attributes of the axis to create.
#'   Depending on which axis is created one or more attributes may be added or
#'   amended.
#'
#' @seealso [makeLongitudeAxis()], [makeLatitudeAxis()], [makeTimeAxis()],
#'   [makeDiscreteAxis()]
#' @return An instance of a class descending from [CFAxis].
#' @export
makeAxis <- function(name, orientation, values, bounds = NULL, attributes = NULL) {
  if (orientation == "X") makeLongitudeAxis(name, values, bounds, attributes)
  else if (orientation == "Y") makeLatitudeAxis(name, values, bounds, attributes)
  else if (orientation == "Z") makeVerticalAxis(name, values, bounds, attributes)
  else if (orientation == "T") makeTimeAxis(name, values, attributes)
  else if (orientation == "") {
    if (is.numeric(values)) {
      # Check if we have a potential longitude or latitude axis
      if (tolower(name) %in% c("longitude", "lon") && .check_longitude_domain(values))
        makeLongitudeAxis(name, values, bounds, attributes)
      else if (tolower(name) %in% c("latitude", "lat") && .check_latitude_domain(values))
        makeLatitudeAxis(name, values, bounds, attributes)
      else {
        # Make a generic axis
        axis <- CFAxisNumeric$new(name, values = values, attributes = attributes)

        if (!is.null(bounds)) {
          nm <- paste0(name, "_bnds")
          axis$bounds <- CFBounds$new(nm, bounds)
          axis$set_attribute("bounds", "NC_CHAR", nm)
        }
        axis
      }
    } else if (is.character(values)) {
      axis <- CFAxisCharacter$new(name, values = values, attributes = attributes)
    }
  } else stop("Bad `orientation` value for axis creation.", call. = FALSE)
}

#' Create a longitude axis
#'
#' With this method you can create a longitude axis to use with new [CFVariable]
#' instances.
#'
#' @param name Name of the axis.
#' @param values The coordinate values.
#' @param bounds The bounds of the coordinate values, or `NULL` if not
#'   available.
#' @param attributes `data.frame` with the attributes of the axis to create.
#'   Attributes "standard_name", "units", "actual_range" and "axis" will be set
#'   or updated.
#'
#' @return A [CFAxisLongitude] instance.
#' @export
makeLongitudeAxis <- function(name, values, bounds = NULL, attributes = NULL) {
  if (!.is_valid_name(name))
    stop("Name for axis is not valid", call. = FALSE)

  axis <- CFAxisLongitude$new(name, values = values, attributes = attributes)

  if (is.na(axis$attribute("standard_name"))) {
    axis$set_attribute("standard_name", "NC_CHAR", "longitude")
    axis$set_attribute("units", "NC_CHAR", "degrees_east")
  }
  if (!is.null(bounds)) {
    nm <- paste0(name, "_bnds")
    axis$bounds <- CFBounds$new(nm, values = bounds)
    axis$set_attribute("bounds", "NC_CHAR", nm)
  }
  axis
}

#' Create a latitude axis
#'
#' With this method you can create a latitude axis to use with new [CFVariable]
#' instances.
#'
#' @param name Name of the axis.
#' @param values The coordinate values.
#' @param bounds The bounds of the coordinate values, or `NULL` if not available.
#' @param attributes `data.frame` with the attributes of the axis to create.
#'   Attributes "standard_name", "units", "actual_range" and "axis" will be set
#'   or updated.
#'
#' @return A [CFAxisLatitude] instance.
#' @export
makeLatitudeAxis <- function(name, values, bounds, attributes = NULL) {
  if (!.is_valid_name(name))
    stop("Name for axis is not valid", call. = FALSE)

  axis <- CFAxisLatitude$new(name, values = values, attributes = attributes)

  if (is.na(axis$attribute("standard_name"))) {
    axis$set_attribute("standard_name", "NC_CHAR", "latitude")
    axis$set_attribute("units", "NC_CHAR", "degrees_north")
  }

  if (!is.null(bounds)) {
    nm <- paste0(name, "_bnds")
    axis$bounds <- CFBounds$new(nm, values = bounds)
    axis$set_attribute("bounds", "NC_CHAR", nm)
  }
  axis
}

#' Create a vertical axis
#'
#' With this method you can create a vertical axis to use with new [CFVariable]
#' instances. Note that you should set the "positive" attribute after creating
#' the axis to indicate if values are increasing going upwards (positive = "up")
#' or downwards (positive = "down").
#'
#' @param name Name of the axis.
#' @param values The coordinate values.
#' @param bounds The bounds of the coordinate values, or `NULL` if not
#'   available.
#' @param attributes `data.frame` with the attributes of the axis to create.
#'   Attributes "actual_range" and "axis" will be set or updated.
#'
#' @return A [CFAxisVertical] instance.
#' @export
makeVerticalAxis <- function(name, values, bounds, attributes = NULL) {
  if (!.is_valid_name(name))
    stop("Name for axis is not valid", call. = FALSE)
  # FIXME: Check domain

  axis <- CFAxisVertical$new(name, values = values, attributes = attributes)

  if (!is.null(bounds)) {
    nm <- paste0(name, "_bnds")
    axis$bounds <- CFBounds$new(nm, values = bounds)
    axis$set_attribute("bounds", "NC_CHAR", nm)
  }
  axis
}

#' Create a time axis
#'
#' With this method you can create a time axis to use with new [CFVariable]
#' instances.
#'
#' @param name Name of the axis.
#' @param values A `CFTime` or `CFClimatology` instance with time values and
#'   optionally bounds set.
#' @param attributes `data.frame` with the attributes of the axis to create.
#'   Attributes "standard_name", "units", "calendar", "actual_range" and "axis"
#'   will be set or updated.
#'
#' @return A [CFAxisTime] instance.
#' @export
makeTimeAxis <- function(name, values, attributes = NULL) {
  if (!.is_valid_name(name))
    stop("Name for axis is not valid", call. = FALSE)

  axis <- CFAxisTime$new(name, values = values, attributes = attributes)

  if (is.na(axis$attribute("standard_name")))
    axis$set_attribute("standard_name", "NC_CHAR", "time")
  axis$set_attribute("units", "NC_CHAR", values$cal$definition)
  axis$set_attribute("calendar", "NC_CHAR", values$cal$name)

  if (!is.null(values$bounds)) {
    nm <- paste0(name, "_bnds")
    axis$bounds <- CFBounds$new(nm, values = values$get_bounds())
    axis$set_attribute("bounds", "NC_CHAR", nm)
  }
  axis
}

#' Create a discrete axis
#'
#' With this method you can create a discrete axis to use with new [CFVariable]
#' instances.
#'
#' @param name Name of the axis.
#' @param length The length of the axis.
#'
#' @return A [CFAxisDiscrete] instance. The values will be a sequence of size
#'   `length`.
#' @export
makeDiscreteAxis <- function(name, length) {
  if (!.is_valid_name(name))
    stop("Name for axis is not valid", call. = FALSE)

  length <- as.integer(length)
  CFAxisDiscrete$new(name, start = 1L, count = length)
}

#' Create a character axis
#'
#' With this method you can create a character axis to use with new [CFVariable]
#' instances.
#'
#' @param name Name of the axis.
#' @param values The character coordinate values of the axis.
#' @param attributes `data.frame` with the attributes of the axis to create.
#'
#' @return A [CFAxisCharacter] instance.
#' @export
makeCharacterAxis <- function(name, values, attributes = data.frame()) {
  CFAxisCharacter$new(name, values = values, attributes = attributes)
}

#' Create a `CFVariable` instance from an R object
#'
#' With this function you can convert an R object into a [CFVariable]. This can
#' be an array, matrix or vector of type `logical`, `integer`, `numeric` or
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
#' The axes of the `CFVariable` are oriented as in the R object. Note that this
#' is different from standard practice in the netCDF community and the
#' portability of saved datasets is thus limited. You can improve this situation
#' by setting the orientation of the axes and by adding attributes.
#'
#' After creation of the `CFVariable`, it is recommended to set other
#' properties, such as attributes or a coordinate reference system.
#'
#' @param name The name of the `CFVariable` to create.
#' @param values The data of this object. This can be an array, matrix or vector
#'   of type `logical`, `integer`, `numeric` or `character.`
#' @return An instance of class [CFVariable].
#' @export
as_CF <- function(name, values) {
  # Check the properties of "values" and create axes from dimnames
  if (is.logical(values)) values <- as.integer(values)
  dt <- typeof(values)
  if (!(dt %in% c("integer", "double", "character")))
    stop("Argument 'values' is of an unsupported type", call. = FALSE) # nocov

  # Helper function - "nm" is name for the axis, "len" is length of the dimension, "vals" is coordinate values
  .makeArrayAxis <- function(nm, len, vals) {
    if (is.null(vals))              # values has no dimnames so make discrete axis
      return(CFAxisDiscrete$new(nm, count = len))

    crds <- suppressWarnings(as.numeric(vals))
    if (any(is.na(crds))) {         # Not numeric so time or character
      t <- try(CFtime::CFTime$new("days since 1970-01-01T00:00:00", "standard", vals), silent = TRUE)
      if (inherits(t, "try-error")) # Not time
        CFAxisCharacter$new(nm, values = vals)
      else CFAxisTime$new(nm, t)
    } else {
      # Check if we have a potential longitude or latitude axis
      if (tolower(nm) %in% c("longitude", "lon") && .check_longitude_domain(crds))
        CFAxisLongitude$new(nm, values = crds)
      else if (tolower(nm) %in% c("latitude", "lat") && .check_latitude_domain(vals))
        CFAxisLatitude$new(nm, values = crds)
      else
        CFAxisNumeric$new(nm, values = crds)
    }
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

  CFVariable$new(name, values = values, axes = axes)
}
