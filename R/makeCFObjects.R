#' Create a group in memory to hold CF objects
#'
#' With this function a group is created in memory, i.e. not associated with a
#' netCDF resource on file. This can be used to prepare new CF objects before
#' writing them to file. Extracting data from a `CFVariable` into a [CFArray]
#' instance will also create a virtual group.
#'
#' @param id The id of the group, default -1L.
#' @param name The name of the group, default "/".
#' @param fullname The full path and name of the group, default "/".
#' @param parent Optionally, a parent group to which the new group will be added
#' as a child.
#'
#' @return A `NCGroup` instance.
#' @export
makeGroup <- function(id = -1L, name = "/", fullname = "/", parent = NULL) {
  if (name != "/" && !.is_valid_name(name))
    stop("Name for group is not valid", call. = FALSE)
  NCGroup$new(id, name, fullname, parent, NULL)
}

#' Create an axis
#'
#' With this method you can create an axis to use with new [CFArray] instances.
#' Depending on the `orientation` argument and the type of the `values` argument
#' an instance of a class descending from [CFAxis] will be returned.
#'
#' There are several restrictions on the combination of `orientation` and
#' `values` arguments. Longitude and latitude axes (`orientation` of "X" or "Y")
#' must have numeric `values`. For a time axis (`orientation` of "T") the
#' `values` argument must be an instance of `CFTime` or `CFClimatology`.
#'
#' @param name Name of the axis.
#' @param group Group to place the axis in.
#' @param orientation The orientation of the axis. Must be one of "X", "Y", "Z",
#'   or "T" for longitude, latitude, height or depth, and time axes,
#'   respectively. For any other axis, indicate an empty string ""
#' @param values The coordinate values.
#' @param bounds The bounds of the coordinate values, or `NULL` if not
#'   available.
#'
#' @seealso [makeLongitudeAxis()], [makeLatitudeAxis()], [makeTimeAxis()], [makeDiscreteAxis()]
#' @return An instance of a class descending from [CFAxis].
#' @export
makeAxis <- function(name, group, orientation, values, bounds = NULL) {
  if (orientation == "X") makeLongitudeAxis(name, group, values, bounds)
  else if (orientation == "Y") makeLatitudeAxis(name, group, values, bounds)
  else if (orientation == "T") makeTimeAxis(name, group, values)
  else if (orientation == "") {
    if (is.numeric(values)) {
      var <- NCVariable$new(-1L, name, group, "NC_DOUBLE", 1L, NULL)
      dim <- NCDimension$new(-1L, name, length(values), FALSE)
      axis <- CFAxisNumeric$new(var, dim, "", values)

      axis$set_attribute("actual_range", "NC_DOUBLE", range(values))
      if (!is.null(bounds)) {
        nm <- paste0(name, "_bnds")
        var <- NCVariable$new(-1L, nm, group, "NC_DOUBLE", 2L, NULL)
        dim <- NCDimension$new(-1L, "nv", 2L, FALSE)
        axis$bounds <- CFBounds$new(var, dim, bounds)
        axis$set_attribute("bounds", "NC_CHAR", nm)
      }
      axis
    } else if (is.character(values)) {
      var <- NCVariable$new(-1L, name, group, "NC_STRING", 1L, NULL)
      dim <- NCDimension$new(-1L, name, length(values), FALSE)
      CFAxisCharacter$new(var, dim, "", values)
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
#'
#' @return A [CFAxisLongitude] instance.
#' @export
makeLongitudeAxis <- function(name, group, values, bounds = NULL) {
  if (!.is_valid_name(name))
    stop("Name for axis is not valid", call. = FALSE)
  # FIXME: Check domain
  # FIXME: Arguments should not be NULL

  var <- NCVariable$new(-1L, name, group, "NC_DOUBLE", 1L, NULL)
  dim <- NCDimension$new(-1L, name, length(values), FALSE)
  axis <- CFAxisLongitude$new(var, dim, values)

  axis$set_attribute("units", "NC_CHAR", "degrees_east")
  axis$set_attribute("actual_range", "NC_DOUBLE", range(values))
  axis$set_attribute("axis", "NC_CHAR", "X")
  if (!is.null(bounds)) {
    nm <- paste0(name, "_bnds")
    var <- NCVariable$new(-1L, nm, group, "NC_DOUBLE", 2L, NULL)
    dim <- NCDimension$new(-1L, "nv", 2L, FALSE)
    axis$bounds <- CFBounds$new(var, dim, bounds)
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
#'
#' @return A [CFAxisLatitude] instance.
#' @export
makeLatitudeAxis <- function(name, group, values, bounds) {
  if (!.is_valid_name(name))
    stop("Name for axis is not valid", call. = FALSE)
  # FIXME: Check domain

  var <- NCVariable$new(-1L, name, group, "NC_DOUBLE", 1L, NULL)
  dim <- NCDimension$new(-1L, name, length(values), FALSE)
  axis <- CFAxisLatitude$new(var, dim, values)

  axis$set_attribute("units", "NC_CHAR", "degrees_north")
  axis$set_attribute("axis", "NC_CHAR", "Y")
  axis$set_attribute("actual_range", "NC_DOUBLE", range(values))
  if (!is.null(bounds)) {
    nm <- paste0(name, "_bnds")
    var <- NCVariable$new(-1L, nm, group, "NC_DOUBLE", 2L, NULL)
    dim <- NCDimension$new(-1L, "nv", 2L, FALSE)
    axis$bounds <- CFBounds$new(var, dim, bounds)
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
#'
#' @return A [CFAxisTime] instance.
#' @export
makeTimeAxis <- function(name, group, values) {
  if (!.is_valid_name(name))
    stop("Name for axis is not valid", call. = FALSE)

  var <- NCVariable$new(-1L, name, group, "NC_DOUBLE", 1L, NULL)
  dim <- NCDimension$new(-1L, name, length(values), FALSE)
  axis <- CFAxisTime$new(var, dim, values)

  axis$set_attribute("units", "NC_CHAR", values$cal$definition)
  axis$set_attribute("calendar", "NC_CHAR", values$cal$name)
  axis$set_attribute("axis", "NC_CHAR", "T")
  axis$set_attribute("actual_range", "NC_DOUBLE", range(values$offsets))
  if (!isFALSE(values$bounds)) {
    nm <- paste0(name, "_bnds")
    var <- NCVariable$new(-1L, nm, group, "NC_DOUBLE", 2L, NULL)
    dim <- NCDimension$new(-1L, "nv", 2L, FALSE)
    axis$bounds <- CFBounds$new(var, dim, values$get_bounds())
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
#'
#' @return A [CFAxisDiscrete] instance. The values will be a sequence of size
#' `length`.
#' @export
makeDiscreteAxis <- function(name, group, length) {
  if (!.is_valid_name(name))
    stop("Name for axis is not valid", call. = FALSE)

  length <- as.integer(length)
  var <- NCVariable$new(-1L, name, group, "NC_INT", 1L, NULL)
  dim <- NCDimension$new(-1L, name, length, FALSE)
  axis <- CFAxisDiscrete$new(var, dim, "")

  axis$set_attribute("actual_range", "NC_int", c(1L, length))
  axis
}
