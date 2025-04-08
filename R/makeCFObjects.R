#' Create a group in memory to hold CF objects
#'
#' With this function a group is created in memory, i.e. not associated with a
#' netCDF resource on file. This can be used to prepare new CF objects before
#' writing them to file. Extracting data from a `CFVariable` into a [CFArray]
#' instance will also create a virtual group.
#'
#' @param id The id of the group.
#' @param name The name of the group.
#' @param fullname The full path and name of the group.
#' @param parent Optionally, a parent group to which the new group will be added
#' as a child.
#'
#' @return A `NCGroup` instance.
#' @export
makeGroup <- function(id, name, fullname, parent = NULL) {
  if (name != "/" && !.is_valid_name(name))
    stop("Name for group is not valid", call. = FALSE)
  NCGroup$new(id, name, fullname, parent, NULL)
}

#' Create a longitude axis
#'
#' With this method you can create a longitude axis to use with new [CFArray]
#' instances.
#'
#' @param id Id of the axis.
#' @param name Name of the axis.
#' @param group Group to place the axis in.
#' @param length Length of the axis.
#' @param values The coordinate values.
#' @param bounds The bounds of the coordinate values, or `NULL` if not available.
#' @param units A character string with the axis units.
#'
#' @return A `CFAxisLongitude` instance.
#' @export
makeLongitudeAxis <- function(id, name, group, length, values, bounds = NULL, units = "") {
  if (!.is_valid_name(name))
    stop("Name for axis is not valid", call. = FALSE)
  # FIXME: Check domain

  var <- NCVariable$new(id, name, group, "NC_DOUBLE", 1L, NULL)
  dim <- NCDimension$new(-1L, name, length, FALSE)
  axis <- CFAxisLongitude$new(group, var, dim, values)
  if (nzchar(units))
    axis$set_attribute("units", "NC_CHAR", units)
  if (!is.null(bounds)) {
    nm <- paste0(name, "_bnds")
    var <- NCVariable$new(-1L, nm, group, "NC_DOUBLE", 2L, NULL)
    dim <- NCDimension$new(-1L, "nv", 2L, FALSE)
    axis$bounds <- CFBounds$new(var, dim, bounds)
    axis$set_attribute("bounds", "NC_CHAR", nm)
  }
  axis$set_attribute("axis", "NC_CHAR", "X")
  axis
}

#' Create a latitude axis
#'
#' With this method you can create a latitude axis to use with new [CFArray]
#' instances.
#'
#' @param id Id of the axis.
#' @param name Name of the axis.
#' @param group Group to place the axis in.
#' @param length Length of the the axis.
#' @param values The coordinate values.
#' @param bounds The bounds of the coordinate values, or `NULL` if not available.
#' @param units A character string with the axis units.
#'
#' @return A `CFAxisLatitude` instance.
#' @export
makeLatitudeAxis <- function(id, name, group, length, values, bounds, units) {
  if (!.is_valid_name(name))
    stop("Name for axis is not valid", call. = FALSE)
  # FIXME: Check domain

  var <- NCVariable$new(id, name, group, "NC_DOUBLE", 1L, NULL)
  dim <- NCDimension$new(-1L, name, length, FALSE)
  axis <- CFAxisLatitude$new(group, var, dim, values)
  if (nzchar(units))
    axis$set_attribute("units", "NC_CHAR", units)
  if (!is.null(bounds)) {
    nm <- paste0(name, "_bnds")
    var <- NCVariable$new(-1L, nm, group, "NC_DOUBLE", 2L, NULL)
    dim <- NCDimension$new(-1L, "nv", 2L, FALSE)
    axis$bounds <- CFBounds$new(var, dim, bounds)
    axis$set_attribute("bounds", "NC_CHAR", nm)
  }
  axis$set_attribute("axis", "NC_CHAR", "Y")
  axis
}

#' Create a time axis
#'
#' With this method you can create a time axis to use with new [CFArray]
#' instances.
#'
#' @param id Id of the axis.
#' @param name Name of the axis.
#' @param group Group to place the axis in.
#' @param values A `CFTime` instance with time values and bounds set.
#'
#' @return A `CFAxisTime` instance.
#' @export
makeTimeAxis <- function(id, name, group, values) {
  if (!.is_valid_name(name))
    stop("Name for axis is not valid", call. = FALSE)

  var <- NCVariable$new(id, name, group, "NC_DOUBLE", 1L, NULL)
  if (length(values) == 1L)
    axis <- CFAxisScalar$new(group, var, "T", values)
  else {
    dim <- NCDimension$new(-1L, name, length(values), FALSE)
    axis <- CFAxisTime$new(group, var, dim, values)
  }
  axis$set_attribute("units", "NC_CHAR", values$cal$definition)
  axis$set_attribute("calendar", "NC_CHAR", values$cal$name)
  axis$set_attribute("axis", "NC_CHAR", "T")
  if (!isFALSE(values$bounds)) {
    nm <- paste0(name, "_bnds")
    var <- NCVariable$new(-1L, nm, group, "NC_DOUBLE", 2L, NULL)
    dim <- NCDimension$new(-1L, "nv", 2L, FALSE)
    axis$bounds <- CFBounds$new(var, dim, values$get_bounds())
    axis$set_attribute("bounds", "NC_CHAR", nm)
  }
  axis
}
