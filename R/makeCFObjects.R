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

#' Create a longitude axis
#'
#' With this method you can create a longitude axis to use with new [CFArray]
#' instances.
#'
#' @param id Id of the axis.
#' @param name Name of the axis.
#' @param group Group to place the axis in.
#' @param values The coordinate values.
#' @param bounds The bounds of the coordinate values, or `NULL` if not
#'   available.
#'
#' @return A `CFAxisLongitude` or `CFAxisScalar` instance.
#' @export
makeLongitudeAxis <- function(id, name, group, values, bounds = NULL) {
  if (!.is_valid_name(name))
    stop("Name for axis is not valid", call. = FALSE)
  # FIXME: Check domain

  var <- NCVariable$new(id, name, group, "NC_DOUBLE", 1L, NULL)
  length <- length(values)
  axis <- if (length == 1L)
    CFAxisScalar$new(var, "X", values)
  else {
    dim <- NCDimension$new(-1L, name, length, FALSE)
    CFAxisLongitude$new(var, dim, values)
  }

  axis$set_attribute("units", "NC_CHAR", "degrees_east")
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
#' @param id Id of the axis.
#' @param name Name of the axis.
#' @param group Group to place the axis in.
#' @param values The coordinate values.
#' @param bounds The bounds of the coordinate values, or `NULL` if not available.
#'
#' @return A `CFAxisLatitude` or `CFAxisScalar` instance.
#' @export
makeLatitudeAxis <- function(id, name, group, values, bounds) {
  if (!.is_valid_name(name))
    stop("Name for axis is not valid", call. = FALSE)
  # FIXME: Check domain

  var <- NCVariable$new(id, name, group, "NC_DOUBLE", 1L, NULL)
  length <- length(values)
  axis <- if (length == 1L)
    CFAxisScalar$new(var, "X", values)
  else {
    dim <- NCDimension$new(-1L, name, length, FALSE)
    CFAxisLatitude$new(var, dim, values)
  }

  axis$set_attribute("units", "NC_CHAR", "degrees_north")
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
#' @param id Id of the axis.
#' @param name Name of the axis.
#' @param group Group to place the axis in.
#' @param values A `CFTime` instance with time values and optionally bounds set.
#'
#' @return A `CFAxisTime` instance.
#' @export
makeTimeAxis <- function(id, name, group, values) {
  if (!.is_valid_name(name))
    stop("Name for axis is not valid", call. = FALSE)

  var <- NCVariable$new(id, name, group, "NC_DOUBLE", 1L, NULL)
  axis <- if (length(values) == 1L)
    CFAxisScalar$new(var, "T", values)
  else {
    dim <- NCDimension$new(-1L, name, length(values), FALSE)
    CFAxisTime$new(var, dim, values)
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
