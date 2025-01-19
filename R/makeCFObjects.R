#' Create a group in memory to hold CF objects
#'
#' With this function a group is created in memory, i.e. not associated with a
#' netCDF resource on file. This can be used to prepare new CF objects before
#' writing them to file. Extracting data from a `CFVariable` into a [CFData]
#' instance will also create a memory group.
#'
#' @param id The id of the group.
#' @param name The name of the group.
#' @param fullname The full path and name of the group.
#' @param title The title attribute of the group.
#' @param history The history attribute of the group.
#'
#' @return A `MemoryGroup` instance.
#' @export
makeMemoryGroup <- function(id, name, fullname, title, history) {
  MemoryGroup$new(id, name, fullname, NULL, title, history)
}

#' Create a longitude axis
#'
#' With this method you can create a longitude axis to use with new [CFData]
#' instances.
#'
#' @param id Id of the axis.
#' @param name Name of the axis.
#' @param group Group to place the axis in.
#' @param length Length of the dimension of the axis.
#' @param values The dimension values.
#' @param bounds The bounds of the dimension values, or `NULL` if not available.
#' @param units The name of the axis units.
#'
#' @return A `CFAxisLongitude` instance.
#' @export
makeLongitudeAxis <- function(id, name, group, length, values, bounds = NULL, units = "") {
  var <- NCVariable$new(id, name, group, "NC_DOUBLE", 1L, NULL)
  var$attributes <- data.frame(id = 0L, name = "units", type = "NC_CHAR", length = nchar(units), value = units)
  dim <- NCDimension$new(-1L, name, length, FALSE)
  axis <- CFAxisLongitude$new(group, var, dim, values)
  if (!is.null(bounds)) {
    var <- NCVariable$new(-1L, paste0(name, "_bnds"), group, "NC_DOUBLE", 2L, NULL)
    axis$bounds <- CFBounds$new(var, bounds)
  }
  axis
}

#' Create a latitude axis
#'
#' With this method you can create a longitude axis to use with new [CFData]
#' instances.
#'
#' @param id Id of the axis.
#' @param name Name of the axis.
#' @param group Group to place the axis in.
#' @param length Length of the dimension of the axis.
#' @param values The dimension values.
#' @param bounds The bounds of the dimension values, or `NULL` if not available.
#' @param units The name of the axis units.
#'
#' @return A `CFAxisLatitude` instance.
#' @export
makeLatitudeAxis <- function(id, name, group, length, values, bounds, units) {
  var <- NCVariable$new(id, name, group, "NC_DOUBLE", 1L, NULL)
  var$attributes <- data.frame(id = 0, name = "units", type = "NC_CHAR", length = nchar(units), value = units)
  dim <- NCDimension$new(-1L, name, length, FALSE)
  axis <- CFAxisLatitude$new(group, var, dim, values)
  if (!is.null(bounds)) {
    var <- NCVariable$new(-1L, paste0(name, "_bnds"), group, "NC_DOUBLE", 2L, NULL)
    axis$bounds <- CFBounds$new(var, bounds)
  }
  axis
}

#' Create a time axis
#'
#' With this method you can create a time axis to use with new [CFData]
#' instances.
#'
#' @param id Id of the axis.
#' @param name Name of the axis.
#' @param group Group to place the axis in.
#' @param values A `CFTime` instance with dimension values and bounds set.
#'
#' @return A `CFAxisTime` instance.
#' @export
makeTimeAxis <- function(id, name, group, values) {
  var <- NCVariable$new(id, name, group, "NC_DOUBLE", 1L, NULL)
  if (length(values) == 1L)
    CFAxisScalar$new(group, var, "T", values)
  else {
    dim <- NCDimension$new(-1L, name, length(values), FALSE)
    CFAxisTime$new(group, var, dim, values)
  }
}
