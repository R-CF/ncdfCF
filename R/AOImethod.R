#' Area of Interest
#'
#' @description This function constructs the area of interest of an analysis. It
#'   consists of an extent and a resolution of longitude and latitude, all in
#'   decimal degrees.
#'
#'   The AOI is used to define the subset of data to be extracted from a
#'   variable that has an auxiliary longitude-latitude grid (see the
#'   [CFAuxiliaryLongLat] class) at a specified resolution. The variable thus
#'   has a primary coordinate system where the horizontal components are not a
#'   geographic system of longitude and latitude coordinates.
#'
#' @details Following the CF Metadata Conventions, axis coordinates represent
#'   the center of grid cells. So when specifying `aoi(20, 30, -10, 10, 1, 2)`,
#'   the south-west grid cell coordinate is at `(20.5, -9)`. If the axes of the
#'   longitude-latitude grid have bounds, then the bounds will coincide with the
#'   AOI and the `CFVariable$subset()` method that uses the AOI will attach
#'   those bounds as attributes to the resulting array.
#'
#'   If no resolution is specified, it will be determined from the separation
#'   between adjacent grid cells in both longitude and latitude directions in
#'   the middle of the area of interest. If no extent is specified (meaning,
#'   none of the values; if some but not all values are specified an error will
#'   be thrown), then the whole extent of the variable is used, extended
#'   outwards by the bounds if they are set or half the resolution otherwise.
#'   Thus, to get the entire extent of the variable but in a longitude-latitude
#'   grid and with a resolution comparable to the resolution at the original
#'   Cartesian coordinate system of the variable, simply pass `aoi()` as an
#'   argument to [CFVariable$subset()][CFVariable]. Note that any missing
#'   arguments are calculated internally and stored in the returned object, but
#'   only after the call to `CFVariable$subset()`.
#'
#'   ## Caching
#'
#'   In data collections that are composed of multiple variables in a single
#'   netCDF resource, a single auxiliary longitude-latitude grid may be
#'   referenced by multiple variables, such as in [ROMS](https://www.myroms.org)
#'   data which may have dozens of variables using a shared grid. When
#'   subsetting with an AOI, the instance of this class is cached to improve
#'   performance. The successive calls to `CFVariable$subset()` should use the
#'   same object returned from a single call to this function for this caching
#'   to work properly.
#'
#' @param lonMin,lonMax,latMin,latMax The minimum and maximum values of the
#'   longitude and latitude of the AOI, in decimal degrees. The longitude values
#'   must agree with the range of the longitude in the variable to which this
#'   AOI will be applied, e.g. `[-180,180]` or `[0,360]`.
#' @param resX,resY The separation between adjacent grid cell, in the longitude
#'   and latitude directions respectively, in decimal degrees. The permitted
#'   values lie within the range `[0.01 ... 10]`. If `resY` is missing it will
#'   use the value of `resX`, yielding square grid cells.
#'
#' @return The return value of the function is an `R6` object which uses
#'   reference semantics. Making changes to the returned object will be visible
#'   in all copies made of the object.
#'
#' @export
#' @examples
#' (aoi <- aoi(20, 60, -40, -20, 0.5))
aoi <- function(lonMin, lonMax, latMin, latMax, resX, resY) {
  if (missing(resX)) resolution <- c(NULL, NULL)
  else {
    if (missing(resY)) resY <- resX
    resolution <- c(resX, resY)
    .aoi_check_resolution(resolution)
  }

  if (missing(lonMin) && missing(lonMin) && missing(lonMin) && missing(lonMin))
    AOI$new(NULL, NULL, NULL, NULL, resolution)
  else {
    .aoi_check_longitude(lonMin, lonMax)
    .aoi_check_latitude(latMin, latMax)
    AOI$new(lonMin, lonMax, latMin, latMax, resolution)
  }
}

#' The dimensions of the grid of an AOI
#'
#' This method returns the dimensions of the grid that would be created for the
#' AOI.
#'
#' @param x An instance of the `AOI` class.
#'
#' @return A vector of two values giving the longitude and latitude dimensions
#' of the grid that would be created for the AOI.
#' @export
#'
#' @examples
#' a <- aoi(30, 40, 10, 30, 0.1)
#' dim(a)
dim.AOI <- function(x) {
  x$dim
}

# Internal functions -----------------------------------------------------------

.aoi_check_resolution <- function(res) {
  if (res[1L] < 0.01 || res[1L] > 10 || res[2L] < 0.01 || res[2L] > 10)
    stop("Argument 'resolution' is outside of the permitted range of [0.01 ... 10]", call. = FALSE)
}

.aoi_check_longitude <- function(min, max) {
  if (min < -180 || (min < 0 && max > 180) || max > 360 || min >= max)
    stop("Longitude range is not valid", call. = FALSE)
}

.aoi_check_latitude <- function(min, max) {
  if (min < -90 || max > 90 || min >= max)
    stop("Latitude range is not valid", call. = FALSE)
}
