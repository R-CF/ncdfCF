#' Area of Interest class
#'
#' @description This class represents an are of interest for analysis.
#'
#' @details See [aoi()] for details.
#'
#' @docType class
#'
#' @export
AOI <- R6::R6Class("AOI",
  private = list(
    minLon  = NULL,
    maxLon  = NULL,
    minLat  = NULL,
    maxLat  = NULL,
    res     = NULL
  ),
  public = list(
    #' @field aux The [CFAuxiliaryLongLat] instance using this AOI.
    aux        = NULL,

    #' @description Creating an instance of the class
    #' @param lonMin,lonMax,latMin,latMax The minimum and maximum values of the
    #'   longitude and latitude of the AOI, in decimal degrees. The longitude values
    #'   must agree with the range of the longitude in the variable to which this
    #'   AOI will be applied, e.g. `[-180,180]` or `[0,360]`.
    #' @param resolution The separation between adjacent grid cell, in decimal
    #'   degrees.
    initialize = function(lonMin, lonMax, latMin, latMax, resolution) {
      private$minLon <- lonMin
      private$maxLon <- lonMax
      private$minLat <- latMin
      private$maxLat <- latMax
      private$res <- resolution
    },

    #' @description Summary of the area of interest
    print = function() {
      cat("<Area of Interest>\n")
      if (!is.null(self$aux))
        cat("Auxiliary long-lat object:", self$aux$fullname, "\n\n")

      if (!(is.null(private$minLon))) {
        cat(sprintf("Longitude:  [%5.3f ... %5.3f]\n", private$minLon, private$maxLon))
        cat(sprintf("Latitude:   [%5.3f ... %5.3f]\n", private$minLat, private$maxLat))
      } else {
        cat("Longitude:  (full extent)\n")
        cat("Latitude :  (full extent)\n")
      }

      if (!is.null(private$res))
        cat(sprintf("Resolution: [%5.3f]\n", private$res))
      else
        cat("Resolution: (from variable)\n")
    }
  ),
  active = list(
    #' @field lonMin Set or retrieve the lower longitude value.
    lonMin = function(value) {
      if (missing(value)) private$minLon
      else {
        .aoi_check_longitude(value, private$maxLon)
        private$minLon <- value
        if (inherits(self$aux, "CFAuxiliaryLongLat"))
          self$aux$clear_cache(full = FALSE)
      }
    },

    #' @field lonMax Set or retrieve the higher longitude value.
    lonMax = function(value) {
      if (missing(value)) private$maxLon
      else {
        .aoi_check_longitude(private$minLon, value)
        private$maxLon <- value
        if (inherits(self$aux, "CFAuxiliaryLongLat"))
          self$aux$clear_cache(full = FALSE)
      }
    },

    #' @field latMin Set or retrieve the lower latitude value.
    latMin = function(value) {
      if (missing(value)) private$minLat
      else {
        .aoi_check_latitude(value, private$maxLat)
        private$minLat <- value
        if (inherits(self$aux, "CFAuxiliaryLongLat"))
          self$aux$clear_cache(full = FALSE)
      }
    },

    #' @field latMax Set or retrieve the higher latitude value.
    latMax = function(value) {
      if (missing(value)) private$maxLat
      else {
        .aoi_check_latitude(private$minLat, value)
        private$maxLat <- value
        if (inherits(self$aux, "CFAuxiliaryLongLat"))
          self$aux$clear_cache(full = FALSE)
      }
    },

    #' @field extent Set of retrieve the four extremes of the AOI, a numeric
    #' vector in the order lomgitude minimum, maximum, latitude minimum, maximum.
    extent = function(value) {
      if (missing(value))
        c(private$minLon, private$maxLon, private$minLat, private$maxLat)
      else {
        .aoi_check_longitude(value[1L], value[2L])
        .aoi_check_latitude(value[3L], value[4L])
        private$minLon <- value[1L]
        private$maxLon <- value[2L]
        private$minLat <- value[3L]
        private$maxLat <- value[4L]
        if (inherits(self$aux, "CFAuxiliaryLongLat"))
          self$aux$clear_cache(full = FALSE)
      }
    },

    #' @field resolution Set or retrieve the resolution.
    resolution = function(value) {
      if (missing(value)) private$res
      else {
        .aoi_check_resolution(value)
        private$res <- value
        if (inherits(self$aux, "CFAuxiliaryLongLat"))
          self$aux$clear_cache(full = FALSE)
      }
    },

    #' @field dim (read-only) The dimensions of the grid of the AOI once generated.
    dim = function(value) {
      if (missing(value))
        as.integer(c((private$maxLon - private$minLon) / private$res,
                     (private$maxLat - private$minLat) / private$res))
      else
        warning("Cannot set the grid dimensions of an AOI: auto-generated\n", call. = FALSE)
    }
  )
)

# Public functions -------------------------------------------------------------

#' @title Area of Interest
#'
#' @description This function constructs the area of interest of an analysis. It
#'   consists of an extent of longitude and latitude and a resolution, all in
#'   decimal degrees.
#'
#'   The AOI is used to define the subset of data to be extracted from a
#'   variable that has an auxiliary longitude-latitude grid (see the
#'   [CFAuxiliaryLongLat] class) at a specified resolution. The variable thus
#'   has a primary coordinate system where the horizontal components are not a
#'   geographic system of longitude and latitude coordinates.
#'
#' @details Following the CF Metadata Conventions, axis coordinates represent
#'   the center of grid cells. So when specifying `aoi(20, 30, -10, 10, 1)`, the
#'   south-west coordinate is at `(20.5, -9.5)`. If the axes of the
#'   longitude-latitude grid have bounds, then the bounds will coincide with the
#'   AOI and the `CFVariable$subset()` method that uses the AOI will attach
#'   those bounds as attributes to the resulting array.
#'
#'   If no resolution is specified, it will be determined from the separation
#'   between adjacent grid cells in both longitude and latitude directions in
#'   the middle of the extent of interest. If no extent is specified (meaning,
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
#'   # Caching
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
#' @param resolution The separation between adjacent grid cell, in decimal
#'   degrees. The permitted values lie within the range `[0.01 ... 10]`.
#'
#' @returns The return value of the function is an [R6] object which uses
#'   reference semantics. Making changes to the returned object will be visible
#'   in all copies made of the object.
#'
#' @export
#' @name aoi_
#' @examples
#' aoi <- aoi(20, 60, -40, -20, 0.5)
#' aoi
#'
aoi <- function(lonMin, lonMax, latMin, latMax, resolution) {
  if (missing(resolution)) resolution <- NULL
  else .aoi_check_resolution(resolution)

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
#' @param x An instance of [AOI].
#'
#' @returns A vector of two values giving the longitude and latitude dimensions
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
  if (res < 0.01 || res > 10)
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
