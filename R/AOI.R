#' Region of Interest class
#'
#' @description This class represents an area of interest for analysis. See
#'   [aoi()] for details.
#'
#' @docType class
AOI <- R6::R6Class("AOI",
  private = list(
    minLon  = NULL,
    maxLon  = NULL,
    minLat  = NULL,
    maxLat  = NULL,
    res     = NULL  # vector of X and Y resolutions
  ),
  public = list(
    #' @field aux The [CFAuxiliaryLongLat] instance using this AOI.
    aux        = NULL,

    #' @description Creating an instance of the class.
    #' @param lonMin,lonMax,latMin,latMax The minimum and maximum values of the
    #'   longitude and latitude of the AOI, in decimal degrees. The longitude
    #'   values must agree with the range of the longitude in the variable to
    #'   which this AOI will be applied, e.g. `[-180,180]` or `[0,360]`.
    #' @param resolution The separation between adjacent grid cell, in longitude
    #'   and latitude directions, in decimal degrees.
    initialize = function(lonMin, lonMax, latMin, latMax, resolution) {
      private$minLon <- lonMin
      private$maxLon <- lonMax
      private$minLat <- latMin
      private$maxLat <- latMax
      private$res <- resolution
    },

    #' @description Summary of the area of interest printed to the console.
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
        cat(sprintf("Resolution: [%5.3f, %5.3f]\n", private$res[1L], private$res[2L]))
      else
        cat("Resolution: (from variable)\n")
    },

    #' @description Retrieve the bounds of the AOI.
    #' @param group The group in which the bounds should be located. This should
    #' usually be the group that contains the `CFAxis` instances built from the
    #' AOI.
    #' @return A list with two [CFBounds] instances in elements "lon" and "lat".
    bounds = function(group) {
      dims <- self$dim

      lon_var <- NCVariable$new(-1L, "lon_bnds_aoi", group, "NC_DOUBLE", 1L, NULL)
      lon_vals <- seq(from = private$minLon, by = private$res[1L], length = dims[2L] + 1L)
      lon_vals <- c(lon_vals[1:dims[2L]], lon_vals[2:(dims[2L] + 1L)])
      dim(lon_vals) <- c(2L, dims[2L])

      lat_var <- NCVariable$new(-1L, "lat_bnds_aoi", group, "NC_DOUBLE", 1L, NULL)
      lat_vals <- seq(from = private$minLat, by = private$res[2L], length = dims[1L] + 1L)
      lat_vals <- c(lat_vals[1:dims[1L]], lat_vals[2:(dims[1L] + 1L)])
      dim(lat_vals) <- c(2L, dims[1L])

      dim <- NCDimension$new(-1L, "nv", 2L, FALSE)

      list(lon = CFBounds$new(lon_var, dim, lon_vals),
           lat = CFBounds$new(lat_var, dim, lat_vals))
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
    #' vector in the order longitude minimum, maximum, latitude minimum, maximum.
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

    #' @field dim (read-only) The dimensions of the grid of the AOI once
    #' generated. Returned as a numeric vector with the dimensionality of the
    #' AOI in the Y and X directions, respectively.
    dim = function(value) {
      if (missing(value))
        as.integer(c((private$maxLat - private$minLat) / private$res[2L],
                     (private$maxLon - private$minLon) / private$res[1L]))
      else
        warning("Cannot set the grid dimensions of an AOI: auto-generated\n", call. = FALSE)
    },

    #' @field dimnames (read-only) Retrieve the dimnames of the AOI, in numeric
    #' form, as a list with the dimnames of the AOI in the Y and X directions,
    #' respectively. These are the center points of the grid cells.
    dimnames = function(value) {
      if (missing(value)) {
        if (is.null(private$minLat)) NULL
        else {
          d <- self$dim
          list(seq(from = private$minLat + private$res[2L] * 0.5, by = private$res[2L], length = d[1L]),
               seq(from = private$minLon + private$res[1L] * 0.5, by = private$res[1L], length = d[2L]))
        }
      } else
        warning("Cannot set the dimension names of an AOI: auto-generated\n", call. = FALSE)
    }
  )
)
