#' CF auxiliary longitude-latitude variable
#'
#' @description This class represents the longitude and latitude variables that
#'   compose auxiliary coordinate variable axes for X-Y grids that are not
#'   longitude-latitude.
#'
#'   The class provides access to the data arrays for longitude and latitude
#'   from the netCDF resource, as well as all the details that have been
#'   associated with both axes. Additionally, this class can generate the index
#'   to extract values on a long-lat grid of the associated X-Y grid data
#'   variable using a user-selectable extent and resolution.
#'
#'   Auxiliary longitude-latitude grids are only supported for reading from a
#'   netCDF resource. Creating an instance of this class manually therefore has
#'   no practical purpose.
#'
#' @docType class
CFAuxiliaryLongLat <- R6::R6Class("CFAuxiliaryLongLat",
  private = list(
    .ext   = NULL, # The extent of the longitude and latitude grids, in lon min, max, lat min, max
    .res   = c(NULL, NULL), # Resolution of the lat/long grid, vector of (lon, lat)
    .aoi   = NULL, # The AOI for the grid
    .index = NULL, # The index values into the linearized grids that cover the AOI

    # The [CFVariable] instance of the longitude values.
    .varLong = NULL,

    # The [CFVariable] instance of the latitude values.
    .varLat = NULL,

    # The [CFBounds] instance for the longitude values of the grid.
    .boundsLong = NULL,

    # The [CFBounds] instance for the latitude values of the grid.
    .boundsLat = NULL,

    # Either `c("X", "Y")` (default) or `c("Y", "X")` to
    # indicate the orientation of the latitude and longitude grids.
    .axis_order = c("X", "Y"),

    # Set the nominal resolution of the grid at the location indicated. If no
    # location is indicated, use the center of the grids. The location must be
    # given as an integer vector of length with indices into the lat-long grids.
    set_resolution = function(location = NULL) {
      dim <- private$.varLong$dim()
      if (is.null(location))
        location <- as.integer(dim * 0.5)

      lon <- private$.varLong$raw()
      lat <- private$.varLat$raw()
      private$.res[1L] <- abs(if (location[1L] == 1L)
        lon[location[1L] + 1L, location[2L]] - lon[location[1L], location[2L]]
      else if (location[1L] == dim[1L])
        lon[location[1L], location[2L]] - lon[location[1L] - 1L, location[2L]]
      else
        (lon[location[1L] + 1L, location[2L]] - lon[location[1L] - 1L, location[2L]]) * 0.5)

      private$.res[2L] <- abs(if (location[2L] == 1L)
        lat[location[1L], location[2L] + 1L] - lat[location[1L], location[2L]]
      else if (location[2L] == dim[2L])
        lat[location[1L], location[2L]] - lat[location[1L], location[2L] - 1L]
      else
        (lat[location[1L], location[2L] + 1L] - lat[location[1L], location[2L] - 1L]) * 0.5)
    },

    setAOI = function(aoi) {
      self$clear_cache()
      private$.aoi <- aoi

      # If there are any NULL values in aoi, use the bounds or extent
      expand <- FALSE
      if (is.null(aoi$latMin))
        if (inherits(self$.boundsLong, "CFBounds")) {
          aoi$extent <- c(self$.boundsLong$range(), self$.boundsLat$range())
        } else {
          aoi$extent <- self$extent
          expand <- TRUE
        }
      lonExt <- aoi$lonMax - aoi$lonMin
      latExt <- aoi$latMax - aoi$latMin

      # Resolution, use supplied or calculate from grid
      if (is.null(aoi$resolution)) {
        center <- self$sample_index(aoi$lonMin + lonExt * 0.5, aoi$latMin + latExt * 0.5)[1L,]
        if (is.na(center[1L]))
          stop("Center of AOI contains no data so resolution cannot be derived. Please specify explicitly.", call. = FALSE)
        private$set_resolution(center)
        aoi$resolution <- round(c(private$.res[1L], private$.res[2L]), CF.options$digits)
      }

      if (expand) {
        halfres <- aoi$resolution * 0.5
        aoi$extent <- aoi$extent + c(-halfres[1L], halfres[1L], -halfres[2L], halfres[2L])
      }

      # Update upper-left to match resolution
      aoi$lonMax <- if (self$extent[1L] < 0)
                      min(aoi$lonMin + private$.res[1L] * ceiling(lonExt / private$.res[1L]), 180)
                    else
                      min(aoi$lonMin + private$.res[1L] * ceiling(lonExt / private$.res[1L]), 360)
      aoi$latMax <- min(aoi$latMin + private$.res[2L] * ceiling(latExt / private$.res[2L]), 90)

      invisible(self)
    }
  ),
  public = list(
    #' @description Creating a new instance. It should normally not be useful to
    #'   create an instance of this class other than upon reading a netCDF
    #'   resource.
    #' @param varLong,varLat The [CFVariable] instances with the longitude and
    #'   latitude grid values, respectively.
    #' @param boundsLong,boundsLat The [CFBounds] instances of the grid cells
    #'   for the longitude and latitude, respectively, if set. Defaults to
    #'   `NULL`.
    initialize = function(varLong, varLat, boundsLong = NULL, boundsLat = NULL) {
      private$.varLong <- varLong
      private$.varLat <- varLat
      private$.boundsLong <- boundsLong
      private$.boundsLat <- boundsLat

      private$set_resolution()
    },

    #' @description Summary of the auxiliary longitude-latitude variable printed
    #'   to the console.
    print = function() {
      cat("<", self$friendlyClassName, ">\n", sep = "")
      cat("Longitude grid :", private$.varLong$name, "\n")
      cat("Latitude grid  :", private$.varLat$name, "\n")

      ext <- self$extent
      cat(sprintf("\nLongitude range: [%5.3f ... %5.3f] degrees_east\n", ext[1], ext[2]))
      cat(sprintf("Latitude range : [%5.3f ... %5.3f] degrees_north\n", ext[3], ext[4]))

      if (inherits(private$.aoi, "AOI")) {
        aoi <- private$.aoi
        cat(sprintf("\nAOI  longitude : [%5.3f ... %5.3f] degrees_east\n", aoi$lonMin, aoi$lonMax))
        cat(sprintf("      latitude : [%5.3f ... %5.3f] degrees_north\n", aoi$latMin, aoi$latMax))
        aoidim <- aoi$dim
        aoires <- aoi$resolution
        cat(sprintf("    resolution : [%5.3f degrees_east x %5.3f degrees_north]\n", aoires[1L], aoires[2L]))
        cat(sprintf("        extent : [%d rows x %d columns]", aoidim[1L], aoidim[2L]))
      } else
        cat("\nAOI            : (not set)\n")
    },

    #' @description Some details of the auxiliary longitude-latitude grid.
    #' @return A 2-row `data.frame` with some details of the grid components.
    brief = function() {
      lon <- private$.varLong
      lat <- private$.varLat
      out <- data.frame(axis = c("X", "Y"),
                        name = c(lon$name, lat$name))

      if (!identical(lon$longname, lon$name) || !identical(lat$longname, lat$name))
        out[["long_name"]] <- c(lon$longname, lat$longname)

      ext <- self$extent
      out[["extent"]] <- c(sprintf("[%5.3f ... %5.3f]", ext[1], ext[2]),
                           sprintf("[%5.3f ... %5.3f]", ext[3], ext[4]))

      out[["unit"]] <- c("degrees_east", "degrees_north")
      out
    },

    #' @description Return the indexes into the X (longitude) and Y (latitude)
    #'   axes of the original data grid of the points closest to the supplied
    #'   longitudes and latitudes, up to a maximum distance.
    #' @param x,y Vectors of longitude and latitude values in decimal degrees,
    #'   respectively.
    #' @param maxDist Numeric value in decimal degrees of the maximum distance
    #'   between the sampling point and the closest grid cell. If omitted
    #'   (default), the distance is calculated from the nominal resolution of
    #'   the grids.
    #' @return A matrix with two columns `X` and `Y` and as many rows as
    #'   arguments `x` and `y`. The `X` and `Y` columns give the index into the
    #'   grid of the sampling points, or `c(NA, NA)` is no grid point is located
    #'   within the `maxDist` distance from the sampling point.
    sample_index = function(x, y, maxDist = NULL) {
      if (is.null(x) || is.null(y) || length(x) != length(y))
        stop("Arguments `x` and `y` must be vectors of the same length", call. = FALSE)

      if (is.null(maxDist))
        maxDist <- max(private$.res)

      varlon <- private$.varLong$raw()
      varlat <- private$.varLat$raw()
      out <- mapply(function(lon, lat, max2) {
        dlon <- varlon - lon
        dlat <- varlat - lat
        dist2 <- dlon * dlon + dlat * dlat
        min <- which.min(dist2)
        if (dist2[min] <= max2) min else NA_integer_
      }, x, y, MoreArgs = list(max2 = maxDist * maxDist))

      out <- arrayInd(out, dim(varlon))
      colnames(out) <- c("X", "Y")
      out
    },

    #' @description Compute the indices for the AOI into the data grid.
    #' @return An integer matrix with the dimensions of the AOI, where each
    #' grid cell gives the linear index value into the longitude and latitude
    #' grids.
    grid_index = function() {
      # Use the cached index, if available
      if (!is.null(private$.index)) return(private$.index)

      # Otherwise calculate it
      if (is.null(private$.aoi))
        private$setAOI(aoi())

      # Orient AOI with the grid (for aoires and aoidim). AOI is always North
      # up but the grid may be oriented differently, hence why indices into AOI
      # properties aoires and aoidim are determined based on the orientation.
      # Create vectors of AOI longitude and latitude coordinates.
      lat <- private$.varLat$raw()
      lon <- private$.varLong$raw()
      aoiext <- private$.aoi$extent
      aoidim <- private$.aoi$dim
      aoires <- private$.aoi$resolution
      if (private$.axis_order[1L] == "X") {
        Xidx <- 1L; Yidx <- 2L
        aoiY <- seq(from = aoiext[1L] + aoires[1L] * 0.5, by = aoires[1L], length.out = aoidim[2L])
        aoiX <- seq(from = aoiext[3L] + aoires[2L] * 0.5, by = aoires[2L], length.out = aoidim[1L])
        gridX <- lat
        gridY <- lon
      } else {
        Xidx <- 2L; Yidx <- 1L
        aoiX <- seq(from = aoiext[1L] + aoires[1L] * 0.5, by = aoires[1L], length.out = aoidim[2L])
        aoiY <- seq(from = aoiext[3L] + aoires[2L] * 0.5, by = aoires[2L], length.out = aoidim[1L])
        gridX <- lon
        gridY <- lat
      }

      # Find pixels in the full grid that are within the AOI
      grid <- (lon >= aoiext[1L]) & (lon < aoiext[2L]) &
              (lat >= aoiext[3L]) & (lat < aoiext[4L])
      if (!any(grid)) {
        private$.index <- matrix(NA_integer_, aoidim[Yidx], aoidim[Xidx])
        dimnames(private$.index) <- list(aoiY, aoiX)
        return(private$.index)
      }

      grid_idx <- seq_len(prod(dim(lon)))[grid]
      gridX <- gridX[grid]
      gridY <- gridY[grid]

      # Build the index for the AOI extent
      out <- data.frame(aoiY)
      for (x in 1L:aoidim[Xidx]) {
        nearx <- abs(gridX - aoiX[x]) < (aoires[Xidx] * 2) # * 2 for buffer
        out[[x]] <- sapply(aoiY, function(y, dx, xidx, xlat) {
          neary <- abs(xlat - y) < (aoires[Yidx] * 2)
          len <- sum(neary)
          if (len == 0L) return(NA_integer_)
          if (len == 1L) return(xidx[neary])
          dxy <- dx[neary]
          dy <- xlat[neary] - y
          xidx[neary][which.min(dxy * dxy + dy * dy)]
        }, dx = gridX[nearx] - aoiX[x], xidx = grid_idx[nearx], xlat = gridY[nearx])
      }

      out <- data.matrix(out)
      dimnames(out) <- list(aoiY, aoiX)
      private$.index <- out
      out
    },

    #' @description Clears the cache of pre-computed grid index values if an AOI
    #' has been set.
    clear_cache = function() {
      private$.index <- NULL
      invisible(self)
    },

    #' @description Detach the latitude and longitude from an underlying netCDF
    #' resource.
    #' @return Self, invisibly.
    detach = function() {
      private$.varLong$detach()
      private$.varLat$detach()
      if (!is.null(private$.boundsLong)) private$.boundsLong$detach()
      if (!is.null(private$.boundsLat)) private$.boundsLat$detach()
      invisible(self)
    }
  ),
  active = list(
    #' @field friendlyClassName (read-only) A nice description of the class.
    friendlyClassName = function(value) {
      if (missing(value))
        "Auxiliary longitude-latitude grid"
    },

    #' @field name (read-only) The name of the auxiliary lon-lat grid.
    name = function(value) {
      if (missing(value))
        paste(private$.varLong$name, private$.varLat$name, sep = "_")
    },

    #' @field grid_names (read-only) Read the names of the longitude and
    #'   latitude grid as a vector of length 2.
    grid_names = function(value) {
      if (missing(value))
        c(private$.varLong$name, private$.varLat$name)
    },

    #' @field dimids (read-only) Retrieve the dimension ids used by the
    #'   longitude and latitude grids.
    dimids = function(value) {
      if (missing(value))
        private$.varLong$dimids
    },

    #' @field aoi Set or retrieve the AOI for the long-lat grid.
    aoi = function(value) {
      if (missing(value))
        private$.aoi
      else
        private$setAOI(value)
    },

    #' @field lon (read-only) Retrieve the longitude grid.
    lon = function(value) {
      if (missing(value))
        private$.varLong
    },

    #' @field lat (read-only) Retrieve the latitude grid.
    lat = function(value) {
      if (missing(value))
        private$.varLat
    },

    #' @field lon_bounds (read-only) Retrieve the boundary values of the
    #'   longitude grid.
    lon_bounds = function(value) {
      if (missing(value))
        private$.boundsLong
    },

    #' @field lat_bounds (read-only) Retrieve the boundary values of the
    #'   latitude grid.
    lat_bounds = function(value) {
      if (missing(value))
        private$.boundsLat
    },

    #' @field extent (read-only) Retrieve the extent of the longitude and
    #'   latitude grids, including bounds if they have been set. The extent is
    #'   reported as a numeric vector of the four elements minimum and maximum
    #'   longitude and minimum and maximum latitude.
    extent = function(value) {
      if (missing(value)) {
        if (is.null(private$.ext)) {
          if (inherits(private$.boundsLong, "CFBounds")) {
            private$.ext <- c(private$.boundsLong$range(), private$.boundsLat$range())
          } else {
            private$.ext <- c(range(private$.varLong$raw()), range(private$.varLat$raw()))
          }
        }
        private$.ext
      }
    },

    #' @field dim (read-only) The dimensions of the longitude and latitude
    #' grids.
    dim = function(value) {
      if (missing(value))
        private$.varLong$dim()
    }
  )
)
