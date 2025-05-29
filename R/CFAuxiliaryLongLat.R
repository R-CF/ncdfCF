#' CF auxiliary longitude-latitude variable
#'
#' @description This class represents the longitude and latitude variables that
#' compose auxiliary coordinate variable axes for X-Y grids that are not
#' longitude-latitude.
#'
#' The class provides access to the data arrays for longitude and
#' latitude from the netCDF resource, as well as all the details that have been
#' associated with both axes. Additionally, this class can generate the index
#' to extract values on a long-lat grid of the associated X-Y grid data variable
#' using a user-selectable extent and resolution.
#'
#' Auxiliary longitude-latitude grids are only supported for reading from a
#' netCDF resource. Creating an instance manually therefore has no practical
#' purpose.
#'
#' @docType class
CFAuxiliaryLongLat <- R6::R6Class("CFAuxiliaryLongLat",
  inherit = CFObject,
  private = list(
    lon_   = NULL, # The longitude data
    lat_   = NULL, # The latitude data
    ext_   = NULL, # The extent of the longitude and latitude grids, in lon min, max, lat min, max
    res_   = c(NULL, NULL), # Resolution of the lat/long grid, vector of (lon, lat)
    aoi_   = NULL, # The AOI for the grid
    index_ = NULL, # The index values into the linearized grids that cover the AOI

    loadData = function() {
      if (is.null(private$lon_)) {
        private$lon_ <- try(RNetCDF::var.get.nc(self$varLong$group$handle, self$varLong$name), silent = TRUE)
        if (inherits(private$lon_, "try-error"))
          stop("Could not read longitude data for auxiliary long-lat grid", call. = FALSE)
      }
      if (is.null(private$lat_)) {
        private$lat_ <- try(RNetCDF::var.get.nc(self$varLat$group$handle, self$varLat$name), silent = TRUE)
        if (inherits(private$lat_, "try-error"))
          stop("Could not read latitude data for auxiliary long-lat grid", call. = FALSE)
      }

      private$setResolution()
    },

    # Set the nominal resolution of the grid at the location indicated. If no
    # location is indicated, use the center of the grids.
    setResolution = function(location = NULL) {
      dim <- dim(private$lon_)
      if (is.null(location))
        location <- as.integer(dim * 0.5)

      private$res_[1L] <- if (location[1L] == 1L)
        private$lon_[location[1L] + 1L, location[2L]] - private$lon_[location[1L], location[2L]]
      else if (location[1L] == dim[1L])
        private$lon_[location[1L], location[2L]] - private$lon_[location[1L] - 1L, location[2L]]
      else
        (private$lon_[location[1L] + 1L, location[2L]] - private$lon_[location[1L] - 1L, location[2L]]) * 0.5

      private$res_[2L] <- if (location[2L] == 1L)
        private$lat_[location[1L], location[2L] + 1L] - private$lat_[location[1L], location[2L]]
      else if (location[2L] == dim[2L])
        private$lat_[location[1L], location[2L]] - private$lat_[location[1L], location[2L] - 1L]
      else
        (private$lat_[location[1L], location[2L] + 1L] - private$lat_[location[1L], location[2L] - 1L]) * 0.5
    },

    setAOI = function(aoi) {
      self$clear_cache(full = FALSE)
      private$aoi_ <- aoi

      private$loadData()

      # If there are any NULL values in aoi, use the bounds or extent
      expand <- FALSE
      if (is.null(aoi$latMin))
        if (inherits(self$boundsLong, "CFBounds")) {
          aoi$extent <- c(self$boundsLong$range(), self$boundsLat$range())
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
        private$setResolution(center)
        aoi$resolution <- round(c(private$res_[1L], private$res_[2L]), CF$digits)
      }

      if (expand) {
        halfres <- aoi$resolution * 0.5
        aoi$extent <- aoi$extent + c(-halfres[1L], halfres[1L], -halfres[2L], halfres[2L])
      }

      # Update upper-left to match resolution
      aoi$lonMax <- if (self$extent[1L] < 0)
                      min(aoi$lonMin + private$res_[1L] * ceiling(lonExt / private$res_[1L]), 180)
                    else
                      min(aoi$lonMin + private$res_[1L] * ceiling(lonExt / private$res_[1L]), 360)
      aoi$latMax <- min(aoi$latMin + private$res_[2L] * ceiling(latExt / private$res_[2L]), 90)

      invisible(self)
    }
  ),
  public = list(
    #' @field varLong The [NCVariable] instance of the longitude values.
    varLong = NULL,

    #' @field varLat The [NCVariable] instance of the latitude values.
    varLat = NULL,

    #' @field boundsLong The [CFBounds] instance for the longitude values of the
    #' grid.
    boundsLong = NULL,

    #' @field boundsLat The [CFBounds] instance for the latitude values of the
    #' grid.
    boundsLat = NULL,

    #' @field axis_order Either `c("X", "Y")` (default) or `c("Y", "X")` to
    #' indicate the orientation of the latitude and longitude grids.
    axis_order = c("X", "Y"),

    #' @description Creating a new instance. It should normally not be useful to
    #'   create an instance of this class other than upon reading a netCDF
    #'   resource.
    #' @param varLong,varLat The [NCVariable] instances with the longitude and
    #'   latitude grid values, respectively.
    #' @param boundsLong,boundsLat The bounds of the grid cells for the
    #'   longitude and latitude, respectively, if set.
    initialize = function(varLong, varLat, boundsLong, boundsLat) {
      self$varLong <- varLong
      self$varLat <- varLat
      self$boundsLong <- boundsLong
      self$boundsLat <- boundsLat

      varLong$CF <- self
      varLat$CF <- self
    },

    #' @description Summary of the auxiliary longitude-latitude variable printed
    #'   to the console.
    print = function() {
      cat("<", self$friendlyClassName, ">\n", sep = "")
      cat("Longitude grid :", self$varLong$name, "\n")
      cat("Latitude grid  :", self$varLat$name, "\n")
      grpLon <- self$varLong$group$name
      grpLat <- self$varLat$group$name
      if (identical(grpLon, grpLat)) {
        if (grpLon != "/")
          cat("Group          :", grpLon, "\n")
      } else
        cat("Groups         :", grpLon, "(longitude) ||", grpLat, "(latitude)\n")

      ext <- self$extent
      cat(sprintf("\nLongitude range: [%5.3f ... %5.3f] degrees_east\n", ext[1], ext[2]))
      cat(sprintf("Latitude range : [%5.3f ... %5.3f] degrees_north\n", ext[3], ext[4]))

      if (inherits(private$aoi_, "AOI")) {
        aoi <- private$aoi_
        cat(sprintf("\nAOI  longitude : [%5.3f ... %5.3f] degrees_east\n", aoi$lonMin, aoi$lonMax))
        cat(sprintf("      latitude : [%5.3f ... %5.3f] degrees_north\n", aoi$latMin, aoi$latMax))
        aoi_dim <- aoi$dim
        aoi_res <- aoi$resolution
        cat(sprintf("    resolution : [%5.3f degrees_east x %5.3f degrees_north]\n", aoi_res[1L], aoi_res[2L]))
        cat(sprintf("        extent : [%d rows x %d columns]", aoi_dim[1L], aoi_dim[2L]))
      } else
        cat("\nAOI            : (not set)\n")
    },

    #' @description Some details of the auxiliary longitude-latitude grid.
    #' @return A 2-row `data.frame` with some details of the grid components.
    brief = function() {
      lon <- self$varLong
      lat <- self$varLat
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

      private$loadData()
      if (is.null(maxDist))
        maxDist <- max(private$res_)

      out <- mapply(function(lon, lat, max2) {
        dlon <- private$lon_ - lon
        dlat <- private$lat_ - lat
        dist2 <- dlon * dlon + dlat * dlat
        min <- which.min(dist2)
        if (dist2[min] <= max2) min else NA_integer_
      }, x, y, MoreArgs = list(max2 = maxDist * maxDist))

      out <- arrayInd(out, dim(private$lon_))
      colnames(out) <- c("X", "Y")
      out
    },

    #' @description Compute the indices for the AOI into the data grid.
    #' @return An integer matrix with the dimensions of the AOI, where each
    #' grid cell gives the linear index value into the longitude and latitude
    #' grids.
    grid_index = function() {
      # Use the cached index, if available
      if (!is.null(private$index_)) return(private$index_)

      # Otherwise calculate it
      private$loadData()
      if (is.null(private$aoi_))
        private$setAOI(aoi())

      # Orient AOI with the grid (for aoi_res and aoi_dim). AOI is always North
      # up but the grid may be oriented differently, hence why indices into AOI
      # properties aoi_res and aoi_dim are determined based on the orientation.
      # Create vectors of AOI longitude and latitude coordinates.
      aoi_ext <- private$aoi_$extent
      aoi_dim <- private$aoi_$dim
      aoi_res <- private$aoi_$resolution
      if (self$axis_order[1L] == "X") {
        Xidx <- 1L; Yidx <- 2L
        aoiY <- seq(from = aoi_ext[1L] + aoi_res[1L] * 0.5, by = aoi_res[1L], length.out = aoi_dim[2L])
        aoiX <- seq(from = aoi_ext[3L] + aoi_res[2L] * 0.5, by = aoi_res[2L], length.out = aoi_dim[1L])
        gridX <- private$lat_
        gridY <- private$lon_
      } else {
        Xidx <- 2L; Yidx <- 1L
        aoiX <- seq(from = aoi_ext[1L] + aoi_res[1L] * 0.5, by = aoi_res[1L], length.out = aoi_dim[2L])
        aoiY <- seq(from = aoi_ext[3L] + aoi_res[2L] * 0.5, by = aoi_res[2L], length.out = aoi_dim[1L])
        gridX <- private$lon_
        gridY <- private$lat_
      }

      # Find pixels in the full grid that are within the AOI
      grid <- (private$lon_ >= aoi_ext[1L]) & (private$lon_ < aoi_ext[2L]) &
              (private$lat_ >= aoi_ext[3L]) & (private$lat_ < aoi_ext[4L])
      if (!any(grid)) {
        private$index_ <- matrix(NA_integer_, aoi_dim[Yidx], aoi_dim[Xidx])
        dimnames(private$index_) <- list(aoiY, aoiX)
        return(private$index_)
      }

      grid_idx <- seq_len(prod(dim(private$lon_)))[grid]
      gridX <- gridX[grid]
      gridY <- gridY[grid]

      # Build the index for the AOI extent
      out <- data.frame(aoiY)
      for (x in 1L:aoi_dim[Xidx]) {
        nearx <- abs(gridX - aoiX[x]) < (aoi_res[Xidx] * 2) # * 2 for buffer
        out[[x]] <- sapply(aoiY, function(y, dx, xidx, xlat) {
          neary <- abs(xlat - y) < (aoi_res[Yidx] * 2)
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
      private$index_ <- out
      out
    },

    #' @description Clears the cache of pre-computed grid index values if an AOI
    #' has been set.
    #' @param full Logical (default = `FALSE`) that indicates if longitude and
    #' latitude grid arrays should be cleared as well to save space. These will
    #' then be re-read from file if a new AOI is set.
    clear_cache = function(full = FALSE) {
      private$index_ <- NULL
      if (full) {
        private$lon_ <- NULL
        private$lat_ <- NULL
      }
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
        paste(self$varLong$name, self$varLat$name, sep = "_")
    },

    #' @field aoi Set or retrieve the AOI for the long-lat grid.
    aoi = function(value) {
      if (missing(value))
        private$aoi_
      else
        private$setAOI(value)
    },

    #' @field lon (read-only) Retrieve the longitude grid.
    lon = function(value) {
      if (missing(value))
        private$lon_
    },

    #' @field lat (read-only) Retrieve the latitude grid.
    lat = function(value) {
      if (missing(value))
        private$lat_
    },

    #' @field extent (read-only) Retrieve the extent of the longitude and
    #'   latitude grids, including bounds if they have been set. The extent is
    #'   reported as a numeric vector of the four elements minumum and maximum
    #'   longitude and minimum and maximum latitude.
    extent = function(value) {
      if (missing(value)) {
        if (is.null(private$ext_)) {
          if (inherits(self$boundsLong, "CFBounds")) {
            private$ext_ <- c(self$boundsLong$range(), self$boundsLat$range())
          } else {
            private$loadData()
            private$ext_ <- c(range(private$lon_), range(private$lat_))
          }
        }
        private$ext_
      }
    },

    #' @field dim (read-only) The dimensions of the longitude and latitude
    #' grids.
    dim = function(value) {
      if (missing(value)) {
        private$loadData()
        dim(private$lon_)
      }
    },

    #' @field dimids (read-only) The dimids of the longitude and latitude grids.
    dimids = function(value) {
      if (missing(value))
        self$varLong$dimids
    }
  )
)
