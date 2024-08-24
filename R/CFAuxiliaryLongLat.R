#' CF auxiliary longitude-latitude variable
#'
#' @description This class represents the longitude and latitude variables that
#' compose auxiliary coordinate variable axes for X-Y grids that are not
#' longitude-latitude.
#'
#' @details The class provides access to the data arrays for longitude and
#' latitude from the netCDF resource, as well as all the details that have been
#' associated with both axes. Additionally, this class can generate the index
#' to extract values on a long-lat grid of the associated X-Y grid data variable
#' using a user-selectable extent and resolution.
#'
#' @docType class
#'
#' @name CFAuxiliaryLongLat
#' @export
CFAuxiliaryLongLat <- R6::R6Class("CFAuxiliaryLongLat",
  inherit = CFObject,
  private = list(
    lon_   = NULL, # The longitude data
    lat_   = NULL, # The latitude data
    aoi_   = NULL, # The AOI for the grid
    index_ = NULL, # The index values into the linearized grids that cover the AOI

    loadData = function() {
      if (is.null(private$lon_)) {
        private$lon_ <- try(RNetCDF::var.get.nc(self$varLong$group$handle, self$varLong$name), silent = TRUE)
        if (inherits(private$lon_, "try-error")) {
          private$lon_ <- NULL
          stop("Could not read longitude data for auxiliary long-lat grid", call. = FALSE)
        }
      }
      if (is.null(private$lat_)) {
        private$lat_ <- try(RNetCDF::var.get.nc(self$varLat$group$handle, self$varLat$name), silent = TRUE)
        if (inherits(private$lat_, "try-error")) {
          private$lat_ <- NULL
          stop("Could not read latitude data for auxiliary long-lat grid", call. = FALSE)
        }
      }
    },

    setAOI = function(aoi) {
      self$clear_cache(full = FALSE)
      private$aoi_ <- aoi

      private$loadData()

      # If there are any NULL values in aoi, compute them
      expand <- FALSE
      if (is.null(aoi$latMin)) { # Meaning all extent values are NULL
        # Use bounds if present
        if (inherits(self$boundsLong, "CFBounds")) {
          aoi$extent <- c(self$boundsLong$range(), self$boundsLat$range())
        } else {
          aoi$extent <- c(range(private$lon_), range(private$lat_))
          expand <- TRUE
        }
      }

      lonExt <- aoi$lonMax - aoi$lonMin
      latExt <- aoi$latMax - aoi$latMin

      # Resolution, use supplied or calculate from grid
      if (is.null(aoi$resolution)) {
        centerLon <- aoi$lonMin + lonExt * 0.5
        centerLat <- aoi$latMin + latExt * 0.5
        center <- self$sample_index(centerLon, centerLat)

        # FIXME: Need better algorithm to calculate local resolution
        aoi$resolution <- round(
                           ((private$lon_[center[1L] + 1L, center[2L]] -
                            private$lon_[center[1L] - 1L, center[2L]]) +
                           (private$lon_[center[1L], center[2L] + 1L] -
                            private$lon_[center[1L], center[2L] - 1L])) * 0.5, 5)
      }

      if (expand) {
        halfres <- aoi$resolution * 0.5
        aoi$extent <- aoi$extent + c(-halfres, halfres, -halfres, halfres)
      }

      # Update upper-left to match resolution
      # FIXME: constrain to domain limits
      aoi$lonMax <- aoi$lonMin + aoi$resolution * ceiling(lonExt / aoi$resolution)
      aoi$latMax <- aoi$latMin + aoi$resolution * ceiling(latExt / aoi$resolution)

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

    #' @description Creating a new instance
    #'
    #' @param varLong,varLat The NCVariables with the longitude and latitude
    #' grid values, respectively.
    #' @param boundsLong,boundsLat The bounds of the grid cells for the
    #' longitude and latitude, respectively, if set.
    initialize = function(varLong, varLat, boundsLong, boundsLat) {
      self$varLong <- varLong
      self$varLat <- varLat
      self$boundsLong <- boundsLong
      self$boundsLat <- boundsLat

      varLong$CF <- self
      varLat$CF <- self
    },

    #' @description Summary of the data variable
    #'
    #' Prints a summary of the data variable to the console.
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

      if (inherits(self$boundsLong, "CFBounds")) {
        lonRange <- self$boundsLong$range()
        latRange <- self$boundsLat$range()
      } else {
        private$loadData()
        lonRange <- range(private$lon_)
        latRange <- range(private$lat_)
      }
      cat(sprintf("\nLongitude range: [%5.3f ... %5.3f] degrees_east\n", lonRange[1], lonRange[2]))
      cat(sprintf("Latitude range : [%5.3f ... %5.3f] degrees_north\n", latRange[1], latRange[2]))

      if (inherits(private$aoi_, "AOI")) {
        cat(sprintf("\nAOI            : [%5.3f ... %5.3f], [%5.3f ... %5.3f]\n",
                    private$aoi_$lonMin, private$aoi_$lonMax, private$aoi_$latMin, private$aoi_$latMax))
        dim <- private$aoi_$dim
        cat(sprintf("               : [%d x %d], [resolution %5.3f]", dim[1L], dim[2L], private$aoi_$resolution))
      } else
        cat("\nAOI            : (not set)\n")
    },

    #' @description Some details of the longitude-latitude grid
    #'
    #' @returns A 2-row `data.frame` with some details of the grid components.
    brief = function() {
      out <- data.frame(orientation = c("longitude", "latitude"),
                        axis = c("X", "Y"),
                        name = c(self$varLong$name, self$varLat$name))

      if (!identical(self$varLong$longname, self$varLong$name) ||
          !identical(self$varLat$longname, self$varLat$name))
        out[["long_name"]] <- c(self$varLong$longname, self$varLat$longname)

      private$loadData()
      lonRange <- range(private$lon_)
      latRange <- range(private$lat_)
      out[["values"]] <- c(sprintf("[%5.3f ... %5.3f]", lonRange[1], lonRange[2]),
                           sprintf("[%5.3f ... %5.3f]", latRange[1], latRange[2]))

      if (inherits(self$boundsLong, "CFBounds")) {
        lonRange <- self$boundsLong$range()
        latRange <- self$boundsLat$range()
        out[["bounds"]] <- c(sprintf("[%5.3f ... %5.3f]", lonRange[1], lonRange[2]),
                             sprintf("[%5.3f ... %5.3f]", latRange[1], latRange[2]))
      }

      out[["unit"]] <- c("degrees_east", "degrees_north")
      out
    },

    #' @description Return the indexes into the X (longitude) and Y (latitude)
    #' axes of the original data grid of the points closest to the supplied
    #' longitudes and latitudes.
    #'
    #' @param x,y Vectors of longitude and latitude values in decimal
    #' degrees, respectively.
    sample_index = function(x, y) {
      private$loadData()

      out <- mapply(function(lon, lat, dims) {
        dlon <- private$lon_ - lon
        dlat <- private$lat_ - lat
        dist2 <- dlon * dlon + dlat * dlat
        ndx <- arrayInd(which.min(dist2), dims)
        # FIXME: Check for points outside of the grid
        ndx
      }, x, y, MoreArgs = list(dims = dim(private$lon_)))

      out <- t(out)
      colnames(out) <- c("X", "Y")
      out
    },

    #' @description Return the indices for the AOI into the data grid as an
    #' integer vector to index data values on the data grid for the AOI.
    grid_index = function() {
      # Must have data.table installed
      if (!requireNamespace("data.table"))
        stop("Must install package 'data.table' for this functionality.")

      # Use the cached index, if available
      if (!is.null(private$index_)) return(private$index_)

      # Otherwise calculate it
      private$loadData()
      if (is.null(private$aoi_))
        private$setAOI(aoi())

      # Find pixels in the full grid that are within the AOI
      extent <- private$aoi_$extent
      grid <- (private$lon_ >= extent[1L]) & (private$lon_ < extent[2L]) &
              (private$lat_ >= extent[3L]) & (private$lat_ < extent[4L])
      if (!any(grid))
        return(private$index_ <- replicate(prod(private$aoi_$dim), NA_integer_))

      grid_idx <- seq_len(prod(dim(private$lon_)))[grid]
      grid_lon <- private$lon_[grid]
      grid_lat <- private$lat_[grid]
      dt <- data.table(idx = grid_idx, lon = grid_lon, lat = grid_lat)

      # Vectors of AOI longitude and latitude coordinates
      aoi_dim <- private$aoi_$dim
      res <- private$aoi_$resolution
      aoi_lon <- seq(from = extent[1L] + res * 0.5, by = res, length.out = aoi_dim[1L])
      aoi_lat <- seq(from = extent[3L] + res * 0.5, by = res, length.out = aoi_dim[2L])

      # Build the index for the AOI extent
      out <- data.table(aoi_lat)
      for (x in 1:aoi_dim[1L]) {
        dtx <- dt[abs(lon - aoi_lon[x]) < res]
        out[[x]] <- sapply(aoi_lat, function(y, xval) {
          dty <- dtx[abs(lat - y) < res]
          len <- nrow(dty)
          if (len == 0L) return(NA_integer_)
          if (len == 1L) return(dty[, idx])
          dty[, dist := (lon - xval) * (lon - xval) + (lat - y) * (lat - y)]
          vals <- dty[order(dist), idx]
          vals[1L]
        }, xval = aoi_lon[x])
      }

      data.matrix(out)
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

    #' @field lon Retrieve the longitude grid.
    lon = function(value) {
      private$lon_
    },

    #' @field lat Retrieve the latitude grid.
    lat = function(value) {
      private$lat_
    }
  )
)
