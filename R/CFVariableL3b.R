#' CF data variable for the NASA L3b format
#'
#' @description This class represents a CF data variable that provides access to
#' data sets in NASA level-3 binned format, used extensively for satellite
#' imagery.
#'
#' @references https://oceancolor.gsfc.nasa.gov/resources/docs/technical/ocean_level-3_binned_data_products.pdf
#' @docType class
#' @importFrom abind abind
CFVariableL3b <- R6::R6Class("CFVariableL3b",
  inherit = CFVariable,
  private = list(
    # Minimum and maximum row in the L3b variable.
    file_rows = c(0L, 0L),

    # Minimum and maximum bin in the L3b variable.
    file_bins = c(0L, 0L)
  ),
  public = list(
    #' @field variable The name of the variable contained in this L3b data.
    variable = NULL,

    #' @field index The index data of the L3b structure.
    index = NULL,

    #' @description Create an instance of this class.
    #'
    #' @param grp The group that this CF variable lives in. Must be called
    #'   "/level-3_binned_data".
    #' @param units Vector of two character strings with the variable name and
    #'   the physical units of the data variable in the netCDF resource.
    #' @return An instance of this class.
    initialize = function(grp, units) {
      self$variable <- units[1L]

      ncv <- grp$NCvars
      var <- ncv[[self$variable]]
      if (is.null(ncv[["BinIndex"]]) || is.null(ncv[["BinList"]]) || (is.null(var)))
        stop("L3b: required netCDF variables not found.")

      # Read the index
      self$index <- RNetCDF::var.get.nc(grp$handle, "BinIndex", unpack = TRUE, fitnum = TRUE)
      lat_len <- length(self$index$start)

      # Latitude axis
      lat_rows <- which(self$index$begin > 0)
      private$file_rows <- as.integer(range(lat_rows))
      len <- as.integer(private$file_rows[2L] - private$file_rows[1L]) + 1L
      bnds <- (private$file_rows[1L] - 1):private$file_rows[2L] * 180 / lat_len - 90
      lat <- makeLatitudeAxis(-1L, "latitude", grp, len,
                              (private$file_rows[1L]:private$file_rows[2L] - 0.5) * 180 / lat_len - 90,
                              rbind(bnds[-len], bnds[-1L]), "degrees_north")

      # Longitude axis
      lon_bins <- self$index$max[lat_len * 0.5]
      west <- self$index$begin[lat_rows] - self$index$start_num[lat_rows] + 1
      east <- west + self$index$extent[lat_rows] - 1
      expand <- lon_bins / self$index$max[lat_rows]
      private$file_bins[1L] <- min(floor(west * expand))
      private$file_bins[2L] <- max(ceiling(east * expand))
      lon <- (private$file_bins[1L]:private$file_bins[2L] - 0.5) * 360 / lon_bins - 180
      bnds <- (private$file_bins[1L]-1):private$file_bins[2L] * 360 / lon_bins - 180
      len <- private$file_bins[2L] - private$file_bins[1L] + 1
      lon <- makeLongitudeAxis(-1L, "longitude", grp, len, lon,
                               rbind(bnds[-len], bnds[-1L]), "degrees_east")

      axes <- list(
        longitude = lon,
        latitude = lat
      )

      # Scalar time axis
      tc_start <- grp$parent$attribute("time_coverage_start")
      tc_end <- grp$parent$attribute("time_coverage_end")
      if (!is.na(tc_start) && !is.na(tc_end)) {
        tc <- CFtime::CFtime("seconds since 1970-01-01", "proleptic_gregorian", c(tc_start, tc_end))
        ymd <- tc$cal$offset2date(mean(tc$offsets) / 86400)
        cft <- CFtime::CFtime("seconds since 1970-01-01", "proleptic_gregorian")
        cft <- cft + CFtime::parse_timestamps(cft, sprintf("%04d-%02d-%02dT12:00:00", ymd$year, ymd$month, as.integer(ymd$day)))$offset
        cft$set_bounds(matrix(tc$offsets, nrow = 2))
        axes[["time"]] <- makeTimeAxis(-1L, "time", grp, cft)
      }

      grp$CFaxes <- axes

      # CRS
      v <- NCVariable$new(-1L, "latitude_longitude", grp, "NC_CHAR", 0L, NULL)
      self$crs <- CFGridMapping$new(grp, v, "latitude_longitude")
      self$crs$set_attribute("grid_mapping_name", "NC_CHAR", "latitude_longitude")
      self$crs$set_attribute("semi_major_axis", "NC_DOUBLE", 6378145)
      self$crs$set_attribute("inverse_flattening", "NC_DOUBLE", 0)
      self$crs$set_attribute("prime_meridian_name", "NC_CHAR", "Greenwich")

      # Construct the object
      super$initialize(grp, var, axes)
      self$set_attribute("units", "NC_CHAR", units[2L])
      self$NCvar$CF <- self # the variable
      ncv[["BinIndex"]]$CF <- ncv[["BinList"]]$CF <- self
    },

    #' @description Read all the data from the file and turn the data into a
    #'   matrix. If an `aoi` is specified, the data will be subset to that area.
    #'
    #'   This method returns a bare-bones matrix without any metadata or other
    #'   identifying information. Use method `data()`, `subset()` or the `[`
    #'   operator rather than this method to obtain a more informative result.
    #' @param aoi An instance of class `AOI`, optional, to select an area in
    #'   latitude - longitude coordinates.
    #' @return A matrix with the data of the variable in raw format.
    as_matrix = function(aoi = NULL) {
      binList <- RNetCDF::var.get.nc(self$NCvar$group$handle, "BinList")
      binData <- RNetCDF::var.get.nc(self$NCvar$group$handle, self$variable)

      # Get the binned data in rows
      data_row <- findInterval(binList$bin_num, self$index$start_num)
      data_row_bin <- as.integer(binList$bin_num - self$index$start_num[data_row]) + 1L

      numRows <- length(self$index$start_num)
      maxBins <- self$index$max[numRows * 0.5]

      if (is.null(aoi)) {
        startRow <- private$file_rows[1L]
        startBin <- private$file_bins[1L]
        endRow <- private$file_rows[2L]
        endBin <- private$file_bins[2L]
      } else {
        # Subset the latitude
        startRow <- floor((aoi[3L] + 90) * numRows / 180) + 1L
        endRow <- floor((aoi[4L] + 90) * numRows / 180)

        # Subset the longitude
        startBin <- floor((aoi[1L] + 180) * maxBins / 360) + 1L
        endBin <- floor((aoi[2L] + 180) * maxBins / 360)
      }

      l <- lapply(startRow:endRow, function(r) {
        out <- rep(NA_real_, maxBins)
        idx <- which(data_row == r)
        if (!length(idx)) return(out[startBin:endBin])

        d <- binData$sum[idx] / binList$weights[idx]   # The data of the physical property
        b <- data_row_bin[idx]                         # The bins to put the data in

        # Expand to maxBins grid cells
        allocate <- ceiling(1:maxBins * self$index$max[r] / maxBins)

        # Allocate the data to the expanded grid cells
        for (bin in seq_along(b))
          out[which(allocate == b[bin])] <- d[bin]

        out[startBin:endBin]
      })
      abind::abind(l, along = 2)
    },

    #' @description Retrieve all data of the L3b variable.
    #'
    #' @return A [CFArray] instance with all data from this L3b variable.
    data = function() {
      out_group <- NCGroup$new(-1L, "/", "/", NULL, NULL)
      out_group$set_attribute("title", "NC_CHAR", paste("L3b variable", self$name, "regridded to latitude-longitude"))
      out_group$set_attribute("history", "NC_CHAR", paste0(format(Sys.time(), "%FT%T%z"), " R package ncdfCF(", packageVersion("ncdfCF"), ")::CFVariableL3b$data()"))

      axes <- lapply(self$axes, function(ax) ax$clone())

      CFArray$new(self$name, out_group, self$as_matrix(), private$values_type, axes, self$crs, self$attributes)
    },

    #' @description This method extracts a subset of values from the data of the
    #'   variable, with the range along both axes expressed in decimal degrees.
    #'
    #' @details The range of values along both axes of latitude and longitude is
    #'   expressed in decimal degrees. Any axes for which no information is
    #'   provided in the `subset` argument are extracted in whole. Values can be
    #'   specified in a variety of ways that should (resolve to) be a vector of
    #'   real values. A range (e.g. `100:200`), a vector (`c(23, 46, 3, 45,
    #'   17`), a sequence (`seq(from = 78, to = 100, by = 2`), all work. Note,
    #'   however, that only a single range is generated from the vector so these
    #'   examples resolve to `(100, 200)`, `(3, 46)`, and `(78, 100)`,
    #'   respectively.
    #'
    #'   If the range of values for an axis in argument `subset` extend the
    #'   valid range of the axis in `x`, the extracted slab will start at the
    #'   beginning for smaller values and extend to the end for larger values.
    #'   If the values envelope the valid range the entire axis will be
    #'   extracted in the result. If the range of `subset` values for any axis
    #'   are all either smaller or larger than the valid range of the axis in
    #'   `x` then nothing is extracted and `NULL` is returned.
    #'
    #'   The extracted data has the same dimensional structure as the data in
    #'   the variable, with degenerate dimensions dropped. The order of the axes
    #'   in argument `subset` does not reorder the axes in the result; use the
    #'   [CFArray]$array() method for this.
    #'
    #' @param subset A list with the range to extract from each axis. The list
    #'   should have elements for the axes to extract a subset from - if an axis
    #'   is not present in the list the entire axis will be extracted from the
    #'   array. List element names should be the axis name or designator
    #'   `longitude` or `X`, or `latitude` or `Y`. Axis designators and names
    #'   are case-sensitive and can be specified in any order. If values for the
    #'   range per axis fall outside of the extent of the axis, the range is
    #'   clipped to the extent of the axis.
    #' @param aoi Optional, an area-of-interest instance of class `AOI` created
    #'   with the [aoi()] function to indicate the horizontal area that should
    #'   be extracted. The longitude and latitude coordinates must be included;
    #'   the X and Y resolution will be calculated if not given. When provided,
    #'   this argument will take precedence over the `subset` argument.
    #' @param rightmost.closed Single logical value to indicate if the upper
    #'   boundary of range in each axis should be included.
    #' @param ... Ignored.
    #'
    #' @return A [CFArray] instance, having an array with axes and attributes of
    #'   the variable, or `NULL` if one or more of the elements in the `subset`
    #'   argument falls entirely outside of the range of the axis. Note that
    #'   degenerate dimensions (having `length(.) == 1`) are dropped from the
    #'   array but the corresponding axis is maintained in the result as a
    #'   scalar axis.
    subset = function(subset = NULL, aoi = NULL, rightmost.closed = FALSE, ...) {
      if (is.null(subset) && is.null(aoi))
        stop("Either `subset` or `aoi` arguments must be set", call. = FALSE)
      if (!is.null(aoi) && is.null(aoi$lonMin))
        stop("Argument `aoi` must have coordinates set", call. = FALSE)
      if (!is.null(subset)) {
        sub_names <- names(subset)
        bad <- sub_names[!(sub_names %in% c("longitude", "latitude", "X", "Y"))]
        if (length(bad))
          stop("Argument `subset` contains elements not corresponding to an axis:", paste(bad, collapse = ", "), call. = FALSE)
      }

      out_group <- NCGroup$new(-1L, "/", "/", NULL, NULL)
      out_group$set_attribute("title", "NC_CHAR", paste("Processing result of variable", self$name))
      out_group$set_attribute("history", "NC_CHAR", paste0(format(Sys.time(), "%FT%T%z"), " R package ncdfCF(", packageVersion("ncdfCF"), ")::CFVariableL3b$subset()"))

      out_axes_dim <- list()
      out_axes_other <- list()
      for (ax in 1:2) {
        axis <- self$axes[[ax]]

        rng <- NULL
        if (!is.null(aoi))
          rng <- if (axis$orientation == "X") c(aoi$lonMin, aoi$lonMax)
                 else c(aoi$latMin, aoi$latMax)
        if (is.null(rng)) rng <- subset[[ c("longitude", "latitude")[ax] ]]
        if (is.null(rng)) rng <- subset[[ c("X", "Y")[ax] ]]
        if (is.null(rng)) {
          out_axis <- axis$subset(out_group, NULL)
        } else {
          idx <- private$range2index(axis, rng, rightmost.closed)
          if (is.null(idx)) return(NULL)
          out_axis <- axis$subset(out_group, idx)
        }

        # Collect axes for result
        if (inherits(out_axis, "CFAxisScalar"))
          out_axes_other <- append(out_axes_other, out_axis)
        else
          out_axes_dim <- append(out_axes_dim, out_axis)
      }

      # Read the data
      d <- self$as_matrix(aoi)
      d <- drop(d)

      # Assemble the CFArray instance
      axes <- c(out_axes_dim, out_axes_other)
      names(axes) <- sapply(axes, function(a) a$name)
      CFArray$new(self$name, out_group, d, private$values_type, axes, self$crs, self$attributes)
    }
  )
)

# Public S3 methods ------------------------------------------------------------

#' Extract data for a variable
#'
#' Extract data from a `CFVariableL3b` instance, optionally sub-setting the
#' axes to load only data of interest.
#'
#' If all the data of the variable in `x` is to be extracted, simply use `[]`
#' (unlike with regular arrays, this is required, otherwise the details of the
#' variable are printed on the console).
#'
#' The indices into the axes to be subset can be specified in a variety of
#' ways; in practice it should (resolve to) be a vector of integers. A range
#' (e.g. `100:200`), an explicit vector (`c(23, 46, 3, 45, 17`), a sequence
#' (`seq(from = 78, to = 100, by = 2`), all work. Note, however, that only a
#' single range is generated from the vector so these examples resolve to
#' `100:200`, `3:46`, and `78:100`, respectively. It is also possible to use a
#' custom function as an argument.
#'
#' This method works with "bare" indices into the axes of the array. If
#' you want to use domain values of the axes (e.g. longitude values or
#' timestamps) to extract part of the variable array, use the
#' `CFVariableL3b$subset()` method.
#'
#' Scalar axes should not be included in the indexing as they do not represent a
#' dimension into the data array.
#'
#' @param x An `CFVariableL3b` instance to extract the data of.
#' @param i,j,... Expressions, one for each of the two axes of `x`, that select
#'   a number of elements along each axis. `i` is for the longitude axis, `j`
#'   for the latitude axis, `...` (additional named arguments) is invalid as
#'   there are only two axes to subset from. If either expression is missing,
#'   the entire axis is extracted. The values for the arguments may be an
#'   integer vector or a function that returns an integer vector. The range of
#'   the values in the vector will be used. See examples, below.
#' @param drop Logical, ignored. Axes are never dropped. Any degenerate
#'   dimensions of the array are returned as such, with dimnames and appropriate
#'   attributes set.
#'
#' @return An array with dimnames and other attributes set.
#' @export
#' @aliases bracket_select_l3b
#' @docType methods
#' @examples
#' fn <- system.file("extdata",
#'   "pr_day_EC-Earth3-CC_ssp245_r1i1p1f1_gr_20230101-20231231_vncdfCF.nc",
#'   package = "ncdfCF")
#' ds <- open_ncdf(fn)
#' pr <- ds[["pr"]]
#'
#' # How are the dimensions organized?
#' dimnames(pr)
#'
#' # Precipitation data for March for a single location
#' x <- pr[5, 12, 61:91]
#' str(x)
#'
#' # Summer precipitation over the full spatial extent
#' summer <- pr[, , 173:263]
#' str(summer)
"[.CFVariableL3b" <- function(x, i, j, ..., drop = FALSE) {
  data <- x$as_matrix()

  if (missing(i) && missing(j)) {
    dnames <- list(longitude = x$axes[[1L]]$dimnames, latitude = x$axes[[2L]]$dimnames)
  } else {
    dnames <- vector("list", 2L)
    names(dnames) <- names(x$axes)[1L:2L]
    if (missing(i)) {
      dnames[[1L]] <- x$axes[[1L]]$dimnames
      lon <- 1L:(length(x$axes[[1L]]))
    } else {
      ex <- range(i)
      dnames[[1L]] <- x$axes[[1L]]$dimnames[seq(ex[1L], ex[2L])]
      lon <- ex[1L]:ex[2L]
    }
    if (missing(j)) {
      dnames[[2L]] <- x$axes[[2L]]$dimnames
      lat <- 1L:(length(x$axes[[2L]]))
    } else {
      ex <- range(j)
      dnames[[2L]] <- x$axes[[2L]]$dimnames[seq(ex[1L], ex[2L])]
      lat <- ex[1L]:ex[2L]
    }
    data <- data[lon, lat]
  }

  dimnames(data) <- dnames
  ax <- sapply(x$axes, function(x) if (!inherits(x, "CFAxisScalar")) x$orientation)
  ax <- ax[lengths(ax) > 0L]
  attr(data, "axis") <- ax
  data
}
