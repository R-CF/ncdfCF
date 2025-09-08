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
    .file_rows = c(0L, 0L),

    # Minimum and maximum bin in the L3b variable.
    .file_bins = c(0L, 0L),

    # The index data of the L3b structure.
    .index = NULL,

    # The values from the file after reading.
    .values = NULL,

    # Read all the data from the file and turn the data into a
    #   matrix. If an `aoi` is specified, the data will be subset to that area.
    #
    #   This method returns a bare-bones matrix without any metadata or other
    #   identifying information. Use method `data()`, `subset()` or the `[`
    #   operator rather than this method to obtain a more informative result.
    # Params start and count are the vectors to read data from the netCDF file.
    # Returns a matrix with the data of the variable in raw format.
    as_matrix = function(start, count) {
      binList <- RNetCDF::var.get.nc(private$.NCvar$group$handle, "BinList")
      binData <- RNetCDF::var.get.nc(private$.NCvar$group$handle, self$name)

      # Get the binned data in rows
      data_row <- findInterval(binList$bin_num, private$.index$start_num)
      data_row_bin <- as.integer(binList$bin_num - private$.index$start_num[data_row]) + 1L

      numRows <- length(private$.index$start_num)
      maxBins <- private$.index$max[numRows * 0.5]

      if (missing(start)) {
        startRow <- private$.file_rows[1L]
        startBin <- private$.file_bins[1L]
        endRow <- private$.file_rows[2L]
        endBin <- private$.file_bins[2L]
      } else {
        startRow <- private$.file_rows[1L] + start[1L] - 1L
        startBin <- private$.file_bins[1L] + start[2L] - 1L
        endRow <- startRow + count[1L] - 1L
        endBin <- startBin + count[2L] - 1L
      }

      l <- lapply(startRow:endRow, function(r) {
        out <- rep(NA_real_, maxBins)

        idx <- which(data_row == r)
        if (!length(idx)) return(out[startBin:endBin])

        d <- binData$sum[idx] / binList$weights[idx]   # The data of the physical property
        b <- data_row_bin[idx]                         # The bins to put the data in

        # Expand to maxBins grid cells
        allocate <- ceiling(1:maxBins * private$.index$max[r] / maxBins)

        # Allocate the data to the expanded grid cells
        for (bin in seq_along(b))
          out[which(allocate == b[bin])] <- d[bin]

        out[startBin:endBin]
      })
      do.call(rbind, l)
    },

    # Read all the data from the file and turn the data into a matrix.
    #
    #   This method returns a bare-bones matrix without any metadata or other
    #   identifying information. Use method `subset()` or the `[`
    #   operator rather than this method to obtain a more informative result.
    # @param refresh Flag to indicate if a fresh read from file should be made.
    # @return A matrix with the data of the variable in raw format.
    read_data = function(refresh = FALSE) {
      if (is.null(private$.values) || refresh)
        private$.values <- private$as_matrix()
      private$.values
    },

    # Read some data from the file and turn the data into a matrix.
    #
    #   This method returns a bare-bones matrix without any metadata or other
    #   identifying information. Use method `subset()` or the `[` operator
    #   rather than this method to obtain a more informative result.
    # @param start,count Start and count vectors that delimit the geographic
    #   area to return data for.
    # @return A matrix with the data of the variable in raw format.
    read_chunk = function(start, count) {
      private$as_matrix(start, count)
    }
  ),
  public = list(
    #' @description Create an instance of this class.
    #'
    #' @param grp The group that this CF variable lives in. Must be called
    #'   "/level-3_binned_data".
    #' @param units Vector of two character strings with the variable name and
    #'   the physical units of the data variable in the netCDF resource.
    #' @return An instance of this class.
    initialize = function(grp, units) {
      ncv <- grp$NCvars
      var <- ncv[[units[1L]]]
      if (is.null(ncv[["BinIndex"]]) || is.null(ncv[["BinList"]]) || (is.null(var)))
        stop("L3b: required netCDF variables not found.", call. = FALSE) # nocov

      # Read the index
      private$.index <- RNetCDF::var.get.nc(grp$handle, "BinIndex", unpack = TRUE, fitnum = TRUE)
      lat_len <- length(private$.index$start)

      # Latitude axis
      lat_rows <- which(private$.index$begin > 0)
      private$.file_rows <- as.integer(range(lat_rows))
      lat <- CFAxisLatitude$new("latitude", values = (private$.file_rows[1L]:private$.file_rows[2L] - 0.5) * 180 / lat_len - 90)
      len <- as.integer(private$.file_rows[2L] - private$.file_rows[1L]) + 1L
      bnds <- (private$.file_rows[1L] - 1):private$.file_rows[2L] * 180 / lat_len - 90
      lat$bounds <- CFBounds$new("lat_bnds", values = rbind(bnds[-len], bnds[-1L]))

      # Longitude axis
      lon_bins <- private$.index$max[lat_len * 0.5]
      west <- private$.index$begin[lat_rows] - private$.index$start_num[lat_rows] + 1
      east <- west + private$.index$extent[lat_rows] - 1
      expand <- lon_bins / private$.index$max[lat_rows]
      private$.file_bins[1L] <- min(floor(west * expand))
      private$.file_bins[2L] <- max(ceiling(east * expand))
      lon <- CFAxisLongitude$new("longitude", values = (private$.file_bins[1L]:private$.file_bins[2L] - 0.5) * 360 / lon_bins - 180)
      bnds <- (private$.file_bins[1L]-1):private$.file_bins[2L] * 360 / lon_bins - 180
      len <- private$.file_bins[2L] - private$.file_bins[1L] + 1
      lon$bounds <- CFBounds$new("lon_bnds", values = rbind(bnds[-len], bnds[-1L]))

      axes <- list(
        latitude = lat,
        longitude = lon
      )

      # Scalar time axis
      tc_start <- grp$parent$attribute("time_coverage_start")
      tc_end <- grp$parent$attribute("time_coverage_end")
      if (!is.na(tc_start) && !is.na(tc_end)) {
        tc <- CFtime::CFTime$new("seconds since 1970-01-01", "proleptic_gregorian", c(tc_start, tc_end))
        ymd <- tc$cal$offset2date(mean(tc$offsets) / 86400)
        cft <- CFtime::CFTime$new("seconds since 1970-01-01", "proleptic_gregorian")
        cft <- cft + CFtime::parse_timestamps(cft, sprintf("%04d-%02d-%02dT12:00:00", ymd$year, ymd$month, as.integer(ymd$day)))$offset
        cft$set_bounds(matrix(tc$offsets, nrow = 2))
        axes[["time"]] <- CFAxisTime$new("time", cft)
      }

      grp$CFaxes <- axes

      # CRS
      gm <- CFGridMapping$new("geo", "latitude_longitude")
      gm$set_attribute("semi_major_axis", "NC_DOUBLE", 6378145)
      gm$set_attribute("inverse_flattening", "NC_DOUBLE", 0)
      gm$set_attribute("prime_meridian_name", "NC_CHAR", "Greenwich")
      self$crs <- gm
      self$set_attribute("grid_mapping", "NC_CHAR", "geo")

      # Construct the object
      super$initialize(var, axes)
      self$set_attribute("units", "NC_CHAR", units[2L])
      # FIXME
      ncv[["BinIndex"]]$CF <- ncv[["BinList"]]$CF <- self
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
    #'   [CFVariable]$array() method for this.
    #'
    #' @param ... One or more arguments of the form `axis = range`. The "axis"
    #'   part should be the name of axis `longitude` or `latitude` or its
    #'   orientation `X` or `Y`. The "range" part is a vector of values
    #'   representing coordinates along the axis where to extract data. Axis
    #'   designators and names are case-sensitive and can be specified in any
    #'   order. If values for the range of an axis fall outside of the extent of
    #'   the axis, the range is clipped to the extent of the axis.
    #' @param rightmost.closed Single logical value to indicate if the upper
    #'   boundary of range in each axis should be included.
    #'
    #' @return A [CFVariable] instance, having an array with axes and attributes of
    #'   the variable, or `NULL` if one or more of the elements in the `...`
    #'   argument falls entirely outside of the range of the axis. Note that
    #'   degenerate dimensions (having `length(.) == 1`) are dropped from the
    #'   array but the corresponding axis is maintained in the result as a
    #'   scalar axis.
    subset = function(..., rightmost.closed = FALSE) {
      # Organize the selectors
      selectors <- list(...)
      if (is.list(selectors[[1L]]))
        selectors <- selectors[[1L]]
      sel_names <- names(selectors)
      axis_names <- names(self$axes)
      axis_order <- private$check_names(sel_names)

      start <- c(1L, 1L)
      count <- c(self$axes[[1L]]$length, self$axes[[2L]]$length)

      out_axes_dim <- list()
      out_axes_other <- list()
      for (ax in seq_along(self$axes)) {
        axis <- self$axes[[ax]]
        orient <- axis$orientation

        rng <- NULL
        if (is.null(rng)) rng <- selectors[[ axis_names[ax] ]]
        if (is.null(rng)) rng <- selectors[[ orient ]]
        if (is.null(rng)) {
          out_axis <- axis$copy()
        } else {
          idx <- axis$slice(rng)
          if (is.null(idx)) return(NULL)
          start[ax] <- idx[1L]
          count[ax] <- idx[2L] - idx[1L] + 1L
          out_axis <- axis$subset(rng = idx)
        }

        # Collect axes for result
        if (out_axis$length == 1L)
          out_axes_other <- append(out_axes_other, out_axis)
        else
          out_axes_dim <- append(out_axes_dim, out_axis)
      }

      # Read the data
      d <- private$read_chunk(start, count)
      d <- drop(d)

      # Assemble the CFVariable instance
      axes <- c(out_axes_dim, out_axes_other)
      names(axes) <- sapply(axes, function(a) a$name)
      v <- CFVariable$new(self$name, values = d, axes = axes, attributes = self$attributes)
      v$crs <- self$crs
      v
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
  data <- x$raw()

  if (missing(i) && missing(j)) {
    dnames <- list(longitude = x$axes[[1L]]$dimnames, latitude = x$axes[[2L]]$dimnames)
  } else {
    dnames <- vector("list", 2L)
    names(dnames) <- x$axis_names[1L:2L]
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
  ax <- sapply(x$axes, function(x) if (x$length > 1L) x$orientation)
  ax <- ax[lengths(ax) > 0L]
  attr(data, "axis") <- ax
  data
}
