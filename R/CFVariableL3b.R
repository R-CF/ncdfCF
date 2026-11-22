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
  cloneable = FALSE,
  private = list(
    # Minimum and maximum row in the L3b variable.
    .file_rows = c(0L, 0L),

    # Minimum and maximum bin in the L3b variable.
    .file_bins = c(0L, 0L),

    # The index data of the L3b structure.
    .index = NULL,

    # The values from the file after reading.
    .values = NULL,

    # Read the bins for a window of the grid and expand them to a matrix.
    # `start` and `count` are validated and relative to the axes. Only the
    # records of the requested rows are read from file.
    as_matrix = function(start, count) {
      idx  <- private$.index
      rows <- private$.file_rows[1L] + start[1L] - 2L + seq_len(count[1L])
      bins <- private$.file_bins[1L] + start[2L] - 2L + seq_len(count[2L])
      maxBins <- idx$max[length(idx$max) * 0.5]
      out <- matrix(NA_real_, count[1L], count[2L])

      ext <- idx$extent[rows]
      n <- sum(ext)
      if (!n) return(out)

      # Records are ordered by bin number: the rows form one contiguous range
      offset <- sum(idx$extent[seq_len(rows[1L] - 1L)]) + 1
      h <- private$.NCobj$group$handle
      binList <- RNetCDF::var.get.nc(h, "BinList", offset, n)
      binData <- RNetCDF::var.get.nc(h, self$name, offset, n)
      d <- binData$sum / binList$weights
      last <- cumsum(ext)

      for (r in which(ext > 0)) {
        rec <- seq.int(last[r] - ext[r] + 1, last[r])
        row_max <- idx$max[rows[r]]
        v <- rep(NA_real_, row_max)
        v[binList$bin_num[rec] - idx$start_num[rows[r]] + 1] <- d[rec]
        out[r, ] <- v[ceiling(bins * row_max / maxBins)]   # expand to the grid
      }
      out
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
      ncv <- grp$NC$NCvars
      var <- ncv[[units[1L]]]
      if (is.null(ncv[["BinIndex"]]) || is.null(ncv[["BinList"]]) || (is.null(var)))
        stop("L3b: required netCDF variables not found.", call. = FALSE) # nocov

      # Read the index
      private$.index <- RNetCDF::var.get.nc(grp$NC$handle, "BinIndex", unpack = TRUE, fitnum = TRUE)
      lat_len <- length(private$.index$start)

      # Latitude axis
      lat_rows <- which(private$.index$begin > 0)
      private$.file_rows <- as.integer(range(lat_rows))
      lat <- CFAxisLatitude$new("latitude", grp, values = (private$.file_rows[1L]:private$.file_rows[2L] - 0.5) * 180 / lat_len - 90)
      len <- as.integer(private$.file_rows[2L] - private$.file_rows[1L]) + 1L
      bnds <- (private$.file_rows[1L] - 1):private$.file_rows[2L] * 180 / lat_len - 90
      lat$bounds <- CFBounds$new("lat_bnds", grp, values = rbind(bnds[-len], bnds[-1L]))

      # Longitude axis
      lon_bins <- private$.index$max[lat_len * 0.5]
      west <- private$.index$begin[lat_rows] - private$.index$start_num[lat_rows] + 1
      east <- west + private$.index$extent[lat_rows] - 1
      expand <- lon_bins / private$.index$max[lat_rows]
      private$.file_bins[1L] <- min(floor(west * expand))
      private$.file_bins[2L] <- max(ceiling(east * expand))
      lon <- CFAxisLongitude$new("longitude", grp, values = (private$.file_bins[1L]:private$.file_bins[2L] - 0.5) * 360 / lon_bins - 180)
      bnds <- (private$.file_bins[1L]-1):private$.file_bins[2L] * 360 / lon_bins - 180
      len <- private$.file_bins[2L] - private$.file_bins[1L] + 1
      lon$bounds <- CFBounds$new("lon_bnds", grp, values = rbind(bnds[-len], bnds[-1L]))

      axes <- list(
        latitude = lat,
        longitude = lon
      )

      # Scalar time axis
      tc_start <- grp$parent$attribute("time_coverage_start")
      tc_end <- grp$parent$attribute("time_coverage_end")
      if (!is.na(tc_start) && !is.na(tc_end)) {
        tc <- CFtime::CFTime$new("seconds since 1970-01-01", "proleptic_gregorian", c(tc_start, tc_end))
        ymd <- tc$calendar$offset2date(mean(tc$offsets) / 86400)
        cft <- CFtime::CFTime$new("seconds since 1970-01-01", "proleptic_gregorian")
        cft <- cft + CFtime::parse_timestamps(cft, sprintf("%04d-%02d-%02dT12:00:00", ymd$year, ymd$month, as.integer(ymd$day)))$offset
        cft$set_bounds(matrix(tc$offsets, nrow = 2))
        axes[["time"]] <- CFAxisTime$new("time", grp, cft)
      }

      # CRS
      gm <- CFGridMapping$new("geo", grp, "latitude_longitude")
      gm$set_attribute("semi_major_axis", "NC_DOUBLE", 6378145)
      gm$set_attribute("inverse_flattening", "NC_DOUBLE", 0)
      gm$set_attribute("prime_meridian_name", "NC_CHAR", "Greenwich")

      # Construct the object
      super$initialize(var, grp, axes)

      # The NC variable is a 1-D list of bins; this object presents the gridded view.
      private$.dims <- c(lat$length, lon$length)
      private$.data_type <- "NC_DOUBLE"

      self$crs <- gm
      self$set_attribute("grid_mapping", "NC_CHAR", "geo")
      self$set_attribute("units", "NC_CHAR", units[2L])
      ncv[["BinIndex"]]$CF <- ncv[["BinList"]]$CF <- self
    },

    #' @description Read all the data of the variable and cache it.
    #' @param refresh Should the data be read from file again? Default `FALSE`.
    #' @return An array of data, invisibly.
    read_data = function(refresh = FALSE) {
      if (is.null(private$.values) || refresh) {
        private$.values <- private$as_matrix(c(1L, 1L), private$.dims)
        self$set_attribute("actual_range", private$.data_type,
                           range(private$.values, na.rm = TRUE))
      }
      invisible(private$.values)
    },

    #' @description Read a window of the data. The data is not cached.
    #' @param start,count Vectors of indices to start reading and the number
    #'   of elements to read, along each axis; `NA` to read everything.
    #' @return An array of data, or `NULL` if there is no data.
    read_chunk = function(start, count) {
      sc <- private$check_start_count(start, count)
      if (!length(sc)) return(NULL)
      if (!is.null(private$.values))
        return(super$read_chunk(sc$start, sc$count))
      private$as_matrix(sc$start, sc$count)
    },

    #' @description As `read_chunk()`, with a warning if the window exceeds
    #'   `CF.options$memory_cell_limit`. The window is read in one pass.
    #' @param start,count As for `read_chunk()`.
    #' @return An array of data, or `NULL` if there is no data.
    read_window = function(start, count) {
      sc <- private$check_start_count(start, count)
      if (!length(sc)) return(NULL)
      private$check_memory_limit(sc$count)
      self$read_chunk(sc$start, sc$count)
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

      nd <- length(private$.dims)
      start <- rep(1L, nd)
      count <- private$.dims
      grp <- CFGroup$new("", NULL)

      out_axes <- lapply(seq_along(self$axes), function(ax) {
        axis <- self$axes[[ax]]
        rng <- if (ax <= nd) selectors[[ axis_names[ax] ]] %||% selectors[[ axis$orientation ]]
        if (is.null(rng)) return(axis$copy(group = grp))
        idx <- axis$slice(rng)
        if (is.null(idx)) return(NULL)
        start[ax] <<- idx[1L]
        count[ax] <<- idx[2L] - idx[1L] + 1L
        axis$subset(group = grp, rng = idx)
      })
      if (any(vapply(out_axes[seq_len(nd)], is.null, TRUE))) return(NULL)
      names(out_axes) <- sapply(out_axes, function(a) a$name)

      # Read the data and assemble the CFVariable instance
      d <- self$read_window(start, count)
      v <- CFVariable$new(self$name, group = grp, values = d, axes = out_axes,
                          attributes = self$attributes)
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
#'   a number of elements along each axis. `i` is for the latitude axis, `j`
#'   for the longitude axis, `...` (additional named arguments) is invalid as
#'   there are only two axes to subset from. If either expression is missing,
#'   the entire axis is extracted. The values for the arguments should be an
#'   integer vector; the range of the values in the vector will be used. See
#'   examples, below.
#' @param drop Logical, ignored. Axes are never dropped. Any degenerate
#'   dimensions of the array are returned as such, with dimnames and appropriate
#'   attributes set.
#'
#' @return An array with dimnames and other attributes set.
#' @export
#' @aliases bracket_select_l3b
#' @docType methods
#' @examples
#' \dontrun{
#' # A NASA Ocean Color level-3 binned file
#' ds <- open_ncdf("AQUA_MODIS.20030101.L3b.DAY.CHL.nc")
#' chl <- ds[["/level-3_binned_data/chlor_a"]]
#'
#' # The dimensions of the grid: latitude, longitude
#' chl$dim()
#'
#' # Chlorophyll concentration over part of the tropical Atlantic Ocean
#' x <- chl[1712:1752, 4000:4300]
#' str(x)
#'
#' # A band of latitudes over the full longitude extent
#' band <- chl[1712:1752, ]
#' str(band)
#' }
"[.CFVariableL3b" <- function(x, i, j, ..., drop = FALSE) {
  d <- x$dim()
  lat <- if (missing(i)) c(1L, d[1L]) else as.integer(range(i))
  lon <- if (missing(j)) c(1L, d[2L]) else as.integer(range(j))

  data <- x$read_window(c(lat[1L], lon[1L]), c(lat[2L] - lat[1L] + 1L, lon[2L] - lon[1L] + 1L))

  dn <- x$dimnames
  dn[[1L]] <- dn[[1L]][lat[1L]:lat[2L]]
  dn[[2L]] <- dn[[2L]][lon[1L]:lon[2L]]
  dimnames(data) <- dn

  attr(data, "axis") <- list(latitude = "Y", longitude = "X")
  data
}
