#' Data extracted from a CF data variable
#'
#' @description This class holds the data that is extracted from a [CFVariable],
#'   using the `data()` or `subset()` method. The instance of this class will
#'   additionally have the axes and other relevant information such as its
#'   attributes (as well as those of the axes) and the coordinate reference
#'   system.
#'
#'   The class has a number of utility functions to extract the data in specific
#'   formats:
#' * `raw()`: The data without any further processing. The axes are
#'   as they are stored in the netCDF resource; there is thus no guarantee as to
#'   how the data is organized in the array. Dimnames will be set.
#' * `array()`: An array of the data which is organized as a standard R array
#'   with the axes of the data permuted to Y-X-others and Y-values in decreasing
#'   order. Dimnames will be set.
#' * `terra()`: The data is returned as a `terra::SpatRaster` (3D) or
#'   `terra::SpatRasterDataset` (4D) object, with all relevant structural
#'   metadata set. Package `terra` must be installed for this to work.
#' * `data.table()`: The data is returned as a `data.table`, with all data
#'   points on inidivual rows. Metadata is not maintained. Package `data.table`
#'   must be installed for this to work.
#'
#'   The temporal dimension of the data, if present, may be summarised using the
#'   `summarise()` method. The data is returned as an array in the standard R
#'   format.
#'
#'   In general, the metadata from the netCDF resource will be lost when
#'   exporting to a different format insofar as those metadata are not
#'   recognized by the different format.
#'
#' @docType class
#'
#' @export
CFData <- R6::R6Class("CFData",
  inherit = CFObject,
  private = list(
    # Return the order of dimensional axes that "receive special treatment".
    # Scalar axes are not considered here.
    YXZT = function() {
      orient <- sapply(1:length(dim(self$value)), function(x) self$axes[[x]]$orientation)
      match(c("Y", "X", "Z", "T"), orient, nomatch = 0L)
    },

    # Orient self$value in such a way that it conforms to regular R arrays: axis
    # order will be Y-X-Z-T-others and Y values will go from the top to the bottom.
    # Returns a new array.
    orient = function() {
      order <- private$YXZT()
      if (sum(order) == 0L) {
        warning("Cannot orient data array because axis orientation has not been set")
        return(self$value)
      }
      if (all(order == 1L:4L))
        out <- self$value
      else {
        all_dims <- seq(length(dim(self$value)))
        perm <- c(order[which(order > 0L)], all_dims[!(all_dims %in% order)])
        out <- aperm(self$value, perm)
      }

      # Flip Y-axis, if necessary
      ynames <- dimnames(out)[[1L]]
      if (length(ynames) > 1L && as.numeric(ynames[2L]) > as.numeric(ynames[1L])) {
        dn <- dimnames(out)
        dims <- dim(out)
        dim(out) <- c(dims[1L], prod(dims[-1L]))
        out <- apply(out, 2L, rev)
        dim(out) <- dims
        dn[[1L]] <- rev(dn[[1L]])
        dimnames(out) <- dn
      }

      out
    }
  ),
  public = list(
    #' @field value The data of this object. The structure of the data depends
    #' on the method that produced it. Typical structures are an array or a
    #' `data.table`.
    value = NULL,

    #' @field axes List of instances of classes descending from [CFAxis] that
    #'   are the axes of the data object. If there are any scalar axes, they are
    #'   listed after the axes that associate with the dimensions of the data.
    #'   (In other words, axes `1..n` describe the `1..n` data dimensions, while
    #'   any axes `n+1..m` are scalar axes.)
    axes  = list(),

    #' @field crs Character string of the WKT2 of the CRS of the data object.
    crs = "",

    #' @description Create an instance of this class.
    #' @param name The name of the object.
    #' @param group The group that this data should live in. This is usually an
    #' in-memory group, but it could be a regular group if the data is prepared
    #' for writing into a new netCDF file.
    #' @param value The data of this object. The structure of the data depends
    #'   on the method that produced it.
    #' @param axes A `list` of [CFAxis] descendant instances that describe the
    #'   axes of the argument `value`.
    #' @param crs The WKT string of the CRS of this data object.
    #' @param attributes A `data.frame` with the attributes associated with the
    #'   data in argument `value`.
    #' @return An instance of this class.
    initialize = function(name, group, value, axes, crs, attributes) {
      var <- NCVariable$new(-1L, name, group, "NC_FLOAT", 0L, NULL)
      var$attributes <- attributes
      super$initialize(var, group)

      self$value <- value
      self$axes <- axes
      self$crs <- crs
    },

    #' @description Print a summary of the data object to the console.
    print = function() {
      cat("<Data>", self$name, "\n")
      longname <- self$attribute("long_name")
      if (nzchar(longname) && longname != self$name)
        cat("Long name:", longname, "\n")

      if (all(is.na(self$value))) {
        cat("\nValues: -\n")
        cat(sprintf("    NA: %d (100%%)\n", length(self$value)))
      } else {
        rng <- range(self$value, na.rm = TRUE)
        units <- self$attribute("units")
        cat("\nValues: [", rng[1L], " ... ", rng[2L], "] ", units, "\n", sep = "")
        NAs <- sum(is.na(self$value))
        cat(sprintf("    NA: %d (%.1f%%)\n", NAs, NAs * 100 / length(self$value)))
      }

      cat("\nAxes:\n")
      axes <- do.call(rbind, lapply(self$axes, function(a) a$brief()))
      axes <- lapply(axes, function(c) if (all(c == "")) NULL else c)
      axes$group <- NULL
      axes <- as.data.frame(axes[lengths(axes) > 0L])
      print(.slim.data.frame(axes, 50L), right = FALSE, row.names = FALSE)

      self$print_attributes()
    },

    #' @description Retrieve the data in the object exactly as it was produced
    #' by the operation on `CFVariable`.
    #' @return The data in the object. This is usually an `array` with the
    #' contents along axes varying.
    raw = function() {
      dimnames(self$value) <- self$dimnames
      self$value
    },

    #' @description Retrieve the data in the object in the form of an R array,
    #' with axis ordering Y-X-others and Y values going from the top down.
    #' @return An `array` of data in R ordering.
    array = function() {
      if (length(self$axes) < 2L)
        stop("Cannot create an array from data object with only one axis.", call. = FALSE)

      dimnames(self$value) <- self$dimnames
      private$orient()
    },

    #' @description Convert the data to a `terra::SpatRaster` (3D) or a
    #' `terra::SpatRasterDataset` (4D) object. The data
    #' will be oriented to North-up. The 3rd dimension in the data will become
    #' layers in the resulting `SpatRaster`, any 4th dimension the data sets.
    #' The `terra` package needs to be installed for this method to work.
    #' @return A `terra::SpatRaster` or `terra::SpatRasterDataset` instance.
    terra = function() {
      if (!requireNamespace("terra", quietly = TRUE))
        stop("Please install package 'terra' before using this funcionality")

      # Extent
      YX <- private$YXZT()[1L:2L]
      if (!all(YX))
        stop("Cannot create `terra` object because data does not have X and Y axes.", call. = FALSE)

      Xbnds <- self$axes[[YX[2L]]]$bounds
      if (inherits(Xbnds, "CFBounds")) Xbnds <- Xbnds$range()
      else {
        vals <- self$axes[[YX[2L]]]$values
        halfres <- (vals[2L] - vals[1L]) * 0.5 # this assumes regular spacing
        Xbnds <- c(vals[1L] - halfres, vals[length(vals)] + halfres)
      }
      Ybnds <- self$axes[[YX[1L]]]$bounds
      if (inherits(Ybnds, "CFBounds")) Ybnds <- Ybnds$range()
      else {
        vals <- self$axes[[YX[1L]]]$values
        halfres <- (vals[2L] - vals[1L]) * 0.5 # this assumes regular spacing
        Ybnds <- c(vals[1L] - halfres, vals[length(vals)] + halfres)
      }
      if (Ybnds[1L] > Ybnds[2L]) Ybnds <- rev(Ybnds)
      ext <- round(c(Xbnds, Ybnds), 4) # Round off spurious "accuracy"

      arr <- self$array()
      numdims <- length(dim(self$value))
      dn <- dimnames(arr)
      if (numdims == 4L) {
        r <- terra::sds(arr, extent = ext, crs = self$crs)
        for (d4 in seq_along(dn[[4L]]))
          names(r[d4]) <- dn[[3L]]
        names(r) <- dn[[4L]]
      } else {
        r <- terra::rast(arr, extent = ext, crs = self$crs)
        if (numdims == 3L)
          names(r) <- dn[[3L]]
      }

      r
    },

    #' @description Retrieve the data in the object in the form of a
    #' `data.table`. The `data.table` package needs to be installed for this
    #' method to work.
    #' @return A `data.table` with all data points in individual rows.
    data.table = function() {
      if (!requireNamespace("data.table", quietly = TRUE))
        stop("Please install package 'data.table' before using this funcionality")

      dt <- data.table::as.data.table(self$raw())
      long_name <- self$attribute("long_name")
      units <- self$attribute("units")
      data.table::setattr(dt, "value", list(name = long_name, units = units))
      dt
    },

    #' @description Summarise the temporal dimension of the data, if present, to
    #'   a lower resolution, using a user-supplied aggregation function.
    #' @param period The period to summarise to. Must be one of either "day",
    #'   "dekad", "month", "quarter", "season", "year". A "quarter" is the
    #'   standard calendar quarter such as January-March, April-June, etc. A
    #'   "season" is a meteorological season, such as December-February,
    #'   March-May, etc. (any December data is from the year preceding the
    #'   January data). The period must be of lower resolution than the
    #'   resolution of the time dimension.
    #' @param fun A function or a symbol or character string naming a function
    #'   that will be applied to each grouping of data.
    summarise = function(period, fun) {
      if (missing(period) || missing(fun))
        stop("Arguments 'period' and 'fun' ar required.", call. = FALSE)
      if (!(period %in% c("day", "dekad", "month", "quarter", "season", "year")))
        stop("Argument 'period' has invalid value.", call. = FALSE)

      # Find the time axis, create the factor
      tm <- which(sapply(self$axes, function(x) inherits(x, "CFAxisTime")))
      if (!length(tm))
        stop("No 'time' dimension found to summarise on.", call. = FALSE)
      fac <- self$axes[[tm]]$time()$factor(period)
      nm <- self$axes[[tm]]$name

      # Summarise
      num_dim_axes <- length(dim(self$value))
      if (num_dim_axes == 1L) {
        tapply(self$value, fac, fun, na.rm = TRUE)
      } else {
        tm <- sum(private$YXZT() > 0L) # Test which oriented axes are present, T is the last one
        perm <- seq(num_dim_axes)
        out <- apply(self$array(), perm[-tm], tapply, fac, fun, na.rm = TRUE)
        perm <- c(perm[2L:tm], 1L, perm[-(1L:tm)])
        out <- aperm(out, perm)

        # Fix name of time dimension in dimnames
        dn <- dimnames(out)
        axn <- names(dn)
        axn[tm] <- nm
        names(dn) <- axn
        dimnames(out) <- dn
        out
      }
    }
  ),
  active = list(
    #' @field dimnames (read-only) Retrieve dimnames of the data object.
    dimnames = function(value) {
      if (missing(value)) {
        len <- length(dim(self$value))
        dn <- lapply(1:len, function(ax) dimnames(self$axes[[ax]]))
        names(dn) <- sapply(1:len, function(ax) self$axes[[ax]]$name)
        dn
      }
    }
  )
)
