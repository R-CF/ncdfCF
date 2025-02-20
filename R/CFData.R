# FIXME: summarise() returns new CFData in same group

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
#'   points on individual rows. Metadata is not maintained. Package `data.table`
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

    #' @field crs An instance of [CFGridMapping] or `NULL` when no grid mapping
    #' is available.
    crs = NULL,

    #' @description Create an instance of this class.
    #' @param name The name of the object.
    #' @param group The group that this data should live in. This is usually an
    #'   in-memory group, but it could be a regular group if the data is
    #'   prepared for writing into a new netCDF file.
    #' @param value The data of this object. The structure of the data depends
    #'   on the method that produced it.
    #' @param axes A `list` of [CFAxis] descendant instances that describe the
    #'   axes of the argument `value`.
    #' @param crs The [CFGridMapping] instance of this data object, or `NULL`
    #'   when no grid mapping is available.
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
      if (!is.na(longname) && longname != self$name)
        cat("Long name:", longname, "\n")

      if (all(is.na(self$value))) {
        cat("\nValues: -\n")
        cat(sprintf("    NA: %d (100%%)\n", length(self$value)))
      } else {
        rng <- range(self$value, na.rm = TRUE)
        units <- self$attribute("units")
        if (is.na(units)) units <- ""
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

    #' @description Return the time object from the axis representing time.
    #' @param want Character string with value "axis" or "time", indicating
    #' what is to be returned.
    #' @return If `want = "axis"` the [CFAxisTime] axis; if `want = "time"` the
    #' `CFTime` instance of the axis, or `NULL` if the variable does not have a
    #' "time" dimension.
    time = function(want = "axis") {
      ndx <- sapply(self$axes, inherits, "CFAxisTime")
      if (any(ndx))
        if (want == "axis") self$axes[[which(ndx)]]
        else self$axes[[which(ndx)]]$time()
      else NULL
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

      wkt <- if (is.null(self$crs)) .wkt2_crs_geo(4326L)
             else self$crs$wkt2(.wkt2_axis_info(self))
      arr <- self$array()
      numdims <- length(dim(self$value))
      dn <- dimnames(arr)
      if (numdims == 4L) {
        r <- terra::sds(arr, extent = ext, crs = wkt)
        for (d4 in seq_along(dn[[4L]]))
          names(r[d4]) <- dn[[3L]]
        names(r) <- dn[[4L]]
      } else {
        r <- terra::rast(arr, extent = ext, crs = wkt)
        if (numdims == 3L)
          names(r) <- dn[[3L]]
      }

      r
    },

    #' @description Retrieve the data in the object in the form of a
    #'   `data.table`. The `data.table` package needs to be installed for this
    #'   method to work.
    #' @return A `data.table` with all data points in individual rows. All axes,
    #'   including scalar axes, will become columns. The `name` of this data
    #'   variable will be used as the column that holds the data values. Two
    #'   attributes are added: `name` indicates the long name of this data
    #'   variable, `units` indicates the physical unit of the data values.
    data.table = function() {
      if (!requireNamespace("data.table", quietly = TRUE))
        stop("Please install package 'data.table' before using this functionality")
      .datatable.aware = TRUE

      exp <- expand.grid(lapply(self$axes, function(ax) ax$coordinates),
                         KEEP.OUT.ATTRS = FALSE, stringsAsFactors = FALSE)
      dt <- as.data.table(exp)
      nm <- names(dt)
      dt[ , eval(self$name) := self$value]

      long_name <- self$attribute("long_name")
      if (is.na(long_name)) long_name <- ""
      units <- self$attribute("units")
      if (is.na(units)) units <- ""
      data.table::setattr(dt, "value", list(name = long_name, units = units))
      dt
    },

    #' @description Summarise the temporal dimension of the data, if present, to
    #'   a lower resolution, using a user-supplied aggregation function.
    #' @param name Character string with the name for the summarised data.
    #' @param period The period to summarise to. Must be one of either "day",
    #'   "dekad", "month", "quarter", "season", "year". A "quarter" is the
    #'   standard calendar quarter such as January-March, April-June, etc. A
    #'   "season" is a meteorological season, such as December-February,
    #'   March-May, etc. (any December data is from the year preceding the
    #'   January data). The period must be of lower resolution than the
    #'   resolution of the time dimension.
    #' @param fun A function or a symbol or character string naming a function
    #'   that will be applied to each grouping of data.
    #' @return A new `CFData` object in the same group as `self` with the
    #' summarised data.
    summarise = function(name, period, fun) {
      if (missing(name) || missing(period) || missing(fun))
        stop("Arguments 'name', 'period' and 'fun' are required.", call. = FALSE)
      if (!(period %in% c("day", "dekad", "month", "quarter", "season", "year")))
        stop("Argument 'period' has invalid value.", call. = FALSE)

      # Find the time object, create the factor
      tax <- self$time("axis")
      if (is.null(tax))
        stop("No 'time' dimension found to summarise on.", call. = FALSE)
      fac <- try(tax$time()$factor(period), silent = TRUE)
      if (inherits(fac, "try-error"))
        stop("The time dimension is too short to summarise on.", call. = FALSE)

      # Make a new time axis for the result
      new_tm <- attr(fac, "CFTime")
      var <- NCVariable$new(-1L, tax$name, self$group, "NC_DOUBLE", 1L, NULL)
      len <- length(new_tm)
      new_ax <- if (len == 1L)
        CFAxisScalar$new(self$group, var, "T", new_tm)
      else {
        dim <- NCDimension$new(-1L, tax$name, len, FALSE)
        CFAxisTime$new(self$group, var, dim, new_tm)
      }

      # Summarise
      num_dim_axes <- length(dim(self$value))
      if (num_dim_axes == 1L) {
        dt <- tapply(self$value, fac, fun, na.rm = TRUE)
        ax <- new_ax
      } else {
        tm <- sum(private$YXZT() > 0L) # Test which oriented axes are present, T is the last one
        perm <- seq(num_dim_axes)
        dt <- apply(self$array(), perm[-tm], tapply, fac, fun, na.rm = TRUE)
        perm <- c(perm[2L:tm], 1L, perm[-(1L:tm)])
        dt <- aperm(dt, perm)

        # Organise the axes
        ax <- self$axes
        ax[[tm]] <- new_ax

        # Fix name of time dimension in dimnames
        dn <- dimnames(dt)
        axn <- names(dn)
        axn[tm] <- tax$name
        names(dn) <- axn
        dimnames(dt) <- dn
      }

      # Attributes
      atts <- self$attributes
      # FIXME: set cell_methods

      # Create the output
      out <- CFData$new(name, self$group, dt, ax, self$crs, atts)
    },

    #' @description Plot a 2D slice of data to the display.
    #' @param ... Arguments passed to the `base::plot()` function.
    plot = function(...) {
      image(self$axes[["lon"]]$values, self$axes[["lat"]]$values, self$raw(),
            xlab = "Longitude", ylab = "Latitude", useRaster = T,
            xaxp = c(0, 360, 8), yaxp = c(-90, 90, 4))
    },

    #' @description Save the data object to a netCDF file.
    #' @param fn The name of the netCDF file to create.
    #' @return Self, invisibly.
    save = function(fn) {
      nc <- RNetCDF::create.nc(fn, prefill = FALSE, format = "netcdf4")
      if (!inherits(nc, "NetCDF"))
        stop("Could not create the netCDF file. Please check that the location of the supplied file name is writable.", call. = FALSE)

      # Global attributes
      self$group$set_attribute("Conventions", "NC_CHAR", "CF-1.12")
      self$group$write_attributes(nc, "NC_GLOBAL")

      # Axes
      lapply(self$axes, function(ax) ax$write(nc))

      # CRS
      if (!is.null(self$crs)) {
        self$crs$write(nc)
        self$set_attribute("grid_mapping", "NC_CHAR", self$crs$name)
      }

      # Data variable
      # FIXME: Pack data
      dim_axes <- length(dim(self$value))
      axis_names <- sapply(self$axes, function(ax) ax$name)
      RNetCDF::var.def.nc(nc, self$name, self$NCvar$vtype, axis_names[1L:dim_axes])
      if (length(self$axes) > dim_axes)
        self$set_attribute("coordinates", "NC_CHAR", paste(axis_names[-(1L:dim_axes)]))
      self$write_attributes(nc, self$name)
      RNetCDF::var.put.nc(nc, self$name, self$value)

      RNetCDF::close.nc(nc)
      invisible(self)
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
