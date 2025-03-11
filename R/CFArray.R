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
#'   `summarise()` method. The data is returned as a new `CFArray` instance.
#'
#'   In general, the metadata from the netCDF resource will be lost when
#'   exporting to a different format insofar as those metadata are not
#'   recognized by the different format.
#'
#' @docType class
#'
#' @export
CFArray <- R6::R6Class("CFArray",
  inherit = CFVariableBase,
  private = list(
    # Orient self$values in such a way that it conforms to regular R arrays: axis
    # order will be Y-X-Z-T-others and Y values will go from the top to the bottom.
    # Returns a new array.
    orient = function() {
      order <- private$YXZT()
      if (sum(order) == 0L) {
        warning("Cannot orient data array because axis orientation has not been set")
        return(self$values)
      }
      if (all(order == 1L:4L))
        out <- self$values
      else {
        all_dims <- seq(length(dim(self$values)))
        perm <- c(order[which(order > 0L)], all_dims[!(all_dims %in% order)])
        out <- aperm(self$values, perm)
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
    },

    # Get all the data values
    get_values = function() {
      self$values
    },

    # Internal apply/tapply over the temporal dimension.
    process_data = function(tdim, fac, fun, ...) {
      .process.data(self$values, tdim, fac, fun, ...)
    }
  ),
  public = list(
    #' @field values The data of this object.
    values = NULL,

    #' @description Create an instance of this class.
    #' @param name The name of the object.
    #' @param group The group that this data should live in. This is usually an
    #'   in-memory group, but it could be a regular group if the data is
    #'   prepared for writing into a new netCDF file.
    #' @param values The data of this object. The structure of the data depends
    #'   on the method that produced it.
    #' @param axes A `list` of [CFAxis] descendant instances that describe the
    #'   axes of the argument `value`.
    #' @param crs The [CFGridMapping] instance of this data object, or `NULL`
    #'   when no grid mapping is available.
    #' @param attributes A `data.frame` with the attributes associated with the
    #'   data in argument `value`.
    #' @return An instance of this class.
    initialize = function(name, group, values, axes, crs, attributes) {
      # FIXME: Various other data types
      first <- typeof(as.vector(values)[1L])
      dt <- if (first == "double") "NC_DOUBLE"
            else if (first == "integer") "NC_INT"
            else stop("Unsupported data type for the values", call. = FALSE)

      var <- NCVariable$new(-1L, name, group, dt, 0L, NULL)
      var$attributes <- attributes
      super$initialize(var, group, axes, crs)

      self$values <- values
      if (!all(is.na(self$values)))
        self$set_attribute("valid_range", dt, range(values, na.rm = TRUE))
    },

    #' @description Print a summary of the data object to the console.
    print = function() {
      cat("<Data array>", self$name, "\n")
      longname <- self$attribute("long_name")
      if (!is.na(longname) && longname != self$name)
        cat("Long name:", longname, "\n")

      if (all(is.na(self$values))) {
        cat("\nValues: -\n")
        cat(sprintf("    NA: %d (100%%)\n", length(self$values)))
      } else {
        rng <- range(self$values, na.rm = TRUE)
        units <- self$attribute("units")
        if (is.na(units)) units <- ""
        cat("\nValues: [", rng[1L], " ... ", rng[2L], "] ", units, "\n", sep = "")
        NAs <- sum(is.na(self$values))
        cat(sprintf("    NA: %d (%.1f%%)\n", NAs, NAs * 100 / length(self$values)))
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
      dimnames(self$values) <- self$dimnames
      self$values
    },

    #' @description Retrieve the data in the object in the form of an R array,
    #' with axis ordering Y-X-others and Y values going from the top down.
    #' @return An `array` of data in R ordering.
    array = function() {
      if (length(self$axes) < 2L)
        stop("Cannot create an array from data object with only one axis.", call. = FALSE)

      dimnames(self$values) <- self$dimnames
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
      numdims <- length(dim(self$values))
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
      dt[ , eval(self$name) := self$values]

      long_name <- self$attribute("long_name")
      if (is.na(long_name)) long_name <- ""
      units <- self$attribute("units")
      if (is.na(units)) units <- ""
      data.table::setattr(dt, "value", list(name = long_name, units = units))
      dt
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
      dim_axes <- length(dim(self$values))
      axis_names <- sapply(self$axes, function(ax) ax$name)
      RNetCDF::var.def.nc(nc, self$name, self$NCvar$vtype, axis_names[1L:dim_axes])
      if (length(self$axes) > dim_axes)
        self$set_attribute("coordinates", "NC_CHAR", paste(axis_names[-(1L:dim_axes)]))
      self$write_attributes(nc, self$name)
      RNetCDF::var.put.nc(nc, self$name, self$values)

      RNetCDF::close.nc(nc)
      invisible(self)
    }
  ),
  active = list(
    #' @field dimnames (read-only) Retrieve dimnames of the data object.
    dimnames = function(value) {
      if (missing(value)) {
        len <- length(dim(self$values))
        dn <- lapply(1:len, function(ax) dimnames(self$axes[[ax]]))
        names(dn) <- sapply(1:len, function(ax) self$axes[[ax]]$name)
        dn
      }
    }
  )
)
