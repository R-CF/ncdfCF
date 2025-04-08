#' Array data extracted from a CF data variable
#'
#' @description This class holds the data that is extracted from a [CFVariable],
#'   using the `data()` or `subset()` method. The instance of this class will
#'   additionally have the axes and other relevant information such as its
#'   attributes (as well as those of the axes) and the coordinate reference
#'   system.
#'
#'   Otherwise, a `CFArray` is detached from the data set where it was derived
#'   from. It is self-contained in the sense that all its constituent parts
#'   (axes, bounds, attributes, etc) are available and directly linked to the
#'   instance. For performance reasons, axes and their parts (e.g. bounds) are
#'   shared between instances of `CFArray` and `CFVariable`.
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
#'   The temporal axis of the data, if present, may be summarised using the
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
    # The data of this object.
    values = NULL,

    # The range of the values.
    actual_range = c(NA_real_, NA_real_),

    # Orient private$values in such a way that it conforms to regular R arrays: axis
    # order will be Y-X-Z-T-others and Y values will go from the top to the bottom.
    # Alternatively, order private$values in the CF canonical order.
    # Argument ordering must be "R" or "CF".
    # Returns a new array with an attribute "axes" that lists axis names in the
    # order of the new array.
    orient = function(ordering = "R") {
      if (ordering == "R")
        order <- private$YXZT()
      else if (ordering == "CF")
        order <- private$XYZT()
      else
        stop("Invalid argument for ordering.", call. = FALSE)

      if (sum(order) == 0L) {
        warning("Cannot orient data array because axis orientation has not been set")
        return(private$values)
      }
      if (all(diff(order[which(order > 0L)]) > 0L)) {
        out <- private$values
        attr(out, "axes") <- private$dim_names()
      } else {
        all_dims <- seq(length(dim(private$values)))
        perm <- c(order[which(order > 0L)], all_dims[!(all_dims %in% order)])
        out <- aperm(private$values, perm)
        attr(out, "axes") <- private$dim_names()[perm]
      }

      if (ordering == "R") {
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
      }

      out
    },

    # Get all the data values
    get_values = function() {
      private$values
    },

    # Internal apply/tapply over the temporal axis.
    process_data = function(tdim, fac, fun, ...) {
      .process.data(private$values, tdim, fac, fun, ...)
    }
  ),
  public = list(
    #' @description Create an instance of this class.
    #' @param name The name of the object.
    #' @param group The group that this data should live in. This is usually an
    #'   in-memory group, but it could be a regular group if the data is
    #'   prepared for writing into a new netCDF file.
    #' @param values The data of this object. The structure of the data depends
    #'   on the method that produced it.
    #' @param values_type The unpacked netCDF data type for this object.
    #' @param axes A `list` of [CFAxis] descendant instances that describe the
    #'   axes of the argument `value`.
    #' @param crs The [CFGridMapping] instance of this data object, or `NULL`
    #'   when no grid mapping is available.
    #' @param attributes A `data.frame` with the attributes associated with the
    #'   data in argument `value`.
    #' @return An instance of this class.
    initialize = function(name, group, values, values_type, axes, crs, attributes) {
      var <- NCVariable$new(-1L, name, group, dt, 0L, NULL)
      var$attributes <- attributes
      super$initialize(var, group, axes, crs)

      private$values <- values
      private$values_type <- values_type
      if (!all(is.na(private$values))) {
        private$actual_range <- round(range(values, na.rm = TRUE), 8)
        self$set_attribute("actual_range", values_type, private$actual_range)
      }
    },

    #' @description Print a summary of the data object to the console.
    #' @param ... Arguments passed on to other functions. Of particular interest
    #' is `width = ` to indicate a maximum width of attribute columns.
    print = function(...) {
      cat("<Data array>", self$name, "\n")
      longname <- self$attribute("long_name")
      if (!is.na(longname) && longname != self$name)
        cat("Long name:", longname, "\n")

      if (is.na(private$actual_range[1L])) {
        cat("\nValues: -\n")
        cat(sprintf("    NA: %d (100%%)\n", length(private$values)))
      } else {
        units <- self$attribute("units")
        if (is.na(units)) units <- ""
        cat("\nValues: [", private$actual_range[1L], " ... ", private$actual_range[2L], "] ", units, "\n", sep = "")
        NAs <- sum(is.na(private$values))
        cat(sprintf("    NA: %d (%.1f%%)\n", NAs, NAs * 100 / length(private$values)))
      }

      cat("\nAxes:\n")
      axes <- do.call(rbind, lapply(self$axes, function(a) a$brief()))
      axes <- lapply(axes, function(c) if (all(c == "")) NULL else c)
      axes$group <- NULL
      axes <- as.data.frame(axes[lengths(axes) > 0L])
      print(.slim.data.frame(axes, 50L), right = FALSE, row.names = FALSE)

      self$print_attributes(...)
    },

    #' @description Retrieve the data in the object exactly as it was produced
    #' by the operation on `CFVariable`.
    #' @return The data in the object. This is usually an `array` with the
    #' contents along axes varying.
    raw = function() {
      dimnames(private$values) <- self$dimnames
      private$values
    },

    #' @description Retrieve the data in the object in the form of an R array,
    #' with axis ordering Y-X-others and Y values going from the top down.
    #' @return An `array` of data in R ordering.
    array = function() {
      if (length(self$axes) < 2L)
        stop("Cannot create an array from data object with only one axis.", call. = FALSE)

      dimnames(private$values) <- self$dimnames
      private$orient("R")
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

      # CRS
      wkt <- if (is.null(self$crs)) .wkt2_crs_geo(4326L)
             else self$crs$wkt2(.wkt2_axis_info(self))

      # Create the object
      arr <- self$array()
      numdims <- length(dim(private$values))
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
        else if (length(self$axes) > 2L) # Use coordinate of a scalar axis
          names(r) <- self$axes[[3L]]$coordinates
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
      dt <- data.table::as.data.table(exp)
      suppressWarnings(dt[ , eval(self$name) := private$values])

      long_name <- self$attribute("long_name")
      if (is.na(long_name)) long_name <- ""
      units <- self$attribute("units")
      if (is.na(units)) units <- ""
      data.table::setattr(dt, "value", list(name = long_name, units = units))
      dt
    },

    #' @description Save the data object to a netCDF file.
    #' @param fn The name of the netCDF file to create.
    #' @param pack Logical to indicate if the data should be packed. Packing is
    #' only useful for numeric data; packing is not performed on integer values.
    #' Packing is always to the "NC_SHORT" data type, i.e. 16-bits per value.
    #' @return Self, invisibly.
    save = function(fn, pack = FALSE) {
      nc <- RNetCDF::create.nc(fn, prefill = FALSE, format = "netcdf4")
      if (!inherits(nc, "NetCDF"))
        stop("Could not create the netCDF file. Please check that the location of the supplied file name is writable.", call. = FALSE)

      # Global attributes
      self$group$set_attribute("Conventions", "NC_CHAR", "CF-1.12")
      self$group$write_attributes(nc, "NC_GLOBAL")

      # Axes
      lbls <- unlist(sapply(self$axes, function(ax) {ax$write(nc); ax$coordinate_names}))

      # CRS
      if (!is.null(self$crs)) {
        self$crs$write(nc)
        self$set_attribute("grid_mapping", "NC_CHAR", self$crs$name)
      }

      # Packing
      pack <- pack && !is.na(private$actual_range[1L]) && private$values_type %in% c("NC_FLOAT", "NC_DOUBLE")
      if (pack) {
        self$set_attribute("add_offset", private$values_type, (private$actual_range[1L] + private$actual_range[2L]) * 0.5)
        self$set_attribute("scale_factor", private$values_type, (private$actual_range[2L] - private$actual_range[1L]) / 65534)
        self$set_attribute("missing_value", "NC_SHORT", -32767)
      }

      # Data variable
      dt <- private$orient("CF")
      axes <- attr(dt, "axes")
      dim_axes <- length(axes)
      RNetCDF::var.def.nc(nc, self$name, if (pack) "NC_SHORT" else private$values_type, axes)
      if (length(self$axes) > dim_axes || length(lbls)) {
        non_dim_axis_names <- sapply(self$axes, function(ax) ax$name)[-(1L:dim_axes)]
        self$set_attribute("coordinates", "NC_CHAR", paste(c(non_dim_axis_names, lbls), collapse = " "))
      }
      self$write_attributes(nc, self$name)
      RNetCDF::var.put.nc(nc, self$name, dt, pack = pack, na.mode = 2)
      RNetCDF::close.nc(nc)

      if (pack)
        self$delete_attribute(c("scale_factor", "add_offset", "missing_value"))

      invisible(self)
    }
  ),
  active = list(
    #' @field dimnames (read-only) Retrieve dimnames of the data object.
    dimnames = function(value) {
      if (missing(value)) {
        len <- length(dim(private$values))
        dn <- lapply(1:len, function(ax) dimnames(self$axes[[ax]]))
        names(dn) <- sapply(1:len, function(ax) self$axes[[ax]]$name)
        dn
      }
    }
  )
)
