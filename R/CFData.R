#' Data extracted from a CF data variable
#'
#' @description This class holds the data that is extracted from a [CFVariable],
#'   using the `subset()` method. The instance of this class will additionally
#'   have the axes and other relevant information such as its attributes (as
#'   well as those of the axes) and the coordinate reference system.
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
    # Set dimnames on self$value
    set_dimnames = function() {
      dn <- lapply(1:length(dim(self$value)), function(ax) {
        if (!inherits(ax, "CFAxisScalar")) dimnames(self$axes[[ax]])
      })
      dn <- dn[lengths(dn) > 0L]
      names(dn) <- sapply(1:length(dim(self$value)), function(ax) {self$axes[[ax]]$name})
      dimnames(self$value) <- dn
    },

    # Orient self$value in such a way that it conforms to regular R arrays: axis
    # order will be Y-X-others and Y values will go from the top to the bottom.
    # Returns a new array.
    orient = function() {
      # Drop any CFAxisScalar axes
      axes <- lapply(self$axes, function(ax) if (!inherits(ax, "CFAxisScalar")) ax$orientation)
      axes <- unlist(axes[lengths(axes) > 0L])

      # Permute, if necessary
      YX_order <- match(c("Y", "X"), axes, nomatch = 0L)
      if (!all(YX_order)) {
        warning("Cannot orient data array because axis orientation has not been set")
        return(self$value)
      }
      if (!all(YX_order == c(1L, 2L))) {
        all_axes <- seq(length(axes))
        permutate <- c(YX_order, all_axes[!(all_axes %in% YX_order)])
        out <- aperm(self$value, permutate)
      } else out <- self$value

      # Flip Y-axis, if necessary
      ynames <- dimnames(out)[[1L]]
      if (length(ynames) > 1L && as.numeric(ynames[2L]) > as.numeric(ynames[1L])) {
        dn <- dimnames(out)
        dims <- dim(out)
        dim(out) <- c(dims[1L], prod(dims[-1L]))
        out <- apply(out, 2, rev)
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
    #'   are the axes of the data object.
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
      private$set_dimnames()
      self$value
    },

    #' @description Retrieve the data in the object in the form of an R array,
    #' with axis ordering Y-X-others and Y values going from the top down.
    #' @return An `array` of data in R ordering.
    array = function() {
      if (length(self$axes) < 2L)
        stop("Cannot create an array from data object with only one axis.", call. = FALSE)

      private$set_dimnames()
      private$orient()
    },

    #' @description Convert the data to a `terra::SpatRaster` (3D) or a
    #' `terra::SpatRasterDataset` (4D) object. The data
    #' will be oriented to North-up. The 3rd dimension in the data will become
    #' layers in the resulting `SpatRaster`, any 4th dimension the data sets.
    #' @return A `terra::SpatRaster` or `terra::SpatRasterDataset` instance.
    terra = function() {
      if (!requireNamespace("terra", quietly = TRUE))
        stop("Please install package 'terra' before using this funcionality")

      # Extent
      YX_order <- match(c("Y", "X"), sapply(self$axes, function(a) a$orientation), nomatch = 0L)
      if (!all(YX_order))
        stop("Cannot create terra object because data does not have X and Y axes.", call. = FALSE)

      Xbnds <- self$axes[[YX_order[2L]]]$bounds
      if (inherits(Xbnds, "CFBounds")) Xbnds <- Xbnds$range()
      else {
        vals <- self$axes[[YX_order[2L]]]$values
        halfres <- (vals[2L] - vals[1L]) * 0.5 # this assumes regular spacing
        Xbnds <- c(vals[1L] - halfres, vals[length(vals)] + halfres)
      }
      Ybnds <- self$axes[[YX_order[1L]]]$bounds
      if (inherits(Ybnds, "CFBounds")) Ybnds <- Ybnds$range()
      else {
        vals <- self$axes[[YX_order[1L]]]$values
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
    } #,
    # Below code works in the console but not here
    # data.table = function() {
    #   if (!requireNamespace("data.table", quietly = TRUE))
    #     stop("Please install package 'data.table' before using this funcionality")
    #
    #   dt <- data.table::as.data.table(self$raw())
    #   cols <- c("lat", "lon")
    #   dt[, (cols) := type.convert(.SD, as.is = TRUE), .SDcols = cols]
    #   dt[dt[, do.call(order, .SD), .SDcols = cols]]
    # }
  )
)
