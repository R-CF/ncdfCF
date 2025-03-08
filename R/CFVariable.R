#' CF data variable
#'
#' @description This class represents the basic structure of a CF data variable,
#'   the object that provides access to an array of data.
#'
#'   The CF data variable instance provides access to all the details that have
#'   been associated with the data variable, such as axis information, grid
#'   mapping parameters, etc. The actual data array can be accessed through the
#'   `data()` and `subset()` methods of this class.
#'
#' @docType class
CFVariable <- R6::R6Class("CFVariable",
  inherit = CFVariableBase,
  private = list(
    # Auxiliary coordinates reference, if the data variable uses them.
    llgrid = NULL,

    .varProperties = function() {
      unit <- self$attribute("units")
      if (is.na(unit)) unit <- ""
      longname <- self$attribute("long_name")
      if (is.na(longname)) longname <- ""
      if (longname == self$name) longname <- ""
      list(longname = longname, unit = unit)
    },

    # Turn the rng argument into index values into the axis.
    range2index = function(axis, rng, closed) {
      axl <- axis$length
      if (inherits(axis, "CFAxisTime")) {
        idx <- axis$slice(rng, closed)
        if (!length(idx)) return(NULL)
        idx <- range(idx)
      } else if (inherits(axis, "CFAxisCharacter")) {
        idx <- range(match(rng, axis$values))
      } else {
        if (length(rng) == 1L) closed <- TRUE
        rng <- range(rng)
        vals <- axis$values
        idx <- if (closed)
          which(vals >= rng[1L] & vals <= rng[2L], arr.ind = TRUE)
        else
          which(vals >= rng[1L] & vals < rng[2L], arr.ind = TRUE)
        if (!length(idx)) return(NULL)
        else idx <- range(idx)
        if (!closed && isTRUE(all.equal(vals[idx[2L]], rng[2L])))
          idx[2L] <- idx[2L] - 1L
      }
      as.integer(idx)
    },

    # Do the auxiliary grid interpolation. Argument "subset" is passed from the
    # `subset()` method. Argument "aoi" is an AOI, if supplied by the caller
    # to `subset()`. Return a list of useful objects to `subset()`.
    auxiliary_interpolation = function(subset, aoi) {
      # This code assumes that the grid orientation of the data variable is the
      # same as that of the longitude-latitude grid

      private$llgrid$aoi <- if (is.null(aoi)) {
        ext <- private$llgrid$extent
        Xrng <- if (is.na(subset$X[[1L]])) ext[1:2] else range(subset$X)
        Yrng <- if (is.na(subset$Y[[1L]])) ext[3:4] else range(subset$Y)
        aoi(Xrng[1L], Xrng[2L], Yrng[1L], Yrng[2L])
      } else aoi

      index <- private$llgrid$grid_index()
      dim_index <- dim(index)

      # The below appears counter-intuitive (XY relationship to indices) but it
      # works for long-lat grids that use the recommended X-Y-Z-T axis ordering.
      # Report any problems to https://github.com/pvanlaake/ncdfCF/issues
      dim_ll <- private$llgrid$dim
      xyidx <- arrayInd(index, dim_ll)          # convert indices to row,column
      rx <- range(xyidx[, 2L], na.rm = TRUE)    # full range of columns
      xyidx[, 2L] <- xyidx[, 2L] - rx[1L] + 1L  # reduce columns to 1-based
      cols <- rx[2L] - rx[1L] + 1L              # no of columns in reduced grid
      ry <- range(xyidx[, 1L], na.rm = TRUE)    # full range of rows
      xyidx[, 1L] <- xyidx[, 1L] - ry[1L] + 1L  # reduce rows to 1-based
      rows <- ry[2L] - ry[1L] + 1L              # no of rows in reduced grid
      index <- rows * (xyidx[, 2L] - 1L) + xyidx[, 1L] # reduced index value

      # index = the index values into the reduced grid
      # X,Y = start and count values for data to read
      # aoi = the AOI that was used to index
      # box = the dim of the original index
      list(index = index, X = c(ry[1L], rows), Y = c(rx[1L], cols), aoi = private$llgrid$aoi, box = dim_index)
    },

    # Return a vector with the two names of the auxiliary coordinate variables,
    # if they have been set.
    aux_var_names = function() {
      if (is.null(private$llgrid))
        NULL
      else
        c(private$llgrid$varLong$name, private$llgrid$varLat$name)
    },

    # Read a chunk of data from file
    read_chunk = function(start, count) {
      RNetCDF::var.get.nc(self$NCvar$group$handle, self$name, start, count, collapse = FALSE, unpack = TRUE, fitnum = TRUE)
    },

    # Read all the data values from file. Note that this may overwhelm computer
    # memory if the data is large.
    get_values = function() {
      RNetCDF::var.get.nc(self$NCvar$group$handle, self$name, collapse = FALSE, unpack = TRUE, fitnum = TRUE)
    },

    # Internal apply/tapply method for this class. If the size of the data
    # variable is below a certain threshold, read the data and process in one
    # go. Otherwise processing goes per factor level. In other words, for each
    # factor level the data is read from file, to which the function is applied.
    # Note that the data per factor level MUST be contiguous or else the
    # calculation will be erroneous.
    # This is usually applied over the temporal dimension but could be others
    # as well (untested).
    process_data = function(tdim, fac, fun, ...) {
      # Read the whole data array if size is manageable
      #if (prod(sapply(self$axes, function(x) x$length)) < 1e8) #FIXME: Make package env variable
      #  return(.process.data(private$get_values(), tdim, fac, fun, ...))

      # If data variable is large, go by individual factor levels
      num_dims <- private$num_dim_axes()
      start <- rep(1L, num_dims)
      count <- rep(NA_integer_, num_dims)

      lvls <- nlevels(fac)
      d <- vector("list", lvls)
      ndx <- as.integer(fac)
      h <- self$NCvar$group$handle
      nm <- self$name
      for (l in 1L:lvls) {
        rng <- range(which(ndx == l))
        start[tdim] <- rng[1L]
        count[tdim] <- rng[2L] - rng[1L] + 1L
        values <- RNetCDF::var.get.nc(h, nm, start, count, collapse = FALSE, unpack = TRUE, fitnum = TRUE)
        d[[l]] <- .process.data(values, tdim, FUN = fun, ...)
        # d is a list with lvls elements, each element a list with elements for
        # the number of function results, possibly 1; each element having an
        # array of dimensions from self$values that are not tdim.
      }
      res_dim <- dim(d[[1L]][[1L]])
      dims <- c(if ((len <- length(d)) > 1L) len, res_dim)

      len <- length(d[[1L]])
      if (len > 1L) {
        # Multiple function result values so get all arrays for every result
        # value and unlist those
        lapply(1:len, function(r) {
            x <- unlist(lapply(d, function(lvl) lvl[[r]]), recursive = FALSE, use.names = FALSE)
            dim(x) <- dims
            x
          })
      } else {
        # Single function result so unlist
        d <- unlist(d, recursive = FALSE, use.names = FALSE)
        dim(d) <- dims
        list(d)
      }
    }
  ),
  public = list(
    #' @description Create an instance of this class.
    #'
    #' @param grp The group that this CF variable lives in.
    #' @param nc_var The netCDF variable that defines this CF variable.
    #' @param axes List of [CFAxis] instances that describe the dimensions.
    #' @return An instance of this class.
    initialize = function(grp, nc_var, axes) {
      super$initialize(nc_var, grp, axes, NULL)
      nc_var$CF <- self
    },

    #' @description Print a summary of the data variable to the console.
    #' @param ... Arguments passed on to other functions. Of particular interest
    #' is `width = ` to indicate a maximum width of attribute columns.
    print = function(...) {
      cat("<Variable>", self$name, "\n")
      if (self$group$name != "/")
        cat("Group    :", self$group$name, "\n")

      longname <- self$attribute("long_name")
      if (!is.na(longname) && longname != self$name)
        cat("Long name:", longname, "\n")

      if (!is.null(self$crs)) {
        cat("\nCoordinate reference system:\n")
        print(.slim.data.frame(self$crs$brief(), ...), right = FALSE, row.names = FALSE)
      }

      cat("\nAxes:\n")
      axes <- do.call(rbind, lapply(self$axes, function(a) a$brief()))
      axes <- lapply(axes, function(c) if (all(c == "")) NULL else c)
      if (all(axes$group == "/")) axes$group <- NULL
      axes <- as.data.frame(axes[lengths(axes) > 0L])
      print(.slim.data.frame(axes, ...), right = FALSE, row.names = FALSE)

      if (!is.null(private$llgrid)) {
        cat("\nAuxiliary longitude-latitude grid:\n")
        ll <- private$llgrid$brief()
        print(.slim.data.frame(ll, ...), right = FALSE, row.names = FALSE)
      }

      self$print_attributes(...)
    },

    #' @description Some details of the data variable.
    #'
    #' @return A 1-row `data.frame` with some details of the data variable.
    brief = function() {
      props <- private$.varProperties()
      ax <- sapply(self$axes, function(x) x$name)
      data.frame(group = self$group$fullname, name = self$name,
                 long_name = props$longname, units = props$unit,
                 data_type = self$NCvar$vtype, axes = paste(ax, collapse = ", "))
    },

    #' @description The information returned by this method is very concise
    #'   and most useful when combined with similar information from other
    #'   variables.
    #'
    #' @return Character string with very basic variable information.
    shard = function() {
      self$NCvar$shard()
    },

    #' @description Retrieve interesting details of the data variable.
    #' @param with_groups Should group information be included? The save option
    #' is `TRUE` (default) when the netCDF resource has groups because names may
    #' be duplicated among objects in different groups.
    #' @return A 1-row `data.frame` with details of the data variable.
    peek = function(with_groups = TRUE) {
      out <- data.frame(id = self$id)
      if (with_groups) out$group <- self$group$fullname
      out$name <- self$name
      out$long_name <- self$attribute("long_name")
      out$standard_name <- self$attribute("standard_name")
      out$units <- self$attribute("units")
      if (with_groups)
        out$axes <- paste(sapply(self$axes, function(a) a$fullname), collapse = ", ")
      else
        out$axes <- paste(sapply(self$axes, function(a) a$name), collapse = ", ")
      out
    },

    #' @description Retrieve all data of the variable.
    #' @return A [CFArray] instance with all data from this variable.
    data = function() {
      out_group <- VirtualGroup$new(-1L, "/", "/", NULL)
      out_group$set_attribute("title", "NC_CHAR", paste("Data copy of variable", self$name))
      out_group$set_attribute("history", "NC_CHAR", paste0(format(Sys.time(), "%FT%T%z"), " R package ncdfCF(", packageVersion("ncdfCF"), "): CFVariable::data()"))

      axes <- lapply(self$axes, function(ax) ax$clone())
      d <- RNetCDF::var.get.nc(self$NCvar$group$handle, self$name, collapse = FALSE, unpack = TRUE, fitnum = TRUE)
      atts <- self$attributes
      atts <- atts[!(atts$name == "coordinates"), ]

      CFArray$new(self$name, out_group, d, axes, self$crs, atts)
    },

    #' @description This method extracts a subset of values from the array of
    #' the variable, with the range along each axis to extract expressed in
    #' values of the domain of each axis.
    #'
    #' @details The range of values along each axis to be subset is expressed in
    #' values of the domain of the axis. Any axes for which no information is
    #' provided in the `subset` argument are extracted in whole. Values can be
    #' specified in a variety of ways that are specific to the nature of the
    #' axis. For numeric axes it should (resolve to) be a vector of real
    #' values. A range (e.g. `100:200`), a vector (`c(23, 46, 3, 45, 17`),
    #' a sequence (`seq(from = 78, to = 100, by = 2`), all work. Note, however,
    #' that only a single range is generated from the vector so these examples
    #' resolve to `(100, 200)`, `(3, 46)`, and `(78, 100)`, respectively. For
    #' time axes a vector of character timestamps, `POSIXct` or `Date` values
    #' must be specified. As with numeric values, only the two extreme values in
    #' the vector will be used.
    #'
    #' If the range of values for an axis in argument `subset` extend the valid
    #' range of the axis in `x`, the extracted slab will start at the beginning
    #' for smaller values and extend to the end for larger values. If the
    #' values envelope the valid range the entire axis will be extracted in
    #' the result. If the range of `subset` values for any axis are all either
    #' smaller or larger than the valid range of the axis in `x` then nothing
    #' is extracted and `NULL` is returned.
    #'
    #' The extracted data has the same dimensional structure as the data in the
    #' variable, with degenerate dimensions dropped. The order of the axes in
    #' argument `subset` does not reorder the axes in the result; use the
    #' [CFArray]$array() method for this.
    #'
    #' As an example, to extract values of a variable for Australia for the year
    #' 2020, where the first axis in `x` is the longitude, the second
    #' axis is the latitude, both in degrees, and the
    #' third (and final) axis is time, the values are extracted by
    #' `x$subset(list(X = c(112, 154), Y = c(-9, -44), T = c("2020-01-01", "2021-01-01")))`.
    #' You could take the longitude-latitude values from `sf::st_bbox()` or
    #' `terra::ext()` if you have specific spatial geometries for whom you want to
    #' extract data. Note that this works equally well for projected coordinate
    #' reference systems - the key is that the specification in argument `subset`
    #' uses the same domain of values as the respective axes in `x` use.
    #'
    #' ## Auxiliary coordinate variables
    #'
    #' A special case exists for variables where the horizontal dimensions (X
    #' and Y) are not in longitude and latitude values but in some other
    #' coordinate system. In this case the netCDF resource may have so-called
    #' *auxiliary coordinate variables* for longitude and latitude that are two
    #' grids with the same dimension as the horizontal axes of the data variable
    #' where each pixel gives the corresponding value for the longitude and
    #' latitude. If the variable has such *auxiliary coordinate variables* then
    #' they will be used automatically if, and only if, the axes are labeled in
    #' argument `subset` as `X` and `Y`. The resolution of the grid that is
    #' produced by this method is automatically calculated. If you want to
    #' subset those axes then specify values in decimal degrees; if you want to
    #' extract the full extent, specify `NA` for both `X` and `Y`. **Note** that
    #' if you want to extract the data in the original grid, you should use the
    #' horizontal axis names in argument `subset`.
    #'
    #' @param subset A list with the range to extract from each axis. The
    #' list should have elements for the axes to extract a subset from - if an
    #' axis is not present in the list the entire axis will be extracted
    #' from the array. List element names should be the axis designator `X`, `Y`,
    #' `Z` or `T`, or the name of the axis - axes without an axis designator
    #' and any additional axes beyond the four standard ones can only
    #' be specified by name. Axis designators and names are case-sensitive and
    #' can be specified in any order. If values for the range per axis fall
    #' outside of the extent of the axis, the range is clipped to the extent of
    #' the axis.
    #' @param aoi Optional, an area-of-interest instance of class `AOI` created
    #' with the [aoi()] function to indicate the horizontal area that should be
    #' extracted. The longitude and latitude coordinates must be included; the X
    #' and Y resolution will be calculated if not given. When provided, this
    #' argument will take precedence over the corresponding axis information for
    #' the X and Y axes in the `subset` argument.
    #' @param rightmost.closed Single logical value to indicate if the upper
    #' boundary of range in each axis should be included.
    #' @param ... Ignored. Included to avoid "unused argument" errors on
    #' argument `rightmost.closed`.
    #'
    #' @return A [CFArray] instance, having an array with its axes and
    #' attributes of the variable, or `NULL` if one or more of the elements in
    #' the `subset` argument falls entirely outside of the range of the axis.
    #' Note that degenerate dimensions (having `length(.) == 1`) are dropped
    #' from the array but the corresponding axis is maintained in the result as
    #' a scalar axis.
    subset = function(subset, aoi = NULL, rightmost.closed = FALSE, ...) {
      num_axes <- private$num_dim_axes()
      if (!num_axes)
        stop("Cannot subset a scalar variable", call. = FALSE)
      if (!is.null(aoi) && is.null(aoi$lonMin))
        stop("Argument `aoi` must have coordinates set", call. = FALSE)

      axis_names <- names(self$axes)
      orientations <- sapply(self$axes, function(a) a$orientation)
      axis_order <- match(c("X", "Y", "Z", "T"), orientations)
      names(axis_order) <- c("X", "Y", "Z", "T")

      sub_names <- names(subset)

      if ((all(c("X", "Y") %in% sub_names) || !missing(aoi)) && inherits(private$llgrid, "CFAuxiliaryLongLat")) {
        aux <- private$auxiliary_interpolation(subset, aoi)
        sub_names <- sub_names[!grepl("X|Y", sub_names)]
      } else aux <- NULL

      bad <- sub_names[!(sub_names %in% c(axis_names, orientations))]
      if (length(bad))
        stop("Argument `subset` contains elements not corresponding to an axis:", paste(bad, collapse = ", "), call. = FALSE)

      out_group <- VirtualGroup$new(-1L, "/", "/", NULL)
      out_group$set_attribute("title", "NC_CHAR", paste("Processing result of variable", self$name))
      out_group$set_attribute("history", "NC_CHAR", paste0(format(Sys.time(), "%FT%T%z"), " R package ncdfCF(", packageVersion("ncdfCF"), ")::CFVariable$subset()"))

      start <- rep(1L, num_axes)
      count <- rep(NA_integer_, num_axes)
      ZT_dim <- vector("integer")
      out_axes_dim <- list()
      out_axes_other <- list()
      for (ax in 1:num_axes) {
        axis <- self$axes[[ax]]

        if (!is.null(aux) && orientations[ax] == "X") {
          start[ax] <- aux$X[1L]
          count[ax] <- aux$X[2L]
          var <- NCVariable$new(-1L, private$llgrid$varLong$name, out_group, "NC_DOUBLE", 1L, NULL)
          var$attributes <- private$llgrid$varLong$attributes
          out_axis <- if (aux$X[2L] == 1L)
            CFAxisScalar$new(out_group, var, "X", aux$aoi$dimnames[[2L]])
          else {
            dim <- NCDimension$new(-1L, private$llgrid$varLong$name, aux$aoi$dim[2L], FALSE)
            newax <- CFAxisLongitude$new(out_group, var, dim, aux$aoi$dimnames[[2L]])
            newax$bounds <- aux$aoi$bounds(out_group)$lon
            newax
          }
        } else if (!is.null(aux) && orientations[ax] == "Y") {
          start[ax] <- aux$Y[1L]
          count[ax] <- aux$Y[2L]
          var <- NCVariable$new(-1L, private$llgrid$varLat$name, out_group, "NC_DOUBLE", 1L, NULL)
          var$attributes <- private$llgrid$varLat$attributes
          out_axis <- if (aux$X[2L] == 1L)
            CFAxisScalar$new(out_group, var, "Y", aux$aoi$dimnames[[1L]])
          else {
            dim <- NCDimension$new(-1L, private$llgrid$varLat$name, aux$aoi$dim[1L], FALSE)
            newax <- CFAxisLatitude$new(out_group, var, dim, aux$aoi$dimnames[[1L]])
            newax$bounds <- aux$aoi$bounds(out_group)$lat
            newax
          }
        } else { # No auxiliary coordinates
          rng <- NULL
          if (!is.null(aoi))
            rng <- if (orientations[ax] == "X") c(aoi$lonMin, aoi$lonMax)
          else if (orientations[ax] == "Y") c(aoi$latMin, aoi$latMax)
          if (is.null(rng)) rng <- subset[[ axis_names[ax] ]]
          if (is.null(rng)) rng <- subset[[ orientations[ax] ]]
          if (is.null(rng)) {
            ZT_dim <- c(ZT_dim, axis$length)
            out_axis <- axis$sub_axis(out_group, NULL)
          } else {
            idx <- private$range2index(axis, rng, rightmost.closed)
            if (is.null(idx)) return(NULL)
            start[ax] <- idx[1L]
            count[ax] <- idx[2L] - idx[1L] + 1L
            ZT_dim <- c(ZT_dim, count[ax])
            out_axis <- axis$sub_axis(out_group, idx)
          }
        }

        # Collect axes for result
        if (inherits(out_axis, "CFAxisScalar"))
          out_axes_other <- append(out_axes_other, out_axis)
        else
          out_axes_dim <- append(out_axes_dim, out_axis)
      }

      # Read the data, index as required
      d <- private$read_chunk(start, count)
      if (!is.null(aux)) {
        dim(d) <- c(aux$X[2L] * aux$Y[2L], prod(ZT_dim))
        d <- d[aux$index, ]
        dim(d) <- c(aux$box, ZT_dim)
      }
      d <- drop(d)

      # Sanitize the attributes for the result, as required
      atts <- self$attributes
      atts <- atts[!(atts$name == "coordinates"), ]     # drop: these have been set in axes

      # Get a proper CRS
      if (is.null(aux)) crs <- self$crs
      else {
        atts <- atts[!(atts$name == "grid_mapping"), ]  # drop: warped to lat-long
        crs <- NULL
      }

      # Assemble the CFArray instance
      axes <- c(out_axes_dim, out_axes_other)
      names(axes) <- sapply(axes, function(a) a$name)
      CFArray$new(self$name, out_group, d, axes, crs, atts)
    }
  ),
  active = list(
    #' @field friendlyClassName (read-only) A nice description of the class.
    friendlyClassName = function(value) {
      if (missing(value))
        "Variable"
    },

    #' @field gridLongLat The grid of longitude and latitude values of every
    #' grid cell when the main variable grid has a different coordinate system.
    gridLongLat = function(value) {
      if (missing(value))
        private$llgrid
      else {
        private$llgrid <- value
        dimids <- value$dimids
        ax1 <- sapply(self$axes, function(x) if (x$dimid == dimids[[1L]]) x$orientation)
        ax1 <- ax1[lengths(ax1) > 0L]
        ax2 <- sapply(self$axes, function(x) if (x$dimid == dimids[[2L]]) x$orientation)
        ax2 <- ax2[lengths(ax2) > 0L]
        c(ax1[[1L]], ax2[[1L]])
      }
    },

    #' @field crs_wkt2 (read-only) Retrieve the coordinate reference system
    #' description of the variable as a WKT2 string.
    crs_wkt2 = function(value) {
      if (missing(value)) {
        if (is.null(self$crs)) {
          # If no CRS has been set, return the default GEOGCRS unless
          # the axis coordinates fall out of range in which case an empty string
          # is returned.
          orient <- lapply(self$axes, function(x) x$orientation)
          X <- match("X", orient, nomatch = 0L)
          Y <- match("Y", orient, nomatch = 0L)
          if (X && Y) {
            X <- self$axes[[X]]$range()
            Y <- self$axes[[Y]]$range()
            if (Y[1L] >= -90 && Y[2L] <= 90 && ((X[1L] >= 0 && X[2L] <= 360) || (X[1L] >= -180 && X[2L] <= 180)))
              .wkt2_crs_geo(4326)
            else ""
          } else ""
        } else
          self$crs$wkt2(.wkt2_axis_info(self))
      }
    }
  )
)

# Public S3 methods ------------------------------------------------------------

#' @export
dim.CFVariable <- function(x) {
  sapply(x$axes, function(z) z$NCdim$length)
}

#' @export
dimnames.CFVariable <- function(x) {
  ax <- x$axes
  if (length(ax)) names(ax)
  else NULL
}

#' Extract data for a variable
#'
#' Extract data from a `CFVariable` instance, optionally sub-setting the
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
#' `CFVariable$subset()` method.
#'
#' Scalar axes should not be included in the indexing as they do not represent a
#' dimension into the data array.
#'
#' @param x An `CFVariable` instance to extract the data of.
#' @param i,j,... Expressions, one for each axis of `x`, that select a
#'   number of elements along each axis. If any expressions are missing,
#'   the entire axis is extracted. The values for the arguments may be an
#'   integer vector or a function that returns an integer vector. The range of
#'   the values in the vector will be used. See examples, below.
#' @param drop Logical, ignored. Axes are never dropped. Any degenerate
#'   dimensions of the array are returned as such, with dimnames and appropriate
#'   attributes set.
#'
#' @return An array with dimnames and other attributes set.
#' @export
#' @aliases bracket_select
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
"[.CFVariable" <- function(x, i, j, ..., drop = FALSE) {
  numaxes <- sum(!sapply(x$axes, inherits, "CFAxisScalar"))
  t <- vector("list", numaxes)
  names(t) <- dimnames(x)[1:numaxes]

  sc <- sys.call()
  if (numaxes == 0L && length(sc) == 3L) { # Variable is a scalar value
    start <- 1L
    count <- NA_integer_
  } else {
    sc <- sc[-(1L:2L)] # drop [ and x
    if ((length(sc) == 1L) && (identical(sc[[1L]], quote(expr = )))) {
      # [] specified, read whole array
      start <- rep(1L, numaxes)
      count <- rep(NA_integer_, numaxes)
      dnames <- lapply(x$axes, dimnames)
      t <- lapply(x$axes, function(z) z$time())
    } else if (length(sc) != numaxes) {
      stop("Indices specified are not equal to the number of axes of the variable")
    } else {
      start <- vector("integer", numaxes)
      count <- vector("integer", numaxes)
      dnames <- vector("list", numaxes)
      for (d in seq_along(sc)) {
        ax <- x$axes[[d]]
        tm <- ax$time()
        if (identical(sc[[d]], quote(expr = ))) {
          # Axis not subsetted, read the whole thing
          start[d] <- 1L
          count[d] <- NA_integer_
          dnames[[d]] <- dimnames(ax)
          if (!is.null(tm)) t[[d]] <- tm
        } else {
          # Subset the axis
          v <- eval(sc[[d]])
          ex <- range(v)
          start[d] <- ex[1L]
          count[d] <- ex[2L] - ex[1L] + 1L
          dnames[[d]] <- dimnames(ax)[seq(ex[1], ex[2])]
          if (!is.null(tm)) {
            idx <- tm$indexOf(ex[1L]:ex[2L], "constant")
            t[[d]] <- attr(idx, "CFTime")
          }
        }
      }
    }
  }
  data <- RNetCDF::var.get.nc(x$NCvar$group$handle, x$name, start, count, collapse = FALSE, unpack = TRUE, fitnum = TRUE)

  # Apply dimension data and other attributes
  if (length(x$axes) && length(dim(data)) == length(dnames)) { # dimensions may have been dropped automatically, e.g. NC_CHAR to character string
    dimnames(data) <- dnames
    ax <- sapply(x$axes, function(x) if (!inherits(x, "CFAxisScalar")) x$orientation)
    ax <- ax[lengths(ax) > 0L]
    attr(data, "axis") <- ax
  }
  time <- t[lengths(t) > 0L]
  if (length(time))
    attr(data, "time") <- time
  data
}
