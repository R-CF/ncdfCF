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
        idx <- .range_match(rng, axis$values)
      } else {
        if (length(rng) == 1L) closed <- TRUE
        rng <- range(rng)
        vals <- axis$coordinates
        idx <- if (is.character(rng)) {
          closed <- TRUE
          .range_match(rng, vals)
        } else if (closed)
          which(vals >= rng[1L] & vals <= rng[2L], arr.ind = TRUE)
        else
          which(vals >= rng[1L] & vals < rng[2L], arr.ind = TRUE)
        if (!length(idx)) return(NULL)
        else idx <- range(idx)
        if (!closed && isTRUE(all.equal(vals[idx[2L]], rng[2L])))
          idx[2L] <- idx[2L] - 1L
      }
      if (is.null(idx)) NULL else as.integer(idx)
    },

    # Do the auxiliary grid interpolation. Argument "subset" is passed from the
    # `subset()` method. Argument "ll_names" give the auxiliary longitude and
    # latitude names. Return a list of useful objects to `subset()`.
    auxiliary_interpolation = function(subset, ll_names) {
      # This code assumes that the grid orientation of the data variable is the
      # same as that of the longitude-latitude grid
      ext <- private$llgrid$extent
      sel_names <- names(subset)
      Xrng <- if (ll_names[1L] %in% sel_names) range(subset[[ ll_names[1L] ]]) else ext[1L:2L]
      Yrng <- if (ll_names[2L] %in% sel_names) range(subset[[ ll_names[2L] ]]) else ext[3L:4L]
      private$llgrid$aoi <-  aoi(Xrng[1L], Xrng[2L], Yrng[1L], Yrng[2L])

      index <- private$llgrid$grid_index()
      dim_index <- dim(index)

      # The below appears counter-intuitive (XY relationship to indices) but it
      # works for long-lat grids that use the recommended X-Y-Z-T axis ordering.
      # Report any problems to https://github.com/R-CF/ncdfCF/issues
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
      self$NCvar$get_data(start, count)
    },

    # Read all the data values from file. Note that this may overwhelm computer
    # memory if the data is large.
    get_values = function() {
      self$NCvar$get_data()
    },

    # Internal apply/tapply method for this class. If the size of the data
    # variable is below a certain threshold, read the data and process in one
    # go. Otherwise processing goes per factor level. In other words, for each
    # factor level the data is read from file, to which the function is applied.
    # This is usually applied over the temporal domain but could be others
    # as well (untested).
    process_data = function(tdim, fac, fun, ...) {
      # Read the whole data array if size is manageable
      if (prod(sapply(self$axes, function(x) x$length)) < CF.options$memory_cell_limit)
        return(.process.data(private$get_values(), tdim, fac, fun, ...))

      # If data variable is large, go by individual factor levels
      num_dims <- private$num_dim_axes()
      start <- rep(1L, num_dims)
      count <- rep(NA_integer_, num_dims)

      lvls <- nlevels(fac)
      d <- vector("list", lvls)
      ndx <- as.integer(fac)
      nm <- self$name
      for (l in 1L:lvls) {
        indices <- which(ndx == l)
        dff <- diff(indices)
        if (all(dff == 1L)) {       # Data is contiguous per factor level
          rng <- range(indices)
          start[tdim] <- rng[1L]
          count[tdim] <- rng[2L] - rng[1L] + 1L
          values <- self$NCvar$get_data(start, count)
        } else {                    # Era factors have disparate indices
          cutoffs <- c(0L, which(c(dff, 2L) > 1L))
          values <- lapply(2L:length(cutoffs), function(i) {
            start[tdim] <- indices[cutoffs[i - 1L] + 1L]
            count[tdim] <- cutoffs[i] - cutoffs[i - 1L]
            self$NCvar$get_data(start, count)
          })
          values <- abind::abind(values, along = num_dims)
        }
        d[[l]] <- .process.data(values, tdim, FUN = fun, ...)
        # d is a list with lvls elements, each element a list with elements for
        # the number of function results, possibly 1; each element having an
        # array of dimensions from private$values that are not tdim.
      }
      res_dim <- dim(d[[1L]][[1L]])
      tdim_len <- length(d)
      if (tdim_len > 1L) {
        dims <- c(res_dim,  tdim_len)
        perm <- c(num_dims, 1L:(num_dims - 1L))
      } else
        dims <- res_dim

      fun_len <- length(d[[1L]])
      out <- if (fun_len > 1L) {
        # Multiple function result values so get all arrays for every result
        # value and unlist those
        lapply(1:fun_len, function(r) {
            x <- unlist(lapply(d, function(lvl) lvl[[r]]), recursive = FALSE, use.names = FALSE)
            dim(x) <- dims
            if (tdim_len > 1L)
              aperm(x, perm)
            else x
          })
      } else {
        # Single function result so unlist
        d <- unlist(d, recursive = TRUE, use.names = FALSE)
        dim(d) <- dims
        list(if (tdim_len > 1L) aperm(d, perm) else d)
      }
    }
  ),
  public = list(
    #' @description Create an instance of this class.
    #' @param nc_var The netCDF variable that defines this CF variable.
    #' @param axes List of [CFAxis] instances that describe the dimensions.
    #' @return An instance of this class.
    initialize = function(nc_var, axes) {
      super$initialize(nc_var, axes, NULL)
      nc_var$CF <- self

      # Sanitize attributes for valid range, missing values and packing
      private$values_type <- self$attribute("scale_factor", "type")
      if (is.na(private$values_type))
        private$values_type <- self$attribute("add_offset", "type")
      if (!is.na(private$values_type))
        # Data is packed in the netCDF file, throw away the attributes and let
        # RNetCDF deal with unpacking when reading the data.
        self$delete_attribute(c("_FillValue", "scale_factor", "add_offset",
                                "valid_range", "valid_min", "valid_max",
                                "missing_value"))
      else
        private$values_type <- nc_var$vtype
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
      if (length(axes)) {
        if (all(axes$group == "/")) axes$group <- NULL
        axes <- as.data.frame(axes[lengths(axes) > 0L])
        print(.slim.data.frame(axes, ...), right = FALSE, row.names = FALSE)
      } else cat(" (none)\n")

      len <- length(self$cell_measures)
      if (len) {
        cat("\nCell measure", if (len > 1L) "s", ": ",
            paste(sapply(self$cell_measures, function(cm) paste0(cm$name, " (", cm$measure, ")")), collapse = "; "),
            "\n", sep = "")
      }

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
      out_group <- makeGroup()
      out_group$set_attribute("title", "NC_CHAR", paste("Data copy of variable", self$name))
      out_group$set_attribute("history", "NC_CHAR", paste0(format(Sys.time(), "%FT%T%z"), " R package ncdfCF(", packageVersion("ncdfCF"), "): CFVariable::data()"))

      axes <- lapply(self$axes, function(ax) ax$clone())
      atts <- self$attributes
      atts <- atts[!(atts$name == "coordinates"), ]

      CFArray$new(self$name, out_group, self$NCvar$get_data(), private$values_type, axes, self$crs, atts)
    },

    #' @description This method extracts a subset of values from the array of
    #'   the variable, with the range along each axis to extract expressed in
    #'   coordinate values of the domain of each axis.
    #'
    #' @details The range of values along each axis to be subset is expressed in
    #'   coordinates of the domain of the axis. Any axes for which no selection
    #'   is made in the `...` argument are extracted in whole. Coordinates can
    #'   be specified in a variety of ways that are specific to the nature of
    #'   the axis. For numeric axes it should (resolve to) be a vector of real
    #'   values. A range (e.g. `100:200`), a vector (`c(23, 46, 3, 45, 17`), a
    #'   sequence (`seq(from = 78, to = 100, by = 2`), all work. Note, however,
    #'   that only a single range is generated from the vector so these examples
    #'   resolve to `(100, 200)`, `(3, 46)`, and `(78, 100)`, respectively. For
    #'   time axes a vector of character timestamps, `POSIXct` or `Date` values
    #'   must be specified. As with numeric values, only the two extreme values
    #'   in the vector will be used.
    #'
    #'   If the range of coordinate values for an axis in argument `...` extend
    #'   the valid range of the axis in `self`, the extracted data will start at
    #'   the beginning for smaller values and extend to the end for larger
    #'   values. If the values envelope the valid range the entire axis will be
    #'   extracted in the result. If the range of coordinate values for any axis
    #'   are all either smaller or larger than the valid range of the axis then
    #'   nothing is extracted and `NULL` is returned.
    #'
    #'   The extracted data has the same dimensional structure as the data in
    #'   the variable, with degenerate dimensions dropped. The order of the axes
    #'   in argument `...` does not reorder the axes in the result; use the
    #'   [CFArray]$array() method for this.
    #'
    #'   As an example, to extract values of a variable for Australia for the
    #'   year 2020, where the first axis in `x` is the longitude, the second
    #'   axis is the latitude, both in degrees, and the third (and final) axis
    #'   is time, the values are extracted by `x$subset(X = c(112, 154), Y =
    #'   c(-9, -44), T = c("2020-01-01", "2021-01-01"))`. Note that this works
    #'   equally well for projected coordinate reference systems - the key is
    #'   that the specification in argument `...` uses the same domain of values
    #'   as the respective axes in `x` use.
    #'
    #'   ## Auxiliary coordinate variables
    #'
    #'   A special case exists for variables where the horizontal dimensions (X
    #'   and Y) are not in longitude and latitude coordinates but in some other
    #'   coordinate system. In this case the netCDF resource may have so-called
    #'   *auxiliary coordinate variables* for longitude and latitude that are
    #'   two grids with the same dimension as the horizontal axes of the data
    #'   variable where each pixel gives the corresponding value for the
    #'   longitude and latitude. If the variable has such *auxiliary coordinate
    #'   variables* then you can specify their names (instead of specifying the
    #'   names of the primary planar axes). The resolution of the grid that is
    #'   produced by this method is automatically calculated. If you want to
    #'   subset those axes then specify values in decimal degrees; if you want
    #'   to extract the full extent, specify `NA` for both `X` and `Y`.
    #' @param ... One or more arguments of the form `axis = range`. The "axis"
    #'   part should be the name of an axis or its orientation `X`, `Y`, `Z` or
    #'   `T`. The "range" part is a vector of values representing coordinates
    #'   along the axis where to extract data. Axis designators and names are
    #'   case-sensitive and can be specified in any order. If values for the
    #'   range per axis fall outside of the extent of the axis, the range is
    #'   clipped to the extent of the axis.
    #' @param rightmost.closed Single logical value to indicate if the upper
    #'   boundary of range in each axis should be included. You must use the
    #'   argument name when specifying this, like `rightmost.closed = TRUE`, to
    #'   avoid the argument being treated as an axis name.
    #' @return A [CFArray] instance, having an array with its axes and
    #'   attributes of the variable, or `NULL` if one or more of the selectors
    #'   in the `...` argument fall entirely outside of the range of the axis.
    #'   Note that degenerate dimensions (having `length(.) == 1`) are dropped
    #'   from the array but the corresponding axis is maintained in the result
    #'   as a scalar axis.
    subset = function(..., rightmost.closed = FALSE) {
      num_axes <- private$num_dim_axes()
      if (!num_axes)
        stop("Cannot subset a scalar variable", call. = FALSE)

      # Organize the selectors
      selectors <- list(...)
      if (is.list(selectors[[1L]]))
        selectors <- selectors[[1L]]
      sel_names <- names(selectors)
      axis_names <- self$axis_names
      axis_order <- private$check_names(sel_names)

      aux <- NULL
      if (inherits(private$llgrid, "CFAuxiliaryLongLat")) {
        aux_names <- c(private$llgrid$varLong$name, private$llgrid$varLat$name)
        if (any(aux_names %in% sel_names)) {
          aux <- private$auxiliary_interpolation(selectors, aux_names)
          sel_names <- sel_names[-which(sel_names %in% aux_names)]
          ll_dimids <- private$llgrid$varLong$dimids # lat and long have identical dimids
        }
      }

      out_group <- makeGroup()
      out_group$set_attribute("title", "NC_CHAR", paste("Processing result of variable", self$name))
      out_group$set_attribute("history", "NC_CHAR", paste0(format(Sys.time(), "%FT%T%z"), " R package ncdfCF(", packageVersion("ncdfCF"), ")::CFVariable$subset()"))

      start <- rep(1L, num_axes)
      count <- rep(NA_integer_, num_axes)
      ZT_dim <- vector("integer")
      out_axes_dim <- list(); ax_nm_dim <- list()
      out_axes_other <- list(); ax_nm_other <- list()
      for (ax in 1:num_axes) {
        axis <- self$axes[[ax]]
        orient <- axis$orientation
        ax_dimid <- axis$dimid

        # In every section, set start and count values and create a corresponding axis
        if (!is.null(aux) && ax_dimid == ll_dimids[1L]) {
          start[ax] <- aux$X[1L]
          count[ax] <- aux$X[2L]
          out_axis <- makeLongitudeAxis(private$llgrid$varLong$name, out_group, aux$aoi$dimnames[[2L]], aux$aoi$bounds(out_group)$lon$coordinates)
        } else if (!is.null(aux) && ax_dimid == ll_dimids[2L]) {
          start[ax] <- aux$Y[1L]
          count[ax] <- aux$Y[2L]
          out_axis <- makeLatitudeAxis(private$llgrid$varLat$name, out_group, aux$aoi$dimnames[[1L]], aux$aoi$bounds(out_group)$lat$coordinates)
        } else { # No auxiliary coordinates
          rng <- selectors[[ axis_names[ax] ]]
          if (is.null(rng)) rng <- selectors[[ orient ]]
          if (is.null(rng)) { # Axis not specified so take the whole axis
            ZT_dim <- c(ZT_dim, axis$length)
            out_axis <- axis$subset(out_group, NULL)
          } else { # Subset the axis
            idx <- private$range2index(axis, rng, rightmost.closed)
            if (is.null(idx)) return(NULL)
            start[ax] <- idx[1L]
            count[ax] <- idx[2L] - idx[1L] + 1L
            ZT_dim <- c(ZT_dim, count[ax])
            out_axis <- axis$subset(out_group, idx)
          }
        }

        # Set the label set of the axis on the new axis
        out_axis$active_coordinates <- axis$active_coordinates

        # Collect axes for result
        if (out_axis$length == 1L) {
          out_axes_other <- append(out_axes_other, out_axis)
          ax_nm_other <- append(ax_nm_other, axis$name)
        } else {
          out_axes_dim <- append(out_axes_dim, out_axis)
          ax_nm_dim <- append(ax_nm_dim, axis$name)
        }
      }

      # Read the data, index as required
      d <- private$read_chunk(start, count)
      if (!is.null(aux)) {
        dim(d) <- dim_in <- c(aux$X[2L] * aux$Y[2L], prod(ZT_dim))
        d <- d[aux$index, ]
        dim(d) <- dim_out <- c(aux$box, ZT_dim)
      }
      d <- drop(d)

      # Put the dimensional axes in one list, with original names
      axes <- c(out_axes_dim, out_axes_other)
      names(axes) <- unlist(c(ax_nm_dim, ax_nm_other))

      # If there is a vertical axis, subset any parametric terms
      lapply(axes, function(ax) if (ax$is_parametric) ax$parametric_subset(axes, start, count, aux$index, dim_in, dim_out))

      # Sanitize the attributes for the result, as required, and get a CRS
      if (is.null(aux)) {
        atts <- self$attributes
        crs <- self$crs
      } else {
        atts <- private$dropCoordinates(self$attributes,
                                        c(private$llgrid$varLong$name, private$llgrid$varLat$name))
        atts <- atts[!(atts$name == "grid_mapping"), ]  # drop: warped to lat-long
        crs <- NULL
      }

      # Assemble the CFArray instance
      axes <- c(axes, self$axes[-(1L:num_axes)])      # Add the scalar axes to the list
      names(axes) <- sapply(axes, function(a) a$name) # New axis names
      CFArray$new(self$name, out_group, d, private$values_type, axes, crs, atts)
    }
  ),
  active = list(
    #' @field friendlyClassName (read-only) A nice description of the class.
    friendlyClassName = function(value) {
      if (missing(value))
        "Variable"
    },

    #' @field auxiliary_names (read-only) Retrieve the names of the auxiliary
    #'   longitude and latitude grids as a vector of two character strings, in
    #'   that order. If no auxiliary grids are defined, returns `NULL`.
    auxiliary_names = function(value) {
      if (missing(value)) {
        if (!is.null(private$llgrid))
          c(private$llgrid$varLong$name, private$llgrid$varLat$name)
        else NULL
      }
    },

    #' @field gridLongLat  Retrieve or set the grid of longitude and latitude
    #'   values of every grid cell when the main variable grid has a different
    #'   coordinate system.
    gridLongLat = function(value) {
      if (missing(value))
        private$llgrid
      else if (inherits(value, "CFAuxiliaryLongLat"))
        if (all(value$dimids %in% sapply(self$axes, function(x) x$dimid)))
          private$llgrid <- value
        else
          warning("Dimension ids of auxiliary lat-lon grid do not match those of the axis.", call. = FALSE)
      else stop("Trying to set wrong object as auxiliary lat-lon grid.", call. = FALSE)
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
  numaxes <- sum(sapply(x$axes, function(ax) ax$length > 1L))
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
      names(dnames) <- dimnames(x)[1:numaxes]
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
  data <- x$NCvar$get_data(start, count)

  # Apply dimension data and other attributes
  if (length(x$axes) && length(dim(data)) == length(dnames)) { # dimensions may have been dropped automatically, e.g. NC_CHAR to character string
    dimnames(data) <- dnames
    ax <- sapply(x$axes, function(ax) if (ax$length > 1L) ax$orientation)
    ax <- ax[lengths(ax) > 0L]
    attr(data, "axis") <- ax
  }
  time <- t[lengths(t) > 0L]
  if (length(time))
    attr(data, "time") <- time
  data
}
