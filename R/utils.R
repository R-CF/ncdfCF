# NetCDF data types
# Additionally, UDTs are allowable as data type
netcdf_data_types <- c("NC_BYTE", "NC_UBYTE", "NC_CHAR", "NC_SHORT",
                       "NC_USHORT", "NC_INT", "NC_UINT", "NC_INT64",
                       "NC_UINT64", "NC_FLOAT", "NC_DOUBLE", "NC_STRING")

# This function returns TRUE if argument storage_mode is compatible with the
# nc_data argument, FALSE otherwise.
.compatible_type <- function(storage_mode, nc_data) {
  switch(storage_mode,
         "character" = nc_data %in% c("NC_CHAR", "NC_STRING"),
         "double"    = nc_data %in% c("NC_DOUBLE", "NC_FLOAT", "NC_INT64", "NCUINT64"),
         "integer"   = nc_data %in% c("NC_BYTE", "NC_UBYTE", "NC_SHORT", "NC_USHORT", "NC_INT", "NC_UINT"),
        # "integer64" = nc_data %in% c("NC_INT64", "NCUINT64"), NOT SUPPORTED (YET)
         "logical"   = nc_data == "NC_SHORT",
         FALSE)
}

# Return the netCDF data type most appropriate for an R type.
.nc_type <- function(storage_mode) {
  switch(storage_mode,
         "character" = "NC_STRING",
         "double"    = "NC_DOUBLE",
         "integer"   = "NC_INT",
         "integer64" = "NC_INT64",
         "logical"   = "NC_SHORT",
         "NC_NAT")
}

# This function is a bare-bones implementation of `apply(X, MARGIN, tapply, INDEX, FUN, ...)`,
# i.e. apply a factor over a dimension of an array. There are several restrictions
# compared to the base::apply/tapply pair (but note that function arguments are
# named differently): (1) X must be a vector, matrix or array (not a data.frame);
# (2) MARGIN must have all dimensions except the one to operate on; (3) INDEX is
# therefore a single factor; (4) MARGIN must be numeric (not dimnames); (5) FUN
# must be a function (not a formula); and (6) FUN must return a vector of numeric
# values, with each call generating the same number of values. In the interest of
# speed, these restrictions are not tested. Furthermore, no dimnames
# are set and the result is always simplified to a vector, matrix or array.
# On the up side, this function always returns a list, with as many elements as
# FUN returns values. If oper > 1L, the dimensions of the result are rearranged
# such that the dimension that is operated on comes first with the others
# following. This is identical to the tapply output.
#
# The basic version without a factor is about 10% faster than the base::apply()
# function. When used with a factor, this code is twice as fast as apply/tapply.
#
# Arguments:
# X    - Vector, matrix or array
# oper - The ordinal number of the axis to operate on
# fac  - The factor over whose levels to apply FUN, or NULL if no levels
# FUN  - The function to call with the data
# ...  - Additional arguments passed on to FUN
.process.data <- function (X, oper, fac = NULL, FUN, ...) {
  # FUN must return a vector of atomic types. A list is explicitly not going to
  # work. Every call over the fac levels must return the same number of values.
  FUN <- match.fun(FUN)

  # Number of distinct groups in the data, if fac is supplied
  nl <- if (is.factor(fac)) nlevels(fac) else 1L

  d <- dim(X)
  dl <- length(d)
  if (dl < 2L)                               # Vector, maybe a time profile
    res <- if (nl < 2L) list(FUN(X, ...))
           else lapply(split(X, fac), FUN, ...)
  else {                                     # Matrix or array
    d2 <- prod(d[-oper])

    newX <- if (oper == 1L) X
            else aperm(X, c(oper, seq_len(dl)[-oper]))
    dim(newX) <- c(d[oper], d2)

    res <- vector("list", d2)
    if (nl < 2L)
      for (i in 1L:d2)
        res[[i]] <- FUN(newX[, i], ...)
    else {
      for (i in 1L:d2)
        res[[i]] <- lapply(split(newX[, i], fac), FUN, ...)
      res <- unlist(res, recursive = FALSE, use.names = FALSE)
    }
  }

  dimres <- length(res[[1L]])

  # Set dimensions. If FUN returns multiple values, FUN values are in the first
  # dimension.
  res <- unlist(res, recursive = FALSE, use.names = FALSE)
  dims <- c(if(dimres > 1L) dimres, if (nl > 1L) nl, if (dl > 1L) d[-oper])
  if (length(dims) > 1L)
    dim(res) <- dims

  if (dimres == 1L)
    # Always return a list
    list(res)
  else if (dl < 2L && nl < 2L)
    # Vector input, no factor, multiple FUN values
    as.list(res)
  else
    # Separate FUN values into list elements
    asplit(res, 1L)
}

#' Make a data.frame slimmer by shortening long strings. List elements are
#' pasted together.
#' @param df A data.frame
#' @param width Maximum width of character entries. If entries are longer than
#' width - 3, they are truncated and then '...' added.
#' @return data.frame with slim columns
#' @noRd
.slim.data.frame <- function(df, width = 50L) {
  maxw <- width - 3L
  out <- as.data.frame(lapply(df, function(c) {
    if (is.list(c)) c <- sapply(c, paste0, collapse = ", ")
    if (!is.character(c)) c
    else
      sapply(c, function(e)
        if (nchar(e) > width) paste0(substr(e, 1, maxw), "...") else e
      )
  }))
  names(out) <- names(df)
  out
}

#' Flags if the supplied name is a valid name according to the CF Metadata
#' Conventions.
#'
#' @param nm A vector of names of variables, groups or attributes to test. Group
#' names should be plain, i.e. no preceding path.
#' @return Logical vector with `TRUE` for valid `nm` elements, `FALSE` otherwise.
#' @noRd
.is_valid_name <- function(nm) {
  is.character(nm) & nzchar(nm) & grepl("^[a-zA-Z][a-zA-Z0-9_]{0,254}$", nm)
}

#' Convert regular character strings to valid CF names. Non-permitted characters
#' are converted to underscaores "_" and leading underscores are deleted. If the
#' first character in the resulting string is a number `0-9`, an `x` is placed
#' immediately before it. Finally, the string is truncated to a maximum of 255
#' characters
#'
#' @param nm A vector of names to test.
#' @return A vector of the same size as argument `nm` with valid names.
#' @noRd
.make_valid_name <- function(nm) {
  nm <- gsub("[^a-zA-Z0-9_]+", "_", nm)
  nm <- trimws(nm, "left", "_")
  nm <- sub("^([0-9])", "x\\1", nm)
  substr(nm, 1, 255)
}

#' Round values `x` with .5 being rounded up.
#' Adapted from https://stackoverflow.com/a/12688836/3304426
#' @noRd
.round.5up <- function(x) {
  posneg <- sign(x)
  trunc(abs(x) + 0.5 + CF$eps) * posneg
}

#' Round values `x` with .5 being rounded down.
#' @noRd
.round.5down <- function(x) {
  trunc(abs(x) + 0.5 - CF$eps) * sign(x)
}

#' Test if vectors `x` and `y` have near-identical values.
#' @noRd
.near <- function(x, y) {
  abs(x - y) <= max(CF$eps * max(abs(x), abs(y)), 1e-12)
}

#' Test if vector `x` is monotonic, either increasing or decreasing. Return value
#' is -1L for monotonic decreasing, 0L for not monotonic, and 1L for monotonic
#' increasing.
#' @noRd
.monotonicity <- function(x) {
  if (!is.unsorted(x, strictly = TRUE)) 1L
  else if(!is.unsorted(-x, strictly = TRUE)) -1L
  else 0L
}

#' Test if the concatenation of vectors `x` and `y` yields a monotonic result.
#' This assumes that both `x` and `y` are independently monotonic.
#' @noRd
.c_is_monotonic <- function(x, y) {
  xlen <- length(x)
  if (xlen == 1L) {
    if (length(y) == 1L) !.near(x, y)
    else !.near(x, y[1L]) && ((x < y[1L] && y[1L] < y[2L]) || (x > y[1L] && y[1L] > y[2L]))
  } else {
    xlast <- x[xlen]
    if (length(y) == 1L) {
      !.near(xlast, y) && ((xlast < y && x[1L] < xlast) || (xlast > y && x[1L] > xlast))
    } else {
      if (x[1L] < x[2L]) (y[1L] < y[2L]) && (xlast < y[1L]) && !.near(xlast, y[1L])
      else (y[1L] > y[2L]) && (xlast > y[1L]) && !.near(xlast, y[1L])
    }
  }
}

#' Test if vector `x` is regular, meaning the difference between successive
#' values is constant, possibly 0.
#' @noRd
.is_regular <- function(x) {
  d <- diff(x)
  isTRUE(all.equal(d, rep(d[1L], length(d)), tolerance = CF$eps))
}

# This function will take the valid metadata for a Zarr array, the
# [CoordinateSystem] instance of a new `geozarr` array to create, optionally the
# path relative to the location of the new `geo_zarr` array for the group that
# stores any external arrays with coordinate values. The function will then set
# the proper convention attributes based on the coordinate system and return the
# updated metadata.
.geozarr_set_convention <- function(metadata, coord_sys, external_group, registration = 'pixel') {
  # This function is taken from the geozarr package because it is not exported from there
  meta <- metadata
  atts <- meta$attributes %||% list()
  axes <- coord_sys$axes

  # Drop any existing information
  meta$dimension_names <- NULL
  atts$zarr_conventions <- NULL
  if (length(atts)) {
    atts <- atts[!startsWith(names(atts), c('spatial:', 'proj:'))] # Drop any old spatial and proj elements
    atts$cs <- NULL # Remove any previous cs information
  }

  # dimension_names
  meta <- append(meta, list(dimension_names = vapply(axes, function(ax) ax$name, character(1L), USE.NAMES = FALSE)))

  # Axis abbreviation
  ax_abbr <- vapply(axes, function(ax) ax$abbreviation, FUN.VALUE = character(1), USE.NAMES = FALSE)
  X_axis <- which(ax_abbr == 'X')
  Y_axis <- which(ax_abbr == 'Y')
  if (!length(X_axis) && !length(Y_axis))
    stop('Cannot convert to GeoZarr: No X and/or Y axes found', call. = FALSE)

  # Set GeoZarr convention attributes
  if (length(X_axis) && length(Y_axis) && length(ax_abbr) <= 3L &&
      !('Z' %in% ax_abbr) && !('T' %in% ax_abbr) &&
      inherits(axes[[X_axis]]$coordinates$values, 'CoordinateValuesNumericPacked') && # == numeric & regular
      inherits(axes[[Y_axis]]$coordinates$values, 'CoordinateValuesNumericPacked') &&
      axes[[Y_axis]]$coordinates$values$raw[2L] < 0) {                                # == Y values descending
    # spatial convention
    # X + Y, optionally a band, no others, and X + Y coordinates are numeric and regular
    spatial <- geozarr::zarr_conv_spatial$new()
    atts <- spatial$register(atts)

    dimensions <- c(axes[[Y_axis]]$name, axes[[X_axis]]$name)
    spatial$dimensions <- dimensions
    spatial$set_coordinates(shape = c(axes[[X_axis]]$length, axes[[Y_axis]]$length),
                            x = axes[[X_axis]]$coordinates$values$raw,
                            y = axes[[Y_axis]]$coordinates$values$raw,
                            registration = registration)

    atts <- c(atts, spatial$as_list())
  } else {
    # cs convention
    cs_conv <- geozarr::zarr_convention_cs$new()
    atts    <- cs_conv$register(atts)

    # Direction lookup by axis abbreviation
    cs_direction <- c(X = 'EAST', Y = 'NORTH', Z = 'UP', T = 'FUTURE', OTHER = 'OTHER')

    axis_defs <- lapply(axes, function(ax) {
      # Values
      values <- ax$coordinates$raw
      values_def <- if (inherits(ax$coordinates, 'CoordinatesPacked'))
        cs_conv$values_regular(values[1L], values[2L])
      else if (ax$length <= geozarr::geozarr_options()$max_explicit)
        cs_conv$values_explicit(values)
      else
        # External coordinate values: Write coordinate values to an external array.
        # The name of the external array is `<axis_name>_coord`. The actual writing
        # to the external array should be done in the calling code.
        cs_conv$values_external(paste0(external_group, '/', paste0(ax$name, '_coord')))

      # Time
      time_def <- if (inherits(ax$coordinates, 'CoordinatesTime')) {
        def <- strsplit(ax$coordinates$time$calendar$definition, ' ', fixed = TRUE)[[1L]]
        cs_conv$time(unit = def[1L], epoch = def[3L], calendar = ax$coordinates$time$calendar$calendar)
      } else NULL

      # Coordinates and axis
      coords_def <- cs_conv$coordinates(values_def, unit = ax$coordinates$unit, time = time_def)
      abbr <- ax$abbreviation
      direction  <- cs_direction[[abbr]]
      if (abbr == 'OTHER') abbr <- ''
      cs_conv$axis(list(coords_def), abbreviation = abbr, direction = direction)
    })

    # Group axes into separate CRS objects by axis category
    cs_conv$add_crs(axes = axis_defs[c(X_axis, Y_axis)])
    cs_conv$add_crs(axes = axis_defs[which(ax_abbr == 'Z')])
    cs_conv$add_crs(axes = axis_defs[which(ax_abbr == 'T')])
    cs_conv$add_crs(axes = axis_defs[which(ax_abbr == 'OTHER')])

    atts <- c(list(cs = cs_conv$as_list()), atts)
  }

  meta$attributes <- NULL # Remove any previous attributes
  meta$attributes <- atts
  meta
}


unused_imports <- function() {
  stringr::word
}
