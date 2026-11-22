#' Aggregate multiple CF data variables into a single GeoZarr array
#'
#' NetCDF files have file size limitations, leading to large data sets being
#' split up into multiple files, usually over the temporal dimension. This is
#' very typical in climate projection data sets: a typical CMIP6 run over the
#' period 2015-2100 at daily resolution is broken into 3 to 16 separate files
#' for each variable, making data extractions across the temporal boundaries
#' cumbersome. Zarr does not have that same limitation: large data sets are
#' broken up into "chunks", each being stored as a file when the store is on a
#' local file system, but this is transparent to the user of the data. A full
#' CMIP6 run of any variable can be stored and manipulated in a single GeoZarr
#' array.
#'
#' This function makes a best effort. If the data variables provided are
#' sensible, so will be the result. Otherwise, an error is thrown.
#'
#' The data variables must have the same axes, by name and in the same order.
#' Along the axes named in `along` the coordinates of the data variables are
#' combined, in the direction of the axis; any duplicate coordinates are
#' included once. These axes may also be scalar axes of the data variables, such
#' as a height or an ensemble member, which then become new dimensions of the
#' array. All other axes have to be identical in all data variables, including
#' their boundary values. The coordinates of the data variables have to be
#' contiguous subsets of the aggregated axis.
#'
#' The following conditions throw an error:
#'
#' * the data variables do not have the same axes, by name and in the same order;
#' * an axis is not of the same type or, for time axes, does not have the same
#'   calendar, unit and origin in all data variables;
#' * the coordinates of an axis in `along` are partly increasing and partly
#'   decreasing, or have gaps or interleave;
#' * a discrete axis is named in `along`;
#' * the data variables have different data types, or not all have the same
#'   CRS;
#' * only some data variables have an auxiliary longitude-latitude grid, the
#'   grids differ, or, with such a grid, the `X` or `Y` axes are in `along`;
#' * label sets of an axis that is not in `along` differ among the data
#'   variables, or labels for the same coordinate differ;
#' * One or more of the data variables has parametric vertical coordinates.
#'
#' Attributes of the axes are set on the coordinates of the axes, leaving out
#' those that are structural in GeoZarr, such as `axis`, `bounds` and `units`.
#' Global attributes, those of the group of the data variables, are set on the
#' `location` group, overwriting attributes with the same name that are already
#' there. As no single value describes the aggregate when it differs among the
#' files, such as for a creation date or a tracking id, only the global
#' attributes that have the same value in all data variables are set. The
#' exception is the `history` attribute, which is set as an array of entries,
#' most recent first: an entry for the aggregation, followed by the entries of
#' each distinct history among the data variables.
#'
#' Attributes of the data variables are set on the array. If they differ among
#' the data variables, or are not in all of them, a warning is issued and the
#' value from the last data variable is used. This also holds for the attributes
#' of an axis. Missing data are translated to the
#' GeoZarr conventions, see the "Exporting data" vignette for details.
#'
#' Label sets of the axes are included. Any axis can be aggregated along,
#' including the `X` and `Y` axes, except when the data variables have an
#' auxiliary longitude-latitude grid. That grid is set as the geolocation of the
#' array and it is defined over the `X` and `Y` axes, so these cannot be in
#' `along`. The grid, and the axes it is defined over, must be identical in all
#' data variables. If the axes of the grid have no orientation, as is common for
#' curvilinear grids, the first is taken to be the `X` axis and the second the
#' `Y` axis.
#'
#' Limitations: parametric vertical coordinates (CF formula terms) are not
#' supported.
#'
#' @param x Either a character vector with fully-qualified paths to netCDF
#'   files, or a `list` of [CFVariable] instances, to aggregate.
#' @param location A `zarr_group` in which the array is created, or a `zarr`
#'   object whose root node is a `zarr_group`. The array must not already
#'   exist.
#' @param along Optional. Character vector with the names of the axes to
#'   aggregate along. If omitted or `NULL` (the default), the time axis of the
#'   first data variable is used.
#' @param var Optional. When `x` is a character vector, the name of the data
#'   variable to read from each file. If omitted or `NULL` (the default), each
#'   file must have a single data variable.
#' @param name Optional. The name of the array to create. If omitted or `NULL`
#'   (the default), the name of the first data variable is used.
#' @param chunking Optional. Named `vector` or `list`, with numeric values
#'   giving the relative weights of each axis in the chunking - higher values
#'   favour data extraction over that axis. The names correspond to the names
#'   of the axes of the data variables, any non-named axes will be assigned a
#'   value of 1. If omitted or `NULL` (default), all axes will have equal
#'   weight in the chunking.
#' @param external_coordinates Optional. Character string with the path of a
#'   group, relative to `location`, where the arrays with external coordinates
#'   are stored: coordinate values and boundaries that are not regular and too
#'   long to be stored in the array metadata, label sets and the arrays of an
#'   auxiliary longitude-latitude grid. The group is created if it does not
#'   exist. If omitted or `NULL` (the default), these arrays are stored in
#'   `location`, next to the new array. The path may go up the hierarchy with
#'   `..`, e.g. `"../shared"` for a group next to `location`, but not beyond the
#'   root group of the Zarr store, which is an error.
#' @param verbose Logical. If `TRUE`, progress messages are printed to the
#'   console: the files being opened, the array that is created, and the
#'   progress of copying the data of each data variable, per chunk written.
#'   Default is `TRUE` in an interactive session, `FALSE` otherwise.
#' @return The new `geozarr_array`, invisibly.
#' @export
#' @examples
#' \dontrun{
#' pr <- list.files(pattern = '^pr.*\\.nc$', full.names = TRUE)
#' z <- zarr::create_zarr()
#' gza <- aggregate_geozarr(x = pr, location = z)
#' }
aggregate_geozarr <- function(x, location, along = NULL, var = NULL, name = NULL, chunking = NULL,
                              external_coordinates = NULL, verbose = interactive()) {
  .check_geozarr_package()

  # Destination
  if (inherits(location, "zarr"))
    location <- location$root
  if (!inherits(location, "zarr_group"))
    stop("Argument `location` must be a `zarr_group`, or a `zarr` object with a group as root", call. = FALSE)

  # Data variables
  if (is.character(x)) {
    if (!length(x))
      stop("Argument `x` must have at least one file name.", call. = FALSE)
    # The data sets stay open until the variables are no longer referenced
    vars <- lapply(seq_along(x), function(i) {
      fn <- x[i]
      if (verbose) cat(sprintf("Opening file %d of %d: %s\n", i, length(x), basename(fn)))
      ds <- open_ncdf(fn)
      nm <- var %||% if (length(ds$var_names) == 1L) ds$var_names
      if (is.null(nm) || length(nm) != 1L || !(nm %in% ds$var_names))
        stop("Cannot select a data variable from file '", fn, "': use argument `var`", call. = FALSE)
      ds[[nm]]
    })
  } else if (is.list(x) && length(x) && all(vapply(x, inherits, logical(1L), "CFVariable")))
    vars <- x
  else
    stop("Argument `x` must be a character vector of file names or a list of `CFVariable` instances", call. = FALSE)

  # Axis to aggregate along: the time axis by default
  if (is.null(along)) {
    dim_axes <- vars[[1L]]$axes[seq_along(vars[[1L]]$dim())]
    time <- vapply(dim_axes, function(a) identical(a$orientation, "T"), logical(1L))
    if (!any(time))
      stop("Argument `along` is required: the data variables do not have a time axis", call. = FALSE)
    along <- dim_axes[[which(time)[1L]]]$name
  }

  # Weights in the chunking can be given as a list
  if (is.list(chunking))
    chunking <- unlist(chunking)

  if (verbose)
    cat(sprintf("Creating the GeoZarr array, aggregating %d data variables along %s\n",
                length(vars), paste0("'", along, "'", collapse = ", ")))
  gza <- .aggregate_array(vars, along, location, name, chunking, external_coordinates)
  if (verbose)
    cat(sprintf("Array '%s' with shape (%s)\n", gza$name, paste(gza$shape, collapse = ", ")))

  for (i in seq_along(vars)) {
    if (verbose)
      cat(sprintf("Copying data variable %d of %d: %s\n", i, length(vars), vars[[i]]$name))
    copy_to_geozarr(vars[[i]], gza, verbose = verbose)
  }


  if (verbose) cat("Done.\n")
  invisible(gza)
}

#' Copy data from a `CFVariable` to a `geozarr_array`
#'
#' This function will take a [CFVariable] and copy the data into a GeoZarr
#' array in a Zarr store.
#'
#' The GeoZarr array must have been initialised before calling this function.
#' The [create_geozarr_array()] can be used for that purpose.
#'
#' Missing data in the `CFVariable` are translated to the GeoZarr conventions,
#' see the "Exporting data" vignette for details.
#'
#' A scalar axis of the `CFVariable` can be an axis of the GeoZarr array, with a
#' length of 1 for the variable. This is how variables from multiple files, each
#' with its own scalar axis value such as an ensemble member or a height, are
#' combined along a new dimension.
#'
#' The `CFVariable` axes have to be compatible with those of the GeoZarr array.
#' Specifically, the values of the (active) coordinates of each axis have to be
#' a contiguous subset of the corresponding axis (by position) of the GeoZarr
#' array.
#'
#' @param var A `CFVariable` instance to copy the data from.
#' @param geozarr A `geozarr_array` instance to copy data to.
#' @param verbose Logical. If `TRUE`, the progress of copying is printed to the
#'   console, per chunk of the `geozarr_array` that is written. Default is
#'   `FALSE`.
#' @return `var`, invisibly.
copy_to_geozarr <- function(var, geozarr, verbose = FALSE) {
  .check_geozarr_package()

  # Coordinate system of geozarr is (possibly) a superset of var so geozarr leads
  cs  <- geozarr$coordinate_system
  axes <- cs$axes  # named list, in the same order as the array's dimensions

  # The dimensions of the variable are the first axes of the variable, any
  # further axes are scalar. Scalar axes can be dimensions of the GeoZarr array,
  # with a length of 1 for the variable.
  var_axes <- var$axes
  var_names <- vapply(var_axes, function(a) a$name, character(1L), USE.NAMES = FALSE)
  dim_names <- var_names[seq_len(var$ndims)]
  if (!all(names(axes) %in% var_names))
    stop('The data variable does not have all the axes of the GeoZarr array', call. = FALSE)
  if (!identical(names(axes)[names(axes) %in% dim_names], dim_names))
    stop('The dimensions of the data variable are not axes of the GeoZarr array, in the same order', call. = FALSE)
  is_dim <- names(axes) %in% dim_names

  # Resolve indexing per axis
  selection <- lapply(names(axes), function(axis_name) {
    gza_crds <- axes[[axis_name]]$coordinates
    var_crds <- var_axes[[match(axis_name, var_names)]]$values

    target <- if (inherits(gza_crds, 'CoordinatesTime')) gza_crds$offsets
              else gza_crds$values

    low <- if (is.character(var_crds)) which(target == var_crds[1L])
    else which(.near(target, var_crds[1L]))
    if (!length(low))
      stop('First coordinate value not found in the GeoZarr array axis', call. = FALSE)

    high <- low + length(var_crds) - 1L
    if (high > length(target))
      stop('Axis coordinates extend beyond the GeoZarr array axis', call. = FALSE)

    ok <- if (is.character(var_crds)) identical(target[low:high], var_crds)
          else all(.near(target[low:high], var_crds))
    if (!ok)
      stop('Axis coordinates are not a contiguous subset of the GeoZarr array axis', call. = FALSE)

    c(low, high)
  })

  # Build a tile set, on the basis of the geozarr chunk shape
  chunk_shape <- geozarr$chunking$chunk_shape
  per_axis <- lapply(seq_along(selection), function(d) {
    lo <- selection[[d]][1L]; hi <- selection[[d]][2L]
    cs <- chunk_shape[d]
    starts <- seq.int(((lo - 1L) %/% cs) * cs + 1L, hi, by = cs)
    lapply(starts, function(s) c(max(s, lo), min(s + cs - 1L, hi)))
  })
  idx_grid <- expand.grid(lapply(per_axis, seq_along), KEEP.OUT.ATTRS = FALSE)
  tiles <- lapply(seq_len(nrow(idx_grid)), function(r)
             lapply(seq_along(per_axis), function(d) per_axis[[d]][[idx_grid[r, d]]]))

  # Missing cells are written with the missing value of the CF variable, if it
  # has one. Otherwise `NA` is stored as the Zarr fill value by `write()`. The
  # missing value and valid range are also recorded in the array attributes,
  # overwriting those from a previous call for the same array.
  enc <- if (var$has_resource) .cf_missing_encoding(var$NC) else list()
  missing_value <- enc$missing_value
  if (!is.null(missing_value))
    geozarr$set_attribute("missing_value", missing_value)
  if (!is.null(enc$valid_range))
    geozarr$set_attribute("valid_range", enc$valid_range)
  geozarr$save()

  # Read var -> write geozarr, in memory-safe tiles
  n_tiles <- length(tiles)
  for (i in seq_len(n_tiles)) {
    tile <- tiles[[i]]
    if (verbose)
      cat(sprintf("\r  Chunk %d of %d (%d%%)", i, n_tiles, as.integer(100 * (i - 1L) / n_tiles)))
    local_start <- vapply(seq_along(tile), function(d) tile[[d]][1L] - selection[[d]][1L] + 1L, integer(1L))
    local_count <- vapply(seq_along(tile), function(d) tile[[d]][2L] - tile[[d]][1L] + 1L, integer(1L))
    data <- var$read_window(local_start[is_dim], local_count[is_dim])
    dim(data) <- local_count # include any scalar axes, restore dropped dimensions
    data <- .fill_missing(data, missing_value)
    geozarr$write(data, tile, flush = FALSE)
  }
  geozarr$flush()
  if (verbose)
    cat(sprintf("\r  Chunk %d of %d (100%%)\n", n_tiles, n_tiles))

  invisible(var)
}

# The unit for a GeoZarr axis from the "units" attribute of a CF axis.
#' @noRd
.axis_unit <- function(ax) {
  unit <- ax$attribute("units")
  if (is.na(unit) || unit == "") "1"
  else if (grepl("^degree(s?)(_?)(east|E)$", unit) ||
           grepl("^degree(s?)(_?)(north|N)$", unit)) "degree"
  else unit
}

# The unit, calendar and epoch of a `CFAxisTime`, as used by GeoZarr.
#' @noRd
.axis_time_def <- function(ax) {
  tm <- ax$time
  cal <- tm$calendar
  epoch <- cal$origin_date
  if (cal$origin_time != "00:00:00")
    epoch <- paste(epoch, cal$origin_time, sep = "T")
  list(unit = tm$unit, calendar = cal$name, epoch = epoch)
}

# The direction of a `CFAxisVertical` for GeoZarr, from its "positive" attribute.
# If the attribute is absent the direction is down, as in `geozarr_coordinates()`.
#' @noRd
.vertical_direction <- function(ax) {
  positive <- ax$attribute("positive")
  if (is.na(positive)) "DOWN" else toupper(positive)
}

# Is `v` a contiguous subset of the coordinates `target`, by position?
#' @noRd
.contiguous_in <- function(v, target, character) {
  low <- if (character) match(v[1L], target) else which.min(abs(target - v[1L]))
  if (!length(low) || is.na(low)) return(FALSE)
  high <- low + length(v) - 1L
  if (high > length(target)) return(FALSE)
  if (character) identical(target[low:high], v)
  else all(.near(target[low:high], v))
}

# Union of the coordinates of several axes, with the boundary values if given.
# Numeric coordinates are sorted in the direction of the axes, with duplicates
# removed (within tolerance). Character coordinates are in order of appearance.
# `vals` is a list of coordinate vectors, `bnds` a list of 2 x n matrices or
# NULL elements. Returns a list with elements `values` and `boundaries`, the
# latter NULL unless every axis has boundary values.
#' @noRd
.union_axis <- function(vals, bnds, character, name = "") {
  all_bnds <- if (!any(vapply(bnds, is.null, logical(1L)))) do.call(cbind, bnds)

  if (character)
    return(list(values = unique(unlist(vals, use.names = FALSE)), boundaries = NULL))

  all_vals <- unlist(vals, use.names = FALSE)
  dirs <- vapply(vals, function(v) if (length(v) < 2L) 0L else if (v[2L] > v[1L]) 1L else -1L, integer(1L))
  dirs <- unique(dirs[dirs != 0L])
  if (length(dirs) > 1L)
    stop("Axis '", name, "' has increasing coordinates in some data variables and decreasing in others", call. = FALSE)

  ord <- order(all_vals, decreasing = length(dirs) == 1L && dirs == -1L)
  s <- all_vals[ord]
  n <- length(s)
  keep <- rep(TRUE, n)
  if (n > 1L) {
    tol <- pmax(CF$eps * pmax(abs(s[-1L]), abs(s[-n])), 1e-12)
    keep <- c(TRUE, abs(diff(s)) > tol)
  }

  list(values = s[keep],
       boundaries = if (!is.null(all_bnds)) all_bnds[, ord, drop = FALSE][, keep, drop = FALSE])
}

# Build the description of one axis of the aggregated array from the same axis
# in each of the data variables, a list of `CFAxis` instances. If `extend` is
# `FALSE` the axes have to be identical, otherwise the coordinates of the axes
# are combined.
#' @noRd
.aggregate_axis <- function(ax, extend, weight = NULL) {
  first <- ax[[1L]]
  name <- first$name
  cls <- class(first)[1L]
  if (!all(vapply(ax, function(a) class(a)[1L] == cls, logical(1L))))
    stop("Axis '", name, "' is not of the same type in all data variables", call. = FALSE)

  is_time <- inherits(first, "CFAxisTime")
  is_char <- inherits(first, "CFAxisCharacter")
  is_discrete <- inherits(first, "CFAxisDiscrete")
  is_vertical <- inherits(first, "CFAxisVertical")
  if (is_discrete && extend)
    stop("Axis '", name, "' is a discrete axis and cannot be aggregated along", call. = FALSE)

  if (is_time) {
    defs <- lapply(ax, .axis_time_def)
    if (!all(vapply(defs, identical, logical(1L), defs[[1L]])))
      stop("Axis '", name, "' has different units, calendars or epochs in the data variables", call. = FALSE)
  }

  if (is_vertical) {
    dirs <- vapply(ax, .vertical_direction, character(1L))
    if (any(dirs != dirs[1L]))
      stop("Axis '", name, "' has different 'positive' attributes in the data variables", call. = FALSE)
  }

  vals <- lapply(ax, function(a) {
    if (is_time) a$time$offsets
    else if (is_discrete) seq_len(a$length)
    else a$values
  })
  bnds <- lapply(ax, function(a) a$bounds$values)

  if (extend) {
    if (any(vapply(bnds, is.null, logical(1L))) && !all(vapply(bnds, is.null, logical(1L))))
      warning("Axis '", name, "' has boundary values in some data variables but not in all: no boundary values are used", call. = FALSE)
    crds <- .union_axis(vals, bnds, is_char, name)
    for (i in seq_along(vals))
      if (!.contiguous_in(vals[[i]], crds$values, is_char))
        stop("The coordinates of axis '", name, "' of data variable ", i,
             " are not a contiguous subset of the aggregated axis", call. = FALSE)
  } else {
    for (i in seq_along(ax)[-1L]) {
      same <- first$identical(ax[[i]]) &&
        identical(is.null(bnds[[1L]]), is.null(bnds[[i]])) &&
        (is.null(bnds[[1L]]) || all(.near(bnds[[1L]], bnds[[i]])))
      if (!same)
        stop("Axis '", name, "' of data variable ", i, " is different from that of data variable 1 ",
             "and the axis is not in `along`", call. = FALSE)
    }
    crds <- list(values = vals[[1L]], boundaries = bnds[[1L]])
  }

  atts <- .merge_attributes(lapply(ax, function(a) a$geozarr_attributes()),
                            paste0("Attributes of axis '", name, "'"))

  out <- list(coordinates = crds$values,
              boundaries = crds$boundaries,
              attributes = if (length(atts)) atts,
              abbreviation = if (nzchar(first$orientation)) first$orientation,
              direction = if (is_vertical) dirs[1L],
              chunk_weight = weight)
  if (is_time) {
    out$unit <- defs[[1L]]$unit
    out$calendar <- defs[[1L]]$calendar
    out$epoch <- defs[[1L]]$epoch
  } else if (!is_char)
    out$unit <- .axis_unit(first)
  Filter(Negate(is.null), out)
}

# Build the `axes` argument for `geozarr::create_geozarr_array()` from a list of
# `CFVariable` instances.
#
# Details:
# * `vars` is a list of `CFVariable` instances that all have the same
#   dimensional axes, by name and in the same order. A mismatch is an error.
# * `along` is a character vector of the names of the axes along which the
#   variables are aggregated. The coordinates of those axes are combined. Any
#   other axis has to be identical in all variables, including boundary values,
#   or an error is thrown. Overlapping coordinates are not an error: the
#   duplicates are dropped. Gaps are allowed, but the coordinates of each
#   variable have to be a contiguous subset of the combined axis, or an error is
#   thrown.
# * An axis in `along` that is a scalar axis in the variables becomes a new
#   dimension of the aggregated array, placed after the dimensional axes in the
#   order given in `along`. It has to be a scalar axis in every variable.
#   Scalar axes that are not in `along` are ignored.
# * Time axes have to agree in unit, calendar and epoch. Vertical axes have to
#   agree in their "positive" attribute.
# * Discrete axes cannot be aggregated along.
# * `chunking` is an optional named numeric vector with chunk weights for the
#   axes.
#
# Returns a named list with an element for each axis.
#' @noRd
.aggregate_axes <- function(vars, along, chunking = NULL) {
  if (!is.list(vars) || !length(vars) || !all(vapply(vars, inherits, logical(1L), "CFVariable")))
    stop("Argument `vars` must be a list of `CFVariable` instances", call. = FALSE)

  dim_axes <- lapply(vars, function(v) v$axes[seq_along(v$dim())])
  ax_names <- lapply(dim_axes, function(a) vapply(a, function(x) x$name, character(1L)))
  if (!all(vapply(ax_names, identical, logical(1L), ax_names[[1L]])))
    stop("The data variables do not have the same axes, by name and in the same order", call. = FALSE)
  ax_names <- ax_names[[1L]]

  # Axes of each variable by name, dimensional and scalar
  var_axes <- lapply(vars, function(v) setNames(v$axes, vapply(v$axes, function(a) a$name, character(1L))))
  if (!is.character(along) || !all(vapply(var_axes, function(a) all(along %in% names(a)), logical(1L))))
    stop("Argument `along` must be the name of axes present in the data variables", call. = FALSE)

  weight <- function(nm) if (nm %in% names(chunking)) chunking[[nm]]
  axes <- lapply(seq_along(ax_names), function(i) {
    nm <- ax_names[i]
    .aggregate_axis(lapply(dim_axes, `[[`, i), nm %in% along, weight(nm))
  })
  names(axes) <- ax_names

  # Scalar axes to aggregate along become new dimensions
  scalar <- setdiff(along, ax_names)
  scalar_axes <- lapply(scalar, function(nm)
    .aggregate_axis(lapply(var_axes, `[[`, nm), TRUE, weight(nm)))
  names(scalar_axes) <- scalar
  c(axes, scalar_axes)
}

# The CRS of a list of `CFVariable` instances, a `CFGridMapping` instance or
# NULL if the variables have no CRS. The variables must all have the same CRS,
# compared by the attributes of their grid mapping, otherwise an error is
# thrown.
#' @noRd
.aggregate_crs <- function(vars) {
  crs <- lapply(vars, function(v) v$crs)
  none <- vapply(crs, is.null, logical(1L))
  if (all(none)) return(NULL)
  if (any(none) || !all(vapply(crs[-1L], function(c) crs[[1L]]$attributes_identical(c$attributes), logical(1L))))
    stop("The data variables do not all have the same CRS", call. = FALSE)
  crs[[1L]]
}

# Retrieve the auxiliary label sets of an axis as a named list of `CFLabel`
# instances. Other types of auxiliary coordinates are not included. The active
# coordinate set of the axis is left as it was.
#' @noRd
.axis_label_sets <- function(ax) {
  nms <- ax$coordinate_names[-1L]
  if (!length(nms)) return(list())
  active <- ax$active_coordinates
  on.exit(ax$active_coordinates <- active)
  sets <- lapply(nms, function(nm) {
    ax$active_coordinates <- nm
    ax$auxiliary
  })
  names(sets) <- nms
  sets[vapply(sets, inherits, logical(1L), "CFLabel")]
}

# Position of each of the `target` coordinates in the coordinates `v` of one
# axis, `NA` where not present. Numeric coordinates are matched with tolerance.
#' @noRd
.match_coordinates <- function(target, v) {
  if (is.character(target) || is.character(v)) return(match(target, v))
  vapply(target, function(x) {
    i <- which.min(abs(v - x))
    if (length(i) && abs(v[i] - x) <= max(CF$eps * abs(x), 1e-12)) i else NA_integer_
  }, integer(1L), USE.NAMES = FALSE)
}

# Compile the auxiliary label sets for the axes of the aggregated array.
#
# Details:
# * Only label sets (`CFLabel`) are processed, other auxiliary coordinates are
#   ignored. A label set must be present in all data variables, otherwise it is
#   dropped with a warning.
# * For an axis that is not aggregated along, the labels must be identical
#   among the data variables, otherwise an error is thrown.
# * For an axis that is aggregated along, the labels are assembled in the order
#   of the aggregated axis coordinates. If different data variables have
#   different labels for the same coordinate an error is thrown.
# * `axes` is the result of `.aggregate_axes()`.
#
# Returns a named list, by axis name, with for each axis a list of
# `geozarr::CoordinatesString` instances. Axes without label sets are omitted.
#' @noRd
.aggregate_labels <- function(vars, along, axes) {
  var_axes <- lapply(vars, function(v) setNames(v$axes, vapply(v$axes, function(a) a$name, character(1L))))
  out <- list()
  for (ax_name in names(axes)) {
    sets <- lapply(var_axes, function(a) .axis_label_sets(a[[ax_name]]))
    all_names <- unique(unlist(lapply(sets, names), use.names = FALSE))
    if (!length(all_names)) next

    common <- all_names[vapply(all_names, function(nm) all(vapply(sets, function(s) nm %in% names(s), logical(1L))), logical(1L))]
    if (length(dropped <- setdiff(all_names, common)))
      warning("Label sets ", paste0("'", dropped, "'", collapse = ", "), " of axis '", ax_name,
              "' are not in all data variables and are not included", call. = FALSE)

    if (!length(common)) next

    target <- axes[[ax_name]]$coordinates
    crds <- lapply(common, function(nm) {
      labs <- lapply(sets, `[[`, nm)
      if (ax_name %in% along) {
        res <- rep(NA_character_, length(target))
        for (i in seq_along(vars)) {
          ax <- var_axes[[i]][[ax_name]]
          pos <- .match_coordinates(target, ax$coordinates)
          have <- which(!is.na(pos))
          vals <- as.character(labs[[i]]$coordinates)[pos[have]]
          clash <- !is.na(res[have]) & res[have] != vals
          if (any(clash))
            stop("Label set '", nm, "' of axis '", ax_name, "' has different labels for the same coordinate in different data variables", call. = FALSE)
          res[have] <- vals
        }
        if (anyNA(res))
          stop("Label set '", nm, "' of axis '", ax_name, "' does not cover all coordinates", call. = FALSE)
        vals <- res
      } else {
        vals <- as.character(labs[[1L]]$coordinates)
        if (!all(vapply(labs[-1L], function(l) identical(as.character(l$coordinates), vals), logical(1L))))
          stop("Label set '", nm, "' of axis '", ax_name, "' is not identical in all data variables", call. = FALSE)
      }
      geozarr::CoordinatesString$new(nm, "OTHER", "1", vals, labs[[1L]]$attributes_as_list())
    })
    out[[ax_name]] <- setNames(crds, common)
  }
  out
}

# The names of the axes of data variable `v` that its auxiliary
# longitude-latitude grid is defined over, in the order of the grid dimensions.
# They are found by dimension id: the axes of the grid variables themselves are
# not reliable, they are missing for dimensions without a coordinate variable.
#' @noRd
.llgrid_dims <- function(v) {
  ax <- v$axes[seq_along(v$dim())]
  vapply(v$gridLongLat$dimids, function(id) {
    i <- which(vapply(ax, function(a) isTRUE(a$dimid == id), logical(1L)))
    if (length(i) != 1L)
      stop("Cannot find the axes of the auxiliary longitude-latitude grid in data variable '", v$name, "'", call. = FALSE)
    ax[[i]]$name
  }, character(1L), USE.NAMES = FALSE)
}

# Values of the longitude or latitude grid of an auxiliary grid as a matrix.
#' @noRd
.ll_values <- function(v) {
  d <- v$values
  if (length(dim(d)) > 2L) d[, , 1L] else d
}

# Do two numeric arrays have the same shape and (nearly) the same values?
#' @noRd
.same_grid <- function(a, b) {
  identical(dim(a), dim(b)) && identical(is.na(a), is.na(b)) &&
    all(.near(a[!is.na(a)], b[!is.na(b)]))
}

# Compile the auxiliary longitude-latitude grid for the aggregated array.
#
# Details:
# * Either all data variables have an auxiliary grid or none.
# * The grid is defined over two axes of the data variables, which can not be
#   aggregated along, nor can any other X or Y axis. They have to be the same
#   in all data variables, as must the grid values and boundary values.
# * If the axes of the grid do not have an orientation, as is usual for the
#   discrete axes of curvilinear grids, the first is the X axis and the second
#   the Y axis, as is required for geolocation in GeoZarr.
# * `axes` is the result of `.aggregate_axes()`.
#
# Returns `NULL` if there are no auxiliary grids, otherwise a list with `axes`,
# the aggregated axes with orientation set for the grid axes, and `args`, the
# arguments for `geozarr_array$set_geolocation()`.
#' @noRd
.aggregate_geolocation <- function(vars, along, axes) {
  ll <- lapply(vars, function(v) v$gridLongLat)
  has <- !vapply(ll, is.null, logical(1L))
  if (!any(has)) return(NULL)
  if (!all(has))
    stop("Not all data variables have an auxiliary longitude-latitude grid", call. = FALSE)

  dims <- .llgrid_dims(vars[[1L]])
  if (!all(vapply(vars[-1L], function(v) identical(.llgrid_dims(v), dims), logical(1L))) ||
      !all(vapply(ll[-1L], function(g) identical(g$grid_names, ll[[1L]]$grid_names), logical(1L))))
    stop("The auxiliary longitude-latitude grids are not the same in all data variables", call. = FALSE)

  for (d in dims)
    if (is.null(axes[[d]]$abbreviation))
      axes[[d]]$abbreviation <- c("X", "Y")[match(d, dims)]
  is_xy <- vapply(axes, function(a) (a$abbreviation %||% "") %in% c("X", "Y"), logical(1L))
  bad <- intersect(along, c(dims, names(axes)[is_xy]))
  if (length(bad))
    stop("Axes ", paste0("'", bad, "'", collapse = ", "), " cannot be in `along`: the data variables have ",
         "an auxiliary longitude-latitude grid, which is defined over the X and Y axes", call. = FALSE)

  first <- ll[[1L]]
  lon <- .ll_values(first$lon)
  lat <- .ll_values(first$lat)
  bl <- first$lon_bounds
  bt <- first$lat_bounds
  has_bnds <- !is.null(bl) && !is.null(bt) && bl$vertices == 4L && bt$vertices == 4L
  same <- vapply(ll[-1L], function(g) {
    gb <- !is.null(g$lon_bounds) && !is.null(g$lat_bounds)
    .same_grid(lon, .ll_values(g$lon)) && .same_grid(lat, .ll_values(g$lat)) &&
      identical(gb, has_bnds) &&
      (!has_bnds || (.same_grid(bl$values, g$lon_bounds$values) && .same_grid(bt$values, g$lat_bounds$values)))
  }, logical(1L))
  if (!all(same))
    stop("The auxiliary longitude-latitude grids have different values in the data variables", call. = FALSE)

  atts <- function(v) {
    a <- v$attributes_as_list(delete = "bounds")
    a$units <- "degree"
    a
  }
  args <- list(kind = "geodetic", x = lon, y = lat, dims = dims, names = first$grid_names,
               id = geozarr::zarr_conv_proj$new(list(code = "OGC:CRS84")),
               attributes = list(atts(first$lon), atts(first$lat)))
  if (has_bnds)
    args <- c(args, list(x_bounds = bl$values, y_bounds = bt$values,
                         bounds_dims = c("vertices", dims)))
  list(axes = axes, args = args)
}

# Merge the attributes of multiple objects, each a named list, into one. If the
# attributes differ among the objects, or are not in all of them, a warning is
# issued that lists the attributes and the value from the last object that has
# the attribute is used. `subject` starts the warning message.
#' @noRd
.merge_attributes <- function(atts, subject) {
  all_names <- unique(unlist(lapply(atts, names), use.names = FALSE))
  differ <- all_names[!vapply(all_names, function(nm) {
    vals <- lapply(atts, `[[`, nm)
    !any(vapply(vals, is.null, logical(1L))) && all(vapply(vals, identical, logical(1L), vals[[1L]]))
  }, logical(1L))]
  if (length(differ))
    warning(subject, " differ or are not in all data variables: ",
            paste0("'", differ, "'", collapse = ", "), "; the last value is used", call. = FALSE)

  Reduce(function(a, b) { a[names(b)] <- b; a }, atts)
}

# Set the global attributes of the data variables on a Zarr group.
#
# Details:
# * The attributes of the group of each data variable are used, which for a
#   file without subgroups are its global attributes.
# * No single value describes the aggregate when the value of an attribute
#   differs among the files, as is the case for e.g. a creation date or a
#   tracking id. Only attributes that have the same value in all data variables
#   are set.
# * The "history" attribute is the exception. It is set as an array of entries,
#   most recent first: an entry for the aggregation itself, followed by the
#   entries of each distinct history among the data variables, see
#   `.split_history()`. The entries of different histories are not interleaved.
# * Attributes that already exist on `location` are overwritten.
#
# Returns the names of the attributes that were set, invisibly.
#' @noRd
.aggregate_global_attributes <- function(vars, location, name) {
  grps <- lapply(vars, function(v) v$group)
  grps <- grps[!vapply(grps, is.null, logical(1L))]
  if (!length(grps)) return(invisible(character()))

  atts <- lapply(grps, function(g) g$attributes_as_list())
  same <- names(atts[[1L]])[vapply(names(atts[[1L]]), function(nm) {
    vals <- lapply(atts, `[[`, nm)
    !any(vapply(vals, is.null, logical(1L))) && all(vapply(vals, identical, logical(1L), vals[[1L]]))
  }, logical(1L))]
  same <- setdiff(same, "history")

  for (nm in same)
    tryCatch(location$set_attribute(nm, atts[[1L]][[nm]]),
             error = function(e) warning("Global attribute '", nm, "' could not be set", call. = FALSE))

  # An unnamed list is a JSON array, also when there is only one entry
  hist <- unique(unlist(Filter(is.character, lapply(atts, `[[`, "history")), use.names = FALSE))
  entry <- .history_entry(paste0("Aggregated data variables \"", vars[[1L]]$name, "\" from ",
                                length(vars), " netCDF files into Zarr array \"", name, "\""))
  location$set_attribute("history", as.list(c(entry, unlist(lapply(hist, .split_history), use.names = FALSE))))

  location$save()
  invisible(c(same, "history"))
}

# Resolve the group for external coordinate arrays. `path` is relative to the
# group `location`, or `NULL` for `location` itself. The group is created if it
# does not exist. The path may go up the hierarchy with "..", but not beyond
# the root group. Returns the path relative to the array that is created in
# `location`, e.g. "../coords" or "../../shared", or `NULL` if `path` is `NULL`.
#' @noRd
.external_group <- function(location, path) {
  if (is.null(path)) return(NULL)
  if (!is.character(path) || length(path) != 1L || is.na(path))
    stop("Argument `external_coordinates` must be a single character string", call. = FALSE)
  parts <- strsplit(path, "/", fixed = TRUE)[[1L]]
  parts <- parts[nzchar(parts) & parts != "."]
  if (!length(parts)) return(NULL)

  .zarr_group(location, paste(parts, collapse = "/"))
  paste(c("..", parts), collapse = "/")
}

# Create the GeoZarr array to aggregate a list of `CFVariable` instances into.
#
# Details:
# * The axes are determined by `.aggregate_axes()`, which see for the arguments
#   `vars`, `along` and `chunking` and the errors that it throws.
# * The data variables must all have the same netCDF data type, otherwise an
#   error is thrown: other properties are tied to the data type.
# * The Zarr `fill_value` is the default for the data type.
# * The data variables must all have the same CRS, or none must have one,
#   otherwise an error is thrown. The CRS is set on the array.
# * Auxiliary label sets of the axes are added to the array as additional
#   coordinates, see `.aggregate_labels()`.
# * An auxiliary longitude-latitude grid is set as geolocation of the array, see
#   `.aggregate_geolocation()`. The X and Y axes cannot be in `along`.
# * Parametric vertical coordinates (CF formula terms) are not supported.
# * The attributes of the data variables are set on the array, leaving out those
#   that are structural or that are set from the data (`coords`, `grid_mapping`,
#   `actual_range` and the valid range attributes). If the attributes differ
#   between the data variables, or are not present in all of them, a warning is
#   issued that lists the attributes. The value from the last data variable that
#   has the attribute is used.
# * `location` is a `zarr_group` where the array is created, with the name of
#   the first data variable if `name` is not given.
# * `external` is the path of a group relative to `location` where external
#   coordinate, label and geolocation arrays are written, see
#   `.external_group()`.
#
# Returns the new `geozarr_array`.
#' @noRd
.aggregate_array <- function(vars, along, location, name = NULL, chunking = NULL, external = NULL) {
  external <- .external_group(location, external)
  axes <- .aggregate_axes(vars, along, chunking)
  crs <- .aggregate_crs(vars)
  labels <- .aggregate_labels(vars, along, axes)
  geo <- .aggregate_geolocation(vars, along, axes)
  if (!is.null(geo)) axes <- geo$axes

  types <- unique(vapply(vars, function(v) v$data_type, character(1L)))
  if (length(types) > 1L)
    stop("The data variables do not all have the same data type: ", paste(types, collapse = ", "), call. = FALSE)

  if (!is.null(crs))
    crs <- list(compound = list(wkt2 = crs$wkt2(.wkt2_axis_info(vars[[1L]]))))

  if (!is.null(external))
    axes$.convention <- list(coordinate_group = external)

  gza <- geozarr::create_geozarr_array(name %||% vars[[1L]]$name, location, axes,
                                       netcdf_to_zarr_data_type[[types]], crs = crs)

  # Auxiliary label sets
  for (ax_name in names(labels))
    for (crd in labels[[ax_name]])
      gza$add_coordinates(ax_name, crd, external_group = external)

  if (!is.null(geo))
    do.call(gza$set_geolocation, c(geo$args, list(geolocation_group = external)))

  # Attributes, last one wins
  atts <- lapply(vars, function(v)
    v$attributes_as_list(delete = c("coords", "grid_mapping", "actual_range",
                                    "valid_range", "valid_min", "valid_max")))
  merged <- .merge_attributes(atts, "Attributes of the data variables")
  for (nm in names(merged))
    tryCatch(gza$set_attribute(nm, merged[[nm]]),
             error = function(e) warning("Attribute '", nm, "' could not be set on the array", call. = FALSE))
  gza$save()

  .aggregate_global_attributes(vars, location, gza$path)
  gza
}
