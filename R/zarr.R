# This script contains utility functions for converting between netCDF/CF and
# Zarr.

# Check that the suggested package `geozarr` is installed and stop with an
# informative message if not. Package `zarr` is imported by `geozarr`, so it is
# installed if `geozarr` is. Argument `pkg` is for testing only.
#' @noRd
.check_geozarr_package <- function(pkg = "geozarr") {
  if (!requireNamespace(pkg, quietly = TRUE))
    stop("Package '", pkg, "' must be installed for this functionality", call. = FALSE)
  invisible(TRUE)
}

# The Zarr equivalents of the netCDF data types
netcdf_to_zarr_data_type <- c("NC_BYTE" = "int8", "NC_UBYTE" = "uint8",
                              "NC_CHAR" = "string", "NC_SHORT" = "int16",
                              "NC_USHORT" = "uint16", "NC_INT" = "int32",
                              "NC_UINT" = "uint32", "NC_INT64" = "int64",
                              "NC_UINT64" = "uint64", "NC_FLOAT" = "float32",
                              "NC_DOUBLE" = "float64", "NC_STRING" = "string")

# Translate the CF missing data and valid range attributes of a netCDF variable
# into the GeoZarr encoding:
#
# * The Zarr `fill_value` is never derived from CF attributes: it is the Zarr
#   default for the data type and means "never written".
# * `missing_value` is a single value, or NULL. It is the value that written,
#   but missing, cells carry. It is taken from the `missing_value` attribute
#   (first element) or else from a `_FillValue` that differs from the netCDF
#   library default. Packed variables get none: the sentinel belongs to the
#   packed encoding and is not transformed (CF 2.5.1), so missing cells in the
#   unpacked data are plain `NA`.
# * `valid_range` is a numeric vector of length 2 in unpacked units, or NULL.
#   `valid_range` takes precedence over `valid_min`/`valid_max`; a missing bound
#   is replaced by the largest finite double because JSON cannot encode Inf.
#   The valid range is recorded for documentation, because the data were already
#   masked on read.
#
# `nc` is an `NCVariable`. Returns a list with elements `missing_value` and
# `valid_range`.
#' @noRd
.cf_missing_encoding <- function(nc) {
  none <- list(missing_value = NULL, valid_range = NULL)
  vtype <- nc$vtype
  if (!(vtype %in% names(nc_default_fill))) return(none) # character types

  packed <- nc$is_packed
  att <- function(name) {
    val <- nc$attribute(name)
    if (length(val) == 1L && is.na(val)) NULL else val
  }

  # Missing value sentinel
  missing_value <- NULL
  if (!packed) {
    mv <- att("missing_value")
    if (!is.null(mv))
      missing_value <- mv[[1L]]
    else if (!is.null(fv <- att("_FillValue")) &&
             !.near(as.numeric(fv[[1L]]), nc_default_fill[[vtype]]))
      missing_value <- fv[[1L]]
  }

  # Valid range
  vr <- att("valid_range")
  if (is.null(vr)) {
    vmin <- att("valid_min")
    vmax <- att("valid_max")
    if (!is.null(vmin) || !is.null(vmax))
      vr <- c(as.numeric(vmin %||% -.Machine$double.xmax),
              as.numeric(vmax %||%  .Machine$double.xmax))
  }
  if (!is.null(vr)) {
    vr <- as.numeric(vr)[1:2]
    if (packed) {
      scale  <- att("scale_factor") %||% 1
      offset <- att("add_offset") %||% 0
      vr <- sort(vr * scale + offset) # negative scale reverses the order
    }
  }

  list(missing_value = missing_value, valid_range = vr)
}

# Replace `NA` in `x` with the missing value `mv`, coerced to the storage mode of
# `x`. When `mv` is NULL `x` is returned as-is, leaving `NA` to be stored as the
# Zarr fill value by `zarr_array$write()`.
#' @noRd
.fill_missing <- function(x, mv) {
  if (is.null(mv)) return(x)
  storage.mode(mv) <- storage.mode(x)
  x[is.na(x)] <- mv
  x
}

# Write `vals` to a new Zarr array `name` in Zarr group `grp`, unless a node
# with that name already exists in the group (shared arrays are written once).
# The data type and fill value are taken from the netCDF resource that `obj`
# is linked to, if any, otherwise from the storage mode of `vals`. `dims` gives
# the dimension names, in the order of `dim(vals)`.
#' @noRd
.write_zarr_array <- function(grp, name, obj, vals, dims, atts = list()) {
  if (!is.null(grp$children[[name]])) return(invisible(NULL))

  ab <- zarr::array_builder$new()
  ab$shape <- dim(vals) %||% length(vals)
  nc <- obj$NC
  if (is.null(nc))
    ab$data_type_from_storage.mode <- storage.mode(vals)
  else {
    ab$data_type <- netcdf_to_zarr_data_type[[obj$data_type]]
  }
  ab$chunk_shape <- zarr::optimal_chunking(ab$shape)
  if (prod(ab$shape) > zarr::zarr_options("min_compress"))
    ab$add_codec("blosc", list(clevel = 6L))

  # The Zarr fill value is the default for the data type. CF missing data
  # attributes are translated, see `.cf_missing_encoding()`.
  enc <- if (is.null(nc)) list() else .cf_missing_encoding(nc)
  atts[c("valid_min", "valid_max")] <- NULL
  if (!is.null(enc$missing_value)) atts$missing_value <- enc$missing_value
  if (!is.null(enc$valid_range))   atts$valid_range   <- enc$valid_range
  vals <- .fill_missing(vals, enc$missing_value)

  meta <- ab$metadata()
  meta$dimension_names <- as.list(dims)
  if (length(atts))
    meta$attributes <- atts

  arr <- try(grp$add_array(name, meta), silent = TRUE)
  if (inherits(arr, "try-error"))
    stop("Could not create Zarr array with name ", name, call. = FALSE)
  arr$write(vals)
  arr$dirty <- TRUE
  arr$save()
  invisible(arr)
}

# Walk from Zarr group `grp` along the relative `path`, creating any groups
# that do not yet exist. Returns the target `zarr_group`.
#' @noRd
.zarr_group <- function(grp, path) {
  parts <- strsplit(path, "/", fixed = TRUE)[[1L]]
  for (nm in parts[nzchar(parts) & parts != "."]) {
    grp <- if (nm == "..") {
      if (!inherits(grp$parent, "zarr_group"))
        stop("Zarr group path goes beyond the root group", call. = FALSE)
      grp$parent
    } else
      grp$children[[nm]] %||% grp$add_group(nm)
    if (!inherits(grp, "zarr_group"))
      stop("Zarr node '", nm, "' is not a group", call. = FALSE)
  }
  grp
}

# Write the data of CF object `obj` to Zarr array `arr` in slabs, so that the
# data is never held in memory in its entirety. Slabs are whole multiples of
# `chunk` (the chunk shape of `arr`), so every chunk is written exactly once,
# and they are grown from the first dimension up to
# CF.options$memory_cell_limit bytes, counted at 8 bytes per value as read
# into R. Writing a slab takes roughly two more copies of it.
#' @noRd
.write_zarr_slabs <- function(arr, obj, chunk, missing_value = NULL) {
  shape <- obj$dim()
  max_cells <- max(prod(chunk$chunk_shape), CF.options$memory_cell_limit %/% 8)
  slab <- pmin(chunk$chunk_shape, shape)
  for (d in seq_along(shape)) {
    k <- min(ceiling(shape[d] / slab[d]), max_cells %/% prod(slab))
    if (k < 2L) break
    slab[d] <- min(shape[d], slab[d] * k)
  }

  starts <- expand.grid(lapply(seq_along(shape), function(d) seq(1L, shape[d], by = slab[d])))
  for (i in seq_len(nrow(starts))) {
    start <- as.integer(starts[i, ])
    count <- pmin(slab, shape - start + 1L)
    vals <- obj$read_chunk(start, count)
    dim(vals) <- count  # restore any dropped degenerate dimensions
    vals <- .fill_missing(vals, missing_value)
    arr$write(vals, lapply(seq_along(start), function(d) c(start[d], start[d] + count[d] - 1L)))
  }
  invisible(arr)
}

# Splitting the "history" attribute into a vector of entries, which will become
# a JSON array in the attributes of a CFDataset exported to a Zarr store. This
# is a best-effort, based on some implicit "conventions" that are not part of
# CF. In particular, the CMOR and NCO/CDO conventions should split properly. In
# general, the "history" attribute is split on a timestamp preceded by a
# semi-colon. Given the variety of formats having been used over the years, the
# splitting algorithm is heuristic. Extracted entries will not be reordered but
# on the Zarr side new entries will be placed ahead of existing entries.
.history_stamp <- paste0(
  "(?:\\d{4}-\\d{2}-\\d{2}[T ]\\d{2}:\\d{2}(?::\\d{2})?",
  "|(?:Mon|Tue|Wed|Thu|Fri|Sat|Sun) (?:", paste(month.abb, collapse = "|"),
  ") +\\d{1,2} \\d{2}:\\d{2}:\\d{2} \\d{4})")

# First timestamp of each entry as a sortable "YYYY-MM-DD HH:MM[:SS]" string, or NA
.history_time <- function(x) {
  pos <- regexpr(.history_stamp, x, perl = TRUE)
  s <- rep(NA_character_, length(x))
  s[pos > 0L] <- regmatches(x, pos)
  ct <- grepl("^[A-Z]", s)
  s[ct] <- vapply(strsplit(s[ct], " +"), function(e)
    sprintf("%s-%02d-%02d %s", e[5L], match(e[2L], month.abb), as.integer(e[3L]), e[4L]), "")
  sub("T", " ", s, fixed = TRUE)
}

# Best-effort split of a netCDF `history` string into entries, most recent first
.split_history <- function(x) {
  out <- strsplit(x, paste0("\\s*[;\\n]\\s*(?=", .history_stamp, ")"), perl = TRUE)[[1L]]
  out <- trimws(sub(";\\s*$", "", out))
  out <- out[nzchar(out)]
  t <- .history_time(out)
  t <- t[!is.na(t)]
  if (length(t) > 1L && t[1L] < t[length(t)]) rev(out) else out
}
