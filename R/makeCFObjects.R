#' Create a new data set
#'
#' This function creates a new, empty data set with an associated netCDF file in
#' "netcdf4" format.
#' @param fn Optional. The fully qualified file name of the netCDF file if that
#'   should be created. The file cannot already exist. It is recommended that
#'   the filename uses an extension of ".nc". If the argument is not provided, a
#'   virtual data set will be created.
#' @export
create_ncdf <- function(fn) {
  if (missing(fn)) {
    # Create virtual data set
    ds <- CFDataset$new("CF_dataset")
    root <- CFGroup$new("", parent = ds)
  } else {
    # Create the netCDF file on disk
    res <- NCResource$new(fn, write = TRUE)
    res$create()

    # Create the CFDataset and associate it with the new netCDF file
    ds <- CFDataset$new(res, "netcdf4")

    # Fetch the root group and add it to the CFDataset
    g <- RNetCDF::grp.inq.nc(res$handle)
    grp <- NCGroup$new(id = as.integer(g$self), name = g$name, parent = ds, resource = res)
    root <- CFGroup$new(grp, parent = ds)
  }

  # Minimal attributes
  root$set_attribute("Conventions", "NC_CHAR", "CF-1.13")
  root$set_attribute("history", "NC_CHAR", paste("Created with R package ncdfCF", utils::packageVersion("ncdfCF"), "on", format(Sys.time(), usetz = TRUE, digits = 0L)))

  ds$root <- root
  ds
}

#' Create an axis
#'
#' With this method you can create an axis to use with new [CFVariable]
#' instances. Depending on the `orientation` argument and the type of the
#' `values` argument an instance of a class descending from [CFAxis] will be
#' returned.
#'
#' There are several restrictions on the combination of `orientation` and
#' `values` arguments. Longitude, latitude and depth axes (`orientation` of "X",
#' "Y" or "Z") must have numeric `values`. For a time axis (`orientation` of
#' "T") the `values` argument must be an instance of `CFTime` or
#' `CFClimatology`.
#'
#' @param name Name of the axis.
#' @param orientation The orientation of the axis. Must be one of "X", "Y", "Z",
#'   or "T" for longitude, latitude, height or depth, and time axes,
#'   respectively. For any other axis, indicate an empty string ""
#' @param values The coordinate values. In the case of an axis with `orientation
#'   = "T"` this must be a `CFTime` or `CFClimatology` instance.
#' @param bounds The boundary values of the coordinates, or `NULL` if not
#'   available.
#' @param attributes `data.frame` with the attributes of the axis to create.
#'   Depending on which axis is created one or more attributes may be added or
#'   amended.
#' @param group [CFGroup] instance where the axis will be located. If `NULL`
#'   (default), a private group will be created for the axis.
#' @seealso [makeLongitudeAxis()], [makeLatitudeAxis()], [makeTimeAxis()],
#'   [makeDiscreteAxis()]
#' @return An instance of a class descending from [CFAxis].
#' @export
makeAxis <- function(name, orientation, values, bounds = NULL, attributes = data.frame(), group = NULL) {
  if (is.null(group))
    group <- CFGroup$new("", NULL)
  # FIXME: There could be an orientation X but with projected coordinates
  if (orientation == "X") makeLongitudeAxis(name, values, bounds, attributes, group)
  else if (orientation == "Y") makeLatitudeAxis(name, values, bounds, attributes, group)
  else if (orientation == "Z") makeVerticalAxis(name, values, bounds, attributes, group)
  else if (orientation == "T") makeTimeAxis(name, values, attributes, group)
  else if (orientation == "") {
    if (is.numeric(values)) {
      # Check if we have a potential longitude or latitude axis
      if (tolower(name) %in% c("longitude", "lon") && .check_longitude_domain(values))
        makeLongitudeAxis(name, values, bounds, attributes, group)
      else if (tolower(name) %in% c("latitude", "lat") && .check_latitude_domain(values))
        makeLatitudeAxis(name, values, bounds, attributes, group)
      else {
        # Make a generic axis
        axis <- CFAxisNumeric$new(name, group = group, values = values, attributes = attributes)

        if (!is.null(bounds)) {
          nm <- paste0(name, "_bnds")
          axis$bounds <- CFBounds$new(nm, group = group, values = bounds)
          axis$set_attribute("bounds", "NC_CHAR", nm)
        }
        axis
      }
    } else if (is.character(values)) {
      axis <- CFAxisCharacter$new(name, group = group, values = values, attributes = attributes)
    }
  } else stop("Bad `orientation` value for axis creation.", call. = FALSE)
}

#' Create a longitude axis
#'
#' With this method you can create a longitude axis to use with new [CFVariable]
#' instances.
#'
#' @param name Name of the axis.
#' @param values The coordinate values.
#' @param bounds The bounds of the coordinate values, or `NULL` if not
#'   available.
#' @param attributes `data.frame` with the attributes of the axis to create.
#'   Attributes "standard_name", "units", "actual_range" and "axis" will be set
#'   or updated.
#' @param group [CFGroup] instance where the axis will be located. If `NULL`
#'   (default), a private group will be created for the axis.
#' @return A [CFAxisLongitude] instance.
#' @export
makeLongitudeAxis <- function(name, values, bounds = NULL, attributes = data.frame(), group = NULL) {
  if (!.is_valid_name(name))
    stop("Name for axis is not valid", call. = FALSE)

  axis <- CFAxisLongitude$new(name, group = group, values = values, attributes = attributes)

  if (is.na(axis$attribute("standard_name"))) {
    axis$set_attribute("standard_name", "NC_CHAR", "longitude")
    axis$set_attribute("units", "NC_CHAR", "degrees_east")
  }
  if (!is.null(bounds)) {
    nm <- paste0(name, "_bnds")
    axis$bounds <- CFBounds$new(nm, group = group, values = bounds)
    axis$set_attribute("bounds", "NC_CHAR", nm)
  }
  axis
}

#' Create a latitude axis
#'
#' With this method you can create a latitude axis to use with new [CFVariable]
#' instances.
#'
#' @param name Name of the axis.
#' @param values The coordinate values.
#' @param bounds The bounds of the coordinate values, or `NULL` if not available.
#' @param attributes `data.frame` with the attributes of the axis to create.
#'   Attributes "standard_name", "units", "actual_range" and "axis" will be set
#'   or updated.
#' @param group [CFGroup] instance where the axis will be located. If `NULL`
#'   (default), a private group will be created for the axis.
#' @return A [CFAxisLatitude] instance.
#' @export
makeLatitudeAxis <- function(name, values, bounds = NULL, attributes = data.frame(), group = NULL) {
  if (!.is_valid_name(name))
    stop("Name for axis is not valid", call. = FALSE)

  axis <- CFAxisLatitude$new(name, group = group, values = values, attributes = attributes)

  if (is.na(axis$attribute("standard_name"))) {
    axis$set_attribute("standard_name", "NC_CHAR", "latitude")
    axis$set_attribute("units", "NC_CHAR", "degrees_north")
  }

  if (!is.null(bounds)) {
    nm <- paste0(name, "_bnds")
    axis$bounds <- CFBounds$new(nm, group = group, values = bounds)
    axis$set_attribute("bounds", "NC_CHAR", nm)
  }
  axis
}

#' Create a vertical axis
#'
#' With this method you can create a vertical axis to use with new [CFVariable]
#' instances. Note that you should set the "positive" attribute after creating
#' the axis to indicate if values are increasing going upwards (positive = "up")
#' or downwards (positive = "down").
#'
#' @param name Name of the axis.
#' @param values The coordinate values.
#' @param bounds The bounds of the coordinate values, or `NULL` if not
#'   available.
#' @param attributes `data.frame` with the attributes of the axis to create.
#'   Attributes "actual_range" and "axis" will be set or updated.
#' @param group [CFGroup] instance where the axis will be located. If `NULL`
#'   (default), a private group will be created for the axis.
#' @return A [CFAxisVertical] instance.
#' @export
makeVerticalAxis <- function(name, values, bounds = NULL, attributes = data.frame(), group = NULL) {
  if (!.is_valid_name(name))
    stop("Name for axis is not valid", call. = FALSE)
  # FIXME: Check domain

  axis <- CFAxisVertical$new(name, group = group, values = values, attributes = attributes)

  if (!is.null(bounds)) {
    nm <- paste0(name, "_bnds")
    axis$bounds <- CFBounds$new(nm, group = group, values = bounds)
    axis$set_attribute("bounds", "NC_CHAR", nm)
  }
  axis
}

#' Create a time axis
#'
#' With this method you can create a time axis to use with new [CFVariable]
#' instances.
#'
#' @param name Name of the axis.
#' @param values A `CFTime` or `CFClimatology` instance with time values and
#'   optionally bounds set.
#' @param attributes `data.frame` with the attributes of the axis to create.
#'   Attributes "standard_name", "units", "calendar", "actual_range" and "axis"
#'   will be set or updated.
#' @param group [CFGroup] instance where the axis will be located. If `NULL`
#'   (default), a private group will be created for the axis.
#' @return A [CFAxisTime] instance.
#' @export
makeTimeAxis <- function(name, values, attributes = data.frame(), group = NULL) {
  if (!.is_valid_name(name))
    stop("Name for axis is not valid", call. = FALSE)

  axis <- CFAxisTime$new(name, group = group, values = values, attributes = attributes)

  if (is.na(axis$attribute("standard_name")))
    axis$set_attribute("standard_name", "NC_CHAR", "time")
  axis$set_attribute("units", "NC_CHAR", values$cal$definition)
  axis$set_attribute("calendar", "NC_CHAR", values$cal$name)

  if (!is.null(values$bounds)) {
    nm <- paste0(name, "_bnds")
    axis$bounds <- CFBounds$new(nm, group = group, values = values$get_bounds())
    axis$set_attribute("bounds", "NC_CHAR", nm)
  }
  axis
}

#' Create a discrete axis
#'
#' With this method you can create a discrete axis to use with new [CFVariable]
#' instances.
#'
#' @param name Name of the axis.
#' @param length The length of the axis.
#' @param group [CFGroup] instance where the axis will be located. If `NULL`
#'   (default), a private group will be created for the axis.
#' @return A [CFAxisDiscrete] instance. The values will be a sequence of size
#'   `length`.
#' @export
makeDiscreteAxis <- function(name, length, group = NULL) {
  if (!.is_valid_name(name))
    stop("Name for axis is not valid", call. = FALSE)

  length <- as.integer(length)
  CFAxisDiscrete$new(name, group = group, start = 1L, count = length)
}

#' Create a character axis
#'
#' With this method you can create a character axis to use with new [CFVariable]
#' instances.
#'
#' @param name Name of the axis.
#' @param values The character coordinate values of the axis.
#' @param attributes `data.frame` with the attributes of the axis to create.
#' @param group [CFGroup] instance where the axis will be located. If `NULL`
#'   (default), a private group will be created for the axis.
#' @return A [CFAxisCharacter] instance.
#' @export
makeCharacterAxis <- function(name, values, attributes = data.frame(), group = NULL) {
  CFAxisCharacter$new(name, group = group, values = values, attributes = attributes)
}

#' Create a `CFDataset` or `CFVariable` instance from an R object
#'
#' With this function you can convert an R object into a [CFDataset] or
#' [CFVariable], depending on the characteristics of the argument `obj`. The
#' object to convert can be an array, matrix or vector of type `logical`,
#' `integer`, `numeric` or `character`, or a `terra::SpatRaster`.
#'
#' Dimnames on the R object will be converted to instances of a [CFAxis]
#' descendant class, depending on their values. If the dimnames along a
#' dimension of the R object can be converted to `numeric`, then it will be an
#' instance of [CFAxisNumeric]. If the dimnames are `character`, a first attempt
#' is made to create a [CFAxisTime] (i.e. the dimnames have to represent
#' timestamps), failing that a [CFAxisCharacter] will be created. If no dimnames
#' are set, an instance of [CFAxisDiscrete] is generated.
#'
#' The axes of the `CFVariable` instance(s) are oriented as in the object. Note
#' that this is different from standard practice in the netCDF community and the
#' portability of saved data sets is thus limited. You can improve this
#' situation by setting the orientation of the axes and by adding attributes.
#'
#' After creation of the `CFDataset` or `CFVariable`, it is recommended to set
#' other properties, such as attributes or a coordinate reference system.
#'
#' @param name The name of the `CFDataset` or `CFVariable` to create.
#' @param obj The object to convert. This can be an array, matrix or vector of
#'   type `logical`, `integer`, `numeric` or `character`, or a
#'   `terra::SpatRaster`.
#' @return An instance of class [CFDataset] or [CFVariable].
#' @export
as_CF <- function(name, obj) {
  UseMethod("as_CF", object = obj)
}

#' @rdname as_CF
#' @export
as_CF.default <- function(name, obj) {
  # Check the properties of "obj" and create axes from dimnames
  if (is.logical(obj)) obj <- as.integer(obj)
  dt <- typeof(obj)
  if (!(dt %in% c("integer", "double", "character")))
    stop("Argument 'obj' is of an unsupported type", call. = FALSE) # nocov

  grp <- CFGroup$new("", NULL)

  # Helper function - "nm" is name for the axis, "len" is length of the dimension, "vals" is coordinate values
  .makeArrayAxis <- function(nm, len, vals) {
    if (is.null(vals))              # values has no dimnames so make discrete axis
      return(CFAxisDiscrete$new(nm, group = grp, count = len))

    crds <- suppressWarnings(as.numeric(vals))
    if (any(is.na(crds))) {         # Not numeric so time or character
      t <- try(CFtime::CFTime$new("days since 1970-01-01T00:00:00", "standard", vals), silent = TRUE)
      if (inherits(t, "try-error")) # Not time
        CFAxisCharacter$new(nm, group = grp, values = vals)
      else CFAxisTime$new(nm, group = grp, values = t)
    } else {
      # Check if we have a potential longitude or latitude axis
      if (tolower(nm) %in% c("longitude", "lon") && .check_longitude_domain(crds))
        CFAxisLongitude$new(nm, group = grp, values = crds)
      else if (tolower(nm) %in% c("latitude", "lat") && .check_latitude_domain(vals))
        CFAxisLatitude$new(nm, group = grp, values = crds)
      else
        CFAxisNumeric$new(nm, group = grp, values = crds)
    }
  }

  dims <- dim(obj)
  if (is.null(dims))  # obj is a vector
    axes <- list(axis1 = .makeArrayAxis("axis_1", length(obj), names(obj)))
  else {
    dn <- dimnames(obj)
    names <- names(dn)
    if (is.null(names))
      names <- sapply(seq_along(dims), function(x) paste0("axis_", x))
    else
      names <- sapply(seq_along(dims), function(x) if (nzchar(names[x])) names[x] else paste0("axis_", x))

    axes <- lapply(seq_along(dims), function(x) .makeArrayAxis(names[x], dims[x], dn[[x]]))
  }
  axes <- axes[lengths(axes) > 0L]
  if (length(axes))
    names(axes) <- sapply(axes, function(ax) ax$name)

  CFVariable$new(name, group = grp, values = obj, axes = axes)
}

#' @rdname as_CF
#' @export
as_CF.SpatRaster <- function(name, obj) {
  vars <- terra::varnames(obj)
  num_vars <- length(vars)
  if (!num_vars)
    return(NULL)

  # Group for results
  grp <- CFGroup$new("", NULL)

  # Planar axes - the same for all vars
  cols <- terra::ncol(obj)
  rows <- terra::nrow(obj)
  ext <- terra::ext(obj)
  dx <- terra::xres(obj)
  dy <- terra::yres(obj)
  xcoords <- seq(from = ext[1] + dx * 0.5, by = dx, length.out = cols)
  ycoords <- seq(from = ext[4] - dy * 0.5, by = -dy, length.out = rows)
  crs <- terra::crs(obj)
  if (startsWith(crs, "GEOGCRS")) {
    axes <- list(longitude = makeLongitudeAxis("longitude", values = xcoords, group = grp),
                 latitude = makeLatitudeAxis("latitude", values = ycoords, group = grp))
  } else {
    axes <- list(x = makeAxis("x", values = xcoords, group = grp),
                 y = makeAxis("y", values = ycoords, group = grp))
    axes[["x"]]$set_attribute("standard_name", "NC_CHAR", "projected_x_coordinate")
    axes[["y"]]$set_attribute("standard_name", "NC_CHAR", "projected_y_coordinate")
  }

  out <- vector("list", num_vars)
  for (v in seq_along(vars)) {
    var <- vars[v]
    # FIXME: Test if name is valid. If not, make it so.
    rv <- obj[var]
    longname <- terra::longnames(rv)

    if (terra::has.time(rv)) {
      tcoords <- terra::time(rv)
      units <- terra::timeInfo(rv)$step
      ttime <- CFtime::CFTime$new(definition = paste(units, "since 1970-01-01"), calendar = "proleptic_gregorian", offsets = as.character(tcoords))
      rv_axes <- c(axes, setNames(list(makeTimeAxis("time", values = ttime, group = grp)), "time"))
    } else {
      rv_axes <- c(axes, setNames(list(makeCharacterAxis("layers", values = terra::names(rv), group = grp)), "layers"))
    }

    vals <- terra::values(rv)
    dim(vals) <- c(cols, rows, terra::nlyr(rv))

    out[[v]] <- CFVariable$new(var, group = grp, axes = rv_axes, values = vals)
    if (nzchar(longname))
      out[[v]]$set_attribute("long_name", "NC_CHAR", longname)
    if (nzchar(un <- terra::units(rv)[1L]))
      out[[v]]$set_attribute("units", "NC_CHAR", un)
    out[[v]]$set_attribute("comment", "NC_CHAR", "Imported from package terra.")
  }

  if (num_vars == 1L) {
    out[[1L]]$name <- name
    out[[1L]]
  } else {
    ds <- create_ncdf()
    ds$name <- name
    lapply(out, function(dv) ds$add_variable(dv))
    ds
  }
}
