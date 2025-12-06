#' Open a netCDF resource
#'
#' This function will read the metadata of a netCDF resource and interpret the
#' netCDF dimensions, variables and attributes to generate the corresponding CF
#' objects. The data for the CF variables is not read, please see [CFVariable]
#' for methods to read the variable data.
#'
#' @param resource The name of the netCDF resource to open, either a local file
#'   name or a remote URI.
#' @param write `TRUE` if the file is to be opened for writing, `FALSE`
#'   (default) for read-only access. Ignored for online resources, which are
#'   always opened for read-only access.
#' @return An `CFDataset` instance, or an error if the resource was not found or
#'   errored upon reading.
#' @export
#' @importFrom stats setNames
#' @examples
#' fn <- system.file("extdata",
#'   "pr_day_EC-Earth3-CC_ssp245_r1i1p1f1_gr_20230101-20231231_vncdfCF.nc",
#'   package = "ncdfCF")
#' (ds <- open_ncdf(fn))
open_ncdf <- function(resource, write = FALSE) {
  # Parameter check
  if (length(resource) != 1L && !is.character(resource))
    stop("Argument `resource` must be a single character string pointing to a netCDF resource.", call. = FALSE) # nocov
  if (length(write) != 1L && !is.logical(write))
    stop("Argument `write` must be a single logical value.", call. = FALSE) #nocov
  write <- write && file.exists(resource)

  res <- CFResource$new(resource, write)
  h <- res$handle
  g <- RNetCDF::file.inq.nc(h)

  ds <- CFDataset$new(resource = res, format = g$format)

  # Read all netCDF groups recursively
  root <- .readGroup(ds, h, vector("integer"))

  # Identify axes: NUG coordinate variables
  axes <- .buildAxes(root)

  # Find the id's of any "bounds" variables
  bnds <- sapply(root$NC$NCvars, function(v) { # FIXME: what about dims in subgroups?
    nm <- v$attribute("bounds")
    if (is.na(nm))
      nm <- v$attribute("climatology_bounds")
    if (!is.na(nm)) {
      obj <- v$group$find_by_name(nm)
      if (is.null(obj)) {
        warning("Unmatched `bounds` value '", nm, "' found in variable '", v$name, "'.", call. = FALSE)
        -1L
      } else obj$dimids[1L]    # By definition, bounds dimid comes first
    } else -1L # Flag no bounds
  })
  if (length(bnds))
    bnds <- unique(bnds[which(bnds > -1L)])

  # Mop up any non-CV dimensions except bounds - additional to CF Conventions
  all_axis_dims <- sapply(axes, function(x) x$dimid)
  all_axis_dims <- all_axis_dims[!is.na(all_axis_dims)]
  allNCvars <- .listNCvars(root$NC)
  all_var_dims <- unique(unlist(sapply(allNCvars, function(v) v$dimids)))
  all_var_dims <- all_var_dims[!is.na(all_var_dims)]
  add_dims <- all_var_dims[!(all_var_dims %in% c(all_axis_dims, bnds))]
  if (length(add_dims)) {
    axes <- append(axes, .addBareDimensions(root, add_dims))
    axes <- axes[lengths(axes) > 0L]
  }

  # Auxiliary CVs and scalar CVs
  .makeCoordinates(root)

  # Ancillary variables
  .makeAncillary(root)

  # Cell measure variables
  .makeCellMeasures(root, axes)

  # Coordinate reference systems
  .makeCRS(root)

  if (is.null(l3bgrp <- root$subgroups[["level-3_binned_data"]])) {
    # Try to identify the type of the file
    ds$file_type <- if (!is.na(ft <- root$attribute("featureType")) &&
        ft %in% c("point", "timeSeries", "trajectory", "profile", "timeSeriesProfile", "trajectoryProfile"))
      "discrete sampling geometry"
    else if (!is.na(mip_era <- root$attribute("mip_era")))
      mip_era
    else if (!is.na(crd <- root$attribute("project_id")) && crd == "CORDEX")
      "CORDEX"
    else "Generic netCDF data"

    # Configure any parametric vertical axes
    .configureParametricTerms(axes)

    vars <- .buildVariables(root, axes)
  } else {
    # L3b
    units <- root$attribute("units")
    if (!is.na(units)) {
      units <- strsplit(units, ":")[[1L]]
      nm <- names(l3bgrp$NC$NCvars)
      if (all(c("BinList", "BinIndex", units[1L]) %in% nm)) {
        ds$file_type <- "NASA level-3 binned data"
        l3bgrp$add_CF_object(CFVariableL3b$new(l3bgrp, units))
      }
    }
  }

  ds$root <- root
  ds
}

#' Examine a netCDF resource
#'
#' This function will read a netCDF resource and return a list of identifying
#' information, including data variables, axes and global attributes. Upon
#' returning the netCDF resource is closed.
#'
#' If you find that you need other information to be included in the result,
#' [open an issue](https://github.com/R-CF/ncdfCF/issues).
#'
#' @param resource The name of the netCDF resource to open, either a local file
#'   name or a remote URI.
#'
#' @return A list with elements "variables", "axes" and global "attributes",
#' each a `data.frame`.
#' @export
#' @examples
#' fn <- system.file("extdata",
#'   "pr_day_EC-Earth3-CC_ssp245_r1i1p1f1_gr_20230101-20231231_vncdfCF.nc",
#'   package = "ncdfCF")
#' peek_ncdf(fn)
peek_ncdf <- function(resource) {
  ds <- open_ncdf(resource)
  grps <- ds$has_subgroups()
  if (inherits(ds, "CFDataset")) {
    list(uri = ds$uri,
         type = ds$file_type,
         variables  = do.call(rbind, lapply(ds$variables(), function(v) v$peek())),
         axes       = do.call(rbind, lapply(ds$axes(), function(a) a$peek())),
         attributes = ds$attributes())
  } else list() # nocov
}

#' Read a group from a netCDF dataset
#'
#' Variable, dimension, UDT and attribute information are read for the group, as
#' well as all subgroups.
#'
#' @param parent The parent `CFGroup` of this group. `CFDataset` for the root
#'   group.
#' @param h The handle to the corresponding group in the netCDF resource.
#' @param parent_dims The dimids that have been seen at higher levels.
#'
#' @return Either the `CFGroup` instance invisibly or a try-error instance.
#' @noRd
.readGroup <- function(parent, h, parent_dims) {
  g <- RNetCDF::grp.inq.nc(h)

  # Global attributes
  atts <- if (g$ngatts) .readAttributes(h, "NC_GLOBAL", g$ngatts)
          else data.frame()

  if (inherits(parent, "CFDataset")) {
    par <- parent
    res <- parent$resource
  } else {
    par <- parent$NC
    res <- parent$NC$resource
  }
  ncgrp <- NCGroup$new(id = as.integer(g$self), name = g$name, attributes = atts,
                       parent = par, resource = res)
  grp <- CFGroup$new(ncgrp, parent = parent)
  ncgrp$CF <- grp

  # Read all the raw NC variables in the group
  if (length(g$varids))
    lapply(g$varids, function (v) .readNCVariable(ncgrp, g$self, v))

  # Dimensions by dimid
  if (length(g$dimids) && length(new_dims <- g$dimids[!(g$dimids %in% parent_dims)]))
    dims <- lapply(new_dims, function (d) {
      dmeta <- RNetCDF::dim.inq.nc(h, d)
      NCDimension$new(dmeta$id, dmeta$name, dmeta$length, dmeta$unlim, ncgrp)
    })

  # UDTs
  if (length(g$typeids))
    ncgrp$NCudts <- lapply(g$typeids, function(t) RNetCDF::type.inq.nc(h, t, fields = TRUE))

  # Subgroups
  if (length(g$grps)) {
    sub <- lapply(g$grps, function(z) .readGroup(grp, z, g$dimids))
    names(sub) <- sapply(sub, function(z) z$name)
    grp$add_subgroups(sub)
    ncgrp$subgroups <- lapply(sub, function(s) s$NC)
  }

  grp
}

#' Read a raw NC variable from a group, everything except its data
#' @param grp The NC group to read from
#' @param h The NC group handle
#' @param vid The id of the variable to read.
#' @return A new NCVariable instance
#' @noRd
.readNCVariable <- function(grp, h, vid) {
  vmeta <- RNetCDF::var.inq.nc(h, vid)
  NCVariable$new(id = as.integer(vmeta$id), name = vmeta$name, group = grp,
                 vtype = vmeta$type, dimids = vmeta$dimids,
                 attributes = .readAttributes(h, vmeta$name, vmeta$natts),
                 netcdf4 = vmeta[-(1L:6L)])
}

# List all the NCvars in the NC group and any sub-groups
.listNCvars <- function(g) {
  vars <- g$NCvars
  if (length(g$subgroups)) {
    v <- lapply(g$subgroups, .listNCvars)
    vars <- append(vars, unlist(v))
  }
  vars
}

#' Build the axes defined in this group. The axes are added to the CF group.
#' @param grp The CF group to build axes for
#' @return A list with axes built, including from subgroups
#' @noRd
.buildAxes <- function(grp) {
  nc <- grp$NC
  if (length(nc$NCvars) > 0L) {
    # Create axis for local variables with name equal to visible dimensions
    dim_names <- sapply(nc$dimensions("all"), function(d) d$name)
    if (length(dim_names)) {
      local_vars <- nc$NCvars[dim_names]
      local_CVs <- local_vars[lengths(local_vars) > 0L]
      axes <- lapply(local_CVs, function(v) .makeAxis(nc, v))
      grp$add_CF_object(axes)
    } else axes <- list()
  } else axes <- list()

  # Descend into subgroups
  if (grp$has_subgroups) {
    ax <- lapply(grp$subgroups, .buildAxes)
    axes <- append(axes, unlist(ax))
  }

  axes
}

# Create an `CFAxis` from an NC variable and dimension
#
# This method creates the various kinds of axes.
#
# @param grp NC group in which the NC variable is defined.
# @param var `NCVariable` instance to create the axis from.
#
# @return An instance of `CFAxis`.
# @noRd
.makeAxis <- function(grp, var) {
  # Dimension values - can't handle integer64
  vals <- try(if (var$vtype %in% c("NC_INT64", "NCUINT64"))
    RNetCDF::var.get.nc(var$group$handle, var$name, collapse = FALSE, unpack = TRUE, fitnum = FALSE)
  else
    RNetCDF::var.get.nc(var$group$handle, var$name, collapse = FALSE, unpack = TRUE, fitnum = TRUE), silent = TRUE)
  if (inherits(vals, "try-error")) {
    # No dimension values so it's an identity axis
    d <- grp$find_dim_by_id(var$dimids[[1L]])
    return(CFAxisDiscrete$new(var, count = d$length))
  }

  # Does `var` have attributes?
  if (!nrow(var$attributes)) {
    # No attributes so nothing left to do
    if (var$vtype %in% c("NC_CHAR", "NC_STRING"))
      return(CFAxisCharacter$new(var, values = vals))
    else return(CFAxisNumeric$new(var, values = vals))
  }

  # Read some useful attributes
  orient   <- var$attribute("axis")
  standard <- var$attribute("standard_name")
  units    <- var$attribute("units")

  # Read the boundary values
  bnds <- .readBounds(grp, var$attribute("bounds"))

  # See if we can make time
  t <- .makeTimeObject(var, units, vals, bnds)
  if (!is.null(t)) {
    tax <- CFAxisTime$new(var, t)
    tax$bounds <- bnds
    return(tax)
  }

  # Create the axis based on units
  axis <- if (!is.na(units)) {
    if (grepl("^degree(s?)(_?)(east|E)$", units))
      CFAxisLongitude$new(var, values = vals)
    else if (grepl("^degree(s?)(_?)(north|N)$", units))
      CFAxisLatitude$new(var, values = vals)
    else if (requireNamespace("units", quietly = TRUE) && units::ud_are_convertible(units, "bar"))
      CFAxisVertical$new(var, values = vals)
    else
      NULL
  }
  if (is.null(axis) && !is.na(standard)) {
    axis <- switch(standard,
                   "longitude" = CFAxisLongitude$new(var, values = vals),
                   "latitude"  = CFAxisLatitude$new(var, values = vals),
                   "projection_x_coordinate" = CFAxisNumeric$new(var, values = vals, orientation = "X"),
                   "projection_y_coordinate" = CFAxisNumeric$new(var, values = vals, orientation = "Y"),
                   NULL)
    if (is.null(axis) && (standard %in% Z_parametric_standard_names || !is.na(var$attribute("positive"))))
      axis <- CFAxisVertical$new(var, values = vals)
  }
  if (is.na(orient)) orient <- ""
  if (is.null(axis)) axis <- CFAxisNumeric$new(var, values = vals, orientation = orient)

  axis$bounds <- bnds
  axis
}

# Add bare dimensions to the list of axes
#
# There are data sets that do not include a CV for identity dimensions, where
# ancillary CVs define the contents of the axis. This function creates a dummy
# NCVariable and then builds a bare-bones discrete axis, in the group where the
# dimension is defined.
#
# Argument `grp` is the current CF group to scan, `add_dims` is a vector of
# dimension ids for which a discrete axis must be created because NC variables
# refer to the dimension.
.addBareDimensions <- function(grp, add_dims) {
  if (length(grp$NC$NCdims) > 0L) {
    axes <- lapply(grp$NC$NCdims, function(d) {
      if (d$id %in% add_dims) {
        nm <- d$name
        axis <- CFAxisDiscrete$new(nm, count = d$length)
        axis$dimid <- d$id
        lx <- list(axis); names(lx) <- nm
        grp$add_CF_object(lx)
        add_dims <- add_dims[-which(add_dims == d$id)]
        axis
      }
    })
  } else axes <- list()

  # Descend into subgroups
  if (length(grp$subgroups) && length(add_dims)) {
    ax <- lapply(grp$subgroups, function(g) .addBareDimensions(g, add_dims))
    axes <- append(axes, unlist(ax))
  }

  axes
}

#' Make a CFTime object. This will try to create a CFTime or CFClimatology object,
#' including its bounds if set. If it fails it will return NULL, otherwise the
#' object.
#' @noRd
.makeTimeObject <- function(var, units, vals, bnds) {
  if (is.na(units)) return(NULL)
  cal <- if (is.na(cal <- var$attribute("calendar"))) "standard" else cal
  if (!inherits(bnds, "CFBounds"))
    bnds <- .readBounds(var$group, var$attribute("bounds"))
  clim <- if (is.null(bnds))
            .readBounds(var$group, var$attribute("climatology")) # Climatology must have bounds
          else NULL
  t <- if (is.null(clim)) try(CFtime::CFTime$new(units, cal, vals), silent = TRUE)
  else try(CFtime::CFClimatology$new(units, cal, vals, clim$values), silent = TRUE)
  if (inherits(t, "try-error")) return(NULL)
  if (is.null(clim) && inherits(bnds, "CFBounds"))
    t$bounds <- bnds$values
  t
}

#' Make CF constructs for "coordinates" references
#'
#' NC variables are scanned for a "coordinates" attribute (which must reference
#' a data variable, domain variable or geometry container variable). If not
#' already present, the NC variable referenced is converted into one of 3
#' objects, depending on context: 1. A scalar coordinate variable in the group
#' where its NC variable is located; 2. A label variable in the group where its
#' NC variable is located; multiple label coordinates (such as in the case of
#' taxon name and identifier) are stored in a single label variable; 3. A
#' long-lat auxiliary coordinate variable when both a longitude and latitude NC
#' variable are found, in the group of the longitude NC variable.
#' @param grp The CF group to scan.
#' @return Nothing. `CFAxis`, `CFLabel` and `CFAuxiliaryLongLat` instances are
#'   created in the CF groups corresponding to where the NC variables are found.
#'   These will later be picked up when `CFVariable` instances are created.
#' @noRd
.makeCoordinates <- function(grp) {
  vars <- grp$NC$NCvars
  if (length(vars) > 0L) {
    # Scan each unused NCVariable for the "coordinates" attribute and process.
    # The NCVariable must have dimensional axes.
    for (refid in seq_along(vars)) {
      v <- vars[[refid]]
      if (length(vdimids <- v$dimids) &&
          !is.na(coords <- v$attribute("coordinates"))) {
        coords <- strsplit(coords, " ", fixed = TRUE)[[1L]]
        varLon <- varLat <- bndsLon <- bndsLat <- NA
        for (cid in seq_along(coords)) {
          found_one <- FALSE
          aux <- grp$NC$find_by_name(coords[cid])
          if (!is.null(aux)) {
            nd <- aux$ndims
            bounds <- aux$attribute("bounds")

            # If aux is a 2D NCVariable having an attribute "units" with value
            # "degrees_east" or "degrees_north" it is a longitude or latitude,
            # respectively. Record the fact and move on.
            # This also allows for higher-dimensional NCVariables but those
            # additional dimensions will get dropped.
            if (nd >= 2L && !is.na(units <- aux$attribute("units"))) {
              if (grepl("^degree(s?)(_?)(east|E)$", units)) {
                varLon <- aux
                bndsLon <- .readBounds(aux$group, bounds, 2L)
                found_one <- TRUE
              } else if (grepl("^degree(s?)(_?)(north|N)$", units)) {
                varLat <- aux
                bndsLat <- .readBounds(aux$group, bounds, 2L)
                found_one <- TRUE
              }
            }

            if (!found_one) {
              if (nd > 0L && aux$vtype %in% c("NC_CHAR", "NC_STRING")) {
                # Label
                aux$group$CF$add_CF_object(CFLabel$new(aux))
                found_one <- TRUE
              } else if (nd < 2L) {
                # Scalar or auxiliary coordinate with a single dimension: make an axis out of it if it doesn't already exist.
                if (is.null(grp$find_by_name(aux$name))) {
                  aux$group$CF$add_CF_object(.makeAxis(grp$NC, aux))
                  found_one <- TRUE
                }
              }
            }
          }

          if (!found_one && !length(aux$CF))
            warning("Unmatched `coordinates` value '", coords[cid], "' found in variable '", v$name, "'.", call. = FALSE)
        }

        # Make a CFAuxiliaryLongLat if we have found a varLon and a varLat and
        # they have identical dimensions, in the varLon CF group.
        if ((inherits(varLon, "NCVariable") && inherits(varLat, "NCVariable")) &&
            identical(varLon$dimids, varLat$dimids)) {
          ax <- lapply(varLon$dimids, function(did) {
            dname <- varLon$group$find_dim_by_id(did)$name
            varLon$group$find_by_name(dname)$CF[[1L]]
          })
          lon <- CFVariable$new(varLon, ax)
          lat <- CFVariable$new(varLat, ax)
          ll <- CFAuxiliaryLongLat$new(lon, lat, bndsLon, bndsLat)
          varLon$group$CF$add_CF_object(ll)
        }
      }
    }
  }

  # Descend into subgroups
  if (length(grp$subgroups))
    lapply(grp$subgroups, function(g) .makeCoordinates(g))
}

#' Make CF constructs for "ancillary_variables" references
#'
#' NC variables are scanned for an "ancillary_variables" attribute (which must
#' reference data variables). If not already present, the NC variable referenced
#' is converted into a data variable and associated with the referring data
#' variable (and not separately).
#' @param grp The group to scan.
#' @return Nothing. `CFVariable` instances are created in the groups where the
#'   NC variables are found. These will later be picked up when referring
#'   `CFVariable` instances are created.
#' @noRd
.makeAncillary <- function(grp) {
  vars <- grp$NC$NCvars
  if (length(vars) > 0L)
    for (refid in seq_along(vars)) {
      v <- vars[[refid]]
      if (!is.na(ancillary <- v$attribute("ancillary_variables"))) {
        ancillary <- strsplit(ancillary, " ", fixed = TRUE)[[1L]]
        for (aid in seq_along(ancillary)) {
          anc <- grp$NC$find_by_name(ancillary[aid])
          if (is.null(anc))
            warning("Unmatched `ancillary_variables` value '", ancillary[aid], "' found in variable '", v$name, "'.", call. = FALSE)
          else {
            ax <- lapply(anc$dimids, function(did) {
              dname <- anc$group$find_dim_by_id(did)$name
              anc$group$find_by_name(dname)$CF[[1L]]
            })
            anc$group$CF$add_CF_object(CFVariable$new(anc, ax))
          }
        }
      }
    }

  # Descend into subgroups
  if (length(grp$subgroups))
    lapply(grp$subgroups, function(g) .makeAncillary(g))
}

#' @description Configure the formula terms of a parametric vertical axis. If
#'   the vertical axis has a `formula_terms` attribute it has a parametric
#'   coordinate space that is calculated from the formula terms. This method
#'   sets up the axis instance to calculate the dimensional coordinate space
#'   (but it does not do the actual calculation; access the
#'   `parametric_coordinates` field of the vertical axis to get the dimensional
#'   coordinates).
#' @param axes List of `CFAxis` instances to use with the formula term objects.
#' @return Nothing. Any parametric vertical axes in the argument `axes` will be
#'   modified in place.
#' @noRd
.configureParametricTerms <- function(axes) {
  lapply(axes, function(ax) {
    if (!is.na(ft <- ax$attribute("formula_terms"))) {
      ft <- trimws(strsplit(ft, " ")[[1L]], whitespace = ":")
      dim(ft) <- c(2, length(ft) * 0.5)
      rownames(ft) <- c("term", "variable")
      ft <- as.data.frame(t(ft))
      ft$param <- lapply(ft$variable, function(v) {
        if (v == ax$name) NULL
        else {
          ncvar <- ax$NC$group$find_by_name(v)
          if (is.null(ncvar)) {
            CFVerticalParametricTerm$new(0, NULL)
          } else {
            local_axes <- .buildVariableAxisList(ncvar, axes)
            CFVerticalParametricTerm$new(ncvar, local_axes)
          }
        }
      })
      ax$set_parametric_terms(ax$attribute("standard_name"), ft)
    }
  })
}

#' Make CF constructs for "cell_measures" references
#'
#' NC variables are scanned for a "cell_measures" attribute (which must be a
#' data variable or domain variable). The NC variable referenced is converted
#' into a `CFCellMeasure` instance, in the CF group associated with that NC
#' variable.
#'
#' The "cell_measures" may also be located in an external file. It is up to the
#' caller to link to any such external file.
#'
#' @param grp The CF group to scan.
#' @param axes List of available CF axes to use with the cell measure variables.
#'
#' @return Nothing. `CFCellMeasure` instances are created in the group where the
#'   referenced NC variable is found. These will later be picked up when
#'   CFvariables are created.
#' @noRd
.makeCellMeasures <- function(grp, axes) {
  vars <- grp$NC$NCvars
  if (length(vars) > 0L) {
    # Scan each unused NCVariable for the "cell_measures" attribute and process.
    for (refid in seq_along(vars)) {
      v <- vars[[refid]]
      if (!length(v$CF) && !is.na(meas <- v$attribute("cell_measures"))) {
        meas <- trimws(strsplit(meas, " ", fixed = TRUE)[[1L]], whitespace = "[ \t\r\n\\:]")
        meas <- meas[which(nzchar(meas))]
        for (m in 1:(length(meas) * 0.5)) {
          nm <- grp$NC$find_by_name(meas[m * 2L])
          if (is.null(nm)) {
            # External variable
            root <- grp$root
            ev <- root$attribute("external_variables")
            if (is.na(ev) || !(meas[m * 2L] %in% trimws(strsplit(ev, " ", fixed = TRUE)[[1L]]))) {
              # FIXME: warning
              warning("Unmatched `cell_measures` value '", meas[m * 2L], "' found in variable '", v$name, "'", call. = FALSE)
            } else
              root$add_CF_object(CFCellMeasure$new(meas[m * 2L - 1L], meas[m * 2L]), silent = TRUE)
          } else if (!length(nm$CF)) {
            # Cell measures variable is internal and not yet created
            ax <- .buildVariableAxisList(nm, axes)
            cm <- CFCellMeasure$new(meas[m * 2L - 1L], meas[m * 2L], nm, ax)
            nm$group$CF$add_CF_object(cm)
          }
        }
      }
    }
  }

  # Descend into subgroups
  if (grp$has_subgroups)
    lapply(grp$subgroups, function(g) .makeCellMeasures(g, axes))
}

#' Make CRS instances for "grid_mapping" references
#'
#' NC variables are scanned for a "grid_mapping_name" attribute. The NC variable
#' referenced is converted into a CFGridMapping instance in the group where its
#' NC variable is located.
#'
#' @param grp The CF group to scan.
#'
#' @return Nothing. CFGridMapping instances are created in the groups where the
#'   NC variables are found. These will later be picked up when CFvariables are
#'   created.
#' @noRd
.makeCRS <- function(grp) {
  vars <- grp$NC$NCvars
  if (length(vars) > 0L) {
    # Scan each unused NCVariable for the "grid_mapping_name" property and process.
    for (refid in seq_along(vars)) {
      v <- vars[[refid]]
      if (!length(v$CF) && !is.na(gm <- v$attribute("grid_mapping_name")))
        grp$add_CF_object(CFGridMapping$new(v))
    }
  }

  # Descend into subgroups
  if (grp$has_subgroups)
    lapply(grp$subgroups, function(g) .makeCRS(g))
}

# Utility function to read bounds values
# grp - the current group being processed
# bounds - the name of the boundary variable, or NA if no bounds attribute present
# owner_dims - the number of dimensions of the owning object: 1 for an axis, 2 for an aux CV grid
.readBounds <- function(grp, bounds, owner_dims = 1L) {
  if (is.na(bounds)) NULL
  else {
    NCbounds <- grp$find_by_name(bounds)
    if (is.null(NCbounds)) NULL
    else CFBounds$new(NCbounds, owner_dims = owner_dims)
  }
}

#' Build CF variables from unused dimensional NC variables
#'
#' NC variables with dimensions that do not have their `CF` property set will be
#' made into a `CFVariable`. This method is invoked recursively to travel through
#' all groups of the netCDF resource.
#'
#' @param grp The CF group to scan for unused NC variables.
#' @param axes List of available CF axes to use with the CF variables.
#' @return List of created CF variables.
#' @noRd
.buildVariables <- function(grp, axes) {
  ncvars <- grp$NC$NCvars
  if (length(ncvars) > 0L) {
    # Create variable for each unused NCVariable with dimensions
    vars <- lapply(ncvars, function(v) {
      varLon <- varLat <- ll <- NULL
      if (!length(v$CF) && v$ndims > 0L) {
        all_ax <- dim_ax <- .buildVariableAxisList(v, axes)
        ax_names <- names(dim_ax)

        # Add references to any "coordinates" of the variable
        if (!is.na(coords <- v$attribute("coordinates"))) {
          coords <- strsplit(coords, " ", fixed = TRUE)[[1L]]
          for (cid in seq_along(coords)) {
            if (coords[cid] %in% ax_names) next

            aux <- grp$find_by_name(coords[cid])
            if (!is.null(aux) && !inherits(aux, "CFVariable")) {
              clss <- class(aux)
              if (aux$length == 1L)
                all_ax[[aux$name]] <- aux
              # FIXME: Below two branches are identical
              else if (clss[1L] == "CFLabel") {
                ndx <- which(sapply(dim_ax, function(x) x$dimid == aux$dimid))
                if (length(ndx)) all_ax[[ndx]]$auxiliary <- aux
                else {  # FIXME: record warning
                }
              } else if ("CFAxis" %in% clss) {
                ndx <- which(sapply(dim_ax, function(x) x$dimid == aux$dimid))
                if (length(ndx)) all_ax[[ndx]]$auxiliary <- aux
                else {  # FIXME: record warning
                }
              } else {
                # FIXME: Record warning
              }
            } else {
              ll <- grp$NC$find_by_name(coords[cid])
              if (!is.null(ll)) {
                units <- ll$attribute("units")
                if (!is.na(units)) {
                  if (grepl("^degree(s?)(_?)(east|E)$", units)) varLon <- ll
                  else if (grepl("^degree(s?)(_?)(north|N)$", units)) varLat <- ll
                }
              }
            }
          } # coords

          if (inherits(varLon, "NCVariable") && inherits(varLat, "NCVariable"))
            ll <- varLon$group$CF$find_by_name(paste(varLon$name, varLat$name, sep = "_"))
          else ll <- NULL
        } # coordinates

        # Make the CFVariable
        var <- CFVariable$new(v, all_ax)
        if (!is.null(ll)) var$gridLongLat <- ll

        # Add references to any "ancillary_variables" of the variable
        if (!is.na(ancillary <- v$attribute("ancillary_variables"))) {
          ancillary <- strsplit(ancillary, " ", fixed = TRUE)[[1L]]
          for (aid in seq_along(ancillary)) {
            anc <- grp$find_by_name(ancillary[aid])
            if (inherits(anc, "CFVariable"))
              var$add_ancillary_variable(anc)
          }
        } # ancillary variables

        # Add cell_measures
        if (!is.na(cm <- v$attribute("cell_measures"))) {
          cms <- strsplit(cm, " ", fixed = TRUE)[[1L]]
          len <- as.integer(length(cms) * 0.5)
          for (i in 1L:len) {
            cmv <- grp$find_by_name(cms[i * 2L])
            if (inherits(cmv, "CFCellMeasure")) {
              var$add_cell_measure(cmv)
              cmv$register(var)
            }
          }
        }

        # Add grid mapping
        gm <- v$attribute("grid_mapping")
        if (!is.na(gm)) {
          gm <- grp$find_by_name(gm)
          if (inherits(gm, "CFGridMapping"))
            var$crs <- gm
        }

        var
      }
    })
    vars <- vars[lengths(vars) > 0L]
    if (length(vars))
      grp$add_CF_object(vars)
  } else vars <- list()

  # Descend into subgroups
  if (length(grp$subgroups))
    vars <- append(vars, unlist(lapply(grp$subgroups, function(g) .buildVariables(g, axes))))

  vars
}

#' Build a list of axes that a NC variable references. These are the dimensional
#' axes, being referenced by a dimid from the NC variable
#'
#' @param ncvar The NC variable to build the axis list for.
#' @param axes List of available CF axes to use with the CF variables.
#' @return List of axes for the NC variable.
#' @noRd
.buildVariableAxisList <- function(ncvar, axes) {
  xids <- lapply(axes, function(x) x$dimid)
  nd <- ncvar$ndims
  if (nd > 0L) {
    ax <- vector("list", nd)
    for (x in 1:nd) {
      ndx <- which(sapply(xids, function(e) ncvar$dimids[x] %in% e))
      if (!length(ndx)) {
        warning(paste0("Possible variable '", ncvar$name, "' cannot be constructed because of unknown axis identifier ", ncvar$dimids[x]))
        return(NULL)
      }
      ax[[x]] <- axes[[ndx]]
    }
    names(ax) <- sapply(ax, function(x) x$name)
    ax
  } else list()
}

# Read the attributes for a group or a variable
.readAttributes <- function(h, name, num) {
  if (num < 1L) return(data.frame())
  atts <- do.call(rbind, lapply(0L:(num - 1L), function (a) as.data.frame(RNetCDF::att.inq.nc(h, name, a))))
  atts$value <- lapply(0L:(num - 1L), function (a) RNetCDF::att.get.nc(h, name, a, fitnum = TRUE))
  atts
}
