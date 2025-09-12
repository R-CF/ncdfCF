#' Open a netCDF resource
#'
#' This function will read the metadata of a netCDF resource and interpret the
#' netCDF dimensions, variables and attributes to generate the corresponding CF
#' objects. The data for the CF variables is not read, please see [CFVariable]
#' for methods to read the variable data.
#'
#' @param resource The name of the netCDF resource to open, either a local file
#'   name or a remote URI.
#' @return An `CFDataset` instance, or an error if the resource was not found
#'   or errored upon reading.
#' @export
#' @importFrom stats setNames
#' @examples
#' fn <- system.file("extdata",
#'   "pr_day_EC-Earth3-CC_ssp245_r1i1p1f1_gr_20230101-20231231_vncdfCF.nc",
#'   package = "ncdfCF")
#' (ds <- open_ncdf(fn))
open_ncdf <- function(resource) {
  # Parameter check
  if (length(resource) != 1L && !is.character(resource))
    stop("Argument `resource` must be a single character string pointing to a netCDF resource.", call. = FALSE) # nocov

  res <- CFResource$new(resource)
  h <- res$handle
  g <- RNetCDF::file.inq.nc(h)

  ds <- CFDataset$new(resource = res, format = g$format)

  # Read all netCDF groups recursively
  root <- .readGroup(ds, h, vector("integer"))

  # Identify axes: NUG coordinate variables
  axes <- .buildAxes(root)

  # Find the id's of any "bounds" variables
  bnds <- sapply(root$NCvars, function(v) { # FIXME: what about dims in subgroups?
    nm <- v$attribute("bounds")
    # FIXME: climatology
    if (!is.na(nm)) {
      obj <- v$group$find_by_name(nm, "NC")
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
  all_var_dims <- unique(unlist(sapply(root$NCvars, function(v) v$dimids)))
  all_var_dims <- all_var_dims[!is.na(all_var_dims)]
  add_dims <- all_var_dims[!(all_var_dims %in% c(all_axis_dims, bnds))]
  if (length(add_dims)) {
    axes <- append(axes, .addBareDimensions(root, add_dims))
    axes <- axes[lengths(axes) > 0L]
    #names(root$CFaxes) <- sapply(root$CFaxes, function(x) x$name)
    #names(root$NCvars) <- sapply(root$NCvars, function(v) v$name)
  }

  # Auxiliary CVs and scalar CVs
  .makeCoordinates(root)

  # Cell measure variables
  .makeCellMeasures(root, axes)

  # Coordinate reference systems
  .makeCRS(root)

  if (length(axes)) {
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
    .configureParametricTerms(root$CFaxes)

    vars <- .buildVariables(root, root$CFaxes)
  } else {
    # Try L3b
    units <- root$attribute("units")
    if (!is.na(units)) {
      units <- strsplit(units, ":")[[1L]]

      l3bgrp <- root$subgroups[["level-3_binned_data"]]
      if (!is.null(l3bgrp)) {
        nm <- names(l3bgrp$NCvars)
        if (all(c("BinList", "BinIndex", units[1L]) %in% nm)) {
          ds$file_type <- "NASA level-3 binned data"
          l3bgrp$CFvars <- setNames(list(CFVariableL3b$new(l3bgrp, units)), units[1L])
        }
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
#' @param parent The parent `NCGroup` of this group. `NULL` for the root group.
#' @param h The handle to the group in the netCDF resource.
#' @param parent_dims The dimids that have been seen at higher levels.
#'
#' @return Either the `NCGroup` instance invisibly or a try-error instance.
#' @noRd
.readGroup <- function(parent, h, parent_dims) {
  g <- RNetCDF::grp.inq.nc(h)

  # Global attributes
  atts <- if (g$ngatts) .readAttributes(h, "NC_GLOBAL", g$ngatts)
          else data.frame()

  grp <- NCGroup$new(id = as.integer(g$self), name = g$name, atts,
                     parent = parent, resource = parent$resource)

  # Read all the raw NC variables in the group
  if (length(g$varids))
    lapply(g$varids, function (v) .readNCVariable(grp, g$self, v))

  # Dimensions by dimid
  if (length(g$dimids) && length(new_dims <- g$dimids[!(g$dimids %in% parent_dims)]))
    dims <- lapply(new_dims, function (d) {
      dmeta <- RNetCDF::dim.inq.nc(h, d)
      NCDimension$new(dmeta$id, dmeta$name, dmeta$length, dmeta$unlim, grp)
    })

  # UDTs
  if (length(g$typeids))
    grp$NCudts <- lapply(g$typeids, function(t) RNetCDF::type.inq.nc(h, t, fields = TRUE))

  # Subgroups
  if (length(g$grps)) {
    grp$subgroups <- lapply(g$grps, function(z) .readGroup(grp, z, g$dimids))
    names(grp$subgroups) <- sapply(grp$subgroups, function(z) z$name)
  }

  grp
}

#' Read a raw NC variable from a group, everything except its data
#' @noRd
.readNCVariable <- function(grp, h, vid) {
  vmeta <- RNetCDF::var.inq.nc(h, vid)
  NCVariable$new(id = as.integer(vmeta$id), name = vmeta$name, group = grp,
                 vtype = vmeta$type, dimids = vmeta$dimids,
                 attributes = .readAttributes(h, vmeta$name, vmeta$natts),
                 netcdf4 = vmeta[-(1L:6L)])
}

.buildAxes <- function(grp) {
  if (length(grp$NCvars) > 0L) {
    # Create axis for local variables with name equal to visible dimensions
    dim_names <- sapply(grp$dimensions("all"), function(d) d$name)
    if (length(dim_names)) {
      local_vars <- grp$NCvars[dim_names]
      local_CVs <- local_vars[lengths(local_vars) > 0L]
      axes <- lapply(local_CVs, function(v) .makeAxis(grp, v))
      grp$CFaxes <- append(grp$CFaxes, unlist(axes))
    } else axes <- list()
  } else axes <- list()

  # Descend into subgroups
  if (length(grp$subgroups)) {
    ax <- lapply(grp$subgroups, function(g) .buildAxes(g))
    axes <- append(axes, unlist(ax))
  }

  axes
}

# Create an `CFAxis` from an NC variable and dimension
#
# This method creates the various kinds of axes.
#
# @param grp Group in which the NC variable is defined.
# @param var `NCVariable` instance to create the axis from.
#
# @return An instance of `CFAxis`.
# @noRd
.makeAxis <- function(grp, var) {
  h <- grp$handle

  # Dimension values
  vals <- try(as.vector(RNetCDF::var.get.nc(h, var$name)), silent = TRUE)
  if (inherits(vals, "try-error"))
    # No dimension values so it's an identity axis
    return(CFAxisDiscrete$new(var))

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
# Argument `grp` is the current group to scan, `add_dims` is a vector of
# dimension ids for which a discrete axis must be created because NC variables
# refer to the dimension.
.addBareDimensions <- function(grp, add_dims) {
  if (length(grp$NCdims) > 0L) {
    axes <- lapply(grp$NCdims, function(d) {
      if (d$id %in% add_dims) {
        nm <- d$name
        axis <- CFAxisDiscrete$new(nm, count = d$length)
        axis$dimid <- d$id
        lx <- list(axis); names(lx) <- nm
        grp$CFaxes <- append(grp$CFaxes, lx)
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

#' Make a CFtime object. This will try to create a CFTime or CFClimatology object,
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
#' NC variables are scanned for a "coordinates" attribute (which must be a data
#' variable, domain variable or geometry container variable). If not already
#' present, the NC variable referenced is converted into one of 3 objects,
#' depending on context: 1. A scalar coordinate variable in the group where its
#' NC variable is located; 2. A label variable in the group where its NC
#' variable is located; multiple label coordinates (such as in the case of taxon
#' name and identifier) are stored in a single label variable; 3. A long-lat
#' auxiliary coordinate variable when both a longitude and latitude NC variable
#' are found, in the group of the longitude NC variable.
#' @param grp The group to scan.
#' @return Nothing. CFAxis, CFLabel and CFAuxiliaryLongLat instances are created
#'   in the groups where the NC variables are found. These will later be picked
#'   up when CFVariables are created.
#' @noRd
.makeCoordinates <- function(grp) {
  if (length(grp$NCvars) > 0L) {
    # Scan each unused NCVariable for the "coordinates" attribute and process.
    # The NCVariable must have dimensional axes.
    for (refid in seq_along(grp$NCvars)) {
      v <- grp$NCvars[[refid]]
      if (length(vdimids <- v$dimids) &&
          !is.na(coords <- v$attribute("coordinates"))) {
        coords <- strsplit(coords, " ", fixed = TRUE)[[1L]]
        varLon <- varLat <- bndsLon <- bndsLat <- NA
        for (cid in seq_along(coords)) {
          found_one <- FALSE
          aux <- grp$find_by_name(coords[cid], "NC")
          if (!is.null(aux)) {
            nd <- aux$ndims
            bounds <- aux$attribute("bounds")

            # If aux is a 2D NCVariable having an attribute "units" with value
            # "degrees_east" or "degrees_north" it is a longitude or latitude,
            # respectively. Record the fact and move on.
            if (nd == 2L && !is.na(units <- aux$attribute("units"))) {
              if (grepl("^degree(s?)(_?)(east|E)$", units)) {
                varLon <- aux
                bndsLon <- .readBounds(aux$group, bounds)
                found_one <- TRUE
              } else if (grepl("^degree(s?)(_?)(north|N)$", units)) {
                varLat <- aux
                bndsLat <- .readBounds(aux$group, bounds)
                found_one <- TRUE
              }
            }

            if (!found_one) {
              if (nd > 0L && aux$vtype %in% c("NC_CHAR", "NC_STRING")) {
                # Label
                aux$group$add_auxiliary_coordinate(CFLabel$new(aux))
                found_one <- TRUE
              } else if (nd < 2L) {
                # Scalar or auxiliary coordinate with a single dimension: make an axis out of it if it doesn't already exist.
                if (is.null(grp$find_by_name(aux$name))) {
                  aux$group$add_auxiliary_coordinate(.makeAxis(grp, aux))
                  found_one <- TRUE
                }
              }
            }
          }

          if (!found_one && !length(aux$CF))
            warning("Unmatched `coordinates` value '", coords[cid], "' found in variable '", v$name, "'.", call. = FALSE)
        }

        # Make a CFAuxiliaryLongLat if we have found a varLon and a varLat and
        # they have identical dimensions, in the varLon group.
        if ((inherits(varLon, "NCVariable") && inherits(varLat, "NCVariable")) &&
            identical(varLon$dimids, varLat$dimids)) {
          ax <- lapply(varLon$dimids, function(did) {
            dname <- varLon$group$find_dim_by_id(did)$name
            varLon$group$find_by_name(dname, "NC")$CF[[1L]]
          })
          lonCF <- CFVariable$new(varLon, ax)
          latCF <- CFVariable$new(varLat, ax)
          varLon$group$add_auxiliary_longlat(lonCF, latCF, bndsLon, bndsLat)
        }
      }
    }
  }

  # Descend into subgroups
  if (length(grp$subgroups))
    lapply(grp$subgroups, function(g) .makeCoordinates(g))
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
          ncvar <- ax$NCvar$group$find_by_name(v, "NC")
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
#' into a `CFCellMeasure` instance, in the group of that NC variable.
#'
#' The "cell_measures" may also be located in an external file. It is up to the
#' caller to link to any such external file.
#'
#' @param grp The group to scan.
#' @param axes List of available CF axes to use with the cell measure variables.
#'
#' @return Nothing. `CFCellMeasure` instances are created in the group where
#'   the referenced NC variable is found. These will later be picked up when
#'   CFvariables are created.
#' @noRd
.makeCellMeasures <- function(grp, axes) {
  if (length(grp$NCvars) > 0L) {
    # Scan each unused NCVariable for the "cell_measures" attribute and process.
    for (refid in seq_along(grp$NCvars)) {
      v <- grp$NCvars[[refid]]
      if (!length(v$CF) && !is.na(meas <- v$attribute("cell_measures"))) {
        meas <- trimws(strsplit(meas, " ", fixed = TRUE)[[1L]], whitespace = "[ \t\r\n\\:]")
        meas <- meas[which(nzchar(meas))]
        for (m in 1:(length(meas) * 0.5)) {
          nm <- grp$find_by_name(meas[m * 2L], "NC")
          if (is.null(nm)) {
            # External variable
            root <- grp$root
            ev <- root$attribute("external_variables")
            if (is.na(ev) || !(meas[m * 2L] %in% trimws(strsplit(ev, " ", fixed = TRUE)[[1L]]))) {
              # FIXME: warning
              warning("Unmatched `cell_measures` value '", meas[m * 2L], "' found in variable '", v$name, "'", call. = FALSE)
            } else {
              # If it exists, move on, else create a cell measure variable
              cmv <- grp$find_by_name(meas[m * 2L])
              if (!inherits(cmv, "CFCellMeasure")) {
                cm <- CFCellMeasure$new(meas[m * 2L - 1L], meas[m * 2L])
                root$addCellMeasure(cm)
              }
            }
          } else {
            # Cell measures variable is internal. If it already exists, simply
            # continue with the next iteration.
            if (!length(nm$CF)) {
              ax <- .buildVariableAxisList(nm, axes)
              cm <- CFCellMeasure$new(meas[m * 2L - 1L], meas[m * 2L], nm, ax)
              nm$group$addCellMeasure(cm)
            }
          }
        }
      }
    }
  }

  # Descend into subgroups
  if (length(grp$subgroups))
    lapply(grp$subgroups, function(g) .makeCellMeasures(g, axes))
}

#' Make CRS instances for "grid_mapping" references
#'
#' NC variables are scanned for a "grid_mapping_name" attribute. The NC variable
#' referenced is converted into a CFGridMapping instance in the group where its
#' NC variable is located.
#'
#' @param grp The group to scan.
#'
#' @return Nothing. CFGridMapping instances are created in the groups where the
#'   NC variables are found. These will later be picked up when CFvariables are
#'   created.
#' @noRd
.makeCRS <- function(grp) {
  if (length(grp$NCvars) > 0L) {
    # Scan each unused NCVariable for the "grid_mapping_name" property and process.
    for (refid in seq_along(grp$NCvars)) {
      v <- grp$NCvars[[refid]]
      if (!length(v$CF) && !is.na(gm <- v$attribute("grid_mapping_name")))
        grp$CFcrs <- append(grp$CFcrs, CFGridMapping$new(v))
    }
    if (length(grp$CFcrs))
      names(grp$CFcrs) <- sapply(grp$CFcrs, function(c) c$name)
  }

  # Descend into subgroups
  if (length(grp$subgroups))
    lapply(grp$subgroups, function(g) .makeCRS(g))
}

# Utility function to read bounds values
# grp - the current group being processed
# bounds - the name of the boundary variable, or NA if no bounds attribute present
.readBounds <- function(grp, bounds) {
  if (is.na(bounds)) NULL
  else {
    NCbounds <- grp$find_by_name(bounds, "NC")
    if (is.null(NCbounds)) NULL
    else CFBounds$new(NCbounds)
  }
}

#' Build CF variables from unused dimensional NC variables
#'
#' NC variables with dimensions that do not have their `CF` property set will be
#' made into a `CFVariable`. This method is invoked recursively to travel through
#' all groups of the netCDF resource.
#'
#' @param grp The current group to scan for unused NC variables.
#' @param axes List of available CF axes to use with the CF variables.
#' @return List of created CF variables.
#' @noRd
.buildVariables <- function(grp, axes) {
  if (length(grp$NCvars) > 0L) {
    # Create variable for each unused NCVariable with dimensions
    vars <- lapply(grp$NCvars, function(v) {
      varLon <- varLat <- ll <- NULL
      if (!length(v$CF) && v$ndims > 0L) {
        all_ax <- dim_ax <- .buildVariableAxisList(v, axes)
        ax_names <- names(dim_ax)

        # Add references to any "coordinates" of the variable
        if (!is.na(coords <- v$attribute("coordinates"))) {
          coords <- strsplit(coords, " ", fixed = TRUE)[[1L]]
          for (cid in seq_along(coords)) {
            if (coords[cid] %in% ax_names) next

            aux <- grp$find_by_name(coords[cid], "CF")
            if (!is.null(aux)) {
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
              ll <- grp$find_by_name(coords[cid], "NC")
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
            ll <- varLon$group$find_by_name(paste(varLon$name, varLat$name, sep = "_"), "CF")
        } # coordinates

        # Make the CFVariable
        var <- CFVariable$new(v, all_ax)
        if (!is.null(ll)) var$gridLongLat <- ll

        # Add cell_measures
        if (!is.na(cm <- v$attribute("cell_measures"))) {
          cms <- strsplit(cm, " ", fixed = TRUE)[[1L]]
          len <- as.integer(length(cms) * 0.5)
          for (i in 1L:len) {
            cmv <- v$group$find_by_name(cms[i * 2L], "CF")
            if (inherits(cmv, "CFCellMeasure")) {
              var$add_cell_measure(cmv)
              cmv$register(var)
            }
          }
        }

        # Add grid mapping
        gm <- v$attribute("grid_mapping")
        if (!is.na(gm)) {
          gm <- v$group$find_by_name(gm, "CF")
          if (inherits(gm, "CFGridMapping"))
            var$crs <- gm
        }

        var
      }
    })
    vars <- vars[lengths(vars) > 0L]
    if (length(vars))
      grp$CFvars <- append(grp$CFvars, unlist(vars))
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
