#' Open a netCDF resource
#'
#' This function will read the metadata of a netCDF resource and interpret the
#' netCDF dimensions, variables and attributes to generate the corresponding CF
#' objects. The data for the CF variables is not read, please see [CFVariable]
#' for methods to read the variable data.
#'
#' @param resource The name of the netCDF resource to open, either a local file
#'   name or a remote URI.
#' @param keep_open Logical flag to indicate if the netCDF resource has to
#'   remain open after reading the metadata. This should be enabled typically
#'   only for programmatic access or when a remote resource has an expensive
#'   access protocol (i.e. 2FA). The resource has to be explicitly closed with
#'   `close()` after use. Note that when a data set is opened with
#'   `keep_open = TRUE` the resource may still be closed by the operating system
#'   or the remote server.
#'
#' @return An `CFDataset` instance, or an error if the resource was not found
#'   or errored upon reading.
#' @export
#' @examples
#' fn <- system.file("extdata",
#'   "pr_day_EC-Earth3-CC_ssp245_r1i1p1f1_gr_20230101-20231231_vncdfCF.nc",
#'   package = "ncdfCF")
#' (ds <- open_ncdf(fn))
open_ncdf <- function(resource, keep_open = FALSE) {
  # Parameter check
  if (length(resource) != 1L && !is.character(resource))
    stop("Argument `resource` must be a single character string pointing to a netCDF resource.")

  res <- CFResource$new(resource)
  if (is.character(res))
    stop(res)

  h <- res$handle
  g <- RNetCDF::file.inq.nc(h)

  nm <- regmatches(resource, regexec("([^/]*)\\.nc$", resource))[[1L]][2L]
  if (is.na(nm))
    nm <- regmatches(resource, regexec("([^/]*)$", resource))[[1L]][2L]
  ds <- CFDataset$new(name = nm, resource = res, keep_open = keep_open, format = g$format)

  # Read all netCDF groups recursively
  root <- .readGroup(ds, h, vector("integer"))

  # Identify axes: NUG coordinate variables
  axes <- .buildAxes(root, list())

  # Find the id's of any "bounds" variables
  bnds <- sapply(root$NCvars, function(v) {
    nm <- v$attribute("bounds")
    if (nzchar(nm)) {
      obj <- v$group$find_by_name(nm, "NC")
      if (is.null(obj)) {
        # FIXME: warning
        -1L
      } else obj$dimids[1L] # By definition, bounds dimid comes first
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
    names(root$CFaxes) <- sapply(root$CFaxes, function(x) x$name)
    names(root$NCvars) <- sapply(root$NCvars, function(v) v$name)
  }

  # Auxiliary CVs and scalar CVs
  .makeCoordinates(root)

  # Coordinate reference systems
  .makeCRS(root)

  if (length(axes) > 0L) {
    # Try to identify the type of the file
    ft <- root$attribute("featureType")
    if (ft %in% c("point", "timeSeries", "trajectory", "profile",
                  "timeSeriesProfile", "trajectoryProfile"))
      ds$file_type <- "discrete sampling geometry"

    # CMIP6, CMIP5, CORDEX

    vars <- .buildVariables(root, axes)
  } else {
    # Try L3b
    units <- root$attribute("units")
    if (nzchar(units)) {
      units <- strsplit(units, ":")[[1L]]

      l3bgrp <- root$subgroups[["level-3_binned_data"]]
      if (!is.null(l3bgrp)) {
        nm <- names(l3bgrp$NCvars)
        if (all(c("BinList", "BinIndex", units[1L]) %in% nm)) {
          ds$file_type <- "NASA level-3 binned data"
          l3bgrp$CFvars <- list(CFVariableL3b$new(l3bgrp, units))
          names(l3bgrp$CFvars) <- units[1L]
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
#' open an issue: https://github.com/pvanlaake/ncdfCF/issues.
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
         variables  = do.call(rbind, lapply(ds$variables(), function(v) v$peek(grps))),
         axes       = do.call(rbind, lapply(ds$axes(), function(a) a$peek(grps))),
         attributes = ds$attributes())
  } else list()
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
  grp <- NCGroup$new(id = as.integer(g$self), name = g$name, fullname = g$fullname,
                     parent = parent, resource = parent$resource)

  # Read all the raw NC variables in the group
  if (length(g$varids)) {
    NCvars <- lapply(g$varids, function (v) .readNCVariable(grp, g$self, v))
    names(NCvars) <- sapply(NCvars, function(v) v$name)
    grp$NCvars <- NCvars
  }

  # Dimensions by dimid
  if (length(g$dimids) && length(new_dims <- g$dimids[!(g$dimids %in% parent_dims)])) {
    dims <- lapply(new_dims, function (d) {
      dmeta <- RNetCDF::dim.inq.nc(h, d)
      NCDimension$new(dmeta$id, dmeta$name, dmeta$length, dmeta$unlim)
    })
    names(dims) <- sapply(dims, function(d) d$name)
    grp$NCdims <- dims
  }

  # UDTs
  if (length(g$typeids))
    grp$NCudts <- lapply(g$typeids, function(t) RNetCDF::type.inq.nc(h, t, fields = TRUE))

  # Global attributes
  if (g$ngatts)
    grp$attributes <- .readAttributes(h, "NC_GLOBAL", g$ngatts)

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
  var <- NCVariable$new(id = as.integer(vmeta$id), name = vmeta$name, group = grp,
                        vtype = vmeta$type, ndims = vmeta$ndims, dimids = vmeta$dimids)

  if (length(vmeta) > 6L)
    var$netcdf4 <- vmeta[-(1L:6L)]

  # Get the attributes
  if (vmeta$natts > 0L)
    var$attributes <- .readAttributes(h, vmeta$name, vmeta$natts)

  var
}

.buildAxes <- function(grp, parent_dims) {
  # Build locally available dimensions from parent_dims and local dims
  if (length(grp$NCdims) > 0L) {
    if (length(parent_dims) > 0L) {
      # Merge dimensions, mask parent dimensions that have been redefined
      local_dim_names <- sapply(grp$NCdims, function(d) d$name)
      parent_dim_names <- sapply(parent_dims, function(d) d$name)
      keep <- parent_dims[!which(parent_dim_names %in% local_dim_names)]
      visible_dims <- append(keep, grp$NCdims)
    } else
      visible_dims <- grp$NCdims
  } else
    visible_dims <- parent_dims

  if (length(grp$NCvars) > 0L) {
    # Create axis for variables with name equal to dimension names
    dim_names <- sapply(visible_dims, function(d) d$name)
    local_vars <- grp$NCvars[dim_names]
    local_CVs <- local_vars[lengths(local_vars) > 0L]
    axes <- lapply(local_CVs, function(v) .makeAxis(grp, v, visible_dims[[v$name]]))

    grp$CFaxes <- append(grp$CFaxes, unlist(axes))
  } else axes <- list()

  # Descend into subgroups
  if (length(grp$subgroups)) {
    ax <- lapply(grp$subgroups, function(g) .buildAxes(g, visible_dims))
    axes <- append(axes, unlist(ax))
  }

  axes
}

# Create an `CFAxis` from an NC variable and dimension
#
# This method creates the various kinds of axes, with the exception of
# [CFAxisVertical], which is passed off to `.makeAxisParametric` once a
# parametric Z-axis is detected.
#
# @param grp Group in which the NC variable is defined.
# @param var `NCVariable` instance to create the axis from.
# @param dim `NCDimension` associated with argument `var`.
#
# @return An instance of `CFAxis`.
.makeAxis <- function(grp, var, dim) {
  h <- grp$handle

  # Dimension values
  vals <- try(as.vector(RNetCDF::var.get.nc(h, var$name)), silent = TRUE)
  if (inherits(vals, "try-error"))
    # No values so it's an identity axis
    return(CFAxisDiscrete$new(grp, var, dim, ""))
  if (is.numeric(vals)) vals <- round(vals, 5) # Drop spurious "precision"

  # Does `var` have attributes?
  if (!nrow(var$attributes)) {
    # No attributes so nothing left to do
    if (var$vtype %in% c("NC_CHAR", "NC_STRING"))
      return(CFAxisCharacter$new(grp, var, dim, "", vals))
    else return(CFAxisNumeric$new(grp, var, dim, "", vals))
  }

  # Get essential attributes
  props <- var$attribute(c("standard_name", "units", "calendar", "axis", "bounds"))
  standard <- props[["standard_name"]]

  # Z: standard_names and formula_terms for parametric vertical axis
  if (!is.null(standard) && standard %in% Z_parametric_standard_names)
    return(.makeParametricAxis(grp, var, dim, vals, standard))

  # Does the axis have bounds?
  CFbounds <- .readBounds(grp, props[["bounds"]])

  # See if we have a "units" attribute that makes time
  units <- props[["units"]]
  if (!is.null(units)) {
    cal <- props[["calendar"]]
    if (is.null(cal)) cal <- "standard"
    cf <- try(CFtime::CFtime(units, cal, vals), silent = TRUE)
    if (!inherits(cf, "try-error")) {
      if (!is.null(CFbounds))
        CFtime::bounds(cf) <- CFbounds$values
      timeaxis <- CFAxisTime$new(grp, var, dim, cf)
      timeaxis$bounds <- CFbounds
      return(timeaxis)
    }
  }

  # Orientation of the axis
  orient <- props[["axis"]]
  if (is.null(orient)) {
    if (!is.null(units)) {
      if (grepl("^degree(s?)(_?)(east|E)$", units)) orient <- "X"
      else if (grepl("^degree(s?)(_?)(north|N)$", units)) orient <- "Y"
    }
  }
  if (is.null(orient)) {
    # Last option: standard_name, only X and Y
    if (!is.null(standard)) {
      if (standard == "longitude") orient <- "X"
      else if (standard == "latitude") orient <- "Y"
    }
  }
  if (is.null(orient)) {
    # Desperate option: try name of NC variable - non-standard
    nm <- toupper(var$name)
    orient <- if (match(nm, c("X", "Y", "Z", "T"), nomatch = 0L)) nm else ""
  }

  axis <- if (orient == "X")
    CFAxisLongitude$new(grp, var, dim, vals)
  else if (orient == "Y")
    CFAxisLatitude$new(grp, var, dim, vals)
  else CFAxisNumeric$new(grp, var, dim, orient, vals)
  axis$bounds <- CFbounds

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
        v <- NCVariable$new(-2L, d$name, grp, "NC_INTEGER", 1L, d$id)
        axis <- CFAxisDiscrete$new(grp, v, d, "")
        v$CF <- axis
        grp$NCvars <- append(grp$NCvars, v)
        grp$CFaxes <- append(grp$CFaxes, axis)
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

#' Make a parametric vertical axis
#'
#' This function is called when a parametric vertical axis is found during in
#' function .makeAxis().
#'
#' @param grp The group where the axis is found.
#' @param var The NC variable that defines the axis.
#' @param dim The dimension associated with the axis.
#' @param vals The parameter values of the axis.
#' @param param_name The "standard_name" attribute that names the specific
#' parametric form of the axis.
#'
#' @return An instance of CFAxisVertical.
#' @noRd
.makeParametricAxis <- function(grp, var, dim, vals, param_name) {
  Z <- CFAxisVertical$new(grp, var, dim, vals, param_name)

  # Get the formula terms
  ft <- var$attribute("formula_terms")
  if (nzchar(ft)) {
    ft <- trimws(strsplit(ft, " ")[[1L]], whitespace = ":")
    dim(ft) <- c(2, length(ft) * 0.5)
    rownames(ft) <- c("term", "variable")
    ft <- as.data.frame(t(ft))
    ft$NCvar <- lapply(ft$variable, function(v) {
      ncvar <- grp$find_by_name(v, "NC")
      if (!is.null(ncvar)) {
        ncvar$CF <- Z
        ncvar
      }
    })
    Z$formula_terms <- ft
    # FIXME: Z axis may have bounds
  }
  Z
}

#' Make CF constructs for "coordinates" references
#'
#' NC variables are scanned for a "coordinates" attribute (which must be a data
#' variable, domain variable or geometry container variable). The NC variable
#' referenced is converted into one of 3 objects, depending on context:
#' 1. A scalar coordinate variable in the group where its NC variable is
#' located;
#' 2. A label variable in the group where its NC variable is located; multiple
#' label coordinates (such as in the case of taxon name and identifier) are
#' stored in a single label variable;
#' 3. A long-lat auxiliary coordinate variable when both a longitude and
#' latitude NC variable are found, in the group of the longitude NC variable.
#'
#' @param grp The group to scan.
#'
#' @return Nothing. CFAxisScalar and CFAuxiliaryLongLat instances are created
#' in the groups where the NC variables are found. These will later be picked up
#' when CFvariables are created.
#' @noRd
.makeCoordinates <- function(grp) {
  if (length(grp$NCvars) > 0L) {
    # Scan each unused NCVariable for the "coordinates" property and process.
    # The NCVariable must have dimensional axes.
    for (refid in seq_along(grp$NCvars)) {
      v <- grp$NCvars[[refid]]
      if (!length(v$CF) && length(vdimids <- v$dimids) &&
          nzchar(coords <- v$attribute("coordinates"))) {
        coords <- strsplit(coords, " ", fixed = TRUE)[[1L]]
        varLon <- varLat <- bndsLon <- bndsLat <- NA
        for (cid in seq_along(coords)) {
          aux <- grp$find_by_name(coords[cid], "NC")
          if (!is.null(aux)) {
            bounds <- aux$attribute("bounds")

            # Create the auxiliary construct
            nd <- aux$ndims
            if (nd == 0L) {
              # No dimensions so create a scalar axis in the group of aux
              val <- try(RNetCDF::var.get.nc(grp$handle, aux$name), silent = TRUE)
              if (inherits(val, "try-error")) val <- NULL
              orient <- aux$attribute("axis")
              scalar <- CFAxisScalar$new(aux$group, aux, orient, val)
              scalar$bounds <- .readBounds(aux$group, bounds)
              aux$group$CFaxes[[aux$name]] <- scalar
            } else if (aux$vtype %in% c("NC_CHAR", "NC_STRING")) {
              # Label
              val <- try(RNetCDF::var.get.nc(grp$handle, aux$name), silent = TRUE)
              if (inherits(val, "try-error")) val <- NULL
              dim <- grp$find_dim_by_id(aux$dimids[length(aux$dimids)]) # If there are 2 dimids, the first is a string length for a NC_CHAR type
              aux$group$CFlabels[[aux$name]] <- CFLabel$new(aux$group, aux, dim, val)
            } else {
              if (!all(aux$dimids %in% vdimids) || nd > 2L)
                warning("Unmatched `coordinates` value '", coords[cid], "' found in variable '", v$name, "'", call. = FALSE)
              if (nd == 2L) {
                # If the NCVariable aux has an attribute "units" with value
                # "degrees_east" or "degrees_north" it is a longitude or latitude,
                # respectively. Record the fact and move on.
                units <- aux$attribute("units")
                if (nzchar(units)) {
                  if (grepl("^degree(s?)(_?)(east|E)$", units)) {
                    varLon <- aux
                    bndsLon <- .readBounds(aux$group, bounds, 2L)
                  } else if (grepl("^degree(s?)(_?)(north|N)$", units)) {
                    varLat <- aux
                    bndsLat <- .readBounds(aux$group, bounds, 2L)
                  }
                }
              }
            }
          }
        }

        # Make a CFAuxiliaryLongLat if we have found a varLon and a varLat and
        # they have identical dimensions, in the varLon group.
        if ((inherits(varLon, "NCVariable") && inherits(varLat, "NCVariable")) &&
            identical(varLon$dimids, varLat$dimids)) {
          varLon$group$addAuxiliaryLongLat(varLon, varLat, bndsLon, bndsLat)
        }
      }
    }
  }

  # Descend into subgroups
  if (length(grp$subgroups))
    lapply(grp$subgroups, function(g) .makeCoordinates(g))
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
      if (!length(v$CF) && nzchar(gm <- v$attribute("grid_mapping_name")))
        grp$CFcrs <- append(grp$CFcrs, CFGridMapping$new(grp, v, gm))
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
# bounds - the name of the "bounds" variable, or NULL if no bounds present
# axis_dims - number of axes on the coordinate variable; usually 1 but could be
# 2 on an auxiliary coordinate variable
.readBounds <- function(grp, bounds, axis_dims = 1L) {
  if (is.null(bounds)) NULL
  else {
    NCbounds <- grp$find_by_name(bounds, "NC")
    if (is.null(NCbounds)) NULL
    else {
      bnds <- try(RNetCDF::var.get.nc(NCbounds$group$handle, bounds, collapse = FALSE), silent = TRUE)
      if (inherits(bnds, "try-error")) NULL
      else {
        if (length(dim(bnds)) == 3L && axis_dims == 1L) { # Never seen more dimensions than this
          # FIXME: Flag non-standard item
          bnds <- bnds[, , 1L]
        }
        CFBounds$new(NCbounds, bnds)
      }
    }
  }
}

#' Build CF variables from unused dimensional NC variables
#'
#' NC variables with dimensions that do not have their `CF` property set will be
#' made into a CFVariable. This method is invoked recursively to travel through
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
      if (!length(v$CF) && v$ndims > 0L) {
        xids <- lapply(axes, function(x) x$dimid)
        ax <- vector("list", v$ndims)
        for (x in 1:v$ndims) {
          ndx <- which(sapply(xids, function(e) v$dimids[x] %in% e))
          if (!length(ndx)) {
            warning(paste0("Possible variable '", v$name, "' cannot be constructed because of unknown axis identifier ", v$dimids[x]))
            # FIXME
            return(NULL)
          }
          ax[[x]] <- axes[[ndx]]
        }
        names(ax) <- sapply(ax, function(x) x$name)
        var <- CFVariableGeneric$new(grp, v, ax)

        # Add references to any "coordinates" of the variable
        varLon <- varLat <- NULL
        if (nzchar(coords <- v$attribute("coordinates"))) {
          coords <- strsplit(coords, " ", fixed = TRUE)[[1L]]
          for (cid in seq_along(coords)) {
            aux <- grp$find_by_name(coords[cid], "CF")
            if (!is.null(aux)) {
              clss <- class(aux)[1L]
              if (clss == "CFAxisScalar")
                var$axes[[aux$name]] <- aux
              else if (clss == "CFLabel") {
                ndx <- which(sapply(ax, function(x) x$dimid == aux$dimid))
                if (length(ndx)) ax[[ndx]]$labels <- aux
                else {  # FIXME: record warning
                }
              } else {
                # FIXME: Record warning
              }
            } else {
              aux <- grp$find_by_name(coords[cid], "NC")
              if (!is.null(aux)) {
                units <- aux$attribute("units")
                if (nzchar(units)) {
                  if (grepl("^degree(s?)(_?)(east|E)$", units)) varLon <- aux
                  else if (grepl("^degree(s?)(_?)(north|N)$", units)) varLat <- aux
                }
              }
            }
          }

          if (inherits(varLon, "NCVariable") && inherits(varLat, "NCVariable")) {
            aux <- varLon$group$find_by_name(paste(varLon$name, varLat$name, sep = "_"), "CF")
            if (!is.null(aux)) var$gridLongLat <- aux
          }
        }

        # Add grid mapping
        gm <- v$attribute("grid_mapping")
        if (nzchar(gm)) {
          gm <- v$group$find_by_name(gm, "CF")
          if (inherits(gm, "CFGridMapping"))
            var$grid_mapping <- gm
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

# Read the attributes for a group or a variable
.readAttributes <- function(h, name, num) {
  atts <- do.call(rbind, lapply(0L:(num - 1L), function (a) as.data.frame(RNetCDF::att.inq.nc(h, name, a))))
  atts$value <- lapply(0L:(num - 1L), function (a) RNetCDF::att.get.nc(h, name, a, fitnum = TRUE))
  atts
}
