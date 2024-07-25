#' Read a netCDF resource
#'
#' @param resource The name of the netCDF resource to open, either a local file
#'   name or a remote URI.
#' @param keep_open Logical flag to indicate if the netCDF resource has to
#'   remain open after reading the metadata. This should be enabled typically
#'   only for programmatic access or when a remote resource has an expensive
#'   access protocol (i.e. 2FA). The resource has to be explicitly closed with
#'   `close()` after use. Note that when a dataset is opened with
#'   `keep_open = TRUE` the resource may still be closed by the operating system
#'   or the remote server.
#'
#' @returns An `CFDataset` instance, or an error if the resource was not found
#'   or errored upon reading.
#' @export
#' @examples
#' fn <- system.file("extdata",
#'   "pr_day_EC-Earth3-CC_ssp245_r1i1p1f1_gr_20240101-20241231_vncdfCF.nc",
#'   package = "ncdfCF")
#' ds <- open_ncdf(fn)
#' ds
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

  # Identify variables
  vars <- .buildVariables(root, axes)

  ds$root <- root
  ds
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
#' @returns Either the `NCGroup` instance invisibly or a try-error instance.
#' @noRd
.readGroup <- function(parent, h, parent_dims) {
  g <- RNetCDF::grp.inq.nc(h)
  grp <- NCGroup$new(id = as.integer(g$self), name = g$name, fullname = g$fullname,
                     parent = parent, resource = parent$resource())

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
  if (g$ngatts) {
    atts <- do.call(rbind, lapply(0L:(g$ngatts - 1L), function (a) as.data.frame(RNetCDF::att.inq.nc(h, "NC_GLOBAL", a))))
    atts$value <- sapply(0L:(g$ngatts - 1L), function (a) RNetCDF::att.get.nc(h, "NC_GLOBAL", a))
    grp$attributes <- atts
  }

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
  if (vmeta$natts > 0L) {
    atts <- do.call(rbind, lapply(0L:(vmeta$natts - 1L), function (a) as.data.frame(RNetCDF::att.inq.nc(h, vmeta$name, a))))
    atts$value <- sapply(0L:(vmeta$natts - 1L), function (a) RNetCDF::att.get.nc(h, vmeta$name, a))
    var$attributes <- atts
  }

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

#' Create an `CFAxis` from an NC variable and dimension
#'
#' @param grp Group in which the NC variable is defined.
#' @param var `NCVariable` instance to create the axis from.
#' @param dim `NCDimension` associated with argument `var`.
#'
#' @returns An instance of `CFAxis`.
.makeAxis <- function(grp, var, dim) {
  h <- grp$handle

  # Dimension values
  vals <- try(as.vector(RNetCDF::var.get.nc(h, var$name)), silent = TRUE)
  if (inherits(vals, "try-error"))
    # No values so it's an identity axis
    return(CFAxisDiscrete$new(grp, var, dim))

  # Does `var` have attributes?
  if (!nrow(var$attributes)) {
    # No attributes so nothing left to do
    return(CFAxisNumeric$new(grp, var, dim, round(vals, 5)))
  }

  # Get essential attributes
  props <- var$attribute(c("standard_name", "units", "calendar", "axis",
                           "bounds"))

  # Does the axis have bounds?
  bounds <- props["bounds"]
  if (!is.na(bounds)) {
    NCbounds <- grp$find_by_name(bounds, "NC")
    if (!is.null(NCbounds)) {
      CFbounds <- CFBounds$new(NCbounds)
      bnds <- try(RNetCDF::var.get.nc(h, bounds), silent = TRUE)
      if (!inherits(bnds, "try-error")) {
        if (length(dim(bnds)) == 3L) bnds <- bnds[, , 1] # Sometimes there is a 3rd dimension...
        CFbounds$values <- bnds
      }
    }
  } else CFbounds <- NULL

  # See if we have a "units" attribute that makes time
  units <- props["units"]
  if (!is.na(units)) {
    cal <- props["calendar"]
    if (is.na(cal)) cal <- "standard"
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
  orient <- props["axis"]
  if (is.na(orient)) {
    if (!is.na(units)) {
      if (grepl("^degree(s?)(_?)(east|E)$", units)) orient <- "X"
      else if (grepl("^degree(s?)(_?)(north|N)$", units)) orient <- "Y"
    }
  }
  if (is.na(orient)) {
    # Last option: standard_name, only X and Y
    standard <- props["standard_name"]
    if (!is.na(standard)) {
      if (standard == "longitude") orient <- "X"
      else if (standard == "latitude") orient <- "Y"
    }
  }

  # Z: standard_names and formula_terms
  if (is.na(orient) && !is.na(standard) &&
      standard %in% c("atmosphere_ln_pressure_coordinate",
                      "atmosphere_sigma_coordinate",
                      "atmosphere_hybrid_sigma_pressure_coordinate",
                      "atmosphere_hybrid_height_coordinate",
                      "atmosphere_sleve_coordinate",
                      "ocean_sigma_coordinate",
                      "ocean_s_coordinate",
                      "ocean_s_coordinate_g1",
                      "ocean_s_coordinate_g2",
                      "ocean_sigma_z_coordinate",
                      "ocean_double_sigma_coordinate"))
    orient <- "Z"
  if (is.na(orient)) orient <- "" # Fallback value if not set

  axis <- CFAxisNumeric$new(grp, var, dim, round(vals, 5))
  axis$orientation <- orient
  axis$bounds <- CFbounds

  axis
}

.buildVariables <- function(grp, axes) {
  if (length(grp$NCvars) > 0L) {
    # Create variable for each unused NCVariable with dimensions
    vars <- lapply(grp$NCvars, function(v) {
      if (is.null(v$CF) && v$ndims > 0L) {
        xids <- sapply(axes, function(x) x$dimid)
        ax <- vector("list", v$ndims)
        for (x in 1:v$ndims)
          ax[[x]] <- axes[[ which(v$dimids[x] == xids) ]]
        names(ax) <- sapply(ax, function(x) x$name)
        CFVariable$new(grp, v, ax)
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
