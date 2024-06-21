#' @include ncdfVariable.R
NULL

#' Group class
#'
#' This class represents a group in a NetCDF resource. This applies equally to
#' the "classic" data model, which will have only a root group. Resources in the
#' "netcdf4" format can have multiple, hierarchical groups.
#'
#' @slot resource The `ncdfResource` instance that handles the NetCDF file.
#' @slot handle The RNetCDF handle to the group.
#' @slot parent The parent group of this group.
#' @slot subgroups The subgroups of this group, if any.
#' @slot vars A list with the variables in the resource.
#' @slot dims A list holding all the dimension data.
setClass("ncdfGroup",
         contains = "ncdfObject",
         slots = c(
           resource   = "ncdfResource",
           handle     = "ANY",
           parent     = "ncdfObject",   # ncdfDataset or ncdfGroup
           subgroups  = "list",         # of ncdfGroup
           vars       = "list",         # of ncdfVariable
           dims       = "list",         # of ncdfDimension
           udts       = "list"          # list of lists of UDTs in RNetCDF format
         ),
         prototype = c(
           subgroups  = list(),
           vars       = list(),
           dims       = list(),
           udts       = list()
         )
)

#' @rdname showObject
#' @export
setMethod("show", "ncdfGroup", function(object) {
  if (!identical(object@name, "/"))
    cat("\nGroup:", object@name, "\n")

  vars <- do.call(rbind, lapply(object@vars, brief))
  vars <- lapply(vars, function(c) if (all(c == "")) NULL else c)
  vars <- as.data.frame(vars[lengths(vars) > 0L])
  if (nrow(vars)) {
    if (nrow(vars) == 1L) cat("\nVariable  :\n") else cat("\nVariables :\n")
    print(.slim.data.frame(vars), right = FALSE, row.names = FALSE)
  }

  dims <- do.call(rbind, lapply(object@dims, brief))
  dims <- lapply(dims, function(c) if (all(c == "")) NULL else c)
  dims <- as.data.frame(dims[lengths(dims) > 0L])
  if (nrow(dims)) {
    if (nrow(dims) == 1L) cat("\nDimension :\n") else cat("\nDimensions:\n")
    print(.slim.data.frame(dims), right = FALSE, row.names = FALSE)
  }

  if (length(object@udts) > 0L) {
    cat("\nUser-defined data type")
    if (length(object@udts) == 1L) cat(" :\n") else cat("s:\n")
    lapply(object@udts, function(udt) {
      cat(sprintf("[%d] %s: %s (size = %d)\n", udt$id, udt$name, udt$class, udt$size))
      if (udt$class == "compound") {
        cmp <- data.frame(cbind(names(udt$subtype), udt$subtype, udt$offset))
        names(cmp) <- c("element", "data_type", "offset")
        print(cmp, row.names = F)
      }
    })
  }

  show_attributes(object)

  if(length(object@subgroups)) lapply(object@subgroups, print)
})

#' Get the handle for a NetCDF group
#'
#' Get the handle for a group in a NetCDF resource. The resource will be opened
#' when the handle is returned.
#'
#' @param x `ncdfGroup` instance whose handle to retrieve. This can be a handle
#' to the resource itself if the group is the root of the dataset, or to any
#' group contained in the resource.
#'
#' @returns A handle to read or write to an opened NetCDF resource.
#' @noRd
setGeneric("handle", function(x) standardGeneric("handle"), signature = "x")

#' @noRd
setMethod("handle", "ncdfGroup", function(x) {
  invisible(open(x@resource))
  x@handle
})

#' Get a list of all variables in the group and its subgroups
#'
#' @param x The `ncdfGroup` instance.
#'
#' @returns A list of `ncdfVariable`s.
#' @noRd
.collect_group_vars <- function(x) {
  sub_vars <- lapply(x@subgroups, .collect_group_vars)
  c(x@vars, sub_vars, recursive = TRUE)
}

#' Get a list of all dimensions in the group and its subgroups
#'
#' @param x The `ncdfGroup` instance.
#'
#' @returns A list of `ncdfDimension`s.
#' @noRd
.collect_group_dims <- function(x) {
  sub_dims <- lapply(x@subgroups, .collect_group_dims)
  c(x@dims, sub_dims, recursive = TRUE)
}

#' Read a group from a NetCDF dataset
#'
#' Variable, dimension and attribute information are read.
#'
#' @param parent The parent `ncdfDataset` or `ncdfGroup` of this group.
#' @param h The handle to the group in the NetCDF resource to read.
#'
#' @returns Either the `ncdfGroup` instance invisibly or a try-error instance.
#' @noRd
.readGroup <- function(parent, h) {
  g <- RNetCDF::grp.inq.nc(h)
  grp <- methods::new("ncdfGroup", id = as.integer(g$self), name = g$fullname,
                      resource = parent@resource, handle = h, parent = parent)

  # Dimensions
  if (length(g$dimids)) {
    dims <- lapply(g$dimids, function (d) .readDimension(grp, d))
    names(dims) <- sapply(dims, name)
    grp@dims <- dims
  }

  # Variables
  if (length(g$varids)) {
    vars <- lapply(g$varids, function (v) .readVariable(grp, v))
    vars <- vars[!sapply(vars, is.null)]
    names(vars) <- sapply(vars, name)
    grp@vars <- vars
  }

  # UDTs
  if (length(g$typeids))
    grp@udts <- lapply(g$typeids, function(t) RNetCDF::type.inq.nc(h, t, fields = TRUE))

  # Global attributes
  if (g$ngatts) {
    atts <- do.call(rbind, lapply(0L:(g$ngatts - 1L), function (a) as.data.frame(RNetCDF::att.inq.nc(h, "NC_GLOBAL", a))))
    atts$value <- sapply(0L:(g$ngatts - 1L), function (a) RNetCDF::att.get.nc(h, "NC_GLOBAL", a))
    grp@attributes <- atts
  } else grp@attributes <- data.frame()

  # Subgroups
  if (length(g$grps)) {
    grp@subgroups <- lapply(g$grps, function(z) .readGroup(grp, z))
  }

  # Drop unused dimensions, usually just the bounds dimension(s)
  # FIXME: Dimensions may be used by variables in subgroups
  if (length(g$dimids) && length(g$varids))
    grp@dims <- dims[unique(unlist((lapply(vars, function(v) sapply(v@dims, name)))))]

  grp
}
