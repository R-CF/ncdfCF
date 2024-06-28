#' @include ncdfVariable.R
NULL

#' Group class
#'
#' This class represents a group in a netCDF resource. This applies equally to
#' the "classic" data model, which will have only a root group. Resources in the
#' "netcdf4" format can have multiple, hierarchical groups.
#'
#' @slot resource The `ncdfResource` instance that handles the netCDF resource.
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

  if(length(object@subgroups)) {
    grps <- do.call(rbind, lapply(object@subgroups, brief))
    if (nrow(grps)) {
      if (nrow(grps) == 1L) cat("\nGroup     :\n") else cat("\nGroups    :\n")
      print(.slim.data.frame(grps), right = FALSE, row.names = FALSE)
    }
  }

  show_attributes(object)
})

#' @rdname showObject
#' @export
setMethod("brief", "ncdfGroup", function (object) {
  d <- data.frame(id = object@id, name = object@name, vars = length(object@vars),
                  dims = length(object@dims), UDTs = length(object@udts),
                  atts = nrow(object@attributes))
  if(length(object@subgroups)) {
    sub <- do.call(rbind, lapply(object@subgroups, brief))
    d <- rbind(d, sub)
  }
  d
})

#' @rdname showObject
#' @export
setMethod("shard", "ncdfGroup", function (object) {
  paste0("[", object@name, "]")
})

#' @rdname str
#' @export
setMethod("str", "ncdfGroup", function(object, ...) {
  cat(object@name, ": Formal class 'ncdfGroup' [package \"ncdfCF\"] with 10 slots\n")
  cat("  ..@ id        :"); str(object@id)
  cat("  ..@ name      :"); str(object@name)
  cat("  ..@ resource  : reference to <ncdfDataset@resource>\n")

  h <- trimws(utils::capture.output(str(object@handle)))
  cat("  ..@ handle    :", h[1], "\n")
  cat(paste0("  .. ..", h[2], "\n"))

  cat(paste0("  ..@ parent    : <", class(object@parent), ": ", object@parent@id, ">\n"))

  nd <- length(object@dims)
  if (!nd) cat("  ..@ dims      : list()\n")
  else {
    cat("  ..@ dims      : List of", nd, "\n")
    lapply(object@dims, function(d) {
      strd <- trimws(utils::capture.output(str(d)))
      cat("  .. ..$", strd[1L], "\n")
      strd <- strd[-1L]
      invisible(lapply(strd, function (z) cat(paste0("  .. .. .. ", z, "\n"))))
    })
  }

  nv <- length(object@vars)
  if (!nv) cat("  ..@ vars      : list()\n")
  else {
    cat("  ..@ vars      : List of", nv, "\n")
    lapply(object@vars, function(v) {
      strv <- trimws(utils::capture.output(str(v)))
      cat("  .. ..$", strv[1L], "\n")
      strv <- strv[-1L]
      invisible(lapply(strv, function (z) cat(paste0("  .. .. .. ", z, "\n"))))
    })
  }

  nu <- length(object@udts)
  if (!nu) cat("  ..@ udts      : list()\n")
  else {
    cat("  ..@ udts      : List of", nu, "\n")
    lapply(object@udts, function(u) {
      stru <- trimws(utils::capture.output(str(u)))
      cat("  .. ..$", stru[1L], "\n")
      stru <- stru[-1L]
      invisible(lapply(stru, function (z) cat(paste0("  .. .. .. ", z, "\n"))))
    })
  }

  str_attributes(object)

  # subgroups
  nsg <- length(object@subgroups)
  if (!nsg) cat("  ..@ subgroups : list()\n")
  else {
    cat("  ..@ subgroups : List of", nsg, "\n")
    invisible(lapply(object@subgroups, function(sg) {
      strsg <- trimws(utils::capture.output(str(sg)))
      cat("  .. ..$", strsg[1], "\n")
      strsg <- strsg[-1L]
      invisible(lapply(strsg, function(g) cat("  .. .. ..", g, "\n")))
    }))
  }
})

#' Get the handle for a netCDF group
#'
#' Get the handle for a group in a netCDF resource. The resource will be opened
#' when the handle is returned.
#'
#' @param x `ncdfGroup` instance whose handle to retrieve. This can be a handle
#' to the resource itself if the group is the root of the dataset, or to any
#' group contained in the resource.
#'
#' @returns A handle to read or write to an opened netCDF resource.
#' @noRd
setGeneric("handle", function(x) standardGeneric("handle"), signature = "x")

#' @noRd
setMethod("handle", "ncdfGroup", function(x) {
  invisible(open(x@resource))
  x@handle
})

#' Get a list of all groups and its subgroups
#'
#' @param x The `ncdfGroup` instance.
#'
#' @returns A list of `ncdfGroup`s.
#' @noRd
.collect_groups <- function(x) {
  sub_groups <- lapply(x@subgroups, .collect_groups)
  c(x, sub_groups, recursive = TRUE)
}

#' Get a list of all variables in the group and its subgroups
#'
#' @param x The `ncdfGroup` instance.
#'
#' @returns A list of `ncdfVariable`s.
#' @noRd
.collect_vars <- function(x) {
  sub_vars <- lapply(x@subgroups, .collect_vars)
  c(x@vars, sub_vars, recursive = TRUE)
}

#' Get a list of all dimensions in the group and its subgroups
#'
#' @param x The `ncdfGroup` instance.
#'
#' @returns A list of `ncdfDimension`s.
#' @noRd
.collect_dims <- function(x) {
  sub_dims <- lapply(x@subgroups, .collect_dims)
  c(x@dims, sub_dims, recursive = TRUE)
}

#' Read a group from a netCDF dataset
#'
#' Variable, dimension, UDT and attribute information are read.
#'
#' @param parent The parent `ncdfDataset` or `ncdfGroup` of this group.
#' @param h The handle to the group in the netCDF resource to read.
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
