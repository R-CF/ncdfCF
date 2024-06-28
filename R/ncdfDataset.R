#' @include ncdfGroup.R
NULL

#' ncdfDataset class
#'
#' This class represents a single netCDF resource.
#'
#' @slot resource The `ncdfResource` instance that handles the netCDF resource.
#' @slot keep_open Logical flag to indicate if the resource should remain open
#' for data access after the initial read of metadata.
#' @slot root The root group of this dataset. This contains all variables,
#' dimensions and attributes in the "classic" netCDF model, the "netcdf4" model
#' may have subgroups.
#' @slot format A character string with the format of the netCDF resource.
#' @slot has_error logical. Flag to indicate if there was an error opening the resource.
setClass("ncdfDataset",
  contains = "ncdfObject",
  slots = c(
    resource   = "ncdfResource",
    keep_open  = "logical",
    root       = "ncdfGroup",
    format     = "character",
    has_error  = "logical"
  ),
  prototype = c(
    keep_open  = FALSE,
    format     = "",
    has_error  = FALSE
  )
)

#' @name objects_by_standard_name
#' @title Get objects by standard_name
#'
#' @description
#' Several conventions define standard vocabularies for physical properties. The
#' standard names from those vocabularies are usually stored as the
#' "standard_name" attribute with variables or dimensions. This method
#' retrieves all variables or dimensions that list the specified "standard_name"
#' in its attributes.
#'
#' @param x The `ncdfDataset` to query for the "standard_name" attribute.
#' @param standard_name Optional, a character string to search for a specific
#' "standard_name" value in variables and dimensions.
#'
#' @returns If argument `standard_name` is provided, a character vector of
#' variable or dimension names. If argument `standard_name` is missing or an
#' empty string, a named list with all "standard_name" attribute values in the
#' the netCDF resource; each list item is named for the variable or dimension.
#' @export
#'
#' @examples
#' fn <- system.file("extdata",
#'   "pr_day_EC-Earth3-CC_ssp245_r1i1p1f1_gr_20240101-20241231_vncdfCF.nc",
#'   package = "ncdfCF")
#' ds <- open_ncdf(fn)
#' objects_by_standard_name(ds, "precipitation_flux")
setGeneric("objects_by_standard_name", function(x, standard_name)
  standardGeneric("objects_by_standard_name"), signature = "x")

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
#' @returns An `ncdfDataset` instance, or an error if the resource was not found
#'   or errored upon reading.
#' @export
#' @examples
#' fn <- system.file("extdata",
#'   "pr_day_EC-Earth3-CC_ssp245_r1i1p1f1_gr_20240101-20241231_vncdfCF.nc",
#'   package = "ncdfCF")
#' ds <- open_ncdf(fn)
#' ds
open_ncdf <- function(resource, keep_open = FALSE) {
  res <- ncdfResource(resource)
  if (inherits(res, "try-error"))
    stop(as.character(res))

  self <- methods::new("ncdfDataset",
                       name = regmatches(resource, regexec("([^/]*)\\.nc$", resource))[[1L]][2L],
                       resource = res, keep_open = keep_open)

  ds <- .open_dataset(self)
  if (inherits(ds, "try-error"))
    self@has_error <- TRUE
  else {
    self <- ds
    self@has_error <- FALSE
  }
  self
}

#' @rdname showObject
#' @export
setMethod("show", "ncdfDataset", function(object) {
  cat("Dataset   :", object@name, "\n")
  cat("Format    :", object@format, "\n")
  if (object@has_error)
    cat("An error occurred during opening of the dataset\n")
  else print(object@root)
})

#' @rdname showObject
#' @export
setMethod("brief", "ncdfDataset", function(object) {
  if (object@has_error)
    cat("Error  :", object@name, "\n")
  else {
    cat("Dataset:", object@name, "\n")
    cat("  Variables  :", paste(lapply(.collect_vars(object@root), ncdfCF::shard)),
        "\n  Dimensions :", paste(lapply(.collect_dims(object@root), ncdfCF::shard)),
        "\n      Groups :", paste(lapply(.collect_groups(object@root), ncdfCF::shard)), "\n")
  }
})

#' @rdname str
#' @export
setMethod("str", "ncdfDataset", function(object, ...) {
  cat("Formal class 'ncdfDataset' [package \"ncdfCF\"] with 8 slots\n")
  cat("  ..@ id        :"); str(object@id)
  cat("  ..@ name      :"); str(object@name)
  cat("  ..@ format    :"); str(object@format)

  res <- trimws(utils::capture.output(str(object@resource)))
  cat("  ..@ resource  :", res[1L], "\n")
  res <- res[-1L]
  invisible(lapply(res, function(r) cat("  .. ..", r, "\n")))

  cat("  ..@ keep_open :"); str(object@keep_open)
  cat("  ..@ has_error :"); str(object@has_error)

  str_attributes(object)

  # Groups
  root <- trimws(utils::capture.output(str(object@root)))
  cat("  ..@ root      :", root[1L], "\n")
  root <- root[-1L]
  invisible(lapply(root, function(g) cat("  .. ..", g, "\n")))
})

#' Variable names of an `ncdfDataset` instance
#'
#' @param x `ncdfDataset` whose variable names to retrieve.
#'
#' @returns A character vector of variable names.
#' @export
#' @examples
#' fn <- system.file("extdata",
#'   "pr_day_EC-Earth3-CC_ssp245_r1i1p1f1_gr_20240101-20241231_vncdfCF.nc",
#'   package = "ncdfCF")
#' ds <- open_ncdf(fn)
#' names(ds)
setMethod("names", "ncdfDataset", function(x) as.vector(sapply(.collect_vars(x@root), name)))

#' @rdname dimnames
#' @export
setMethod("dimnames", "ncdfDataset", function(x) as.vector(sapply(.collect_dims(x@root), name)))

#' @rdname dimlength
#' @export
setMethod("dim", "ncdfDataset", function(x) sapply(.collect_dims(x@root), length))

#' @rdname objects_by_standard_name
#' @export
setMethod("objects_by_standard_name", "ncdfDataset", function(x, standard_name) {
  nm <- c(sapply(.collect_vars(x@root), attribute, "standard_name"),
          sapply(.collect_dims(x@root), attribute, "standard_name"))
  if (missing(standard_name) || !nzchar(standard_name))
    nm[lengths(nm) > 0L]
  else
    names(nm[which(nm == standard_name)])
})

# FIXME: Must process names with group info prepended

#' Get a group, variable or dimension object from a dataset
#'
#' This method can be used to retrieve a group, variable or dimension from the
#' dataset by name.
#'
#' @param x An `ncdfDataset` to extract a group, variable or a dimension from.
#' @param i The name of a group, variable or dimension in `x`.
#'
#' @returns An instance of `ncdfVariable` or an `ncdfDimension` descendant
#' class, or `NULL` if the name is not found.
#' @export
#'
#' @aliases [[,ncdfDataset-method
#' @docType methods
#' @examples
#' fn <- system.file("extdata", "ERA5land_Rwanda_20160101.nc", package = "ncdfCF")
#' ds <- open_ncdf(fn)
#' v1 <- names(ds)[1]
#' var <- ds[[v1]]
#' var
setMethod("[[", "ncdfDataset", function(x, i) {
  vars <- .collect_vars(x@root)
  idx <- which(names(vars) == i)
  if (length(idx)) return(vars[[idx]])

  dims <- .collect_dims(x@root)
  idx <- which(names(dims) == i)
  if (length(idx)) return(dims[[idx]])

  groups <- .collect_groups(x@root)
  nm <- lapply(groups, function(g) g@name)
  idx <- which(nm == i)
  if (length(idx)) return(groups[[idx]])

  NULL
})

#' Open the dataset from a netCDF resource
#'
#' Variable, dimension and attribute information are read.
#'
#' @param dataset The `ncdfDataset` instance whose resource to open.
#'
#' @returns Either the `ncdfDataset` instance invisibly or a try-error instance.
#' @noRd
.open_dataset <- function(dataset) {
  err <- try({
    h <- open(dataset@resource)
    g <- RNetCDF::file.inq.nc(h)
    dataset@format <- g$format

    # Read all groups recursively
    dataset@root <- .readGroup(dataset, h)

    if (!dataset@keep_open) close(dataset@resource)
  }, silent = TRUE)
  if (inherits(err, "try-error"))
    err
  else invisible(dataset)
}

