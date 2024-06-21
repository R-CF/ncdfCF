#' @include ncdfGroup.R
NULL

#' ncdfDataset class
#'
#' This class represents a single NetCDF resource.
#'
#' @slot resource The `ncdfResource` instance that handles the NetCDF file.
#' @slot keep_open Logical flag to indicate if the resource should remain open
#' for data access after the initial read of metadata.
#' @slot root The root group of this dataset. This contains all variables,
#' dimensions and attributes in the "classic" NetCDF model, the "netcdf4" model
#' may have subgroups.
#' @slot format A character string with the format of the NetCDF resource.
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
#' the NetCDF resource; each list item is named for the variable or dimension.
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

#' Read a NetCDF resource
#'
#' @param resource The name of the NetCDF resource to open, either a local file
#'   name or a remote URI.
#' @param keep_open Logical flag to indicate if the NetCDF resource has to
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

  self <- methods::new("ncdfDataset", name = resource, resource = res, keep_open = keep_open)

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
    cat("  Variables  :", paste(lapply(.collect_vars(object), ncdfCF::shard)),
        "\n  Dimensions :", paste(lapply(.collect_dims(object), ncdfCF::shard)), "\n")
  }
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
setMethod("names", "ncdfDataset", function(x) as.vector(sapply(.collect_vars(x), name)))

#' @rdname ncdfDimnames
#' @export
setMethod("dimnames", "ncdfDataset", function(x) as.vector(sapply(.collect_dims(x), name)))

#' @rdname dimlength
#' @export
setMethod("dim", "ncdfDataset", function(x) sapply(.collect_dims(x), length))

#' @rdname objects_by_standard_name
#' @export
setMethod("objects_by_standard_name", "ncdfDataset", function(x, standard_name) {
  nm <- c(sapply(.collect_vars(x), attribute, "standard_name"),
          sapply(.collect_dims(x), attribute, "standard_name"))
  if (missing(standard_name) || nchar(standard_name) == 0L)
    nm[lengths(nm) > 0L]
  else
    names(nm[which(nm == standard_name)])
})

#' Get a variable object or a dimension object from a dataset
#'
#' This method can be used to retrieve a variable or a dimension from the
#' dataset by name.
#'
#' @param x An `ncdfDataset` to extract a variable or a dimension from.
#' @param i The name of a variable or dimension in `x`.
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
  vars <- .collect_vars(x)
  idx <- which(names(vars) == i)
  if (length(idx)) return(vars[[idx]])

  dims <- .collect_dims(x)
  idx <- which(names(dims) == i)
  if (length(idx)) return(dims[[idx]])

  NULL
})

#' Open a NetCDF dataset
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

#' Get a list of all variables in the dataset
#'
#' @param x The `ncdfDataset` instance
#'
#' @returns A list of `ncdfVariable`s.
#' @noRd
.collect_vars <- function(x) {
  .collect_group_vars(x@root)
}

#' Get a list of all dimensions in the dataset
#'
#' @param x The `ncdfDataset` instance
#'
#' @returns A list of `ncdfDimension`s.
#' @noRd
.collect_dims <- function(x) {
  .collect_group_dims(x@root)
}
