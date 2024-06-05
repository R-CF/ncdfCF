#' @include ncdfVariable.R
NULL

#' ncdfDataset class
#'
#' This class represents a single NetCDF resource.
#'
#' @slot resource The `ncdfResource` instance that handles the NetCDF file.
#' @slot keep_open Logical flag to indicate if the resource should remain open
#' for data access after the initial read of metadata.
#' @slot vars A list with the variables in the resource.
#' @slot dims A list holding all the dimension data.
#' @slot format A character string with the format of the NetCDF resource.
#' @slot has_error logical. Flag to indicate if there was an error opening the resource.
setClass("ncdfDataset",
  contains = "ncdfObject",
  slots = c(
    resource   = "ncdfResource",
    keep_open  = "logical",
    vars       = "list", # of ncdfVariable
    dims       = "list", # of ncdfDimension
    format     = "character",
    has_error  = "logical"
  ),
  prototype = c(
    keep_open  = FALSE,
    format     = "",
    has_error  = FALSE,
    vars       = list(),
    dims       = list()
  )
)

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
  else {
    vars <- do.call(rbind, lapply(object@vars, brief))
    vars <- lapply(vars, function(c) if (all(c == "")) NULL else c)
    vars <- as.data.frame(vars[lengths(vars) > 0L])
    if (nrow(vars)) {
      if (nrow(vars) == 1L) cat("\nVariable  :\n") else cat("\nVariables :\n")
      print(.slim.data.frame(vars, 50), right = FALSE, row.names = FALSE)
    }

    dims <- do.call(rbind, lapply(object@dims, brief))
    dims <- lapply(dims, function(c) if (all(c == "")) NULL else c)
    dims <- as.data.frame(dims[lengths(dims) > 0L])
    if (nrow(dims)) {
      if (nrow(dims) == 1L) cat("\nDimension :\n") else cat("\nDimensions:\n")
      print(.slim.data.frame(dims, 50), right = FALSE, row.names = FALSE)
    }

    show_attributes(object)
  }
})

#' @rdname showObject
#' @export
setMethod("brief", "ncdfDataset", function(object) {
  status <- if (object@has_error) "Error   :" else "Dataset:"
  cat(status, object@name, "\n")
  if (!object@has_error)
    cat("  Variables  :", paste(lapply(object@vars, ncdfCF::shard)),
        "\n  Dimensions :", paste(lapply(object@dims, ncdfCF::shard)), "\n")
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
setMethod("names", "ncdfDataset", function(x) as.vector(sapply(x@vars, name)))

#' @rdname ncdfDimnames
#' @export
setMethod("dimnames", "ncdfDataset", function(x) as.vector(sapply(x@dims, name)))

#' @rdname dimlength
#' @export
setMethod("dim", "ncdfDataset", function(x) {
  .dimension_sizes(x)
})

#' @title Get a variable object or a dimension object from a dataset
#'
#' @description
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
#' @examples
#' fn <- system.file("extdata", "ERA5land_Rwanda_20160101.nc", package = "ncdfCF")
#' ds <- open_ncdf(fn)
#' v1 <- names(ds)[1]
#' var <- ds[[v1]]
#' var
setMethod("[[", "ncdfDataset", function(x, i) {
  if (i %in% names(x)) x@vars[[i]]
  else if (i %in% dimnames(x)) x@dims[[i]]
  else NULL
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

    # Global information
    g <- RNetCDF::file.inq.nc(h)
    dataset@format <- g$format

    # Dimensions
    if (g$ndims) {
      dims <- lapply(0L:(g$ndims - 1L), function (d) .readDimension(dataset, h, d))
      names(dims) <- sapply(dims, name)
      dataset@dims <- dims
    }

    # Variables
    if (g$nvars) {
      vars <- lapply(0L:(g$nvars - 1L), function (v) .readVariable(dataset, h, v))
      vars <- vars[!sapply(vars,is.null)]
      names(vars) <- sapply(vars, name)
      dataset@vars <- vars
    }

    # Global attributes
    if (g$ngatts) {
      atts <- do.call(rbind, lapply(0L:(g$ngatts - 1L), function (a) as.data.frame(RNetCDF::att.inq.nc(h, "NC_GLOBAL", a))))
      atts$value <- sapply(0L:(g$ngatts - 1L), function (a) RNetCDF::att.get.nc(h, "NC_GLOBAL", a))
      dataset@attributes <- atts
    } else dataset@attributes <- data.frame()

    # Drop unused dimensions, usually just the bounds dimension(s)
    if (g$ndims && g$nvars)
      dataset@dims <- dims[unique(unlist((lapply(vars, function(v) sapply(v@dims, name)))))]

    if (!dataset@keep_open) close(dataset@resource)
  }, silent = TRUE)
  if (inherits(err, "try-error"))
    err
  else invisible(dataset)
}
