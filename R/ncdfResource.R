#' Low-level access to the RNetCDF package
#'
#' This class, of which there will be 1 instance in the package, shared by all
#' `ncdfObject`s, provides low-level access to the `RNetCDF` package, mainly to
#' provide a valid handle with which to access the underlying netCDF resource.
#'
#' @slot uri character.
#' @slot handle ANY.
setClass("ncdfResource",
  slots = c(
    uri     = "character",
    handle  = "ANY"
  ))

#' Create an ncdfResource instance
#'
#' This function will create an `ncdfResource` instance with the underlying netCDF
#' resource opened.
#'
#' @param uri The URI of the resource
#'
#' @returns An `ncdfResource` instance with the underlying resource opened, or an
#' error message.
#' @export
ncdfResource <- function(uri) {
  err <- try(h <- RNetCDF::open.nc(uri), silent = TRUE)
  # FIXME: Need better error management
  if (inherits(err, "try-error")) err
  else methods::new("ncdfResource", uri = uri, handle = h)
}

#' Open or close a NetCDF resource
#'
#' These methods open or close a NetCDF resource. It is typically not necessary
#' to call these functions directly, the `ncdfCF` objects call these functions
#' at appropriate times.
#'
#' @param con The `ncdfResource` instance of the `ncdfDataset` to be opened or
#' closed.
#' @param ... Ignored.
#'
#' @name ncdfOpenClose
#' @returns The `open()` method returns a handle to the NetCDF resource, or an
#' error. Method `close()` returns nothing.
NULL

#' @rdname ncdfOpenClose
#' @export
setMethod("open", "ncdfResource", function(con, ...) {
  if (is.na(con@handle)) con@handle <- RNetCDF::open.nc(con@uri)
  else {
    err <- try(RNetCDF::file.inq.nc(con@handle), silent = TRUE)
    if (inherits(err, "try-error")) con@handle <- RNetCDF::open.nc(con@uri)
  }
  con@handle
})

#' @rdname ncdfOpenClose
#' @export
setMethod("close", "ncdfResource", function(con, ...) {
  RNetCDF::close.nc(con@handle)
  con@handle <- NA
})
