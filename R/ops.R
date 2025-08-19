#' Operations on CFArray objects
#'
#' Basic arithmetic, mathematical and logical operations can be applied on the
#' data of [CFArray] objects having a suitable data type, specifically the base
#' R functions from the Ops and Math groups of the S3 [groupGeneric] functions.
#'
#' The functions always return a new CFArray object created in a new group.
#' Functions can thus be concatenated to create more complex expressions. The
#' data type of the new object is determined by the base R function; its name is
#' concatenated from the names in the argument object(s).
#'
#' For the Ops functions with two arguments, if both arguments are a CFArray
#' object they have to be compatible: same shape, axis coordinate values and
#' coordinate reference system. The resulting CFArray object will use the same
#' axes as the CFArray object(s) used as argument.
#'
#' The attributes of the resulting CFArray object should be updated to reflect
#' its contents, in particular the "name", "long_name", "standard_name" and
#' "units" attributes. Attributes are not copied over from the CFArray objects
#' in the arguments.
#'
#' @param e1,e2 CFArray objects, or a single numeric value.
#'
#' @return A new CFArray object. The object will have the same coordinate space
#'   as the CFArray object used as argument. Arguments are not copied and the
#'   new object will only have the "actual_range" attribute set.
#'
#'   Results that are logical (see the examples) are stored using the `NC_SHORT`
#'   data type because netCDF does not have a native logical data type.
#' @rdname arrayOps
#' @export
#' @examples
#' fn <- system.file("extdata", "ERA5land_Rwanda_20160101.nc", package = "ncdfCF")
#' ds <- open_ncdf(fn)
#'
#' # Temperature data in K
#' t2m <- ds[["t2m"]]$data()
#'
#' # Convert to degrees_Celsius
#' t2mC <- t2m - 273.15
#' t2mC$name <- "t2m_Celsius"
#' t2mC$set_attribute("units", "NC_CHAR", "degrees_Celsius")
#' t2mC
#'
#' hot <- t2mC > 20
#' hot$name <- "t2m_Celsius_over_20"
#' hot$set_attribute("long_name", "NC_CHAR", "Flag to indicate where temperature is 20C or hotter")
#' hot$set_attribute("units", "NC_CHAR", "1")
#' hot
#'
Ops.CFArray <- function(e1, e2) {
  fun <- match.fun(.Generic)

  if (inherits(e1, "CFArray")) {
    d1 <- e1$raw()
    name <- e1$name
    grp <- makeGroup()
    crs <- e1$crs
    axes <- e1$axes
  } else {
    d1 <- e1
    name <- paste0("x", e1)
  }
  if (missing(e2)) {
    # Unary operator
    res <- fun(d1)
    name <- paste("inv", name, sep = "_")
  } else {
    if (inherits(e2, "CFArray")) {
      if (inherits(e1, "CFArray")) {
        if (!e1$is_coincident(e2))
          stop("CFArray objects are not coincident.", call. = FALSE)
      } else {
        grp <- makeGroup()
        crs <- e2$crs
        axes <- e2$axes
      }
      d2 <- e2$raw()
      name <- paste(name, e2$name, sep = "_")
    } else {
      d2 <- e2
      name <- paste(name, e2, sep = "_")
    }
    res <- fun(d1, d2)
  }

  name <- gsub("\\.|,", "_", name)

  datatype <- switch(storage.mode(res),
                     "character" = "NC_STRING",
                     "double" = "NC_DOUBLE",
                     "integer" = "NC_INT",
                     "logical" = "NC_SHORT",
                     stop("Must add type", storage.mode(res)))
  CFArray$new(name, grp, res, datatype, axes, crs)
}

#' Mathematical operations on CFArray objects.
#'
#' @param x A CFArray object.
#' @param ... Additional arguments passed on to the math functions.
#'
#' @rdname arrayOps
#' @export
Math.CFArray <- function(x, ...) {
  fun <- match.fun(.Generic)

  res <- fun(x$raw(), ...)
  datatype <- switch(storage.mode(res),
                     "character" = "NC_STRING",
                     "double" = "NC_DOUBLE",
                     "integer" = "NC_INT",
                     "logical" = "NC_SHORT",
                     stop("Must add type", storage.mode(res)))
  CFArray$new(paste(.Generic, x$name, sep = "_"), makeGroup(), res, datatype, x$axes, x$crs)
}
