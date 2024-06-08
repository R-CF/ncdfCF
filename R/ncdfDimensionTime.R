#' @include ncdfDimension.R
NULL

#' Dimension object
#'
#' The 'dimension' is one of the key building blocks of a data set in an netCDF
#' resource. This class is specifically for dimensions that represent the "time"
#' dimension.
#'
#' @slot values The values of the positions along the dimension, as an instance
#' of S4 class 'CFtime'.
#' @importClassesFrom CFtime CFtime
setClass("ncdfDimensionTime",
  contains = "ncdfDimension",
  slots = c(
    values     = "CFtime"
  )
)

#' @rdname showObject
#' @export
#' @importFrom methods show
setMethod("show", "ncdfDimensionTime", function (object) {
  longname <- attribute(object, "long_name")
  if (!length(longname) || longname == object@name) longname <- ""
  cat(paste0("Dimension: [", object@id, "] ", object@name))
  if (longname == "") cat("\n") else cat(paste0(" | ", longname, "\n"))

  cat("Axis     : T\n")

  len <- length(object@values)
  unlim <- if (object@unlim) "(unlimited)" else ""
  rng <- paste0(range(object@values), collapse = " ... ")
  un <- paste0("(", CFtime::unit(object@values), ")")
  bndrng <- if (has_bounds(object))
    paste0(range(object@values, format = "", bounds = TRUE), collapse = " ... ")
  else "(not set)"
  cat("Length   :", len, unlim, "\n")
  cat("Range    :", rng, un, "\n")
  cat("Bounds   :", bndrng, "\n")

  show_attributes(object)
})

#' @rdname showObject
#' @export
setMethod("brief", "ncdfDimensionTime", function (object) {
  longname <- attribute(object, "long_name")
  if (!length(longname) || longname == object@name) longname <- ""
  unlim <- if (object@unlim) "U" else ""

  nv <- length(object@values)
  if (!nv) { # it happens...
    dims <- "(no values)"
    bnds <- ""
  } else {
    if (nv == 1L) dims <- paste0("[1: ", CFtime::format(object@values), "]")
    else {
      rng <- range(object@values, format = "", bounds = FALSE)
      dims <- sprintf("[%d: %s ... %s]", nv, rng[1L], rng[2L])
    }
    if (has_bounds(object)) {
      bndrng <- range(object@values, format = "", bounds = TRUE)
      bnds <- sprintf("[%s ... %s]", bndrng[1L], bndrng[2L])
    } else bnds <- ""
  }

  data.frame(id = object@id, axis = object@axis, name = object@name, long_name = longname,
             dims = dims, unlim = unlim, bounds = bnds)
})

#' @rdname ncdfDimnames
#' @export
setMethod("dimnames", "ncdfDimensionTime", function (x) CFtime::format(x@values))

#' Does the "time" dimension have 'bounds' set?
#'
#' @param x The `ncdfDimensionTime` object to query.
#'
#' @returns Logical to flag if bounds have been set or not.
#' @export
#'
#' @examples
#' fn <- system.file("extdata",
#'                   "pr_day_EC-Earth3-CC_ssp245_r1i1p1f1_gr_20240101-20241231_vncdfCF.nc",
#'                   package = "ncdfCF")
#' ds <- open_ncdf(fn)
#' time <- ds[["time"]]
#' has_bounds(time)
setMethod("has_bounds", "ncdfDimensionTime", function(x) {
  !is.null(CFtime::bounds(x@values))
})

#' @rdname indexOf
#' @importMethodsFrom CFtime indexOf
#' @export
setMethod("indexOf", c("ANY", "ncdfDimensionTime"), function (x, y, method = "constant") {
  indexOf(x, y@values, method)
})

#' Get the full time specification of the dimension
#'
#' This method returns the `CFtime` instance that manages all the conversions
#' and processing for this dimension. See package `CFtime` for more details.
#'
#' @param x A `ncdfDimensionTime` instance.
#'
#' @returns An instance of the `CFtime` class.
#' @export
#'
#' @examples
#' fn <- system.file("extdata",
#'                   "pr_day_EC-Earth3-CC_ssp245_r1i1p1f1_gr_20240101-20241231_vncdfCF.nc",
#'                   package = "ncdfCF")
#' ds <- open_ncdf(fn)
#' time(ds[["time"]])
setMethod("time", "ncdfDimensionTime", function(x) {
  x@values
})
