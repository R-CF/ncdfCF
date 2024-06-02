#' @include ncdfDimension.R
NULL

#' Dimension object
#'
#' The 'dimension' is one of the key building blocks of a data set in an netCDF
#' resource.
#'
#' @slot var_id The ID of the dimension variable if read from file.
#' @slot var_type The type of the dimension variable if read from file.
#' @slot length The number of elements in the dimension.
#' @slot unlim Flag to indicate whether this dimension is unlimited, e.g. can be
#' extended beyond the current 'length'.
#' @slot values The values of the positions along the dimension. For ordinary
#' dimensions these are numeric values but there are other possibilities. For
#' instance, any "time" dimension will have an instance of S4 class 'CFtime'.
#' @slot bounds The bounds of the dimension values, if any.
setClass("ncdfDimensionNumeric",
  contains = "ncdfDimension",
  slots = c(
    values     = "numeric",
    bounds     = "array"
  )
)

#' @rdname showObject
#' @export
#' @importFrom methods show
setMethod("show", "ncdfDimensionNumeric", function (object) {
  longname <- attribute(object, "long_name")
  if (!length(longname) || longname == object@name) longname <- ""
  cat(paste0("Dimension: [", object@id, "] ", object@name))
  if (longname == "") cat("\n") else cat(paste0(" | ", longname, "\n"))

  ax <- if (object@axis == "") "(unknown)" else object@axis
  cat("Axis     :", ax, "\n")

  len <- length(object@values)
  unlim <- if (object@unlim) "(unlimited)" else ""
  rng <- if (length(object@values))
           paste0(range(object@values), collapse = " ... ")
         else "(no values)"
  units <- attribute(object, "units")
  if (!length(units)) units <- ""
  if (length(object@bounds)) {
    vals <- trimws(formatC(c(object@bounds[1L, 1L], object@bounds[2L, len]), digits = 8L))
    bndrng <- paste0(vals, collapse = " ... ")
  } else bndrng <- "(not set)"
  cat("Length   :", len, unlim, "\n")
  cat("Range    :", rng, units, "\n")
  cat("Bounds   :", bndrng, "\n")

  show_attributes(object)
})

#' @rdname showObject
#' @export
setMethod("brief", "ncdfDimensionNumeric", function (object) {
  longname <- attribute(object, "long_name")
  if (!length(longname) || longname == object@name) longname <- ""
  unlim <- if (object@unlim) "U" else ""

  nv <- length(object@values)
  if (!nv) { # it happens...
    dims <- "(no values)"
    bnds <- ""
  } else {
    if (nv == 1L)
      dims <- sprintf("[1: %s]", gsub(" ", "", formatC(object@values[1L], digits = 8L)))
    else {
      vals <- trimws(formatC(c(object@values[1], object@values[nv]), digits = 8L))
      dims <- sprintf("[%d: %s ... %s]", nv, vals[1L], vals[2L])
    }
    if (length(object@bounds)) {
      vals <- trimws(formatC(c(object@bounds[1L, 1L], object@bounds[2L, nv]), digits = 8L))
      bnds <- sprintf("[%s ... %s]", vals[1L], vals[2L])
    } else bnds <- ""
  }

  data.frame(id = object@id, axis = object@axis, name = object@name, long_name = longname,
             dims = dims, unlim = unlim, bounds = bnds)
})

#' @rdname ncdfDimnames
#' @export
setMethod("dimnames", "ncdfDimensionNumeric", function (x) x@values)

#' Does the dimension have 'bounds' set?
#'
#' @param x The `ncdfDimension` object to query.
#'
#' @returns Logical to flag if bounds have been set or not.
#' @export
#'
#' @examples
#' fn <- system.file("extdata",
#'                   "pr_day_EC-Earth3-CC_ssp245_r1i1p1f1_gr_20240101-20241231_vncdfCF.nc",
#'                   package = "ncdfCF")
#' ds <- ncdfDataset(fn)
#' lon <- ds[["lon"]]
#' has_bounds(lon)
setMethod("has_bounds", "ncdfDimensionNumeric", function(x) {
  length(x@bounds) > 0L
})

#' Find indices in the dimension domain
#'
#' Given a vector of numerical values `x`, find their indices in the values of
#' dimension `y`. With `method = "constant"` this returns the index of the value
#' lower than the supplied values in `x`. With `method = "linear"` the return
#' value includes any fractional part.
#'
#' If bounds are set on the dimension, the indices are taken from those bounds.
#' Returned indices may fall in between bounds if the latter are not contiguous,
#' with the exception of the extreme values in `x`.
#'
#' @param x Vector of numeric values to find dimension indices for.
#' @param y An `ncdfDimensionNumeric` instance.
#' @param method Single value of "constant" or "linear".
#'
#' @returns Numeric vector of the same length as `x`. If `method = "constant"`,
#'   return the index value for each match. If `method = "linear"`, return the
#'   index value with any fractional value. Values of `x` outside of the range
#'   of the values in `y` are returned as `0` and `.Machine$integer.max`,
#'   respectively.
#' @export
#' @examples
#' fn <- system.file("extdata",
#'                   "pr_day_EC-Earth3-CC_ssp245_r1i1p1f1_gr_20240101-20241231_vncdfCF.nc",
#'                   package = "ncdfCF")
#' ds <- ncdfDataset(fn)
#' lon <- ds[["lon"]]
#' indexOf(42:45, lon)
#' indexOf(42:45, lon, "linear")
setMethod("indexOf", c("numeric", "ncdfDimensionNumeric"), function (x, y, method = "linear") {
  if (length(y@bounds)) vals <- c(y@bounds[1L, 1L], y@bounds[2L, ])
  else vals <- y@values
  stats::approx(vals, 1L:length(vals), x, method = method, yleft = 0L, yright = .Machine$integer.max)$y
})
