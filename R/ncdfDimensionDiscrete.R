#' @include ncdfDimension.R
NULL

#' Discrete dimension class
#'
#' This class describes discrete dimensions in a netCDF resource. A discrete
#' dimension has no values. The indices along the dimension run from 1 to the
#' length of the dimension.
#'
setClass("ncdfDimensionDiscrete",
  contains = "ncdfDimension"
)

#' @rdname showObject
#' @export
#' @importFrom methods show
setMethod("show", "ncdfDimensionDiscrete", function (object) {
  longname <- attribute(object, "long_name")
  if (!length(longname) || longname == object@name) longname <- ""
  cat(paste0("Dimension: [", object@id, "] ", object@name))
  if (longname == "") cat("\n") else cat(paste0(" | ", longname, "\n"))

  ax <- if (object@axis == "") "(unknown)" else object@axis
  cat("Axis     :", ax, "\n")

  unlim <- if (object@unlim) "(unlimited)" else ""
  cat("Length   :", object@length, unlim, "\n")

  show_attributes(object)
})

#' @rdname showObject
#' @export
setMethod("brief", "ncdfDimensionDiscrete", function (object) {
  longname <- attribute(object, "long_name")
  if (!length(longname) || longname == object@name) longname <- ""
  unlim <- if (object@unlim) "U" else ""
  dims <- if (object@length == 1L) "[1]"
          else paste0("[1 ... ", object@length, "]")
  data.frame(id = object@id, axis = object@axis, name = object@name, long_name = longname,
             length = object@length, values = dims, unlim = unlim, bounds = "")
})

#' @rdname str
#' @export
setMethod("str", "ncdfDimensionDiscrete", function(object, ...) {
  cat(object@name, ": Formal class 'ncdfDimensionDiscrete' [package \"ncdfCF\"] with 8 slots\n")
  str_dimension(object)
  str_attributes(object)
})

#' @rdname dimnames
#' @export
setMethod("dimnames", "ncdfDimensionDiscrete", function(x) seq(x@length))

#' @rdname indexOf
#' @export
setMethod("indexOf", c("numeric", "ncdfDimensionDiscrete"), function (x, y, method = "constant") {
  x[x < 1] <- 0L
  x[x > length(y)] <- .Machine$integer.max
  as.integer(x)
})
