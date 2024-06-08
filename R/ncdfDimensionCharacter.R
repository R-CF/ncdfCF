#' @include ncdfDimension.R
NULL

#' Character dimension class
#'
#' This class describes character dimensions in an NetCDF resource. Character
#' dimensions are discrete by default.
#'
#' @slot values The values of the positions along the dimension.
setClass("ncdfDimensionCharacter",
  contains = "ncdfDimension",
  slots = c(
    values     = "character"
  )
)

#' @rdname showObject
#' @export
#' @importFrom methods show
setMethod("show", "ncdfDimensionCharacter", function (object) {
  longname <- attribute(object, "long_name")
  if (!length(longname) || longname == object@name) longname <- ""
  cat(paste0("Dimension: [", object@id, "] ", object@name))
  if (longname == "") cat("\n") else cat(paste0(" | ", longname, "\n"))

  ax <- if (object@axis == "") "(unknown)" else object@axis
  cat("Axis     :", ax, "\n")

  len <- length(object@values)
  unlim <- if (object@unlim) "(unlimited)" else ""
  vals <- if (len) paste(object@values, collapse = ", ") else "(no values)"
  if (nchar(vals) > globals$df_column_width)
    vals <- paste0(substr(vals, 1L, globals$df_column_width - 3L), "...")
  cat("Length   :", len, unlim, "\n")
  cat("Values   :", vals, "\n")

  show_attributes(object)
})

#' @rdname showObject
#' @export
setMethod("brief", "ncdfDimensionCharacter", function (object) {
  longname <- attribute(object, "long_name")
  if (!length(longname) || longname == object@name) longname <- ""
  unlim <- if (object@unlim) "U" else ""

  nv <- length(object@values)
  dims <- if (!nv) "(no values)"
          else dims <- sprintf("[%d: %s]", nv, paste0(object@values, collapse = ", "))

  data.frame(id = object@id, axis = object@axis, name = object@name, long_name = longname,
             dims = dims, unlim = unlim, bounds = "")
})

#' @rdname ncdfDimnames
#' @export
setMethod("dimnames", "ncdfDimensionCharacter", function (x) x@values)

#' @rdname indexOf
#' @export
setMethod("indexOf", c("character", "ncdfDimensionCharacter"), function (x, y, method = "constant") {
  match(x, y)
})
