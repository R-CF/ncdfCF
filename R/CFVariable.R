#' CF data variable
#'
#' @description This class represents a CF data variable, the object that
#'   provides access to an array of data.
#'
#' @details The CF data variable instance provides access to the data array from
#' the netCDF resource, as well as all the details that have been associated
#' with the data variable, such as axis information, grid mapping parameters,
#' etc.
#'
#' @field group The [NCGroup] that this variable is defined in.
#' @field axes List of instances of classes descending from [CFAxis] that are
#'   the axes of the variable.
#'
#' @docType class
#'
#' @name CFVariable
#' @format An \code{\link{R6Class}} generator object.
NULL

#' @export
CFVariable <- R6::R6Class("CFVariable",
  inherit = CFObject,
  private = list(
    .varProperties = function() {
      unit <- self$attribute("units")
      if (!length(unit)) unit <- ""
      longname <- self$attribute("long_name")
      if (!length(longname)) longname <- ""
      if (longname == self$name) longname <- ""
      list(longname = longname, unit = unit)
    }
  ),
  public = list(
    group      = NULL,
    axes       = list(),

    #' @noRd
    initialize = function(grp, nc_var, axes) {
      super$initialize(nc_var)
      self$group <- grp
      self$axes <- axes

      nc_var$CF <- self
    },

    #' @description Summary of the data variable
    #'
    #' Prints a summary of the data variable to the console.
    print = function() {
      cat("<Variable>", self$name, "\n")
      if (self$group$name != "/")
        cat("Group    :", self$group$name, "\n")

      longname <- self$attribute("long_name")
      if (length(longname) && longname != self$name)
        cat("Long name:", longname, "\n")

      cat("\nAxes:\n")
      axes <- do.call(rbind, lapply(self$axes, function(a) a$brief()))
      axes <- lapply(axes, function(c) if (all(c == "")) NULL else c)
      axes <- as.data.frame(axes[lengths(axes) > 0L])
      print(.slim.data.frame(axes, 50L), right = FALSE, row.names = FALSE)

      self$print_attributes()
    },

    #' @description Some details of the data variable
    #'
    #' @returns A 1-row `data.frame` with some details of the data variable.
    brief = function() {
      props <- private$.varProperties()
      ax <- sapply(self$axes, function(x) x$shard())
      data.frame(group = self$group$fullname, name = self$name,
                 long_name = props$longname, units = props$unit,
                 data_type = self$NCvar$vtype, axes = paste(ax, collapse = ", "))
    },

    #' @description Extract data from the variable
    #'
    #' @details
    #' The `subset()` method extracts a subset of values from the array of the
    #' variable, with the range along each axis to extract expressed in values
    #' of the domain of each axis.
    #'
    #' The range of values along each axis to be subset is expressed in values
    #' of the domain of the axis. Any axes for which no information is
    #' provided in the `subset` argument are extracted in whole. Values can be
    #' specified in a variety of ways that are specific to the nature of the
    #' axis. For numeric axes it should (resolve to) be a vector of real
    #' values. A range (e.g. `100:200`), a long vector (`c(23, 46, 3, 45, 17`),
    #' a sequence (`seq(from = 78, to = 100, by = 2`), all work. Note, however,
    #' that only a single range is generated from the vector so these examples
    #' resolve to `100:200`, `3:46`, and `78:100`, respectively. For time axes a
    #' vector of character timestamps, `POSIXct` or `Date` values must be
    #' specified. As with numeric values, only the two extreme values in the
    #' vector will be used.
    #'
    #' If the range of values for an axis in argument `subset` extend the valid
    #' range of the axis in `x`, the extracted slab will start at the beginning
    #' for smaller values and extend to the end for larger values. If the
    #' values envelope the valid range the entire axis will be extracted in
    #' the result. If the range of `subset` values for any axis are all either
    #' smaller or larger than the valid range of the axis in `x` then nothing
    #' is extracted and `NULL` is returned.
    #'
    #' As an example, to extract values of a variable for Australia for the year
    #' 2020, where the first axis in `x` is the longitude, the second
    #' axis is the latitude, both in degrees, and the
    #' third (and final) axis is time, the values are extracted by
    #' `x$subset(list(X = c(112, 154), Y = c(-9, -44), T = c("2020-01-01", "2021-01-01")))`.
    #' You could take the longitude-latitude values from `sf::st_bbox()` or
    #' `terra::ext()` if you have specific spatial geometries for whom you want to
    #' extract data. Note that this works equally well for projected coordinate
    #' reference systems - the key is that the specification in argument `subset`
    #' uses the same domain of values as the respective axes in `x` use.
    #'
    #' @param subset A list with the range to extract from each axis. The
    #' list should have elements for the axes to extract a subset from - if an
    #' axis is not present in the list the entire axis will be extracted
    #' from the array. List element names should be the axis designator `X`, `Y`,
    #' `Z` or `T`, or the name of the axis - axes without an axis designator
    #' and any additional axes beyond the four standard ones can only
    #' be specified by name. Axis designators and names are case-sensitive and
    #' can be specified in any order. If values for the range per axis fall
    #' outside of the extent of the axis, the range is clipped to the extent of
    #' the axis.
    #' @param rightmost.closed Single logical value to indicate if the upper
    #' boundary of range in each axis should be included.
    #'
    #' @returns An array with as many dimensions as the data variable has axes.
    #' Attributes will be set on the array.
    #' @aliases CFVariable$subset
    #' @seealso [[,CFVariable-method]
    #'
    #' @examples
    #' fn <- system.file("extdata",
    #'   "pr_day_EC-Earth3-CC_ssp245_r1i1p1f1_gr_20240101-20241231_vncdfCF.nc",
    #'   package = "ncdfCF")
    #' ds <- open_ncdf(fn)
    #' pr <- ds[["pr"]]
    #'
    #' # Precipitation data for March for a small area
    #' x <- pr$subset(subset = list(X = c(9, 11),
    #'                              Y = 42:45,
    #'                              T = c("2024-03-01", "2024-04-01")))
    #' dim(x)
    #' dimnames(x)
    subset = function(subset, rightmost.closed = FALSE) {
      num_axes <- length(self$axes)
      if (!num_axes)
        stop("Cannot subset a scalar variable")

      axis_names <- names(self$axes)
      orientations <- sapply(self$axes, function(a) a$orientation)

      sub_names <- names(subset)
      bad <- sub_names[!(sub_names %in% c(axis_names, orientations))]
      if (length(bad))
        stop("Argument `subset` contains elements not corresponding to a dimension:", paste(bad, collapse = ", "))

      t <- vector("list", num_axes)
      names(t) <- axis_names

      start <- rep(1L, num_axes)
      count <- rep(NA_integer_, num_axes)
      dvals <- list()
      for (ax in 1:num_axes) {
        axis <- self$axes[[ax]]
        if(!is.null(rng <- subset[[ axis_names[ax] ]]) ||
           !is.null(rng <- subset[[ orientations[ax] ]])) {
          axl <- axis$length
          idx <- axis$indexOf(rng, method = "linear")
          t[[ax]] <- attr(idx, "CFtime")

          idx <- range(idx)
          if (all(idx == 0) || all(idx > axl)) return(NULL)
          if (idx[1L] == 0) idx[1L] <- 1
          if (idx[2L] == .Machine$integer.max) idx[2L] <- axl
          idx[1L] <- ceiling(idx[1L])
          if (!rightmost.closed)
            idx[2L] <- ceiling(idx[2L] - 1)  # exclude upper boundary
          idx <- as.integer(idx)
          start[ax] <- idx[1L]
          count[ax] <- idx[2L] - idx[1L] + 1L
          dvals[[ax]] <- dimnames(axis)[seq(idx[1L], idx[2L])]
        } else {
          dvals[[ax]] <- dimnames(axis)
          t[[ax]] <- axis$time()
        }
      }

      .read_data(self, start, count, dvals, t[lengths(t) > 0L])
    }
  )
)

# Public S3 methods ------------------------------------------------------------

#' @export
dim.CFVariable <- function(x) {
  sapply(x$axes, function(z) z$NCdim$length)
}

#' @export
dimnames.CFVariable <- function(x) {
  ax <- x$axes
  if (length(ax)) names(ax)
  else NULL
}

#' Extract data for a variable
#'
#' Extract data from a `CFVariable` instance, optionally sub-setting the
#' axes to load only data of interest.
#'
#' If all the data of the variable in `x` is to be extracted, simply use `[]`
#' (unlike with regular arrays, this is required, otherwise the details of the
#' variable are printed on the console).
#'
#' The indices into the axes to be subset can be specified in a variety of
#' ways; in practice it should (resolve to) be a vector of integers. A range
#' (e.g. `100:200`), an explicit vector (`c(23, 46, 3, 45, 17`), a sequence
#' (`seq(from = 78, to = 100, by = 2`), all work. Note, however, that only a
#' single range is generated from the vector so these examples resolve to
#' `100:200`, `3:46`, and `78:100`, respectively. It is also possible to use a
#' custom function as an argument.
#'
#' This method works with "bare" indices into the axes of the array. If
#' you want to use domain values of the axes (e.g. longitude values or
#' timestamps) to extract part of the variable array, use the `CFVariable$subset()`
#' method.
#'
#' @param x An `CFVariable` instance to extract the data of.
#' @param i,j,... Expressions, one for each axis of `x`, that select a
#'   number of elements along each axis. If any expressions are missing,
#'   the entire axis is extracted. The values for the arguments may be an
#'   integer vector or a function that returns an integer vector. The range of
#'   the values in the vector will be used. See examples, below.
#' @param drop Logical, ignored. Axes are never dropped. Any degenerate
#'   dimensions of the array are returned as such, with dimnames and appropriate
#'   attributes set.
#'
#' @returns An array with dimnames and other attributes set.
#' @export
#' @aliases [,CFVariable-method
#' @docType methods
#' @examples
#' fn <- system.file("extdata",
#'   "pr_day_EC-Earth3-CC_ssp245_r1i1p1f1_gr_20240101-20241231_vncdfCF.nc",
#'   package = "ncdfCF")
#' ds <- open_ncdf(fn)
#' pr <- ds[["pr"]]
#'
#' # How are the dimensions organized?
#' dimnames(pr)
#'
#' # Precipitation data for March for a single location
#' x <- pr[5, 12, 61:91]
#' str(x)
#'
#' # Summer precipitation over the full spatial extent
#' summer <- pr[, , 173:263]
#' str(summer)
"[.CFVariable" <- function(x, i, j, ..., drop = FALSE) {
  numaxes <- length(x$axes)
  t <- vector("list", numaxes)
  names(t) <- dimnames(x)

  sc <- sys.call()
  if (numaxes == 0L && length(sc) == 3L) { # Variable is a scalar value
    start <- 1L
    count <- NA_integer_
  } else {
    sc <- sc[-(1L:2L)] # drop [ and x
    if ((length(sc) == 1L) && (identical(sc[[1L]], quote(expr = )))) {
      # [] specified, read whole array
      start <- rep(1L, numaxes)
      count <- rep(NA_integer_, numaxes)
      dnames <- lapply(x$axes, dimnames)
      t <- lapply(x$axes, function(z) z$time())
    } else if (length(sc) != numaxes) {
      stop("Indices specified are not equal to the number of axes of the variable")
    } else {
      start <- vector("integer", numaxes)
      count <- vector("integer", numaxes)
      dnames <- vector("list", numaxes)
      for (d in seq_along(sc)) {
        ax <- x$axes[[d]]
        if (identical(sc[[d]], quote(expr = ))) {
          # Axis not subsetted, read the whole thing
          start[d] <- 1L
          count[d] <- NA_integer_
          dnames[[d]] <- dimnames(ax)
          tm <- ax$time()                 # Direct assignment of NULL in last
          if (!is.null(tm)) t[[d]] <- tm  # dimension drops t[[last]]
        } else {
          # Subset the axis
          v <- eval(sc[[d]])
          ex <- range(v)
          start[d] <- ex[1L]
          count[d] <- ex[2L] - ex[1L] + 1L
          dnames[[d]] <- dimnames(ax)[seq(ex[1], ex[2])]
          if (!is.null(ax$time())) {
            idx <- indexOf(ex[1L]:ex[2L], ax$time(), "constant")
            t[[d]] <- attr(idx, "CFtime")
          }
        }
      }
    }
  }
  .read_data(x, start, count, dnames, t[lengths(t) > 0L])
}

# Non-exported functions -------------------------------------------------------

#' Read the data for the variable
#'
#' @param x CFVariable to read data for
#' @param start,count RNetCDF start and count vectors
#' @param dim_names Dimnames to assign
#' @param time New CFtime for any time dimensions
#'
#' @returns The array with attributes set
#' @noRd
.read_data <- function(x, start, count, dim_names, time) {
  h <- x$group$handle
  data <- RNetCDF::var.get.nc(h, x$name, start, count, collapse = FALSE, unpack = TRUE, fitnum = TRUE)

  # Apply dimension data and other attributes
  if (length(x$axes) && length(dim(data)) == length(dim_names)) { # dimensions may have been dropped automatically, e.g. NC_CHAR to character string
    dimnames(data) <- dim_names
    attr(data, "axis") <- sapply(x$axes, function(x) x$orientation)
    if (length(time))
      attr(data, "time") <- time
  }

  data
}
