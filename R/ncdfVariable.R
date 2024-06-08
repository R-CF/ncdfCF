#' @include ncdfDimension.R
NULL

#' The ncdfVariable class
#'
#' @slot dims `ncdfDimension`. Vector of dimensions of this variable.
setClass("ncdfVariable",
  contains = "ncdfObject",
  slots    = c(
    dims       = "list" # of ncdfDimension
  ))

#' @rdname showObject
#' @export
setMethod("show", "ncdfVariable", function (object) {
  props <- .varProperties(object)
  cat(paste0("Variable: [", object@id, "] ", object@name))
  if (props[1L] == "") cat("\n\n") else cat(paste0(" | ", props[1L], "\n\n"))

  dims <- do.call(rbind, lapply(object@dims, brief))
  dims <- lapply(dims, function(c) if (all(c == "")) NULL else c)
  dims <- as.data.frame(dims[lengths(dims) > 0L])
  if (nrow(dims) == 1L) cat("Dimension :\n") else cat("Dimensions:\n")
  if (nrow(dims) == 0L) cat("(none)\n") else print(dims, row.names = FALSE)

  show_attributes(object)
})

#' @rdname showObject
#' @export
setMethod("brief", "ncdfVariable", function (object) {
  props <- .varProperties(object)
  data.frame(id = object@id, name = object@name, long_name = props[1L],
             units = props[2L], dimensions = paste(dimnames(object), collapse = ", "))
})

#' @rdname showObject
#' @export
setMethod("shard", "ncdfVariable", function(object) {
  props <- .varProperties(object)

  s <- paste0("[", object@id, ": ", object@name)
  if (nchar(props[1L]) > 0) s <- paste0(s, " (", props[1L], ")")
  if (nchar(props[2L]) > 0) s <- paste0(s, " (", props[2L], ")")
  paste0(s, "]")
})

#' @name dimlength
#' @title Lengths of dimensions of the data set or variable
#'
#' @description
#' With this method the lengths of all dimensions of a dataset or a variable are
#' returned. Note that a dataset may have more dimensions than a variable from
#' the same dataset: other variables may use different dimensions.
#'
#' @param x An instance of `ncdfDataset` or `ncdfVariable`.
#'
#' @returns Named integer vector of dimension sizes of dimensions in `x`.
#' @examples
#' fn <- system.file("extdata",
#'   "pr_day_EC-Earth3-CC_ssp245_r1i1p1f1_gr_20240101-20241231_vncdfCF.nc",
#'   package = "ncdfCF")
#' ds <- open_ncdf(fn)
#'
#' # ncdfDataset
#' dim(ds)
#'
#' # ncdfVariable
#' dim(ds[["pr"]])
NULL

#' @rdname dimlength
#' @export
setMethod("dim", "ncdfVariable", function(x) {
  .dimension_sizes(x)
})

#' @rdname ncdfDimnames
#' @export
setMethod("dimnames", "ncdfVariable", function(x) sapply(x@dims, name))

#' Extract data for a variable
#'
#' Extract data from a `ncdfVariable` instance, optionally sub-setting the
#' dimensions to load only data of interest.
#'
#' If all the data of the variable in `x` is to be extracted, simply use `[]`
#' (unlike with regular arrays, this is required, otherwise the details of the
#' variable are printed on the console).
#'
#' The indices into the dimensions to be subset can be specified in a variety of
#' ways; in practice it should (resolve to) be a vector of integers. A range
#' (e.g. `100:200`), an explicit vector (`c(23, 46, 3, 45, 17`), a sequence
#' (`seq(from = 78, to = 100, by = 2`), all work. Note, however, that only a
#' single range is generated from the vector so these examples resolve to
#' `100:200`, `3:46`, and `78:100`, respectively. It is also possible to use a
#' custom function as an argument.
#'
#' This method works with "bare" indices into the dimensions of the array. If
#' you want to use domain values of the dimensions (e.g. longitude values or
#' timestamps) to extract part of the variable array, use the [ncdfCF::subset()]
#' method.
#'
#' @param x An `ncdfVariable` instance to extract the data of.
#' @param i,j,... Expressions, one for each dimension of `x`, that select a
#'   number of elements along each dimension. If any expressions are missing,
#'   the entire dimension is extracted. The values for the arguments may be an
#'   integer vector or a function that returns an integer vector. The range of
#'   the values in the vector will be used. See examples, below.
#' @param drop Logical, ignored. Dimensions are never dropped. Any degenerate
#'   dimensions are returned as such, with dimnames and appropriate attributes
#'   set.
#'
#' @returns An array with dimnames and other attributes set.
#' @export
#' @aliases bracket_select
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
#'
setMethod("[", "ncdfVariable", function(x, ..., drop = FALSE) {
  numdims <- length(x@dims)
  t <- vector("list", numdims)
  sc <- sys.call()
  if (numdims == 0L && length(sc) == 3L) { # Variable is a scalar value
    start <- 1L
    count <- NA_integer_
  } else {
    sc <- sc[-(1L:2L)] # drop [ and x
    if ((length(sc) == 1L) && (identical(sc[[1L]], quote(expr = )))) { # [] specified, read whole array
      start <- rep(1L, numdims)
      count <- rep(NA_integer_, numdims)
      dnames <- lapply(x@dims, dimnames)
      t <- lapply(x@dims, time)
    } else if (length(sc) != numdims) {
      stop("Indices specified are not equal to the number of dimensions of the variable")
    } else {
      start <- vector("integer", numdims)
      count <- vector("integer", numdims)
      dnames <- vector("list", numdims)
      for (d in seq_along(sc)) {
        if (identical(sc[[d]], quote(expr = ))) {
          start[d] <- 1L
          count[d] <- NA_integer_
          dnames[[d]] <- dimnames(x@dims[[d]])
          tm <- time(x@dims[[d]])         # Direct assignment of NULL in last
          if (!is.null(tm)) t[[d]] <- tm  # dimension drops t[[last]]
        } else {
          v <- eval(sc[[d]])
          ex <- range(v)
          start[d] <- ex[1L]
          count[d] <- ex[2L] - ex[1L] + 1L
          dnames[[d]] <- dimnames(x@dims[[d]])[seq(ex[1], ex[2])]
          if (!is.null(time(x@dims[[d]]))) {
            idx <- indexOf(ex[1L]:ex[2L], time(x@dims[[d]]), "constant")
            t[[d]] <- attr(idx, "CFtime")
          }
        }
      }
    }
  }
  names(t) <- dimnames(x)
  .read_data(x, start, count, dnames, t[lengths(t) > 0L])
})

#' Extract a subset of values from a variable
#'
#' This method extracts a subset of values from the array of the variable, with
#' the range along each dimension to extract expressed in values of the domain
#' of each dimension.
#'
#' The range of values along each dimension to be subset is expressed in values
#' of the domain of the dimension. Any dimensions for which no information is
#' provided in the `subset` argument are extracted in whole. Values can be
#' specified in a variety of ways that are specific to the nature of the
#' dimension. For numeric dimensions it should (resolve to) be a vector of real
#' values. A range (e.g. `100:200`), a long vector (`c(23, 46, 3, 45, 17`), a
#' sequence (`seq(from = 78, to = 100, by = 2`), all work. Note, however, that
#' only a single range is generated from the vector so these examples resolve to
#' `100:200`, `3:46`, and `78:100`, respectively. For time dimensions a vector
#' of character timestamps, `POSIXct` or `Date` values must be specified. As
#' with numeric values, only the two extreme values in the vector will be used.
#'
#' If the range of values for a dimension in `subset` extend the valid range
#' of the dimension in `x`, the extracted slab will start at the beginning for
#' smaller values and extend to the end for larger values. If the
#' values envelope the valid range the entire dimension will be extracted in
#' the result. If the range of `subset` values for any dimension are all either
#' smaller or larger than the valid range of the dimension in `x` then nothing
#' is extracted and `NULL` is returned.
#'
#' As an example, to extract values of a variable
#' for Australia for the year 2020, where the first dimension in `x` is
#' the longitude, the second dimension is the latitude, both in degrees, and the
#' third (and final) dimension is time, the values are extracted by
#' `subset(x, list(X = c(112, 154), Y = c(-9, -44), T = c("2020-01-01", "2021-01-01")))`.
#' You could take the longitude-latitude values from `sf::st_bbox()` or
#' `terra::ext()` if you have specific spatial geometries for whom you want to
#' extract data. Note that this works equally well for projected coordinate
#' reference systems - the key is that the specification in argument `subset`
#' uses the same domain of values as the respective dimensions in `x` use.
#'
#' @param x The `ncdfVariable` from which to extract a subset of values.
#' @param subset A list with the range to extract from each dimension of `x`. The
#' list should have elements for the dimensions to extract a subset from - if a
#' dimension is not present in the list the entire dimension will be extracted
#' from the array. List element names should be the axis designator `X`, `Y`,
#' `Z` or `T`, or the name of the dimension - dimensions without a recognized
#' axis and any additional dimensions beyond the four standard ones can only
#' be specified by name. Axis values and dimension names are case-sensitive and
#' can be specified in any order. If values for the range per dimension fall
#' outside of the extent of the dimension, the range is clipped to the extent of
#' the dimension.
#' @param rightmost.closed Single logical value to indicate if the upper
#' boundary of range in each the dimension should be included.
#' @param ... Ignored.
#'
#' @returns An array with dimnames and other attributes set, or `NULL`.
#'
#' @export
#' @examples
#' fn <- system.file("extdata",
#'   "pr_day_EC-Earth3-CC_ssp245_r1i1p1f1_gr_20240101-20241231_vncdfCF.nc",
#'   package = "ncdfCF")
#' ds <- open_ncdf(fn)
#' pr <- ds[["pr"]]
#'
#' # Precipitation data for March for a small area
#' x <- subset(pr, subset = list(X = c(9, 11),
#'                               Y = 42:45,
#'                               T = c("2024-03-01", "2024-04-01")))
#' dim(x)
#' dimnames(x)
setMethod("subset", "ncdfVariable", function(x, subset, rightmost.closed = FALSE, ...) {
  num_dims <- length(x@dims)
  if (!num_dims)
    stop("Cannot subset a scalar variable")

  dim_names <- dimnames(x)
  axes <- sapply(x@dims, axis)

  sub_names <- names(subset)
  bad <- sub_names[!(sub_names %in% c(dim_names, axes))]
  if (length(bad))
    stop("Argument `subset` contains elements not corresponding to a dimension:", paste(bad, collapse = ", "))

  t <- vector("list", num_dims)

  start <- rep(1L, num_dims)
  count <- rep(NA_integer_, num_dims)
  dvals <- list()
  for (d in 1:num_dims) {
    if(!is.null(rng <- subset[[dim_names[d]]]) ||
       !is.null(rng <- subset[[axes[d]]])) {
      idx <- indexOf(rng, x@dims[[d]], method = "linear")
      t[[d]] <- attr(idx, "CFtime")
      idx <- range(idx)
      if (all(idx == 0) || all(idx > length(x@dims[[d]]))) return(NULL)
      if (idx[1L] == 0) idx[1L] <- 1
      if (idx[2L] == .Machine$integer.max) idx[2L] <- length(x@dims[[d]])
      idx[1L] <- ceiling(idx[1L])
      if (!rightmost.closed)
        idx[2L] <- ceiling(idx[2L] - 1)  # exclude upper boundary
      idx <- as.integer(idx)
      start[d] <- idx[1L]
      count[d] <- idx[2L] - idx[1L] + 1L
      dvals[[d]] <- dimnames(x@dims[[d]])[seq(idx[1L], idx[2L])]
    } else {
      dvals[[d]] <- dimnames(x@dims[[d]])
      t[[d]] <- time(x@dims[[d]])
    }
  }

  names(t) <- dimnames(x)
  .read_data(x, start, count, dvals, t[lengths(t) > 0L])
})

#' Read the metadata of a variable
#'
#' @param dataset An `ncdfDataset` instance that contains this variable
#' @param h Handle to the netCDF resource
#' @param vid Variable ID value
#'
#' @returns `ncdfVariable` instance, or error
#' @noRd
.readVariable <- function(dataset, h, vid) {
  err <- try({
    # Names of dimensions and their bounds
    dims <- dataset@dims
    objnames <- c(sapply(seq_along(dims), function (i) dims[[i]]@name),
                  sapply(dims, attribute, "bounds"))

    vmeta <- RNetCDF::var.inq.nc(h, vid)
    if (vmeta$name %in% objnames) return (NULL)

    var <- methods::new("ncdfVariable", id = as.integer(vmeta$id), name = vmeta$name,
                        resource = dataset@resource)

    # Link to the dimensions of the variable
    if (vmeta$ndims > 0L)
      var@dims <- as.vector(sapply(vmeta$dimids, function(id) dims[sapply(dims, function(d) d@id == id)]))

    # Get the attributes
    if (vmeta$natts > 0L) {
      atts <- do.call(rbind, lapply(0L:(vmeta$natts - 1L), function (a) as.data.frame(RNetCDF::att.inq.nc(h, vmeta$name, a))))
      atts$value <- sapply(0L:(vmeta$natts - 1L), function (a) RNetCDF::att.get.nc(h, vmeta$name, a))
      var@attributes <- atts
    }
  }, silent = TRUE)

  if (inherits(err, "try-error")) err else var
}

#' Get the lengths of dimensions
#'
#' @param object A dataset or a variable
#'
#' @returns Named vector of dimension lengths
#' @noRd
.dimension_sizes <- function (object) {
  sapply(object@dims, length)
}

#' Return some properties of the variable as a character vector
#'
#' @param var Single `ncdfVariable` instance.
#' @returns A character vector with the long name and unit values of the variable.
#'
#' @noRd
.varProperties <- function(var) {
  atts <- var@attributes
  if (length(atts) == 0L) {
    unit <- ""
    longname <- ""
  } else {
    unit <- attribute(var, "units")
    if (!length(unit)) unit <- ""
    longname <- attribute(var, "long_name")
    if (!length(longname)) longname <- ""
  }
  if (longname == var@name) longname <- ""
  c(longname, unit)
}

#' Read the data for a variable
#'
#' @param x The variable
#' @param start,count RNetCDF start and count vectors
#' @param dim_names Dimnames to assign
#' @param time New CFtime for any time dimensions
#'
#' @returns The array with attributes set
#' @noRd
.read_data <- function(x, start, count, dim_names, time) {
  h <- open(x@resource)
  on.exit(close(x@resource))
  data <- RNetCDF::var.get.nc(h, x@name, start, count, collapse = FALSE, unpack = TRUE)

  # Apply dimension data and other attributes
  if (length(x@dims) && length(dim(data)) == length(dim_names)) { # dimensions may have been dropped automatically, e.g. NC_CHAR to character string
    dimnames(data) <- dim_names
    attr(data, "axis") <- sapply(x@dims, axis)
    if (length(time))
      attr(data, "time") <- time
  }

  data
}
