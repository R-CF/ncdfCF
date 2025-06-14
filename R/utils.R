# NetCDF data types
# Additionally, UDTs are allowable as data type
netcdf_data_types <- c("NC_BYTE", "NC_UBYTE", "NC_CHAR", "NC_SHORT",
                       "NC_USHORT", "NC_INT", "NC_UINT", "NC_INT64",
                       "NC_UINT64", "NC_FLOAT", "NC_DOUBLE", "NC_STRING")

# Standard names used for parametric Z axes
Z_parametric_standard_names <- c("atmosphere_ln_pressure_coordinate",
  "atmosphere_sigma_coordinate", "atmosphere_hybrid_sigma_pressure_coordinate",
  "atmosphere_hybrid_height_coordinate", "atmosphere_sleve_coordinate",
  "ocean_sigma_coordinate", "ocean_s_coordinate", "ocean_s_coordinate_g1",
  "ocean_s_coordinate_g2", "ocean_sigma_z_coordinate", "ocean_double_sigma_coordinate")

# This function is a bare-bones implementation of `apply(X, MARGIN, tapply, INDEX, FUN, ...)`,
# i.e. apply a factor over a dimension of an array. There are several restrictions
# compared to the base::apply/tapply pair (but note that function arguments are
# named differently): (1) X must be a vector, matrix or array (not a data.frame);
# (2) MARGIN must have all dimensions except the one to operate on; (3) INDEX is
# therefore a single factor; (4) MARGIN must be numeric (not dimnames); (5) FUN
# must be a function (not a formula); and (6) FUN must return a vector of numeric
# values, with each call generating the same number of values. In the interest of
# speed, these restrictions are not tested. Furthermore, no dimnames
# are set and the result is always simplified to a vector, matrix or array.
# On the up side, this function always returns a list, with as many elements as
# FUN returns values.
#
# The basic version without a factor is about 10% faster than the base::apply()
# function. When used with a factor, this code is twice as fast as apply/tapply.
#
# Arguments:
# X    - Vector, matrix or array
# oper - The ordinal number of the axis to operate on
# fac  - The factor over whose levels to apply FUN, or NULL if no levels
# FUN  - The function to call with the data
# ...  - Additional arguments passed on to FUN
.process.data <- function (X, oper, fac = NULL, FUN, ...) {
  # FUN must return a vector of atomic types. A list is explicitly not going to
  # work. Every call over the fac levels must return the same number of values.
  FUN <- match.fun(FUN)

  # Number of distinct groups in the data, if fac is supplied
  nl <- if (is.factor(fac)) nlevels(fac) else 1L

  d <- dim(X)
  dl <- length(d)
  if (dl < 2L)                               # Vector, maybe a time profile
    res <- if (nl < 2L) list(FUN(X, ...))
           else lapply(split(X, fac), FUN, ...)
  else {                                     # Matrix or array
    d2 <- prod(d[-oper])

    newX <- if (oper == 1L) X
            else aperm(X, c(oper, seq_len(dl)[-oper]))
    dim(newX) <- c(d[oper], d2)

    res <- vector("list", d2)
    if (nl < 2L)
      for (i in 1L:d2)
        res[[i]] <- FUN(newX[, i], ...)
    else {
      for (i in 1L:d2)
        res[[i]] <- lapply(split(newX[, i], fac), FUN, ...)
      res <- unlist(res, recursive = FALSE, use.names = FALSE)
    }
  }

  dimres <- length(res[[1L]])

  # Set dimensions. If FUN returns multiple values, FUN values are in the first
  # dimension.
  res <- unlist(res, recursive = FALSE, use.names = FALSE)
  dims <- c(if(dimres > 1L) dimres, if (nl > 1L) nl, if (dl > 1L) d[-oper])
  if (length(dims) > 1L)
    dim(res) <- dims

  if (dimres == 1L)
    # Always return a list
    list(res)
  else if (dl < 2L && nl < 2L)
    # Vector input, no factor, multiple FUN values
    as.list(res)
  else
    # Separate FUN values into list elements
    asplit(res, 1L)
}

#' Make a data.frame slimmer by shortening long strings. List elements are
#' pasted together.
#' @param df A data.frame
#' @param width Maximum width of character entries. If entries are longer than
#' width - 3, they are truncated and then '...' added.
#' @return data.frame with slim columns
#' @noRd
.slim.data.frame <- function(df, width = 50L) {
  maxw <- width - 3L
  out <- as.data.frame(lapply(df, function(c) {
    if (is.list(c)) c <- sapply(c, paste0, collapse = ", ")
    if (!is.character(c)) c
    else
      sapply(c, function(e)
        if (nchar(e) > width) paste0(substr(e, 1, maxw), "...") else e
      )
  }))
  names(out) <- names(df)
  out
}

#' Flags if the supplied name is a valid name according to the CF Metadata
#' Conventions.
#'
#' @param nm A vector of names of variables, groups or attributes to test. Group
#' names should be plain, i.e. no preceding path.
#' @return `TRUE` if all `nm` are valid, `FALSE` otherwise.
#' @noRd
.is_valid_name <- function(nm) {
  all(grepl("^[a-zA-Z][a-zA-Z0-9_]{0,254}$", nm))
}

#' Convert regular character strings to valid CF names. Non-permitted characters
#' are converted to underscaores "_" and leading underscores are deleted. If the
#' first character in the resulting string is a number `0-9`, an `x` is placed
#' immediately before it. Finally, the string is truncated to a maximum of 255
#' characters
#'
#' @param nm A vector of names to test.
#' @return A vector of the same size as argument `nm` with valid names.
#' @noRd
.make_valid_name <- function(nm) {
  nm <- gsub("[^a-zA-Z0-9_]+", "_", nm)
  nm <- trimws(nm, "left", "_")
  nm <- sub("^([0-9])", "x\\1", nm)
  substr(nm, 1, 255)
}

#' Round values `x` with .5 being rounded up.
#' Adapted from https://stackoverflow.com/a/12688836/3304426
#' @noRd
.round <- function(x) {
  posneg <- sign(x)
  trunc(abs(x) + 0.5 + CF$eps) * posneg
}

#' Test if vectors `x` and `y` have near-identical values.
#' @noRd
.near <- function(x, y) {
  abs(x - y) < CF$eps
}

#' Get the range on the matches. Works on numeric and character data. Returns
#' NULL if there is no match
#' @noRd
.range_match <- function(rng, match) {
  res <- range(match(rng, match, nomatch = 0L), na.rm = TRUE)
  if (all(res == 0L)) NULL
  else if (res[1L] == 0L) c(res[2L], res[2L])
  else if (res[2L] == 0L) c(res[1L], res[1L])
  else res
}

.cache_dir <- function() {
  if (as.integer(R.version$major) >= 4)
    tools::R_user_dir("ncdfCF", "cache")
  else {
    if (nzchar(p <- Sys.getenv("R_USER_CACHE_DIR"))) p
    else if (nzchar(p <- Sys.getenv("XDG_CACHE_HOME"))) p
    else if (.Platform$OS.type == "windows") file.path(Sys.getenv("LOCALAPPDATA"), "R", "cache")
    else if (Sys.info()["sysname"] == "Darwin") file.path(normalizePath("~"), "Library", "Caches", "org.R-project.R")
    else file.path(normalizePath("~"), ".cache")
  }
}

#' Test if the concatenation of vectors `x` and `y` yields a monotonic result.
#' @noRd
.c_is_monotonic <- function(x, y) {
  xlen <- length(x)
  if (xlen == 1L) {
    if (length(y) == 1L) !.near(x, y)
    else !.near(x, y[1L]) && ((x < y[1L] && y[1L] < y[2L]) || (x > y[1L] && y[1L] > y[2L]))
  } else {
    xlast <- x[xlen]
    if (length(y) == 1L) {
      !.near(xlast, y) && ((xlast < y && x[1L] < xlast) || (xlast > y && x[1L] > xlast))
    } else {
      if (x[1L] < x[2L]) (y[1L] < y[2L]) && (xlast < y[1L]) && !.near(xlast, y[1L])
      else (y[1L] > y[2L]) && (xlast > y[1L]) && !.near(xlast, y[1L])
    }
  }
}

unused_imports <- function() {
  stringr::word
}
