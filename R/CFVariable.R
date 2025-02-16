# A data variable has dimensions
# Defining attributes: ancillary_variables, cell_methods, coordinate_interpolation,
# coordinates, flag_masks, flag_meanings, flag_values, grid_mapping
# Forbidden attributes: "dimensions", "geometry_type"

#' CF data variable
#'
#' @description This class represents the basic structure of a CF data variable, the object that
#'   provides access to an array of data.
#'
#' The CF data variable instance provides access to the data array from
#' the netCDF resource, as well as all the details that have been associated
#' with the data variable, such as axis information, grid mapping parameters,
#' etc.
#'
#' Do not use this class directly. Instead, use [CFVariableGeneric] of any of
#' the more specific `CFVariable*` classes.
#'
#' @docType class
CFVariable <- R6::R6Class("CFVariable",
  inherit = CFObject,
  private = list(
    .varProperties = function() {
      unit <- self$attribute("units")
      if (is.na(unit)) unit <- ""
      longname <- self$attribute("long_name")
      if (is.na(longname)) longname <- ""
      if (longname == self$name) longname <- ""
      list(longname = longname, unit = unit)
    },

    # Turn the rng argument into index values into the axis.
    range2index = function(axis, rng, closed) {
      axl <- axis$length
      if (inherits(axis, "CFAxisTime")) {
        idx <- axis$slice(rng, closed)
        if (!length(idx)) return(NULL)
        idx <- range(idx)
      } else if (inherits(axis, "CFAxisCharacter")) {
        idx <- range(match(rng, axis$values))
      } else {
        if (length(rng) == 1L) closed <- TRUE
        rng <- range(rng)
        vals <- axis$values
        idx <- if (closed)
          which(vals >= rng[1L] & vals <= rng[2L], arr.ind = TRUE)
        else
          which(vals >= rng[1L] & vals < rng[2L], arr.ind = TRUE)
        if (!length(idx)) return(NULL)
        else idx <- range(idx)
        if (!closed && isTRUE(all.equal(vals[idx[2L]], rng[2L])))
          idx[2L] <- idx[2L] - 1L
      }
      as.integer(idx)
    }
  ),
  public = list(
    #' @field axes List of instances of classes descending from [CFAxis] that
    #'   are the axes of the variable.
    axes  = list(),

    #' @field crs The coordinate reference system of this variable, as an
    #'   instance of [CFGridMapping]. If this field is `NULL`, the horizontal
    #'   component of the axes are in decimal degrees of longitude and latitude.
    crs = NULL,

    #' @description Create an instance of this class.
    #'
    #' @param grp The group that this CF variable lives in.
    #' @param nc_var The netCDF variable that defines this CF variable.
    #' @param axes List of [CFAxis] instances that describe the dimensions.
    #' @return An instance of this class.
    initialize = function(grp, nc_var, axes) {
      super$initialize(nc_var, grp)
      self$axes <- axes

      nc_var$CF <- self
    },

    #' @description Print a summary of the data variable to the console.
    print = function() {
      cat("<Variable>", self$name, "\n")
      if (self$group$name != "/")
        cat("Group    :", self$group$name, "\n")

      longname <- self$attribute("long_name")
      if (!is.na(longname) && longname != self$name)
        cat("Long name:", longname, "\n")

      if (!is.null(self$crs)) {
        cat("\nGrid mapping:\n")
        print(.slim.data.frame(self$crs$brief(), 50L), right = FALSE, row.names = FALSE)
      }

      cat("\nAxes:\n")
      axes <- do.call(rbind, lapply(self$axes, function(a) a$brief()))
      axes <- lapply(axes, function(c) if (all(c == "")) NULL else c)
      if (all(axes$group == "/")) axes$group <- NULL
      axes <- as.data.frame(axes[lengths(axes) > 0L])
      print(.slim.data.frame(axes, 50L), right = FALSE, row.names = FALSE)

      if (!is.null(private$llgrid)) {
        cat("\nAuxiliary longitude-latitude grid:\n")
        ll <- private$llgrid$brief()
        print(.slim.data.frame(ll, 50L), right = FALSE, row.names = FALSE)
      }

      self$print_attributes()
    },

    #' @description Some details of the data variable.
    #'
    #' @return A 1-row `data.frame` with some details of the data variable.
    brief = function() {
      props <- private$.varProperties()
      ax <- sapply(self$axes, function(x) x$name)
      data.frame(group = self$group$fullname, name = self$name,
                 long_name = props$longname, units = props$unit,
                 data_type = self$NCvar$vtype, axes = paste(ax, collapse = ", "))
    },

    #' @description The information returned by this method is very concise
    #'   and most useful when combined with similar information from other
    #'   variables.
    #'
    #' @return Character string with very basic variable information.
    shard = function() {
      self$NCvar$shard()
    },

    #' @description Retrieve interesting details of the data variable.
    #' @param with_groups Should group information be included? The save option
    #' is `TRUE` (default) when the netCDF resource has groups because names may
    #' be duplicated among objects in different groups.
    #' @return A 1-row `data.frame` with details of the data variable.
    peek = function(with_groups = TRUE) {
      out <- data.frame(id = self$id)
      if (with_groups) out$group <- self$group$fullname
      out$name <- self$name
      out$long_name <- self$attribute("long_name")
      out$standard_name <- self$attribute("standard_name")
      out$units <- self$attribute("units")
      if (with_groups)
        out$axes <- paste(sapply(self$axes, function(a) a$fullname), collapse = ", ")
      else
        out$axes <- paste(sapply(self$axes, function(a) a$name), collapse = ", ")
      out
    },

    #' @description Return the time object from the axis representing time.
    #' @return A `CFTime` instance, or `NULL` if the variable does not have a
    #' "time" dimension.
    time = function() {
      ndx <- sapply(self$axes, inherits, "CFAxisTime")
      if (any(ndx))
        self$axes[[which(ndx)]]$time()
      else NULL
    }
  ),
  active = list(
    #' @field friendlyClassName (read-only) A nice description of the class.
    friendlyClassName = function(value) {
      if (missing(value))
        "Variable"
    },

    #' @field gridLongLat The grid of longitude and latitude values of every
    #' grid cell when the main variable grid has a different coordinate system.
    gridLongLat = function(value) {
      if (missing(value))
        private$llgrid
      else {
        private$llgrid <- value
        dimids <- value$dimids
        ax1 <- sapply(self$axes, function(x) if (x$dimid == dimids[[1L]]) x$orientation)
        ax1 <- ax1[lengths(ax1) > 0L]
        ax2 <- sapply(self$axes, function(x) if (x$dimid == dimids[[2L]]) x$orientation)
        ax2 <- ax2[lengths(ax2) > 0L]
        c(ax1[[1L]], ax2[[1L]])
      }
    },

    #' @field crs_wkt2 (read-only) Retrieve the coordinate reference system
    #' description of the variable as a WKT2 string.
    crs_wkt2 = function(value) {
      if (missing(value)) {
        if (is.null(self$crs)) {
          # If no CRS has been set, return the default GEOGCRS unless
          # the axis coordinates fall out of range in which case an empty string
          # is returned.
          orient <- lapply(self$axes, function(x) x$orientation)
          X <- match("X", orient, nomatch = 0L)
          Y <- match("Y", orient, nomatch = 0L)
          if (X && Y) {
            X <- self$axes[[X]]$range()
            Y <- self$axes[[Y]]$range()
            if (Y[1L] >= -90 && Y[2L] <= 90 && ((X[1L] >= 0 && X[2L] <= 360) || (X[1L] >= -180 && X[2L] <= 180)))
              .wkt2_crs_geo(4326)
            else ""
          } else ""
        } else
          self$crs$wkt2(.wkt2_axis_info(self))
      }
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
