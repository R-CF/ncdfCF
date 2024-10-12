#' @title CF data set
#'
#' @description This class represents a CF data set, the object that
#' encapsulates a netCDF resource. You should never have to instantiate this
#' class directly; instead, call [open_ncdf()] which will return an instance
#' that has all properties read from the netCDF resource. Class methods can then
#' be called, or the base R functions called with this instance.
#'
#' @details
#' The CF data set instance provides access to all the objects in the
#' netCDF resource, organized in groups.
#'
#' @docType class
#'
CFDataset <- R6::R6Class("CFDataset",
  private = list(
    res       = NULL,
    format    = "classic"
  ),
  public = list(
    #' @field name The name of the netCDF resource. This is extracted from the
    #'   URI (file name or URL).
    name       = "",

    #' @field keep_open Logical flag to indicate if the netCDF resource has to
    #'   remain open after reading the metadata. This should be enabled
    #'   typically only for programmatic access or when a remote resource has an
    #'   expensive access protocol (i.e. 2FA). The resource has to be explicitly
    #'   closed with `close()` after use. Note that when a data set is opened
    #'   with `keep_open = TRUE` the resource may still be closed by the
    #'   operating system or the remote server.
    keep_open  = FALSE,

    #' @field root Root of the group hierarchy through which all elements of the
    #' netCDF resource are accessed. It is **strongly discouraged** to
    #' manipulate the objects in the group hierarchy directly. Use the provided
    #' access methods instead.
    root      = NULL,

    #' @description Create an instance of this class.
    #' @param name The name that describes this instance.
    #' @param resource An instance of `CFResource` that links to the netCDF
    #' resource.
    #' @param keep_open Logical. Should the netCDF resource be kept open for
    #' further access?
    #' @param format Character string with the format of the netCDF resource as
    #' reported by the call opening the resource.
    initialize = function(name, resource, keep_open, format) {
      self$name <- name
      private$res <- resource
      self$keep_open <- keep_open
      private$format <- format
    },

    #' @description Summary of the data set
    #'
    #' Prints a summary of the data set to the console.
    print = function() {
      cat("<Dataset>", self$name, "\n")
      cat("Resource   :", private$res$uri, "\n")
      cat("Format     :", private$format, "\n")
      cat("Conventions:", self$conventions, "\n")
      cat("Keep open  :", self$keep_open, "\n")

      if (private$format == "netcdf4")
        cat("Has groups :", self$has_subgroups(), "\n")

      nvars <- length(self$root$variables())
      if (nvars) {
        if (nvars == 1L) cat("\nVariable:\n") else cat("\nVariables:\n")
        vars <- do.call(rbind, lapply(self$root$variables(), function(v) v$brief()))
        if (all(vars$group == "/")) vars$group <- NULL
        if (all(vars$long_name == "")) vars$long_name <- NULL
        if (all(vars$units == "")) vars$units <- NULL
        vars <- as.data.frame(vars[lengths(vars) > 0L])
        print(.slim.data.frame(vars, 50L), right = FALSE, row.names = FALSE)

        cat("\nAxes:\n")
        axes <- do.call(rbind, lapply(self$root$axes(), function(a) a$brief()))
        axes <- lapply(axes, function(c) if (all(c == "")) NULL else c)
        if (all(axes$group == "/")) axes$group <- NULL
        axes <- unique(as.data.frame(axes[lengths(axes) > 0L]))
        print(.slim.data.frame(axes, 50L), right = FALSE, row.names = FALSE)

        self$root$print_attributes()
      }
    },

    #' @description Print the group hierarchy to the console
    hierarchy = function() {
      cat("<NetCDF objects>", self$name, "\n")
      hier <- self$root$hierarchy(1L, 1L)
      cat(hier, sep = "")
    },

    #' Get objects by standard_name
    #'
    #' @description Several conventions define standard vocabularies for
    #' physical properties. The standard names from those vocabularies are
    #' usually stored as the "standard_name" attribute with variables or
    #' axes. This method retrieves all variables or axes that list
    #' the specified "standard_name" in its attributes.
    #'
    #' @param standard_name Optional, a character string to search for a
    #'   specific "standard_name" value in variables and axes.
    #'
    #' @aliases standard_name
    #'
    #' @return If argument `standard_name` is provided, a character vector of
    #'   variable or axis names. If argument `standard_name` is missing or
    #'   an empty string, a named list with all "standard_name" attribute values
    #'   in the the netCDF resource; each list item is named for the variable or
    #'   axis.
    objects_by_standard_name = function(standard_name) {
      nm <- c(sapply(self$root$variables(), function(v) v$attribute("standard_name")),
              sapply(self$root$axes(), function(x) x$attribute("standard_name")))
      if (missing(standard_name) || !nzchar(standard_name))
        nm[lengths(nm) > 0L]
      else
        names(nm[which(nm == standard_name)])
    },

    #' Does the netCDF resource have subgroups
    #'
    #' Newer versions of the `netcdf` library, specifically `netcdf4`, can
    #' organize dimensions and variables in groups. This method will report if
    #' the data set is indeed organized with subgroups.
    #'
    #' @return Logical to indicate that the netCDF resource uses subgroups.
    has_subgroups = function() {
      length(self$root$subgroups) > 0L
    },

    #' Find an object by its name
    #'
    #' Given the name of a CF data variable or axis, possibly preceded by an
    #' absolute group path, return the object to the caller.
    #'
    #' @param name The name of a CF data variable or axis, with an optional
    #'   absolute group path.
    #' @param scope The scope to look for the name. Either "CF" (default) to
    #' search for CF variables or axes, or "NC" to look for groups or NC
    #' variables.
    #'
    #' @return The object with the provided name. If the object is not found,
    #'   returns `NULL`.
    find_by_name = function(name, scope = "CF") {
      self$root$find_by_name(name, scope)
    },

    #' List all the CF data variables in this netCDF resource
    #'
    #' This method lists the CF data variables located in this netCDF resource,
    #' including those in subgroups.
    #'
    #' @return A list of `CFVariable`s.
    variables = function() {
      self$root$variables()
    },

    #' List all the axes of CF data variables in this netCDF resource
    #'
    #' This method lists the axes located in this netCDF resource, including
    #' axes in subgroups.
    #'
    #' @return A list of `CFAxis` descendants.
    axes = function() {
      self$root$axes()
    },

    #' List all the attributes of a group
    #'
    #' This method returns a `data.frame` containing all the attributes of the
    #' indicated `group`.
    #'
    #' @param group The name of the group whose attributes to return. If the
    #' argument is missing, the global attributes will be returned.
    #'
    #' @return A `data.frame` of attributes.
    attributes = function(group) {
      if (missing(group))
        self$root$attributes
      else {
        grp <- self$root$find_by_name(group, "NC")
        if (is.null(grp)) NULL
        else grp$attributes
      }
    }
  ),
  active = list(
    #' @field friendlyClassName (read-only) A nice description of the class.
    friendlyClassName = function(value) {
      if (missing(value))
        "Data set"
    },

    #' @field resource The connection details of the netCDF resource.
    resource = function(value) {
      private$res
    },

    #' @field conventions Returns the conventions that this netCDF resource conforms to.
    conventions = function(value) {
      if (missing(value)) {
        conv <- self$root$attribute("Conventions")
        if (!nzchar(conv)) conv <- "(not indicated)"
        conv
      }
    }
  )
)

# Public S3 methods ------------------------------------------------------------

#' @rdname dimnames
#' @export
names.CFDataset <- function(x) {
  vars <- x$variables()
  if (!length(vars))
    NULL
  else if (x$has_subgroups())
    paste0("/", gsub(".", "/", names(vars), fixed = TRUE))
  else
    names(vars)
}

#' @export
dimnames.CFDataset <- function(x) {
  ax <- x$axes()
  if (!length(ax))
    NULL
  else if (x$has_subgroups()) {
    grps <- sapply(ax, function(z) z$group$fullname)
    unique(paste0(ifelse(grps == "/", "/", paste0(grps, "/")), names(ax)))
  } else
    unique(names(ax))
}

#' @rdname dimnames
#' @export
groups <- function(x) {
  UseMethod("groups")
}

#' @rdname dimnames
#' @export
groups.CFDataset <- function(x) {
  nm <- x$root$fullnames()
  names(nm) <- NULL
  nm
}

#' Get a variable or axis object from a data set
#'
#' This method can be used to retrieve a variable or axis from the
#' data set by name.
#'
#' If the data set has groups, the name `i` of the variable or axis should be
#' fully qualified with the path to the group where the object is located. This
#' fully qualified name can be retrieved with the [names()] and [dimnames()]
#' functions, respectively.
#'
#' @param x An `CFDataset` to extract a variable or axis from.
#' @param i The name of a variable or axis in `x`. If data set `x` has groups,
#' `i` should be an absolute path to the object to retrieve.
#'
#' @return An instance of `CFVariable` or an `CFAxis` descendant
#' class, or `NULL` if the name is not found.
#' @export
#'
#' @aliases [[,CFDataset-method
#' @docType methods
#' @examples
#' fn <- system.file("extdata", "ERA5land_Rwanda_20160101.nc", package = "ncdfCF")
#' ds <- open_ncdf(fn)
#' v1 <- names(ds)[1]
#' var <- ds[[v1]]
#' var
`[[.CFDataset` <- function(x, i) {
  obj <- x$find_by_name(i)
  if (is.null(obj))
    obj <- x$find_by_name(i, "NC")
  obj
}
