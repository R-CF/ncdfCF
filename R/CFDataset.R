#' CF data set
#'
#' @description This class represents a CF data set, the object that
#' encapsulates a netCDF resource. You should never have to instantiate this
#' class directly; instead, call [open_ncdf()] which will return an instance
#' that has all properties read from the netCDF resource. Class methods can then
#' be called, or the base R functions called with this instance.
#'
#' The CF data set instance provides access to all the objects in the
#' netCDF resource, organized in groups.
#'
#' @export
#' @docType class
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
    keep_open = FALSE,

    #' @field root Root of the group hierarchy through which all elements of the
    #' netCDF resource are accessed. It is **strongly discouraged** to
    #' manipulate the objects in the group hierarchy directly. Use the provided
    #' access methods instead.
    root = NULL,

    #' @field file_type The type of data in the netCDF resource, if
    #'   identifiable. In terms of the CF Metadata Conventions, this includes
    #'   discrete sampling geometries (DSG). Other file types that can be
    #'   identified include L3b files used by NASA and NOAA for satellite
    #'   imagery (these data sets need special processing), and CMIP5, CMIP6 and
    #'   CORDEX climate projection data.
    file_type = "Generic netCDF data",

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

    #' @description Summary of the data set printed to the console.
    #' @param ... Arguments passed on to other functions. Of particular interest
    #' is `width = ` to indicate a maximum width of attribute columns.
    print = function(...) {
      cat("<Dataset>", self$name, "\n")
      cat("Resource   :", private$res$uri, "\n")
      cat("Format     :", private$format, "\n")
      cat("Collection :", self$file_type, "\n")
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
        print(.slim.data.frame(vars, ...), right = FALSE, row.names = FALSE)

        ev <- self$root$CFmeasures
        if (length(ev))
          cat("\nExternal variable", if (length(ev) > 1L) "s", ": ", paste(names(ev), collapse = ", "), "\n", sep = "")
      }

      self$root$print_attributes(...)
    },

    #' @description Print the group hierarchy to the console.
    hierarchy = function() {
      cat("<NetCDF objects>", self$name, "\n")
      hier <- self$root$hierarchy(1L, 1L)
      cat(hier, sep = "")
    },

    #' @description Get objects by standard_name. Several conventions define
    #'   standard vocabularies for physical properties. The standard names from
    #'   those vocabularies are usually stored as the "standard_name" attribute
    #'   with variables or axes. This method retrieves all variables or axes
    #'   that list the specified "standard_name" in its attributes.
    #'
    #' @param standard_name Optional, a character string to search for a
    #'   specific "standard_name" value in variables and axes.
    #'
    #' @return If argument `standard_name` is provided, a character vector of
    #'   variable or axis names. If argument `standard_name` is missing or an
    #'   empty string, a named list with all "standard_name" attribute values in
    #'   the the netCDF resource; each list item is named for the variable or
    #'   axis.
    objects_by_standard_name = function(standard_name) {
      nm <- c(sapply(self$root$variables(), function(v) v$attribute("standard_name")),
              sapply(self$root$axes(), function(x) x$attribute("standard_name")))
      if (missing(standard_name) || !nzchar(standard_name))
        nm[lengths(nm) > 0L]
      else
        names(nm[which(nm == standard_name)])
    },

    #' @description Does the netCDF resource have subgroups? Newer versions of
    #'   the `netcdf` library, specifically `netcdf4`, can organize dimensions
    #'   and variables in groups. This method will report if the data set is
    #'   indeed organized with subgroups.
    #'
    #' @return Logical to indicate that the netCDF resource uses subgroups.
    has_subgroups = function() {
      length(self$root$subgroups) > 0L
    },

    #' @description Find an object by its name. Given the name of a CF data
    #'   variable or axis, possibly preceded by an absolute group path, return
    #'   the object to the caller.
    #'
    #' @param name The name of a CF data variable or axis, with an optional
    #'   absolute group path.
    #' @param scope The scope to look for the name. Either "CF" (default) to
    #'   search for CF variables or axes, or "NC" to look for groups or NC
    #'   variables.
    #'
    #' @return The object with the provided name. If the object is not found,
    #'   returns `NULL`.
    find_by_name = function(name, scope = "CF") {
      self$root$find_by_name(name, scope)
    },

    #' @description This method lists the CF data variables located in this
    #'   netCDF resource, including those in subgroups.
    #'
    #' @return A list of `CFVariable` instances.
    variables = function() {
      self$root$variables()
    },

    #' @description This method lists the axes located in this netCDF resource,
    #'   including axes in subgroups.
    #'
    #' @return A list of `CFAxis` descendants.
    axes = function() {
      self$root$axes()
    },

    #' @description List all the attributes of a group. This method returns a
    #'   `data.frame` containing all the attributes of the indicated `group`.
    #'
    #' @param group The name of the group whose attributes to return. If the
    #'   argument is missing, the global attributes will be returned.
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
    },

    #' @description Retrieve global attributes of the data set.
    #'
    #' @param att Vector of character strings of attributes to return.
    #' @param field The field of the attribute to return values from. This must
    #'   be "value" (default) or "type".
    #' @return If the `field` argument is "type", a character string. If `field`
    #'   is "value", a single value of the type of the attribute, or a vector
    #'   when the attribute has multiple values. If no attribute is named with a
    #'   value of argument `att` `NA` is returned.
    attribute = function(att, field = "value") {
      self$root$attribute(att, field)
    }
  ),
  active = list(
    #' @field friendlyClassName (read-only) A nice description of the class.
    friendlyClassName = function(value) {
      if (missing(value))
        "Data set"
    },

    #' @field resource (read-only) The connection details of the netCDF
    #'   resource. This is for internal use only.
    resource = function(value) {
      if (missing(value))
        private$res
    },

    #' @field uri (read-only) The connection string to the netCDF resource.
    uri = function(value) {
      if (missing(value))
        private$res$uri
    },

    #' @field conventions (read-only) Returns the conventions that this netCDF
    #'   resource conforms to.
    conventions = function(value) {
      if (missing(value)) {
        conv <- self$root$attribute("Conventions")
        if (is.na(conv)) "(not indicated)" else conv
      }
    },

    #' @field var_names (read-only) Vector of names of variables in this data set.
    var_names = function(value) {
      if (missing(value)) {
        nm <- sapply(self$variables(), function(v) v$name)
        names(nm) <- NULL
        nm
      }
    },

    #' @field axis_names (read-only) Vector of names of axes in this data set.
    axis_names = function(value) {
      if (missing(value)) {
        nms <- sapply(self$axes(), function(ax) ax$name)
        names(nms) <- NULL
        nms
      }
    }
  )
)

# Public S3 methods ------------------------------------------------------------

#' Compact display of a CFDataset
#' @param object A `CFDataset` instance.
#' @param ... Ignored.
#' @export
str.CFDataset <- function(object, ...) {
  len <- length(names(object))
  plural <- if (len != 1L) "s" else ""
  cat("CFDataset with", len, paste0("data variable", plural))
}

#' @rdname dimnames
#' @export
names.CFDataset <- function(x) {
  if (!length(x$variables()))
    NULL
  else if (x$has_subgroups())
    paste0("/", gsub(".", "/", x$var_names, fixed = TRUE))
  else
    x$var_names
}

#' @export
dimnames.CFDataset <- function(x) {
  ax <- x$axes()
  if (!length(ax))
    NULL
  else if (x$has_subgroups()) {
    grps <- sapply(ax, function(z) z$group$fullname)
    unique(paste0(ifelse(grps == "/", "/", paste0(grps, "/")), x$axis_names))
  } else
    unique(x$axis_names)
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
#' This method can be used to retrieve a variable or axis from the data set by
#' name.
#'
#' If the data set has groups, the name `i` of the variable or axis should be
#' fully qualified with the path to the group where the object is located. This
#' fully qualified name can be retrieved with the [names()] and [dimnames()]
#' functions, respectively.
#'
#' @param x An `CFDataset` to extract a variable or axis from.
#' @param i The name of a variable or axis in `x`. If data set `x` has groups,
#'   `i` should be an absolute path to the object to retrieve.
#'
#' @return An instance of `CFVariable` or an `CFAxis` descendant class, or
#'   `NULL` if the name is not found.
#' @export
#'
#' @aliases [[,CFDataset-method
#' @docType methods
#' @examples
#' fn <- system.file("extdata", "ERA5land_Rwanda_20160101.nc", package = "ncdfCF")
#' ds <- open_ncdf(fn)
#' v1 <- ds$var_names[1]
#' var <- ds[[v1]]
#' var
`[[.CFDataset` <- function(x, i) {
  obj <- x$find_by_name(i)
  if (is.null(obj))
    obj <- x$find_by_name(i, "NC")
  obj
}
