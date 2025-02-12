#' @import methods
#' @import R6
#' @include NCObject.R
NULL

#' CF base object
#'
#' @description This class is a basic ancestor to all classes that represent CF
#'   objects, specifically data variables and axes. More useful classes use this
#'   class as ancestor.
#'
#' @docType class
CFObject <- R6::R6Class("CFObject",
  public = list(
    #' @field NCvar The [NCVariable] instance that this CF object represents.
    NCvar = NULL,

    #' @field group The [NCGroup] that this object is located in.
    group = NULL,

    #' @description Create a new CF object instance from a variable in a netCDF
    #'   resource. This method is called upon opening a netCDF resource. It is
    #'   rarely, if ever, useful to call this constructor directly from the
    #'   console. Instead, use the methods from higher-level classes such as
    #'   [CFVariable].
    #'
    #' @param nc_var The [NCVariable] instance upon which this CF object is
    #'   based.
    #' @param group The [NCGroup] that this object is located in.
    #' @return A `CFobject` instance.
    initialize = function(nc_var, group) {
      self$NCvar <- nc_var
      self$group <- group
    },

    #' @description Retrieve attributes of any CF object.
    #'
    #' @param att Vector of character strings of attributes to return.
    #' @param field The field of the attribute to return values from. This must
    #'   be "value" (default) or "type".
    #' @return If the `field` argument is "type", a character string. If `field`
    #'   is "value", a single value of the type of the attribute, or a vector
    #'   when the attribute has multiple values. If no attribute is named with a
    #'   value of argument `att` `NA` is returned.
    attribute = function(att, field = "value") {
      self$NCvar$attribute(att, field)
    },

    #' @description Print the attributes of the CF object to the console.
    #'
    #' @param width The maximum width of each column in the `data.frame` when
    #' printed to the console.
    print_attributes = function(width = 50L) {
      if (nrow(self$NCvar$attributes)) {
        cat("\nAttributes:\n")
        print(.slim.data.frame(self$NCvar$attributes, width), right = FALSE, row.names = FALSE)
      }
    },

    #' @description Add an attribute. If an attribute `name` already exists, it
    #'   will be overwritten.
    #' @param name The name of the attribute. The name must begin with a letter
    #'   and be composed of letters, digits, and underscores, with a maximum
    #'   length of 255 characters. UTF-8 characters are not supported in
    #'   attribute names.
    #' @param type The type of the attribute, as a string value of a netCDF data
    #'   type.
    #' @param value The value of the attribute. This can be of any supported
    #'   type, including a vector or list of values. Matrices, arrays and like
    #'   compound data structures should be stored as a data variable, not as an
    #'   attribute and they are thus not allowed. In general, an attribute
    #'   should be a character value, a numeric value, a logical value, or a
    #'   short vector or list of any of these. Values passed in a list will be
    #'   coerced to their common mode.
    #' @return Self, invisibly.
    set_attribute = function(name, type, value) {
      self$NCvar$set_attribute(name, type, value)
      invisible(self)
    },

    #' @description Append the text value of an attribute. If an attribute
    #'   `name` already exists, the `value` will be appended to the existing
    #'   value of the attribute. If the attribute `name` does not exist it will
    #'   be created. The attribute must be of "NC_CHAR" or "NC_STRING" type; in
    #'   the latter case having only a single string value.
    #' @param name The name of the attribute. The name must begin with a letter
    #'   and be composed of letters, digits, and underscores, with a maximum
    #'   length of 255 characters. UTF-8 characters are not supported in
    #'   attribute names.
    #' @param value The character value of the attribute to append. This must be
    #'   a character string.
    #' @param sep The separator to use. Default is `"; "`.
    #' @param prepend Logical to flag if the supplied `value` should be placed
    #'   before the existing value. Default is `FALSE`.
    #' @return Self, invisibly.
    append_attribute = function(name, value, sep = "; ", prepend = FALSE) {
      self$NCvar$append_attribute(name, value, sep, prepend)
      invisible(self)
    },

    #' @description Delete an attribute. If an attribute `name` is not present
    #' this method simply returns.
    #' @param name The name of the attribute to delete.
    #' @return Self, invisibly.
    delete_attribute = function(name) {
      self$NCvar$delete_attribute(name)
      invisible(self)
    },

    #' @description Write the attributes of this object to a netCDF file.
    #' @param nc The handle to the netCDF file opened for writing.
    #' @param nm The NC variable name or "NC_GLOBAL" to write the attributes to.
    #' @return Self, invisibly.
    write_attributes = function(nc, nm) {
      self$NCvar$write_attributes(nc, nm)
      invisible(self)
    }
  ),

  active = list(
    #' @field friendlyClassName (read-only) A nice description of the class.
    friendlyClassName = function(value) {
      if (missing(value))
        "Generic CF object"
    },

    #' @field id (read-only) The identifier of the CF object.
    id = function(value) {
      if (missing(value))
        self$NCvar$id
    },

    #' @field name (read-only) The name of the CF object.
    name = function(value) {
      if (missing(value))
        self$NCvar$name
    },

    #' @field fullname (read-only) The fully-qualified name of the CF object.
    fullname = function(value) {
      if (missing(value)) {
        grp <- self$group$fullname
        if (!(grp == "/")) grp <- paste0(grp, "/")
        paste0(grp, self$NCvar$name)
      }
    },

    #' @field attributes (read-only) A `data.frame` with the attributes of the
    #'   CF object.
    attributes = function(value) {
      if (missing(value))
        self$NCvar$attributes
    }
  )
)

#' @name dimnames
#' @title Names or dimension values of an CF object
#'
#' @description Retrieve the variable or dimension names of an `ncdfCF` object.
#' The `names()` function gives the names of the variables in the data set,
#' prepended with the path to the group if the resource uses groups.
#' The return value of the `dimnames()` function differs depending on the type
#' of object:
#' * `CFDataset`, `CFVariable`: The dimnames are returned as a vector of the
#' names of the axes of the data set or variable, prepended with the path to the
#' group if the resource uses groups. Note that this differs markedly from the
#' `base::dimnames()` functionality.
#' * `CFAxisNumeric`, `CFAxisLongitude`, `CFAxisLatitude`, `CFAxisVertical`: The
#' values of the elements along the axis as a numeric vector.
#' * `CFAxisTime`: The values of the elements along the axis as a
#' character vector containing timestamps in ISO8601 format. This could be dates
#' or date-times if time information is available in the axis.
#' * `CFAxisScalar`: The value of the scalar.
#' * `CFAxisCharacter`: The values of the elements along the axis as
#' a character vector.
#' * `CFAxisDiscrete`: The index values of the axis, from 1 to the
#' length of the axis.
#'
#' @param x An `CFObject` whose axis names to retrieve. This could be
#' `CFDataset`, `CFVariable`, or a class descending from `CFAxis`.
#'
#' @return A vector as described in the Description section.
#' @examples
#' fn <- system.file("extdata",
#'   "pr_day_EC-Earth3-CC_ssp245_r1i1p1f1_gr_20230101-20231231_vncdfCF.nc",
#'   package = "ncdfCF")
#' ds <- open_ncdf(fn)
#'
#' # CFDataset
#' dimnames(ds)
#'
#' # CFVariable
#' pr <- ds[["pr"]]
#' dimnames(pr)
#'
#' # CFAxisNumeric
#' lon <- ds[["lon"]]
#' dimnames(lon)
#'
#' # CFAxisTime
#' t <- ds[["time"]]
#' dimnames(t)
NULL

