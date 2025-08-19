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
  private = list(
    # The NCVariable instance that this CF object represents if it was read from
    # file.
    .NCvar = NULL,

    # The id and name of this object. They are taken from the NCVariable upon
    # creation, if supplied, otherwise the name should be supplied and the id
    # will be generated.
    .id = -1L,
    .name = "",

    # Start and count vectors for reading data from file. The length of  the
    # vectors must agree with the array on file or be `NA`.
    .start = NA,
    .count = NA,

    # The data type of the data in the object. Taken from NCvar if set,
    # otherwise the descending class must set it explicitly when receiving its
    # data values.
    .data_type = NULL,

    # The attributes of the CF object. Upon read from a netCDF resource they are
    # copied from the NCVariable. For a new CF object, just an empty data.frame.
    .attributes = data.frame()
  ),
  public = list(
    #' @description Create a new CFobject instance from a variable in a netCDF
    #'   resource. This method is called upon opening a netCDF resource. It is
    #'   rarely, if ever, useful to call this constructor directly. Instead, use
    #'   the methods from higher-level classes such as [CFVariable].
    #'
    #' @param var The [NCVariable] instance upon which this CF object is based
    #'   when read from a netCDF resource, or the name for the object new CF
    #'   object to be created.
    #' @param start Optional. Vector of indices where to start reading data
    #'   along the dimensions of the array on file. The vector must be `NA` to
    #'   read all data, otherwise it must have agree with the dimensions of the
    #'   array on file. Ignored when argument `var` is not an NCVariable
    #'   instance.
    #' @param count Optional. Vector of number of elements to read along each
    #'   dimension of the array on file. The vector must be `NA` to read to the
    #'   end of each dimension, otherwise its value must agree with the
    #'   corresponding `start` value and the dimension of the array on file.
    #'   Ignored when argument `var` is not an NCVariable instance.
    #' @param attributes Optional. A `data.frame` with the attributes of the
    #' object. Ignored when argument `var` is an NCVariable instance.
    #' @return A `CFObject` instance.
    initialize = function(var, start = NA, count = NA, attributes = data.frame()) {
      if (is.character(var)) {
        private$.id <- CF$newVarId()
        private$.name <- var
        private$.attributes <- attributes
      } else {
        private$.NCvar <- var
        private$.id <- var$id
        private$.name <- var$name
        private$.start <- start
        private$.count <- count
        private$.data_type <- var$vtype
        private$.attributes <- var$attributes[-1L]
      }
    },

    #' @description Detach the current object from its underlying netCDF
    #'   resource.
    detach = function() {
      private$.NCvar$detach(self)
      private$.NCvar <- NULL
    },

    #' @description Read the data of the CF object from file.
    #' @return An array of data, as prescribed by the `start` and `count` values
    #'   used to create this object. If the object is not backed by a netCDF
    #'   resource, returns `NULL`.
    read_data = function() {
      if (is.null(private$.NCvar)) NULL
      else private$.NCvar$get_data(private$.start, private$.count)
    },

    #' @description Retrieve the dimensions of the data of this object in the
    #'   netCDF resource. If this object is not backed by a netCDF resource,
    #'   returns `NULL`. Descendant classes should get the dimensions from the
    #'   values array they themselves manage.
    #' @param dimension Optional. The index of the dimension to retrieve the
    #' length for. If omitted, retrieve the lengths of all dimensions.
    dim = function(dimension) {
      if (self$has_resource) {
        if (is.na(private$.count[1L])) {
          cnt <- private$.NCvar$dim(dimension)
          stt <- if (is.na(private$.start[1L])) 1L else private$.start
          if (missing(dimension))
            cnt - stt + 1L
          else
            (cnt - stt + 1L)[dimension]
        } else private$.count
      } else NULL
    },

    #' @description Retrieve an attribute of a CF object.
    #'
    #' @param att Single character string of attribute to return.
    #' @param field The field of the attribute to return values from. This must
    #'   be "value" (default) or "type".
    #' @return If the `field` argument is "type", a character string. If `field`
    #'   is "value", a single value of the type of the attribute, or a vector
    #'   when the attribute has multiple values. If no attribute is named with a
    #'   value of argument `att` `NA` is returned.
    attribute = function(att, field = "value") {
      if (length(att) > 1L)
        stop("Can extract only one attribute at a time.", call. = FALSE)

      atts <- private$.attributes
      if (!nrow(atts)) return(NA)
      val <- atts[atts$name == att, ]
      if (!nrow(val)) return(NA)

      val[[field]][[1L]]
    },

    #' @description Print the attributes of the CF object to the console.
    #'
    #' @param width The maximum width of each column in the `data.frame` when
    #' printed to the console.
    print_attributes = function(width = 50L) {
      if (nrow(private$.attributes)) {
        cat("\nAttributes:\n")
        print(.slim.data.frame(private$.attributes, width), right = FALSE, row.names = FALSE)
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
      if (is.na(name) || !is.character(name) || length(name) != 1L)
        stop("Must name one attribute to set values for.", call. = FALSE) # nocov
      if (!type %in% netcdf_data_types)
        stop("Invalid netCDF data type.", call. = FALSE) # nocov

      # Prepare values
      value <- unlist(value, use.names = FALSE)
      if (is.list(value) || is.array(value))
        stop("Unsupported value for attribute (compound list, matrix, array?).", call. = FALSE) # nocov
      if (is.character(value)) {
        if (type == "NC_STRING") len <- length(value)
        else if (type == "NC_CHAR") {
          value <- paste(value, sep = ", ")
          len <- nchar(value)
        } else stop("Wrong attribute type for string value.", call. = FALSE) # nocov
      } else {
        if (is.logical(value)) value <- as.integer(value)
        if (is.numeric(value)) len <- length(value)
        else stop("Unsupported value for attribute.", call. = FALSE) # nocov
      }
      if (type != "NC_CHAR") value <- list(value)

      # Check if the name refers to an existing attribute
      if (nrow(private$.attributes) && nrow(private$.attributes[private$.attributes$name == name, ])) {
        # If so, replace its type and value
        private$.attributes[private$.attributes$name == name, ]$type <- type
        private$.attributes[private$.attributes$name == name, ]$length <- len
        private$.attributes[private$.attributes$name == name, ]$value <- value
      } else {
        # If not, create a new attribute
        if (!.is_valid_name(name))
          stop("Attribute name is not valid.", call. = FALSE) # nocov

        df <- data.frame(name = name, type = type, length = len)
        df$value <- value # Preserve lists
        private$.attributes <- rbind(private$.attributes, df)
      }
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
      if (is.na(name) || !is.character(name) || length(name) != 1L)
        stop("Must name one attribute to append values for.", call. = FALSE)
      if (is.na(name) || !is.character(name) || length(name) != 1L)
        stop("Value to append must be a single character string.", call. = FALSE)

      if (nchar(value) > 0L) {
        # Check if the name refers to an existing attribute
        if (nrow(private$.attributes[private$.attributes$name == name, ])) {
          new_val <- if (prepend)
            paste0(value, sep, private$.attributes[private$.attributes$name == name, ]$value)
          else
            paste0(private$.attributes[private$.attributes$name == name, ]$value, sep, value)
          private$.attributes[private$.attributes$name == name, ]$value <- new_val
          private$.attributes[private$.attributes$name == name, ]$length <- nchar(new_val)
        } else # else create a new attribute
          self$set_attribute(name, "NC_STRING", value)
      }

      invisible(self)
    },

    #' @description Delete an attribute. If an attribute `name` is not present
    #' this method simply returns.
    #' @param name The name of the attribute to delete.
    #' @return Self, invisibly.
    delete_attribute = function(name) {
      private$.attributes <- private$.attributes[!private$.attributes$name %in% name, ]
      invisible(self)
    },

    #' @description Write the attributes of this object to a netCDF file.
    #' @param nc The handle to the netCDF file opened for writing.
    #' @param nm The NC variable name or "NC_GLOBAL" to write the attributes to.
    #' @return Self, invisibly.
    write_attributes = function(nc, nm) {
      if ((num_atts <- nrow(private$.attributes)) > 0L)
        for (a in 1L:num_atts) {
          attr <- private$.attributes[a,]
          RNetCDF::att.put.nc(nc, nm, attr$name, attr$type, unlist(attr$value, use.names = FALSE))
        }
      invisible(self)
    },

    #' @description Add names of axes to the "coordinates" attribute, avoiding
    #' duplicates and retaining previous values.
    #' @param crds Vector of axis names to add to the attribute.
    #' @return Self, invisibly.
    add_coordinates = function(crds) {
      current <- private$.attributes[private$.attributes$name == "coordinates", ]
      if (nrow(current)) {
        # There is a "coordinates" attribute already so append values
        new_val <- paste(unique(c(strsplit(current[[1L, "value"]], " ")[[1L]], crds)), collapse = " ")
        private$.attributes[private$.attributes$name == "coordinates", ]$value <- new_val
        private$.attributes[private$.attributes$name == "coordinates", ]$length <- nchar(new_val)
      } else
        # Make a new "coordinates" attribute
        self$set_attribute("coordinates", "NC_CHAR", paste(crds, collapse = " "))
      invisible(self)
    }
  ),
  active = list(
    #' @field friendlyClassName (read-only) A nice description of the class.
    friendlyClassName = function(value) {
      if (missing(value))
        "Generic CF object"
    },

    #' @field NCvar (read-only) The [NCVariable] instance that this CF object
    #'   represents, or `NULL` if not set.
    NCvar = function(value) {
      if (missing(value))
        private$.NCvar
    },

    #' @field id Set or retrieve the identifier of the CF object. In general,
    #'   the `id` value is immutable so it should never be set to a new value.
    id = function(value) {
      if (missing(value))
        private$.id
      else
        private$.id <- value
    },

    #' @field name Set or retrieve the name of the CF object. The name must be a
    #'   valid netCDF name: start with a character, use only characters, numbers
    #'   and the underscore, and be at most 255 characters long.
    name = function(value) {
      if (missing(value))
        private$.name
      else if (.is_valid_name(value))
        private$.name <- value
      else
        stop("Invalid name for a CF object.", call. = FALSE) # nocov
    },

    #' @field fullname (read-only) The fully-qualified name of the CF object.
    fullname = function(value) {
      if (missing(value)) {
        private$.name
        # grp <- self$group$fullname
        # if (is.null(grp)) return(private$NC$name)
        # grp <- if (grp == "/") "" else paste0(grp, "/")
        # paste0(grp, private$NC$name)
      }
    },

    #' @field group (read-only) Retrieve the [NCGroup] that this object is
    #'   located in.
    group = function(value) {
      if (missing(value))
        private$.NCvar$group
      else
        NULL #FIXME: Cannot change the NCGroup that an object relates to
    },

    #' @field attributes (read-only) Retrieve a `data.frame` with the attributes
    #'   of the CF object.
    attributes = function(value) {
      if (missing(value))
        private$.attributes
      # else
      #   private$.attributes <- value
    },

    #' @field has_resource (read-only) Flag that indicates if this object has an
    #'   underlying netCDF resource.
    has_resource = function(value) {
      !is.null(private$.NCvar)
    },

    #' @field data_type Set or retrieve the data type of the data in the object.
    #'   If this CF object is backed by a netCDF resource, the data type cannot
    #'   be set, only retrieved. If the CF object exists in memory, the
    #'   descending CF object must set the data type upon receiving its values.
    #'   The data type must be one of the allowable netCDF types. Do not set the
    #'   data type otherwise.
    #'
    #'   **Note:** Any data values in the object will be deleted after setting
    #'   the data type. Set the data type before adding values.
    data_type = function(value) {
      if (missing(value))
        private$.data_type
      else if (self$has_resource)
        stop("Cannot set the data type of a variable present on file.", call. = FALSE) # nocov
      else if (value %in% netcdf_data_types) {
        private$.data_type <- value
        private$.values <- NULL
      } else
        stop("Unrecognized data type for a netCDF variable.", call. = FALSE) # nocov
    }
  )
)

#' @name dimnames
#' @title Names or axis values of an CF object
#'
#' @description Retrieve the variable or axis names of an `ncdfCF` object. The
#'   `names()` function gives the names of the variables in the data set,
#'   preceded by the path to the group if the resource uses groups. The return
#'   value of the `dimnames()` function differs depending on the type of object:
#' * `CFDataset`, `CFVariable`: The dimnames are returned as a vector of the
#'   names of the axes of the data set or variable, preceded with the path to
#'   the group if the resource uses groups. Note that this differs markedly from
#'   the `base::dimnames()` functionality.
#' * `CFAxisNumeric`, `CFAxisLongitude`, `CFAxisLatitude`, `CFAxisVertical`: The
#'   coordinate values along the axis as a numeric vector.
#' * `CFAxisTime`: The coordinate values along the axis as a
#'   character vector containing timestamps in ISO8601 format. This could be
#'   dates or date-times if time information is available in the axis.
#' * `CFAxisCharacter`: The coordinate values along the axis as
#'   a character vector.
#' * `CFAxisDiscrete`: The index values of the axis, from 1 to the
#'   length of the axis.
#'
#' @param x An `CFObject` whose axis names to retrieve. This could be
#'   `CFDataset`, `CFVariable`, or a class descending from `CFAxis`.
#'
#' @return A vector as described in the Description section.
#' @examples
#' fn <- system.file("extdata",
#'   "pr_day_EC-Earth3-CC_ssp245_r1i1p1f1_gr_20230101-20231231_vncdfCF.nc",
#'   package = "ncdfCF")
#' ds <- open_ncdf(fn)
#'
#' # Names of data variables
#' names(ds)
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

