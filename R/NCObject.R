#' NetCDF base object
#'
#' @description This class is a basic ancestor to all classes that represent
#'   netCDF objects, specifically groups, dimensions, variables and the
#'   user-defined types in a netCDF file. More useful classes use this class as
#'   ancestor.
#'
#'   The fields in this class are common among all netCDF objects. In addition,
#'   this class manages the attributes for its descendent classes.
#'
#' @docType class
#'
NCObject <- R6::R6Class("NCObject",
  public = list(
    #' @field id Numeric identifier of the netCDF object.
    id         = -1L,

    #' @field name The name of the netCDF object.
    name       = "",

    #' @field attributes `data.frame` with the attributes of the netCDF object.
    attributes = data.frame(),

    #' @description Create a new netCDF object. This class should not be
    #'   instantiated directly, create descendant objects instead.
    #'
    #' @param id Numeric identifier of the netCDF object.
    #' @param name Character string with the name of the netCDF object.
    initialize = function(id, name) {
      self$id <- id
      self$name <- name
    },

    #' @description This function prints the attributes of the netCDF object to
    #'   the console. Through object linkages, this also applies to the CF data
    #'   variables and axes, which each link to a netCDF object.
    #'
    #' @param width The maximum width of each column in the `data.frame` when
    #'   printed to the console.
    print_attributes = function(width = 50L) {
      if (nrow(self$attributes)) {
        cat("\nAttributes:\n")
        print(.slim.data.frame(self$attributes, width), right = FALSE, row.names = FALSE)
      }
    },

    #' @description This method returns netCDF object attributes.
    #' @param att Vector of attribute names whose values to return.
    #' @param field The field of the attributes to return values from. This must
    #'   be "value" (default), "type" or "length".
    #' @return If the `field` argument is "type" or "length", a character vector
    #'   named with the `att` values that were found in the attributes. If
    #'   argument `field` is "value", a list with elements named with the `att`
    #'   values, containing the attribute value(s), except when argument `att`
    #'   names a single attribute, in which case that attribute value is
    #'   returned as a character string. If no attribute is named with a value
    #'   of argument `att` an empty list is returned, or an empty string if
    #'   there was only one value in argument `att`.
    attribute = function(att, field = "value") {
      num <- length(att)
      atts <- self$attributes
      if (!nrow(atts)) return(if (num == 1L) "" else list())
      val <- atts[atts$name %in% att, ]
      if (!nrow(val)) return(if (num == 1L) "" else list())

      out <- val[[field]]
      names(out) <- val[["name"]]
      if (num == 1L && field == "value") out <- val$value[[1L]]
      out
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
        stop("Must name one attribute to set values for.", call. = FALSE)
      if (!type %in% netcdf_data_types)
        stop("Invalid netCDF data type.", call. = FALSE)

      # Prepare values
      value <- unlist(value, use.names = FALSE)
      if (is.list(value) || is.array(value))
        stop("Unsupported value for attribute (compound list, matrix, array?).", call. = FALSE)
      if (is.character(value)) {
        if (type == "NC_STRING") len <- length(value)
        else if (type == "NC_CHAR") {
          value <- paste(value, sep = ", ")
          len <- nchar(value)
        } else stop("Wrong attribute type for string value.", call. = FALSE)
      } else {
        if (is.logical(value)) value <- as.integer(value)
        if (is.numeric(value)) len <- length(value)
        else stop("Unsupported value for attribute.", call. = FALSE)
      }
      if (length(value) > 1L) value <- list(value)

      # Check if the name refers to an existing attribute
      if (nrow(self$attributes[self$attributes$name == name, ])) {
        # If so, replace its type and value
        self$attributes[self$attributes$name == name, ]$type <- type
        self$attributes[self$attributes$name == name, ]$length <- len
        self$attributes[self$attributes$name == name, ]$value <- value
      } else {
        # If not, create a new attribute
        if (!.is_valid_name(name))
          stop("Attribute name is not valid.", call. = FALSE)

        id <- max(self$attributes$id) + 1L
        self$attributes <- rbind(self$attributes,
          data.frame(id = id, name = name, type = type, length = len, value = value))
      }
      # FIXME: Flag that attributes have changed so that object is dirty
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
        if (nrow(self$attributes[self$attributes$name == name, ])) {
          self$attributes[self$attributes$name == name, ]$value <-
            if (prepend)
              paste0(value, sep, self$attributes[self$attributes$name == name, ]$value)
            else
              paste0(self$attributes[self$attributes$name == name, ]$value, sep, value)
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
      self$attributes <- self$attributes[!self$attributes$name == name, ]
      invisible(self)
    },

    #' @description Write the attributes of this object to a netCDF file.
    #' @param nc The handle to the netCDF file opened for writing.
    #' @param nm The NC variable name or "NC_GLOBAL" to write the attributes to.
    #' @return Self, invisibly.
    write_attributes = function(nc, nm) {
      for (a in 1L:nrow(self$attributes)) {
        attr <- self$attributes[a,]
        RNetCDF::att.put.nc(nc, nm, attr$name, attr$type, unlist(attr$value))
      }
      invisible(self)
    }
  )
)

