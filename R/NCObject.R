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
  private = list(
    # Numeric identifier of the netCDF object.
    id_ = -1L,

    # The name of the netCDF object.
    name_ = "",

    # A `data.frame` with the attributes of the netCDF object.
    atts = data.frame()
  ),
  public = list(
    #' @description Create a new netCDF object. This class should not be
    #'   instantiated directly, create descendant objects instead.
    #'
    #' @param id Numeric identifier of the netCDF object.
    #' @param name Character string with the name of the netCDF object.
    initialize = function(id, name) {
      private$id_ <- id
      self$name <- name
    },

    #' @description This function prints the attributes of the netCDF object to
    #'   the console. Through object linkages, this also applies to the CF data
    #'   variables and axes, which each link to a netCDF object.
    #'
    #' @param width The maximum width of each column in the `data.frame` when
    #'   printed to the console.
    print_attributes = function(width = 50L) {
      if (nrow(private$atts)) {
        cat("\nAttributes:\n")
        print(.slim.data.frame(private$atts[-1L], width), right = FALSE, row.names = FALSE)
      }
    },

    #' @description This method returns an attribute of a netCDF object.
    #' @param att Attribute name whose value to return.
    #' @param field The field of the attribute to return values from. This must
    #'   be "value" (default) or "type".
    #' @return If the `field` argument is "type", a character string. If `field`
    #'   is "value", a single value of the type of the attribute, or a vector
    #'   when the attribute has multiple values. If no attribute is named with a
    #'   value of argument `att` `NA` is returned.
    attribute = function(att, field = "value") {
      if (length(att) > 1L)
        stop("Can extract only one attribute at a time.", call. = FALSE)

      atts <- private$atts
      if (!nrow(atts)) return(NA)
      val <- atts[atts$name == att, ]
      if (!nrow(val)) return(NA)

      val[[field]][[1L]]
    },

    #' @description Add an attribute. If an attribute `name` already exists, it
    #'   will be overwritten.
    #' @param name The name of the attribute. The name must begin with a letter
    #'   and be composed of letters, digits, and underscores, with a maximum
    #'   length of 255 characters. UTF-8 characters are not supported in
    #'   attribute names.
    #' @param type The type of the attribute, as a string value of a netCDF data
    #'   type or a user-defined type.
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
      if (type != "NC_CHAR") value <- list(value)

      # Check if the name refers to an existing attribute
      if (nrow(private$atts) && nrow(private$atts[private$atts$name == name, ])) {
        # If so, replace its type and value
        private$atts[private$atts$name == name, ]$type <- type
        private$atts[private$atts$name == name, ]$length <- len
        private$atts[private$atts$name == name, ]$value <- value
      } else {
        # If not, create a new attribute
        if (!.is_valid_name(name))
          stop("Attribute name is not valid.", call. = FALSE)

        id <- if (nrow(private$atts)) max(private$atts$id) + 1L else 0L
        df <- data.frame(id = id, name = name, type = type, length = len)
        df$value <- value # Preserve lists
        private$atts <- rbind(private$atts, df)
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
        if (nrow(private$atts[private$atts$name == name, ])) {
          new_val <- if (prepend)
                       paste0(value, sep, private$atts[private$atts$name == name, ]$value)
                     else
                       paste0(private$atts[private$atts$name == name, ]$value, sep, value)
          private$atts[private$atts$name == name, ]$value <- new_val
          private$atts[private$atts$name == name, ]$length <- nchar(new_val)
        } else # else create a new attribute
          self$set_attribute(name, "NC_STRING", value)
      }

      invisible(self)
    },

    #' @description Delete attributes. If an attribute `name` is not present
    #' this method simply returns.
    #' @param name Vector of names of the attributes to delete.
    #' @return Self, invisibly.
    delete_attribute = function(name) {
      private$atts <- private$atts[!private$atts$name %in% name, ]
      invisible(self)
    },

    #' @description Write the attributes of this object to a netCDF file.
    #' @param nc The handle to the netCDF file opened for writing.
    #' @param nm The NC variable name or "NC_GLOBAL" to write the attributes to.
    #' @return Self, invisibly.
    write_attributes = function(nc, nm) {
      if ((num_atts <- nrow(private$atts)) > 0L)
        for (a in 1L:num_atts) {
          attr <- private$atts[a,]
          RNetCDF::att.put.nc(nc, nm, attr$name, attr$type, unlist(attr$value, use.names = FALSE))
        }
      invisible(self)
    },

    #' @description Add names of axes to the "coordinates" attribute, avoiding
    #' duplicates and retaining previous values.
    #' @param crds Vector of axis names to add to the attribute.
    #' @return Self, invisibly.
    add_coordinates = function(crds) {
      current <- private$atts[private$atts$name == "coordinates", ]
      if (nrow(current)) {
        # There is a "coordinates" attribute already so append values
        new_val <- paste(unique(c(strsplit(current[[1L, "value"]], " ")[[1L]], crds)), collapse = " ")
        private$atts[private$atts$name == "coordinates", ]$value <- new_val
        private$atts[private$atts$name == "coordinates", ]$length <- nchar(new_val)
      } else
        # Make a new "coordinates" attribute
        self$set_attribute("coordinates", "NC_CHAR", paste(crds, collapse = " "))
      invisible(self)
    }
  ),
  active = list(
    #' @field id Set or retrieve the identifier of the netCDF object. In
    #'   general, the `id` value is immutable so it should never be set to a new
    #'   value. Setting the `id` value is only useful when writing a CF object
    #'   to a new netCDF file and this is managed by the respective NC objects
    #'   once a new `id` value is reported by the `netcdf` library.
    id = function(value) {
      if (missing(value))
        private$id_
      else
        private$id_ <- value
    },

    #' @field name Set or retrieve the name of the object. Note that the name
    #'   must comply with CF requirements: start with a letter, followed by
    #'   lettters, numbers or underscores, and having a maximum length of 255
    #'   characters. Multi-byte characters are not allowed in names.
    name = function(value) {
      if (missing(value))
        private$name_
      else if (.is_valid_name(value))
        private$name_ <- value
      else stop("Invalid name for NC object", call. = FALSE)
    },

    #' @field attributes Read or set the attributes of the object. The
    #'   attributes are stored in a `data.frame` with columns "id" (integer),
    #'   "name" (character), "type" (one of the netCDF data types), "length"
    #'   (integer), and "value" (any allowed type). When setting the attributes,
    #'   all existing attributes are deleted; use method `set_attribute()` to
    #'   add attributes to the existing set. Upon reading, when there are no
    #'   attributes, an empty `data.frame` will be returned.
    attributes = function(value) {
      if (missing(value)) {
        private$atts
      } else if (is.data.frame(value) && nrow(value)) {
        req <- c("id", "name", "type", "length", "value")
        cols <- names(value)
        if (all(req %in% cols)) {
          if (is.numeric(value$id) && is.character(value$name) &&
              is.character(value$type) && is.numeric(value$length))
            private$atts <- value[req]
          else
            warning("Attributes to be set have columns with wrong mode.", call. = FALSE)
        } else
          warning("Cannot set attributes without all required columns", call. = FALSE)
      }
    }
  )
)

