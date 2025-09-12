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

    # The values of this object. This class does not manipulate the values in
    # any way - that is the job of descendant classes.
    .values = NULL,

    # List of start and count vectors for reading data from file. The length of
    # the vectors must agree with the array on file or be `NA`.
    .start_count = list(start = NA, count = NA),

    # The data type of the data in the object. Taken from NCvar if set,
    # otherwise the descending class must set it explicitly when receiving its
    # data values.
    .data_type = "NC_NAT", # Not a type

    # Flag to indicate if the data on disk is too large for memory.
    .data_oversized = FALSE,

    # The attributes of the CF object. Upon read from a netCDF resource they are
    # copied from the NCVariable. For a new CF object, just an empty data.frame
    # with the appropriate columns.
    .attributes = data.frame(name = character(0), type = character(0), length = integer(0), value = numeric(0)),

    # Sanitize the attributes argument. Returns a safe data.frame to use.
    check_attributes = function(attributes) {
      if (!is.data.frame(attributes))
        stop("Argument `attributes` must be a valid data.frame.", call. = FALSE) # nocov

      if (!nrow(attributes))
        return(data.frame(name = character(0), type = character(0), length = integer(0), value = numeric(0)))

      # FIXME: Ensure that the appropriate columns and types are present

      attributes
    },

    # Sanitize the start and count values. NAs are converted to numbers and
    # values have to agree with .values and .NCvar. Returns the sanitized start
    # and count vectors as a list.
    check_start_count = function(start, count) {
      d <- as.integer(self$dim())
      if (!length(d)) # When a NC variable does not have any data
        return(list(start = NA, count = NA))

      if (length(start) == 1L && is.na(start))
        start <- rep(1L, length(d))
      else
        start[is.na(start)] <- 1L

      if (any(start > d))
        stop("Start values cannot be larger than the dimensions of the data.", call. = FALSE) # nocov
      if (length(count) == 1L && is.na(count))
        count <- d - start + 1L
      else {
        ndx <- which(is.na(count))
        count[ndx] <- d[ndx] - start[ndx] + 1L
      }

      if (any(count > d - start + 1L))
        stop("Count values cannot extend beyond the dimensions of the data.", call. = FALSE) # nocov

      list(start = start, count = count)
    },

    # Set the values of the object. Perform some basic checks when set programmatically.
    # Values may be NULL
    set_values = function(values) {
      # FIXME: Check that dim(values) agrees with NCvar and data_type

      # Check if we can find a vtype from the NCvar, possibly packed
      if (!is.null(private$.NCvar)) {
        private$.data_type <- private$.NCvar$attribute("scale_factor", "type")
        if (is.na(private$.data_type))
          private$.data_type <- private$.NCvar$attribute("add_offset", "type")
        if (is.na(private$.data_type))
          private$.data_type <- private$.NCvar$vtype
        else
          # Data is packed in the netCDF file: throw away the attributes and let
          # RNetCDF deal with unpacking when reading the data using the
          # attributes in the NCVariable.
          self$delete_attribute(c("_FillValue", "scale_factor", "add_offset",
                                  "valid_range", "valid_min", "valid_max",
                                  "missing_value"))
      } else if (!is.null(values)) {
        if (storage.mode(values) == "double") {
          # If the data is numeric, check attributes to select between NC_DOUBLE and NC_FLOAT
          if (!is.na(dt <- self$attribute("_FillValue", "type"))) private$.data_type <- dt
          else if (!is.na(dt <- self$attribute("missing_value", "type"))) private$.data_type <- dt
          else private$.data_type <- "NC_DOUBLE"
        } else {
          # Get the data_type from the values
          private$.data_type <- switch(storage.mode(values),
                                        "character" = "NC_STRING",
                                        "integer" = "NC_INT",
                                        "logical" = "NC_SHORT",
                                        stop("Unsupported data type for a CF object.", call. = FALSE))
        }
      } else
        private$.data_type <- "NC_NAT"

      # Set the actual_range attribute for the values
      if (is.null(values))
        self$delete_attribute("actual_range")
      else {
        rng <- range(values, na.rm = TRUE)
        if (is.na(rng[1L]))
          self$delete_attribute("actual_range")
        else {
          if (is.numeric(rng))
            rng <- round(rng, CF.options$digits)
          self$set_attribute("actual_range", private$.data_type, rng)
        }
      }

      private$.values <- values
    },

    # Read the data of the CF object from file. The data is cached by `self` so
    # repeated calls do not access the netCDF resource, unless argument
    # `refresh` is `TRUE`.
    # This method will not assess how big the data is before reading it so there
    # is a chance that memory will be exhausted. The calling code should check
    # for this possibility and break up the reading of data into chunks.
    # @param refresh Should the data be read from file if the object is linked?
    #   This will replace current values, if previously loaded. Default `FALSE`.
    # @return An array of data, invisibly, as prescribed by the `start` and
    #   `count` values used to create this object. If the object is not backed
    #   by a netCDF resource, returns `NULL`.
    read_data = function(refresh = FALSE) {
      if ((!is.null(private$.NCvar)) && (is.null(private$.values) || refresh))
        private$set_values(private$.NCvar$get_data(private$.start_count$start, private$.start_count$count))
      invisible(private$.values)
    },

    # Read a chunk of data of the CF object, as defined by the `start` and
    # `count` vectors. Note that these vectors are relative to any subset of the
    # netCDF data variable that this CF object refers to. The data read by this
    # method will not be stored in `self` so the calling code must take a
    # reference to it.
    # @param start Vector of indices where to start reading data along the
    #   dimensions of the array. The vector must be `NA` to read all data,
    #   otherwise it must have agree with the dimensions of the array.
    # @param count Vector of number of elements to read along each dimension of
    #   the array on file. The vector must be `NA` to read to the end of each
    #   dimension, otherwise its value must agree with the corresponding `start`
    #   value and the dimension of the array.
    # @return An array of data, as prescribed by the `start` and `count`
    #   arguments, or `NULL` if there is no data.
    read_chunk = function(start, count) {
      sc <- private$check_start_count(start, count)
      if (is.na(sc$start[1L])) return(NULL)

      if (!is.null(private$.values)) {
        # Extract from loaded data
        cll <- paste0("private$.values[", paste(sc$start, ":", sc$start + sc$count - 1L, sep = "", collapse = ", "), "]")
        eval(parse(text = cll))
      } else {
        # Read from the netCDF resource
        start <- private$.start_count$start + sc$start - 1L
        private$.NCvar$get_data(start, sc$count)
      }
    },

    # Make sure we detach before we poof out.
    finalize = function() {
      if (!is.null(private$.NCvar))
        private$.NCvar$detach(self)
    }
  ),
  public = list(
    #' @description Create a new `CFobject` instance from a variable in a netCDF
    #'   resource. This method is called upon opening a netCDF resource. It is
    #'   rarely, if ever, useful to call this constructor directly. Instead, use
    #'   the methods from higher-level classes such as [CFVariable].
    #'
    #' @param var The [NCVariable] instance upon which this CF object is based
    #'   when read from a netCDF resource, or the name for the new CF object to
    #'   be created.
    #' @param values Optional. The values of the object in an array.
    #' @param start Optional. Vector of indices where to start reading data
    #'   along the dimensions of the array on file. The vector must be `NA` to
    #'   read all data, otherwise it must have agree with the dimensions of the
    #'   array on file. Ignored when argument `var` is not an `NCVariable`
    #'   instance.
    #' @param count Optional. Vector of number of elements to read along each
    #'   dimension of the array on file. The vector must be `NA` to read to the
    #'   end of each dimension, otherwise its value must agree with the
    #'   corresponding `start` value and the dimension of the array on file.
    #'   Ignored when argument `var` is not an `NCVariable` instance.
    #' @param attributes Optional. A `data.frame` with the attributes of the
    #'   object. When argument `var` is an `NCVariable` instance and this
    #'   argument is an empty `data.frame` (default), arguments will be read
    #'   from the resource.
    #' @return A `CFObject` instance.
    initialize = function(var, values, start = 1L, count = NA, attributes = data.frame()) {
      atts <- private$check_attributes(attributes)

      if (is.character(var)) {
        if (!.is_valid_name(var))
          stop("Name is not valid for a CF object.", call. = FALSE) # nocov
        private$.name <- var
        private$.id <- CF$newVarId()
        private$.attributes <- atts
      } else {
        var$CF <- self
        private$.NCvar <- var
        private$.id <- var$id
        private$.name <- var$name
        private$.start_count <- private$check_start_count(start, count)
        private$.attributes <- if (nrow(attributes)) atts
                               else var$attributes[-1L]
      }

      if (missing(values) || (length(values) == 1L && is.na(values)))
        values <- NULL
      private$set_values(values)
    },

    #' @description Detach the current object from its underlying netCDF
    #'   resource. If necessary, data is read from the resource before
    #'   detaching.
    detach = function() {
      if (!is.null(private$.NCvar)) {
        if (is.null(private$.values))
          private$read_data()
        private$.NCvar$detach(self)
        private$.NCvar <- NULL
      }
    },

    #' @description Retrieve the dimensions of the data of this object. This
    #' could be for the data on file or for in-memory data.
    #' @param dimension Optional. The index of the dimension to retrieve the
    #' length for. If omitted, retrieve the lengths of all dimensions.
    #' @return Integer vector with the length of each requested dimension.
    dim = function(dimension) {
      len <- if (!is.null(private$.values)) {
        d <- dim(private$.values)
        if (length(d) == 0L) length(private$.values) else d
      } else if (self$has_resource) {
        if (length(private$.start_count$count) > 1L || !is.na(private$.start_count$count))
          private$.start_count$count - private$.start_count$start + 1L
        else
          private$.NCvar$dim()
      } else
        return(NULL)

      if (missing(dimension))
        len
      else
        len[dimension]
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
        stop("Can extract only one attribute at a time.", call. = FALSE) # nocov

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
    print_attributes = function(width = 30L) {
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

    #' @description Delete attributes. If an attribute `name` is not present
    #' this method simply returns.
    #' @param name Vector of names of the attributes to delete.
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

    #' @description Add names of axes or auxiliary coordinates to the
    #'   "coordinates" attribute, avoiding duplicates and retaining previous
    #'   values.
    #' @param crds Vector of axis or auxiliary coordinate names to add to the
    #'   attribute.
    #' @return Self, invisibly.
    update_coordinates_attribute = function(crds) {
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

    #' @field id (read-only) Retrieve the identifier of the CF object.
    id = function(value) {
      if (missing(value))
        private$.id
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

        grp <- self$group
        if (is.null(grp))
          private$.name
        else {
          if ((gnm <- grp$name) == "/")
            private$.name
          else
            paste0("/", gnm, "/", private$.name)
        }
      }
    },

    #' @field group (read-only) Retrieve the [NCGroup] that this object is
    #'   located in.
    group = function(value) {
      if (missing(value) && self$has_resource)
        private$.NCvar$group
      else
        NULL
    },

    #' @field attributes (read-only) Retrieve a `data.frame` with the attributes
    #'   of the CF object.
    attributes = function(value) {
      if (missing(value))
        private$.attributes
    },

    #' @field has_resource (read-only) Flag that indicates if this object has an
    #'   underlying netCDF resource.
    has_resource = function(value) {
      !is.null(private$.NCvar)
    },

    #' @field NCvar (read-only) The `NCVariable` object that links to an
    #'   underlying netCDF resource, or `NULL` if not linked.
    NCvar = function(value) {
      if (missing(value))
        private$.NCvar
    },

    #' @field data_type Set or retrieve the data type of the data in the
    #'   object. Setting the data type to a wrong value can have unpredictable
    #'   but catastrophic consequences.
    data_type = function(value) {
      if (missing(value))
        private$.data_type
      else if (self$has_resource)
        stop("Cannot set the data type of a variable present on file.", call. = FALSE) # nocov
      else if (value %in% netcdf_data_types)
        private$.data_type <- value
      else
        stop("Unrecognized data type for a netCDF variable.", call. = FALSE) # nocov
    },

    #' @field ndims (read-only) Retrieve the dimensionality of the data in the
    #'   array, or the netCDF resource.
    ndims = function(value) {
      if (missing(value))
        length(self$dim())
    },

    #' @field array_indices Returns a list with columns "start" and "count"
    #'   giving the indices for reading the data of this object from a netCDF
    #'   resource.
    array_indices = function(value) {
      if (missing(value))
        private$.start_count
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
#' * `CFAxisDiscrete`: The index values of the axis, either along the entire
#'   axis, or a portion thereof.
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

