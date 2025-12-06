#' CF data object
#'
#' @description This class is a basic ancestor to all classes that contain data
#'   from a netCDF resource, specifically data variables and axes. More useful
#'   classes use this class as ancestor.
#'
#' @docType class
CFData <- R6::R6Class("CFData",
  inherit = CFObject,
  cloneable = FALSE,
  private = list(
    # The values of this object. This class does not manipulate the values in
    # any way - that is the job of descendant classes.
    .values = NULL,

    # List of start and count vectors for reading data from file. The length of
    # the vectors must agree with the array on file or be `NA`.
    .start_count = list(start = NA, count = NA),

    # The data type of the data in the object. Taken from .NCobj if set,
    # otherwise the descending class must set it explicitly when receiving its
    # data values.
    .data_type = "NC_NAT", # Not a type

    # Flag to indicate if the data on disk is too large for memory.
    .data_oversized = FALSE,

    # Sanitize the start and count values. NAs are converted to numbers and
    # values have to agree with .values and .NCobj. Returns the sanitized start
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
      if (!is.null(private$.NCobj)) {
        private$.data_type <- private$.NCobj$attribute("scale_factor", "type")
        if (is.na(private$.data_type))
          private$.data_type <- private$.NCobj$attribute("add_offset", "type")
        if (is.na(private$.data_type))
          private$.data_type <- private$.NCobj$vtype
        else
          # Data is packed in the netCDF file: throw away the attributes and let
          # RNetCDF deal with unpacking when reading the data using the
          # attributes in the NCVariable.
          self$delete_attribute(c("scale_factor", "add_offset", "valid_range", "valid_min", "valid_max"))
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
      if ((!is.null(private$.NCobj)) && (is.null(private$.values) || refresh))
        private$set_values(private$.NCobj$get_data(private$.start_count$start, private$.start_count$count))
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
        private$.NCobj$get_data(start, sc$count)
      }
    }
  ),
  public = list(
    #' @description Create a new `CFobject` instance from a variable in a netCDF
    #'   resource. This method is called upon opening a netCDF resource. It is
    #'   rarely, if ever, useful to call this constructor directly. Instead, use
    #'   the methods from higher-level classes such as [CFVariable].
    #' @param obj The [NCVariable] instance upon which this CF object is based
    #'   when read from a netCDF resource, or the name for the new CF object to
    #'   be created.
    #' @param values Optional. The values of the object in an array.
    #' @param start Optional. Vector of indices where to start reading data
    #'   along the dimensions of the array on file. The vector must be `NA` to
    #'   read all data, otherwise it must have agree with the dimensions of the
    #'   array on file. Ignored when argument `obj` is not an `NCVariable`
    #'   instance.
    #' @param count Optional. Vector of number of elements to read along each
    #'   dimension of the array on file. The vector must be `NA` to read to the
    #'   end of each dimension, otherwise its value must agree with the
    #'   corresponding `start` value and the dimension of the array on file.
    #'   Ignored when argument `obj` is not an `NCVariable` instance.
    #' @param attributes Optional. A `data.frame` with the attributes of the
    #'   object.
    #' @return A `CFData` instance.
    initialize = function(obj, values, start = 1L, count = NA, attributes = data.frame()) {
      super$initialize(obj, attributes)

      if (inherits(obj, "NCObject"))
        private$.start_count <- private$check_start_count(start, count)

      if (missing(values) || (length(values) == 1L && is.na(values)))
        values <- NULL
      private$set_values(values)
    },

    #' @description Detach the current object from its underlying netCDF
    #'   resource. If necessary, data is read from the resource before
    #'   detaching.
    detach = function() {
      if (!is.null(private$.NCobj) && is.null(private$.values))
        private$read_data()
      super$detach()
    },

    #' @description Retrieve the dimensions of the data of this object. This
    #'   could be for the data on file or for in-memory data.
    #' @param dimension Optional. The index of the dimension to retrieve the
    #'   length for. If omitted, retrieve the lengths of all dimensions.
    #' @return Integer vector with the length of each requested dimension.
    dim = function(dimension) {
      len <- if (!is.null(private$.values)) {
        d <- dim(private$.values)
        if (length(d) == 0L) length(private$.values) else d
      } else if (self$has_resource) {
        if (length(private$.start_count$count) > 1L || !is.na(private$.start_count$count))
          private$.start_count$count - private$.start_count$start + 1L
        else
          private$.NCobj$dim()
      } else
        return(NULL)

      if (missing(dimension))
        len
      else
        len[dimension]
    }

  ),
  active = list(
    #' @field data_type Set or retrieve the data type of the data in the
    #'   object. Setting the data type to a wrong value can have unpredictable
    #'   and mostly catastrophic consequences.
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
