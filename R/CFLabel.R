#' CF label object
#'
#' @description This class represent CF labels, i.e. a variable of character
#' type that provides a textual label for a discrete or general numeric axis.
#' See also [CFAxisCharacter], which is an axis with character labels.
#'
#' @docType class
#' @export
CFLabel <- R6::R6Class("CFLabel",
  inherit = CFObject,
  public = list(
    #' @description Create a new instance of this class.
    #' @param var The [NCVariable] instance upon which this CF object is based
    #'   when read from a netCDF resource, or the name for the object new CF
    #'   object to be created.
    #' @param values Optional. The labels of the CF object. Ignored when
    #'   argument `var` is a `NCVariable` object.
    #' @param start Optional. Integer index value indicating where to start
    #'   reading data from the file. The value may be `NA` (default) to read all
    #'   data, otherwise it must not be larger than the number of labels.
    #'   Ignored when argument `var` is not an `NCVariable` instance.
    #' @param count Optional. Integer value indicating the number of labels to
    #'   read from file. The value may be `NA` to read to the end of the labels,
    #'   otherwise its value must agree with the corresponding `start` value and
    #'   the number of labels on file. Ignored when argument `var` is not an
    #'   `NCVariable` instance.
    #' @return A `CFLabel` instance.
    initialize = function(var, values = NA, start = NA, count = NA) {
      super$initialize(var, values = values, start = start, count = count)
      if (is.null(values) || (length(values) == 1L && is.na(values)))
        private$read_data()
    },

    #' @description  Prints a summary of the labels to the console.
    #' @param ... Arguments passed on to other functions. Of particular interest
    #' is `width = ` to indicate a maximum width of attribute columns.
    print = function(...) {
      cat("<Label set> ", self$name, "\n", sep = "")

      longname <- self$attribute("long_name")
      if (!is.na(longname) && longname != self$name)
        cat("Long name:", longname, "\n")

      cat("Length   :", self$length, "\n")
      cat("Data type:", self$data_type, "\n")
      self$print_attributes(...)
    },

    #' @description Given a range of domain coordinate values, returns the
    #'   indices into the axis that fall within the supplied range.
    #' @param rng A character vector whose extreme (alphabetic) values indicate
    #'   the indices of coordinates to return.
    #' @return An integer vector of length 2 with the lower and higher indices
    #'   into the axis that fall within the range of coordinates in argument
    #'   `rng`. Returns `NULL` if no values of the axis fall within the range of
    #'   coordinates.
    slice = function(rng) {
      res <- range(match(rng, self$coordinates, nomatch = 0L), na.rm = TRUE)
      if (all(res == 0L)) NULL
      else if (res[1L] == 0L) c(res[2L], res[2L])
      else if (res[2L] == 0L) c(res[1L], res[1L])
      else res
    },

    #' @description Retrieve a subset of the labels.
    #' @param name The name for the new label set, optional.
    #' @param rng The range of indices whose values from this axis to include in
    #'   the returned axis.
    #' @return A `CFLabel` instance, or `NULL` if the `rng` values are invalid.
    subset = function(name, rng) {
      if (missing(name)) name <- self$name
      rng <- range(rng)
      if (rng[1L] < 1L || rng[2L] > self$length)
        NULL
      else {
        if (self$has_resource) {
          l <- CFLabel$new(private$.NCvar, values = private$.values[rng[1L]:rng[2L]], start = rng[1L], count = rng[2L] - rng[1L] + 1L)
          l$name <- name
          l
        } else
          CFLabel$new(name, values = self$coordinates[rng[1L]:rng[2L]])
      }
    },

    #' @description Write the labels to a netCDF file, including its attributes.
    #' @param nc The handle of the netCDF file opened for writing or a group in
    #'   the netCDF file. If `NULL`, write to the file or group where the labels
    #'   were read from (the file must have been opened for writing). If not
    #'   `NULL`, the handle to a netCDF file or a group therein.
    #' @param dim The dimension to write the labels for. This is the name or
    #'   dimension id of the object that owns these labels. The dimension must
    #'   have been written to file before this method is called.
    #' @return Self, invisibly.
    write = function(nc, dim) {
      h <- if (inherits(nc, "NetCDF")) nc else self$NCvar$handle

      self$id <- RNetCDF::var.def.nc(h, self$name, private$.data_type, dim)
      self$write_attributes(h, self$name)
      RNetCDF::var.put.nc(h, self$name, self$coordinates)
    }
  ),
  active = list(
    #' @field friendlyClassName (read-only) A nice description of the class.
    friendlyClassName = function(value) {
      if (missing(value))
        "Label set"
    },

    #' @field coordinates Set or retrieve the labels of this object. Upon
    #'   retrieval, label values are read from the netCDF resource, if there is
    #'   one, upon first access and cached thereafter. Upon setting values, if
    #'   there is a linked netCDF resource, this object will be detached from
    #'   it.
    coordinates = function(value) {
      if (missing(value)) {
        if (is.null(private$.values)) {
          vals <- private$read_data()
          if (!is.null(vals)) {
            dim(vals) <- NULL
            private$set_values(trimws(vals))
          }
        }
        private$.values
      } else {
        private$set_values(value)
        self$detach()
      }
    },

    #' @field length (read-only) The number of labels in the set.
    length = function(value) {
      if (missing(value)) {
        if (!is.null(private$.values))
          length(private$.values)
        else self$dim(1L)
      }
    },


    #' @field dimid (read-only) The netCDF dimension id of this label set. This
    #'   field should only be accessed if the label set is backed by a netCDF
    #'   resource.
    dimid = function(value) {
      if (missing(value)) {
        if (self$has_resource) private$.NCvar$dimids[1L]
        else -1L
      }
    }
  )
)
