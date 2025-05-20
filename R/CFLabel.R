#' CF label object
#'
#' @description This class represent CF labels, i.e. an NC variable of character
#' type that provides a textual label for a discrete or general numeric axis.
#' See also [CFAxisCharacter], which is an axis with character labels.
#'
#' @docType class
#' @export
CFLabel <- R6::R6Class("CFLabel",
  inherit = CFObject,
  private = list(
    # The label values, a character vector.
    values = NULL,

    # Method to be consistent with CFAxis retrieval patterns.
    get_values = function() {
      private$values
    }
  ),
  public = list(
    #' @field NCdim The [NCDimension] that stores the netCDF dimension details.
    NCdim = NULL,

    #' @description Create a new instance of this class.
    #' @param nc_var The netCDF variable that describes this instance.
    #' @param nc_dim The netCDF dimension that describes the dimensionality.
    #' @param values Character vector of the label values.
    initialize = function(nc_var, nc_dim, values) {
      super$initialize(nc_var)
      self$NCdim <- nc_dim

      private$values <- values
      if(nc_var$vtype %in% c("NC_CHAR", "NC_STRING"))
        private$values <- trimws(private$values)

      nc_var$CF <- self
    },

    #' @description  Prints a summary of the labels to the console.
    #' @param ... Arguments passed on to other functions. Of particular interest
    #' is `width = ` to indicate a maximum width of attribute columns.
    #' @return `self`, invisibly.
    print = function(...) {
      cat("<Label set> ", self$name, "\n", sep = "")
      if (self$group$name != "/")
        cat("Group    :", self$group$fullname, "\n")

      longname <- self$attribute("long_name")
      if (!is.na(longname) && longname != self$name)
        cat("Long name:", longname, "\n")

      cat("Length   :", self$NCdim$length, "\n")
      cat("Data type:", self$NCvar$vtype, "\n")
      self$print_attributes(...)
    },

    #' @description Retrieve a subset of the labels.
    #' @param grp The group to create the new label object in.
    #' @param rng The range of indices to retrieve.
    #' @return A `CFLabel` instance, or `NULL` if the `rng` values are invalid.
    subset = function(grp, rng) {
      rng <- range(rng)
      if (rng[1L] < 1L || rng[2L] > length(private$values))
        NULL
      else {
        dim <- NCDimension$new(-1L, self$name, rng[2L] - rng[1L] + 1L, FALSE)
        var <- NCVariable$new(-1L, self$name, grp, "NC_STRING", 1L, NULL)
        CFLabel$new(grp, var, dim, private$values[rng[1L]:rng[2L]])
      }
    },

    #' @description Write the labels to a netCDF file, including its attributes.
    #' @param nc The handle of the netCDF file opened for writing or a group in
    #'   the netCDF file. If `NULL`, write to the file or group where the labels
    #'   were read from (the file must have been opened for writing). If not
    #'   `NULL`, the handle to a netCDF file or a group therein.
    #' @return Self, invisibly.
    write = function(nc) {
      # FIXME: Does this work with non-character labels? Conventions require NC_STRING or NC_CHAR
      h <- if (inherits(nc, "NetCDF")) nc else self$group$handle
      self$NCdim$write(h)
      RNetCDF::var.def.nc(h, self$name, self$NCvar$vtype, self$NCdim$name)
      self$write_attributes(h, self$name)
      RNetCDF::var.put.nc(h, self$name, private$values)
    }
  ),
  active = list(
    #' @field friendlyClassName (read-only) A nice description of the class.
    friendlyClassName = function(value) {
      if (missing(value))
        "Label set"
    },

    #' @field coordinates (read-only) The label set as a vector.
    coordinates = function(value) {
      if (missing(value))
        private$values
    },

    #' @field length (read-only) The number of labels in the set.
    length = function(value) {
      if (missing(value))
        self$NCdim$length
    },

    #' @field dimid (read-only) The netCDF dimension id of this label set.
    dimid = function(value) {
      if (missing(value))
        self$NCdim$id
    }
  )
)
