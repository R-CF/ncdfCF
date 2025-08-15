#' NetCDF variable
#'
#' @description This class represents a netCDF variable, the object that holds
#'   the properties and data of elements like dimensions and variables of a
#'   netCDF file.
#'
#' Direct access to netCDF variables is usually not necessary. NetCDF
#'   variables are linked from CF data variables and axes and all relevant
#'   properties are thus made accessible.
#'
#' @docType class
NCVariable <- R6::R6Class("NCVariable",
  inherit = NCObject,
  private = list(
    # List of CF objects that reference this NCVariable. Typically there is just
    # one reference but there could be more in the file (e.g. terms for several
    # parametric vertical axes) and derivative CF objects that do not modify the
    # data retain the link to self as well.
    CFobjects = list(),

    # NetCDF group where this variable is located.
    grp = NULL,

    # The netCDF data type of this variable. This could be the packed type.
    # Don't check this field but use the appropriate method in the class of the
    # object whose data type you are looking for.
    data_type = NULL,

    # Number of dimensions that this variable uses.
    nd = -1L,

    # Vector of dimension identifiers that this variable uses. These are the
    # so-called "NUG coordinate variables".
    dids  = NULL,

    # Additional properties for a `netcdf4` resource.
    ncdf4 = NULL
  ),
  public = list(
    # FIXME: Drop ndims: it follows from length of dimids

    #' @description Create a new netCDF variable. This class should not be
    #'   instantiated directly, they are created automatically when opening a
    #'   netCDF resource.
    #'
    #' @param id Numeric identifier of the netCDF object.
    #' @param name Character string with the name of the netCDF object.
    #' @param group The [NCGroup] this variable is located in.
    #' @param vtype The netCDF data type of the variable.
    #' @param ndims The number of dimensions this variable uses.
    #' @param dimids The identifiers of the dimensions this variable uses.
    #' @param attributes Optional, `data.frame` with the attributes of the
    #'   object.
    #' @param netcdf4 Optional, `netcdf4`-specific arguments in the format of
    #'   RNetCDF.
    #' @return An instance of this class.
    initialize = function(id, name, group, vtype, ndims, dimids, attributes = data.frame(), netcdf4 = list()) {
      if (group$has_name(name))
        stop(paste0("Object with name '", name, "' already exists in the group."), call. = FALSE)

      super$initialize(id, name, attributes)
      private$grp <- group
      private$data_type <- vtype
      private$nd <- ndims
      private$dids <- dimids

      if (length(netcdf4))
        private$ncdf4 <- netcdf4

      # FIXME: Must be NCGroup method: group$append(self)
      # Add self to the group
      l <- list(self)
      names(l) <- name
      group$NCvars <- append(group$NCvars, l)
    },

    #' @description Summary of the NC variable printed to the console.
    #' @param ... Passed on to other methods.
    print = function(...) {
      cat("<netCDF variable> [", self$id, "] ", self$name, "\n", sep = "")
      cat("Group        :", private$grp$fullname, "\n")
      cat("Data type    :", private$data_type, "\n")
      cat("Dimension ids:", paste(private$dids, collapse = ", "), "\n")

      self$print_attributes()
    },

    #' @description Very concise information on the variable. The information
    #'   returned by this function is very concise and most useful when combined
    #'   with similar information from other variables.
    #'
    #' @return Character string with very basic variable information.
    shard = function() {
      if (self$id > -1L) paste0("[", self$id, ": ", self$name, "]")
      else NULL
    },

    #' @description Return the number of dimensions of the array that this
    #'   variable holds in the netCDF file. It is thus not necessarily equal to
    #'   the number of axes of a CF data variable.
    #'
    #' @return Integer value with the number of dimensional axes.
    array_dims = function() {
      if (length(private$dids))
        sum(private$dids > -1L)
      else 0L
    },

    #' @description Read (a chunk of) data from the netCDF file. Degenerate
    #' dimensions are maintained and data is always returned in its smallest
    #' type.
    #'
    #' @param start A vector of indices specifying the element where reading
    #'   starts along each dimension of the data. When `NA`, all data are read
    #'   from the start of the array.
    #' @param count An integer vector specifying the number of values to read
    #'   along each dimension of the data. Any `NA` value in vector count
    #'   indicates that the corresponding dimension should be read from the
    #'   start index to the end of the dimension.
    #' @return An array with the requested data, or an error object.
    get_data = function(start = NA, count = NA) {
      RNetCDF::var.get.nc(private$grp$handle, self$name, start, count, collapse = FALSE, unpack = TRUE, fitnum = TRUE)
    }
  ),
  active = list(
    #' @field group (read-only) NetCDF group where this variable is located.
    group = function(value) {
      if (missing(value))
        private$grp
      else {
        #browser()
        #stop("Cannot set the group of a NC object.", call. = FALSE)
      }
    },

    #' @field vtype (read-only) The netCDF data type of this variable. This could be the
    #' packed type. Don't check this field but use the appropriate method in the
    #' class of the object whose data type you are looking for.
    vtype = function(value) {
      if (missing(value))
        private$data_type
      else
        stop("Cannot set the data type of a NC object.", call. = FALSE)
    },

    #' @field ndims (read-only) Number of dimensions that this variable uses.
    ndims = function(value) {
      if (missing(value))
        private$nd
      else
        stop("Cannot set the number of dimensions of a NC object.", call. = FALSE)
    },

    #' @field dimids (read-only) Vector of dimension identifiers that this variable uses.
    #'   These are the so-called "NUG coordinate variables".
    dimids  = function(value) {
      if (missing(value))
        private$dids
      else
        stop("Cannot set the dimids of a NC object.", call. = FALSE)
    },

    #' @field netcdf4 (read-only) Additional properties for a `netcdf4` resource.
    netcdf4 = function(value) {
      if (missing(value))
        private$ncdf4
      else
        stop("Cannot set the netcdf4 properties of a NC object.", call. = FALSE)
    },

    #' @field CF Register CF object that uses this netCDF variable, or retrieve
    #' the list of registered CF objects.
    CF = function(value) {
      if (missing(value))
        private$CFobjects
      else {
        if (inherits(value, "CFObject")) {
          nm <- value$fullname
          if (is.null(nm)) nm <- value$name
          private$CFobjects[[nm]] <- value
        } else
          warning("Can only reference an object descending from `CFObject` from an `NCVariable`", call. = FALSE)
      }
    },

    #' @field fullname (read-only) Name of the NC variable including the group
    #' path from the root group.
    fullname = function(value) {
      if (missing(value)) {
        g <- private$grp$fullname
        if (g == "/") paste0("/", self$name)
        else paste0(g, "/", self$name)
      }
    }
  )
)
