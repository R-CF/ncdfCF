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
    .CFobjects = list(),

    # NetCDF group where this variable is located.
    .group = NULL,

    # The netCDF data type of this variable. This could be the packed type.
    # Don't check this field but use the appropriate method in the class of the
    # object whose data type you are looking for.
    .vtype = NULL,

    # Vector of dimension identifiers that this variable uses.
    .dimids  = NULL,

    # Additional properties for a `netcdf4` resource.
    .ncdf4 = NULL
  ),
  public = list(
    #' @description Create a new netCDF variable. This class should not be
    #'   instantiated directly, they are created automatically when opening a
    #'   netCDF resource.
    #'
    #' @param id Numeric identifier of the netCDF object.
    #' @param name Character string with the name of the netCDF object.
    #' @param group The [NCGroup] this variable is located in.
    #' @param vtype The netCDF data type of the variable.
    #' @param dimids The identifiers of the dimensions this variable uses.
    #' @param attributes Optional, `data.frame` with the attributes of the
    #'   object.
    #' @param netcdf4 Optional, `netcdf4`-specific arguments in the format of
    #'   RNetCDF.
    #' @return An instance of this class.
    initialize = function(id, name, group, vtype, dimids, attributes = data.frame(), netcdf4 = list()) {
      super$initialize(id, name, attributes)
      private$.group <- group
      private$.vtype <- vtype
      if (!is.na(dimids[1L]))
        private$.dimids <- dimids

      if (length(netcdf4))
        private$.ncdf4 <- netcdf4

      # FIXME: Must be NCGroup method: group$append(self)
      # Add self to the group
      group$NCvars <- append(group$NCvars, setNames(list(self), name))
    },

    #' @description Summary of the NC variable printed to the console.
    #' @param ... Passed on to other methods.
    print = function(...) {
      cat("<netCDF variable> [", self$id, "] ", self$name, "\n", sep = "")
      cat("Group        :", private$.group$fullname, "\n")
      cat("Data type    :", private$.vtype, "\n")
      cat("Dimension ids:", paste(private$.dimids, collapse = ", "), "\n")

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

    #' @description Detach the passed object from this NC variable.
    #' @param obj The CFObject instance to detach from this NC variable.
    #' @return `obj`, invisibly.
    detach = function(obj) {
      cf <- lapply(private$.CFobjects, function(o) if (o$id == obj$id) NULL else o)
      private$.CFobjects <- cf[lengths(cf) > 0L]
      invisible(obj)
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
      RNetCDF::var.get.nc(private$.group$handle, private$.name, start, count, collapse = FALSE, unpack = TRUE, fitnum = TRUE)
    },

    #' @description Get the [NCDimension] object(s) that this variable uses.
    #' @param id The index of the dimension. If missing, all dimensions of this
    #'   variable are returned.
    #' @return A `NCDimension` object or a list thereof. If no `NCDimension`s
    #'   were found, return `NULL`.
    dimension = function(id) {
      if (missing(id)) {
        lapply(private$.dimids, function(did) private$.group$find_dim_by_id(did))
      } else {
        private$.group$find_dim_by_id(private$.dimids[id])
      }
    },

    #' @description The lengths of the data dimensions of this object.
    #' @param dimension Optional. The index of the dimension to retrieve the
    #' length for. If omitted, retrieve the lengths of all dimensions.
    dim = function(dimension) {
      if (missing(dimension)) {
        dims <- self$dimension()
        sapply(dims, function(d) d$length)
      } else
        self$dimension(dimension)$length
    }
  ),
  active = list(
    #' @field group (read-only) NetCDF group where this variable is located.
    group = function(value) {
      if (missing(value))
        private$.group
      else {
        stop("Cannot set the group of a NC object.", call. = FALSE) # nocov
      }
    },

    #' @field vtype (read-only) The netCDF data type of this variable. This could be the
    #' packed type. Don't check this field but use the appropriate method in the
    #' class of the object whose data type you are looking for.
    vtype = function(value) {
      if (missing(value))
        private$.vtype
      else
        stop("Cannot set the data type of a NC object.", call. = FALSE) # nocov
    },

    #' @field ndims (read-only) Number of dimensions that this variable uses.
    ndims = function(value) {
      if (missing(value))
        length(private$.dimids)
      else
        stop("Cannot set the number of dimensions of a NC object.", call. = FALSE) # nocov
    },

    #' @field dimids (read-only) Vector of dimension identifiers that this
    #'   NCVariable uses.
    dimids  = function(value) {
      if (missing(value))
        private$.dimids
      else
        stop("Cannot set the dimids of a NC object.", call. = FALSE) # nocov
    },

    #' @field netcdf4 (read-only) Additional properties for a `netcdf4` resource.
    netcdf4 = function(value) {
      if (missing(value))
        private$.ncdf4
      else
        stop("Cannot set the netcdf4 properties of a NC object.", call. = FALSE) # nocov
    },

    #' @field CF Register CF object that uses this netCDF variable, or retrieve
    #' the list of registered CF objects.
    CF = function(value) {
      if (missing(value))
        private$.CFobjects
      else if (inherits(value, "CFObject"))
        private$.CFobjects[[value$fullname]] <- value
      else
        warning("Can only reference an object descending from `CFObject` from an `NCVariable`", call. = FALSE) # nocov
    },

    #' @field fullname (read-only) Name of this netCDF variable including the
    #'   group path from the root group.
    fullname = function(value) {
      if (missing(value)) {
        g <- private$.group$fullname
        if (g == "/") paste0("/", self$name)
        else paste0(g, "/", self$name)
      }
    }
  )
)
