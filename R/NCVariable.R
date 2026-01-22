# A NCVariable maps 1 variable from a netCDF resource. It is always synchronized
# with the resource. It is mostly immutable so any changes made to a referencing
# CF object should lead to a detach of the latter from this NCVariable.
#
# Properties:
# var.def.nc    - NCVariable - Comments
# return value  - .id        - NetCDF assigned, immutable
# ncfile        - .group     - The .group has a handle which is same as ncfile
# varname       - .name      - May be changed with var.rename.nc()
# vartype       - .vtype     - Immutable
# dimensions    - .dimids    - Immutable
# chunking      - .ncdf4     - Not used
# chunksizes    - .ncdf4     - Not used
# deflate       - .ncdf4     - Not used
# shuffle       - .ncdf4     - Not used
# big_endian    - .ncdf4     - Not used
# fletcher32    - .ncdf4     - Not used
# filter_id     - .ncdf4     - Not used
# filter_params - .ncdf4     - Not used
#
# The NCVariable class operates in the background and therefore does minimal to
# no verification of method arguments. It does not generate its own errors but
# will propagate errors from the RNetCDF package.

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
      if (is.na(id)) {
        # Create the NC variable in the group on file
        h <- group$handle
        id <- try(RNetCDF::var.def.nc(h, name, vtype, dimids), silent = TRUE) # FIXME netcdf4 attributes
        if (inherits(id, "try-error"))
          id <- RNetCDF::var.inq.nc(h, name)
      }

      super$initialize(id, name, attributes)
      private$.group <- group
      private$.vtype <- vtype
      if (!is.na(dimids[1L]))
        private$.dimids <- dimids

      if (length(netcdf4))
        private$.ncdf4 <- netcdf4

      group$append(self)
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
    #'   dimensions are maintained and data is always returned in its smallest
    #'   type.
    #'
    #' @param start A vector of indices specifying the element where reading
    #'   starts along each dimension of the data. When `NA`, all data are read
    #'   from the start of the array.
    #' @param count An integer vector specifying the number of values to read
    #'   along each dimension of the data. Any `NA` value in vector count
    #'   indicates that the corresponding dimension should be read from the
    #'   start index to the end of the dimension.
    #' @return An array, matrix or vector with the requested data, or an error
    #'   object.
    get_data = function(start = NA, count = NA) {
      d <- RNetCDF::var.get.nc(private$.group$handle, private$.name, start, count, collapse = FALSE, unpack = TRUE,
                               fitnum = !(private$.vtype %in% c("NC_INT64", "NCUINT64")))
      if (length(dim(d)) == 1L)
        dim(d) <- NULL
      d
    },

    #' @description Write (a chunk of) data to the netCDF file.
    #' @param d The data to write. This must have appropriate dimensions.
    #' @param start A vector of indices specifying the element where writing
    #'   starts along each dimension of the data. When `NA`, all data are
    #'   written from the start of the array.
    #' @param count An integer vector specifying the number of values to write
    #'   along each dimension of the data. Any `NA` value in vector count
    #'   indicates that the corresponding dimension should be written from the
    #'   start index to the end of the dimension.
    #' @param ... Other parameters passed on to `RNetCDF::var.put.nc()`.
    #' @return Self, invisibly.
    write_data = function(d, start = NA, count = NA, ...) {
      RNetCDF::var.put.nc(private$.group$handle, private$.name, d, start, count, ...)
      invisible(self)
    },

    #' @description Change the name of the NC variable. The new name must be
    #'   valid in the indicated group, it can not already exist in the group.
    #'   The netCDF file must be open for writing to change the name.
    #' @param new_name The new name for the NC variable
    #' @return Self, invisibly.
    set_name = function(new_name) {
      if (new_name != private$.name && is.null(group$find_by_name(new_name)) &&
          private$.resource$can_write) {
        RNetCDF::var.rename.nc(private$.resource$handle, private$.name, new_name)
        private$.name <- new_name
      }
      invisible(self)
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
    },

    #' @field handle (read-only) Get the handle to the netCDF resource for the
    #'   variable.
    handle = function(value) {
      if (missing(value))
        private$.group$handle
    },

    #' @field vtype (read-only) The netCDF data type of this variable. This could be the
    #' packed type. Don't check this field but use the appropriate method in the
    #' class of the object whose data type you are looking for.
    vtype = function(value) {
      if (missing(value))
        private$.vtype
    },

    #' @field ndims (read-only) Number of dimensions that this variable uses.
    ndims = function(value) {
      if (missing(value))
        length(private$.dimids)
    },

    #' @field dimids (read-only) Vector of dimension identifiers that this
    #'   NCVariable uses.
    dimids  = function(value) {
      if (missing(value))
        private$.dimids
    },

    #' @field netcdf4 (read-only) Additional properties for a `netcdf4` resource.
    netcdf4 = function(value) {
      if (missing(value))
        private$.ncdf4
    },

    #' @field CF Register CF objects that use this netCDF variable, or retrieve
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
    },

    #' @field is_packed (read-only) Flag that indicates if the data on file is
    #'   packed.
    is_packed = function(value) {
      if (missing(value)) {
        !is.na(self$attribute("scale_factor")) || !is.na(self$attribute("add_offset"))
      }
    }
  )
)
