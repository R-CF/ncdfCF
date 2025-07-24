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
#'
NCVariable <- R6::R6Class("NCVariable",
  inherit = NCObject,
  private = list(
    # List of CF objects that reference this NCVariable. Typically there is just
    # one reference but there could be more (e.g. terms for several parametric
    # vertical axes).
    CFobjects = list()
  ),
  public = list(
    #' @field group NetCDF group where this variable is located.
    group = NULL,

    #' @field vtype The netCDF data type of this variable. This could be the
    #' packed type. Don't check this field but use the appropriate method in the
    #' class of the object whose data type you are looking for.
    vtype = NULL,

    #' @field ndims Number of dimensions that this variable uses.
    ndims = -1L,

    #' @field dimids Vector of dimension identifiers that this variable uses.
    #'   These are the so-called "NUG coordinate variables".
    dimids  = NULL,

    #' @field netcdf4 Additional properties for a `netcdf4` resource.
    netcdf4 = NULL,

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
    #' @return An instance of this class.
    initialize = function(id, name, group, vtype, ndims, dimids) {
      if (group$has_name(name))
        stop(paste0("Object with name '", name, "' already exists in the group."), call. = FALSE)

      super$initialize(id, name)
      self$group <- group
      self$vtype <- vtype
      self$ndims <- ndims
      self$dimids <- dimids
    },

    #' @description Summary of the NC variable printed to the console.
    #' @param ... Passed on to other methods.
    print = function(...) {
      cat("<netCDF variable> [", self$id, "] ", self$name, "\n", sep = "")
      cat("Group        :", self$group$fullname, "\n")
      cat("Data type    :", self$vtype, "\n")
      cat("Dimension ids:", paste(self$dimids, collapse = ", "), "\n")

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
      if (length(self$dimids))
        sum(self$dimids > -1L)
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
      RNetCDF::var.get.nc(self$group$handle, self$name, start, count, collapse = FALSE, unpack = TRUE, fitnum = TRUE)
    }
  ),
  active = list(
    #' @field CF List of CF objects that use this netCDF variable.
    CF = function(value) {
      if (missing(value))
        private$CFobjects
      else {
        if (inherits(value, "CFObject"))
          private$CFobjects[[value$name]] <- value
        else
          warning("Can only reference an object descending from `CFObject` from an `NCVariable`", call. = FALSE)
      }
    },

    #' @field fullname (read-only) Name of the NC variable including the group
    #' path from the root group.
    fullname = function(value) {
      if (missing(value)) {
        g <- self$group$fullname
        if (g == "/") paste0("/", self$name)
        else paste0(g, "/", self$name)
      }
    }
  )
)
