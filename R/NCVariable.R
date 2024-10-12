#' NetCDF variable
#'
#' @description This class represents a netCDF variable, the object that holds
#'   the properties and data of elements like dimensions and variables of a
#'   netCDF file.
#'
#' @details Direct access to netCDF variables is usually not necessary. NetCDF
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
    group   = NULL,

    #' @field vtype The netCDF data type of this variable.
    vtype   = NULL,

    #' @field ndims Number of dimensions that this variable uses.
    ndims   = -1L,

    #' @field dimids Vector of dimension identifiers that this variable uses. These
    #' are the so-called "NUG coordinate variables".
    dimids  = NULL,

    #' @field netcdf4 Additional properties for a `netcdf4` resource.
    netcdf4 = NULL,

    #' Create a new netCDF variable
    #'
    #' This class should not be instantiated directly, they are created
    #' automatically when opening a netCDF resource.
    #'
    #' @param id Numeric identifier of the netCDF object.
    #' @param name Character string with the name of the netCDF object.
    #' @param group The [NCGroup] this variable is located in.
    #' @param vtype The netCDF data type of the variable.
    #' @param ndims The number of dimensions this variable uses.
    #' @param dimids The identifiers of the dimensions this variable uses.
    #' @return An instance of this class.
    initialize = function(id, name, group, vtype, ndims, dimids) {
      super$initialize(id, name)
      self$group <- group
      self$vtype <- vtype
      self$ndims <- ndims
      self$dimids <- dimids
    },

    #' @description Very concise information on the variable
    #'
    #' The information returned by this function is very concise and most useful
    #' when combined with similar information from other variables.
    #'
    #' @return Character string with very basic variable information.
    shard = function() {
      if (self$id > -1L) paste0("[", self$id, ": ", self$name, "]")
      else NULL
    }
  ),
  active = list(
    #' @field CF List of CF objects that uses this netCDF variable.
    CF = function(value) {
      if (missing(value))
        private$CFobjects
      else {
        private$CFobjects[[value$name]] <- value
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
