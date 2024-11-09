#' NetCDF user-defined type
#'
#' @description This class represents user-defined types in a netCDF file.
#'   Interpretation of the UDT typically requires knowledge of the data set or
#'   application.
#'
#' @docType class
#'
NCUDT <- R6::R6Class("NCUDT",
  inherit = NCObject,
  public = list(
    #' @field clss The class of the UDT, one of "builtin", "compound", "enum",
    #' "opaque", or "vlen".
    clss = "",

    #' @field size Size in bytes of a single item of the type (or a single
    #'   element of a "vlen").
    size = 0L,

    #' @field basetype Name of the netCDF base type of each element ("enum" and
    #' "vlen" only).
    basetype = "",

    #' @field value Named vector with numeric values of all members ("enum"
    #' only).
    value = NULL,

    #' @field offset Named vector with the offset of each field in bytes from
    #' the beginning of the "compound" type.
    offset = NULL,

    #' @field subtype Named vector with the netCDF base type name of each field
    #' of a "compound" type.
    subtype = NULL,

    #' @field dimsizes Named list with array dimensions of each field of a
    #' "compound" type. A `NULL` length indicates a scalar.
    dimsizes = NULL,

    #' @description Create a new netCDF user-defined type. This class represents
    #'   a user-defined type. It is instantiated when opening a netCDF resource.
    #'
    #' @param id Numeric identifier of the user-defined type.
    #' @param name Character string with the name of the user-defined type.
    #' @param clss The class of the UDT, one of "builtin", "compound", "enum",
    #'   "opaque", or "vlen".
    #' @param size Size in bytes of a single item of the type (or a single
    #'   element of a "vlen").
    #' @param basetype Name of the netCDF base type of each element ("enum" and
    #'   "vlen" only).
    #' @param value Named vector with numeric values of all members ("enum"
    #'   only).
    #' @param offset Named vector with the offset of each field in bytes from
    #'   the beginning of the "compound" type.
    #' @param subtype Named vector with the netCDF base type name of each field
    #'   of a "compound" type.
    #' @param dimsizes Named list with array dimensions of each field of a
    #'   "compound" type. A `NULL` length indicates a scalar.
    #'
    #' @return An instance of this class.
    initialize = function(id, name, clss, size, basetype, value, offset, subtype, dimsizes) {
      super$initialize(id, name)
      self$clss <- clss
      self$size <- size
      self$basetype <- basetype
      self$value <- value
      self$offset <- offset
      self$subtype <- subtype
      self$dimsizes <- dimsizes
    }
  )
)
