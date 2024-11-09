#' NetCDF base object
#'
#' @description This class is a basic ancestor to all classes that represent
#'   netCDF objects, specifically groups, dimensions, variables and the
#'   user-defined types in a netCDF file. More useful classes use this class as
#'   ancestor.
#'
#'   The fields in this class are common among all netCDF objects. In addition,
#'   this class manages the attributes for its descendent classes.
#'
#' @docType class
#'
NCObject <- R6::R6Class("NCObject",
  public = list(
    #' @field id Numeric identifier of the netCDF object.
    id         = -1L,

    #' @field name The name of the netCDF object.
    name       = "",

    #' @field attributes `data.frame` with the attributes of the netCDF object.
    attributes = data.frame(),

    #' @description Create a new netCDF object. This class should not be
    #'   instantiated directly, create descendant objects instead.
    #'
    #' @param id Numeric identifier of the netCDF object.
    #' @param name Character string with the name of the netCDF object.
    initialize = function(id, name) {
      self$id <- id
      self$name <- name
    },

    #' @description This function prints the attributes of the netCDF object to
    #'   the console. Through object linkages, this also applies to the CF data
    #'   variables and axes, which each link to a netCDF object.
    #'
    #' @param width The maximum width of each column in the `data.frame` when
    #'   printed to the console.
    print_attributes = function(width = 50L) {
      if (nrow(self$attributes)) {
        cat("\nAttributes:\n")
        print(.slim.data.frame(self$attributes, width), right = FALSE, row.names = FALSE)
      }
    },

    #' @description This method returns netCDF object attributes.
    #'
    #' @param att Vector of attribute names whose values to return.
    #' @param field The field of the attributes to return values from. This must
    #'   be "value" (default), "type" or "length".
    #' @return If the `field` argument is "type" or "length", a character vector
    #'   named with the `att` values that were found in the attributes. If
    #'   argument `field` is "value", a list with elements named with the `att`
    #'   values, containing the attribute value(s), except when argument `att`
    #'   names a single attribute, in which case that attribute value is
    #'   returned as a character string. If no attribute is named with a value
    #'   of argument `att` an empty list is returned, or an empty string if
    #'   there was only one value in argument `att`.
    attribute = function(att, field = "value") {
      num <- length(att)
      atts <- self$attributes
      if (!nrow(atts)) return(if (num == 1L) "" else list())
      val <- atts[atts$name %in% att, ]
      if (!nrow(val)) return(if (num == 1L) "" else list())

      out <- val[[field]]
      names(out) <- val[["name"]]
      if (num == 1L && field == "value") out <- val$value[[1L]]
      out
    }
  )
)

