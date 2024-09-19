#' NetCDF base object
#'
#' @description This class is a basic ancestor to all classes that represent
#'   netCDF objects, specifically groups, dimensions, variables and the
#'   user-defined types in a netCDF file. More useful classes use this class as
#'   ancestor.
#'
#' @details The fields in this class are common among all netCDF objects. In
#' addition, this class manages the attributes for its implementing classes.
#'
#' @docType class
#'
#' @name NCObject
#' @format An \code{\link{R6Class}} generator object.
NULL

NCObject <- R6::R6Class("NCObject",
  public = list(
    #' @field id Numeric identifier of the netCDF object.
    id         = -1L,

    #' @field name The name of the netCDF object.
    name       = "",

    #' @field attributes `data.frame` with the attributes of the netCDF object.
    attributes = data.frame(),

    #' Create a new netCDF object
    #'
    #' This class should not be instantiated directly, create descendant objects
    #' instead.
    #'
    #' @param id Numeric identifier of the netCDF object.
    #' @param name Character string with the name of the netCDF object.
    initialize = function(id, name) {
      self$id <- id
      self$name <- name
    },

    #' Print the attributes of the netCDF object
    #'
    #' This function prints the attributes of the netCDF object to the console.
    #' Through object linkages, this also applies to the CF data variables and
    #' axes, which each link to a netCDF object.
    #'
    #' @param width The maximum width of each column in the `data.frame` when
    #' printed to the console.
    print_attributes = function(width = 50) {
      if (nrow(self$attributes)) {
        cat("\nAttributes:\n")
        print(.slim.data.frame(self$attributes, width), right = FALSE, row.names = FALSE)
      }
    },

    #' Attributes of a netCDF object
    #'
    #' This method returns netCDF object attributes.
    #'
    #' @param att Vector of attribute names whose values to return.
    #' @param field The field of the `data.frame` to return values from. This
    #' must be "value" (default), "type" or "length".
    #' @return A vector of values from the `data.frame`, named with the `att`
    #' value, or an empty string (`""`) if the attribute is not found.
    attribute = function(att, field = "value") {
      atts <- self$attributes
      if (!nrow(atts)) return("")
      val <- atts[atts$name %in% att, ]
      if (!nrow(val))
        return("")

      if (field == "value")
        out <- unlist(lapply(val[field], function(a) {
          if (is.list(a)) paste0(a, collapse = ", ")
          else a
        }))
      else
        out <- unlist(val[field])

      names(out) <- val[["name"]]
      out
    }
  )
)

