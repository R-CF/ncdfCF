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
  private = list(
    # Numeric identifier of the netCDF object.
    .id = -1L,

    # The name of the netCDF object.
    .name = "",

    # A `data.frame` with the attributes of the netCDF object.
    .attributes = data.frame()
  ),
  public = list(
    #' @description Create a new netCDF object. This class should not be
    #'   instantiated directly, create descendant objects instead.
    #'
    #' @param id Numeric identifier of the netCDF object.
    #' @param name Character string with the name of the netCDF object.
    #' @param attributes Optional, `data.frame` with attributes of the object.
    initialize = function(id, name, attributes = data.frame()) {
      private$.id <- id
      private$.name <- name
      private$.attributes <- attributes
    },

    #' @description This function prints the attributes of the netCDF object to
    #'   the console.
    #' @param width The maximum width of each column in the `data.frame` when
    #'   printed to the console.
    print_attributes = function(width = 50L) {
      if (nrow(private$.attributes)) {
        cat("\nAttributes:\n")
        print(.slim.data.frame(private$.attributes[-1L], width), right = FALSE, row.names = FALSE)
      }
    },

    #' @description Retrieve an attribute of a NC object.
    #'
    #' @param att Single character string of attribute to return.
    #' @param field The field of the attribute to return values from. This must
    #'   be "value" (default) or "type".
    #' @return If the `field` argument is "type", a character string. If `field`
    #'   is "value", a single value of the type of the attribute, or a vector
    #'   when the attribute has multiple values. If no attribute is named with a
    #'   value of argument `att` `NA` is returned.
    attribute = function(att, field = "value") {
      if (length(att) > 1L)
        stop("Can extract only one attribute at a time.", call. = FALSE)

      atts <- private$.attributes
      if (!nrow(atts)) return(NA)
      val <- atts[atts$name == att, ]
      if (!nrow(val)) return(NA)

      val[[field]][[1L]]
    },

    #' @description Write the attributes of this object to a netCDF file.
    #' @param nc The handle to the netCDF file opened for writing.
    #' @param nm The NC variable name or "NC_GLOBAL" to write the attributes to.
    #' @return Self, invisibly.
    write_attributes = function(nc, nm) {
      if ((num_atts <- nrow(private$.attributes)) > 0L)
        for (a in 1L:num_atts) {
          attr <- private$.attributes[a,]
          RNetCDF::att.put.nc(nc, nm, attr$name, attr$type, unlist(attr$value, use.names = FALSE))
        }
      invisible(self)
    }
  ),
  active = list(
    #' @field id (read-only) Retrieve the identifier of the netCDF object.
    id = function(value) {
      if (missing(value))
        private$.id
    },

    #' @field name (read-only) Retrieve the name of the object.
    name = function(value) {
      if (missing(value))
        private$.name
    },

    #' @field attributes (read-only) Read the attributes of the object. When
    #'   there are no attributes, an empty `data.frame` will be returned.
    attributes = function(value) {
      if (missing(value))
        private$.attributes
    #   else if (is.null(value))
    #     private$.attributes <- data.frame()
    #   else if (is.data.frame(value) && nrow(value)) {
    #     req <- c("id", "name", "type", "length", "value")
    #     cols <- names(value)
    #     if (all(req %in% cols)) {
    #       if (is.numeric(value$id) && is.character(value$name) &&
    #           is.character(value$type) && is.numeric(value$length))
    #         private$.attributes <- value[req]
    #       else
    #         warning("Attributes to be set have columns with wrong mode.", call. = FALSE)
    #     } else
    #       warning("Cannot set attributes without all required columns", call. = FALSE)
    #   }
    }
  )
)

