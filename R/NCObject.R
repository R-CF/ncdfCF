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

    #' @description Write the attributes of this object to a netCDF file. This
    #'   will retain existing attributes, update modified attributes, and delete
    #'   and add missing attributes from the passed in argument.
    #' @param nm The NC variable name or "NC_GLOBAL" to write the attributes to.
    #' @param new_atts The attributes to write.
    #' @param force Force overwrite of existing attributes. Default is `FALSE`.
    #' @return Self, invisibly.
    write_attributes = function(nm, new_atts, force = FALSE) {
      old_atts <- private$.attributes
      h <- self$handle

      if (force) {
        # Delete all existing attributes
        if (nrow(old_atts)) {
          names <- old_atts$name
          for (a in seq_along(names))
            try(RNetCDF::att.delete.nc(h, nm, names[a]), silent = TRUE)
        }

        # Write all new attributes
        if (nrow(new_atts)) {
          names <- new_atts$name
          for (a in seq_along(names))
            att <- new_atts[new_atts$name == names[a],]
            RNetCDF::att.put.nc(h, nm, names[a], att$type, unlist(att$value, use.names = FALSE))
        }
      } else {
        # Delete attributes
        names <- old_atts$name[!(old_atts$name %in% new_atts$name)]
        names <- names[!(names %in% c("_FillValue", "missing_value", "add_offset",
                                      "scale_factor", "coordinates",
                                      "valid_range", "valid_min", "valid_max",
                                      "external_variables"))]
        for (a in seq_along(names))
          RNetCDF::att.delete.nc(h, nm, names[a])

        # Changed and new attributes
        names <- old_atts$name[old_atts$name %in% new_atts$name]
        names <- unlist(sapply(names, function(n) {
          if (!identical(old_atts[old_atts$name == n, "value"], new_atts[new_atts$name == n, "value"])) n else ""
        }))
        names <- c(names, new_atts$name[!(new_atts$name %in% old_atts$name)])
        for (a in seq_along(names))
          if (nzchar(n <- names[a])) {
            att <- new_atts[new_atts$name == n,]
            RNetCDF::att.put.nc(h, nm, n, att$type, unlist(att$value, use.names = FALSE))
          }
      }
      private$.attributes <- new_atts
      invisible(self)
    }
  ),
  active = list(
    #' @field id (read-only) Retrieve the identifier of the netCDF object.
    id = function(value) {
      if (missing(value))
        private$.id
    },

    #' @field name Set or retrieve the name of the NC object. The netCDF file
    #' must be open for writing to change the name.
    name = function(value) {
      if (missing(value))
        private$.name
      else
        self$set_name(value)
    },

    #' @field attributes (read-only) Read the attributes of the object. When
    #'   there are no attributes, an empty `data.frame` will be returned.
    attributes = function(value) {
      if (missing(value))
        private$.attributes
    },

    #' @field CF Register CF object that uses this netCDF object, or retrieve
    #' the list of registered CF objects.
    CF = function(value) {
      if (missing(value))
        private$.CFobjects
      else if (inherits(value, "CFObject"))
        private$.CFobjects[[value$fullname]] <- value
      else
        warning("Can only reference an object descending from `CFObject` from an NC object", call. = FALSE) # nocov
    }
  )
)

