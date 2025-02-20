#' CF group in memory
#'
#' @description This class represents a CF group in memory. It descends from
#'   [NCGroup] and functions as such with the exception that it has no
#'   associated `CFResource` and the `handle` field thus always returns `NULL`.
#'
#' @docType class
VirtualGroup <- R6::R6Class("VirtualGroup",
  inherit = NCGroup,
  public = list(
    #' @description Create an instance of this class.
    #' @param id The identifier of the group.
    #' @param name The name of the group.
    #' @param fullname The fully qualified name of the group.
    #' @param parent The parent group of this group. The `parent` of the root
    #'   group is `NULL`.
    initialize = function(id, name, fullname, parent) {
      super$initialize(id, name, fullname, parent, NULL)
    }
  ),
  active = list(
    #' @field friendlyClassName (read-only) A nice description of the class.
    friendlyClassName = function(value) {
      if (missing(value))
        "Group (memory)"
    },

    #' @field handle Return NULL as a VirtualGroup has no resource.
    handle = function(value) {
      if (missing(value))
        NULL
      else
        stop("Can't assign a value to a netCDF resource handle", call. = FALSE)
    }
  )
)
