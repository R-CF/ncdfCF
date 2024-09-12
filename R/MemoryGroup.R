#' CF group in memory
#'
#' @description This class represents a CF group in memory. It descends from
#'   [NCGroup] and functions as such with the exception that it has no
#'   associated `CFResource` and the `handle` field thus always returns `NULL`.
#'
#' @details This object descends from [NCGroup] and functions as such with the
#'   exception that it has no associated `CFResource` and the `handle` field
#'   thus always returns `NULL`.
#'
#' @docType class
#' @name MemoryGroup
NULL

#' @export
MemoryGroup <- R6::R6Class("MemoryGroup",
  inherit = NCGroup,
  public = list(
    #' @description Create an instance of this class.
    #' @param id The identifier of the group.
    #' @param name The name of the group.
    #' @param fullname The fully qualified name of the group.
    #' @param parent The parent group of this group. The `parent` of the root
    #'   group is `NULL`.
    #' @param title,history Title and history attributes for the group.
    initialize = function(id, name, fullname, parent, title, history) {
      super$initialize(id, name, fullname, parent, NULL)

      self$attributes <- data.frame(id = 0L:1L,
                                    name = c("title", "history"),
                                    type = c("NC_CHAR", "NC_CHAR"),
                                    length = c(nchar(title), nchar(history)),
                                    value = c(title, history))
    }
  ),
  active = list(
    #' @field friendlyClassName (read-only) A nice description of the class.
    friendlyClassName = function(value) {
      if (missing(value))
        "Group (memory)"
    },

    #' @field handle Return NULL as a MemoryGroup has no resource.
    handle = function(value) {
      if (missing(value))
        NULL
      else
        stop("Can't assign a value to a netCDF resource handle", call. = FALSE)
    }
  )
)
