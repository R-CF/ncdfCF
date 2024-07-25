#' NetCDF group
#'
#' @description This class represents a netCDF group, the object that holds
#'   elements like dimensions and variables of a netCDF file. Additionally, the
#'   group also holds references to any CF objects based on the netCDF elements
#'   held by the group.
#'
#' @details Direct access to groups is usually not necessary. The principal
#'   objects held by the group, CF data variables and axes, are accessible via
#'   other means. Only for access to the group attributes is a reference to a
#'   group required.
#'
#' @field resource Access to the underlying netCDF resource.
#' @field fullname The fully qualified absolute path of the group.
#' @field parent Parent group of this group, `NULL` for the root group.
#' @field subgroups List of child `NCGroup`s of this group.
#' @field NCvars List of netCDF variables that are located in this group.
#' @field NCdims List of netCDF dimensions that are located in this group.
#' @field NCUDTs List of netCDF user-defined types that are located in this
#'   group.
#' @field CFvars List of CF data variables in this group. There must be a
#'   corresponding item in `NCvars` for each item in this list.
#' @field CFaxes List of axes of CF data variables in this group. There must be
#'   a corresponding item in `NCvars` for each item in this list. Note that the
#'   CF data variable(s) that an axis is associated with may be located in a
#'   different group. Also, objects that further describe the basic axis
#'   definition, such as its bounds, labels, ancillary data, may be located in a
#'   different group; all such elements can be accessed directly from the
#'   [CFAxis] instances that this list holds.
#' @docType class
#'
#' @name NCGroup
#' @format An \code{\link{R6Class}} generator object.
NULL

#' @export
NCGroup <- R6::R6Class("NCGroup",
  inherit = NCObject,
  public = list(
    resource  = NULL,
    fullname  = "",
    parent    = NULL,
    subgroups = list(), # of NCGroup
    NCvars    = list(), # of NCVariable
    NCdims    = list(), # of NCDimension
    NCudts    = list(), # list of lists of UDTs in RNetCDF format
    CFvars    = list(), # of CFVariable
    CFaxes    = list(), # of CFAxis

    #' @noRd
    initialize = function(id, name, fullname, parent, resource) {
      super$initialize(id, name)
      self$fullname <- fullname
      self$parent <- parent
      self$resource <- resource
    },

    #' @description Summary of the group
    #'
    #' Prints a summary of the group to the console.
    print = function(...) {
      if (self$name != "/") {
        cat("<Group> [", self$id, "] ", self$name, "\n", sep = "")
        cat("Path      :", self$fullname, "\n")
      }
      if (length(self$subgroups) > 0L)
        cat("Subgroups :", paste(names(self$subgroups), collapse = ", "), "\n")

      self$print_attributes()
    },

    #' @description Group hierarchy
    #'
    #'   Prints the hierarchy of the group and its subgroups to the console,
    #'   with a summary of contained objects. Usually called from the root group
    #'   to display the full group hierarchy.
    hierarchy = function(idx, total) {
      if (idx == total) sep <- "   " else sep <- "|  "
      hier <- paste0("* ", self$name, "\n")

      # Dimensions
      if (length(self$NCdims) > 0L) {
        d <- paste(sapply(self$NCdims, function(d) d$shard()), collapse = ", ")
        hier <- c(hier, paste0(sep, "Dimensions: ", d, "\n"))
      }

      # Variables
      if (length(self$NCvars) > 0L) {
        v <- paste(sapply(self$NCvars, function(v) v$shard()), collapse = ", ")
        hier <- c(hier, paste0(sep, "Variables : ", v, "\n"))
      }

      # Subgroups
      subs <- length(self$subgroups)
      if (subs > 0L) {
        sg <- unlist(sapply(1L:subs, function(g) self$subgroups[[g]]$hierarchy(g, subs)))
        hier <- c(hier, paste0(sep, sg))
      }
      hier
    },

    #' Find an object by its name
    #'
    #' Given the name of an object, possibly preceded by an absolute or
    #' relative group path, return the object to the caller. Typically, this
    #' method is called programmatically; interactive use is provided through
    #' the `[[.CFDataset()` function.
    #'
    #' @param name The name of an object, with an optional absolute or relative
    #' group path from the calling group.
    #' @param scope Either "CF" (default) for a CF data variable, axis or other
    #' construct, or "NC" for a netCDF dimension or variable.
    #'
    #' @returns The object with the provided name in the requested scope. If the
    #' object is not found, returns `NULL`.
    find_by_name = function(name, scope = "CF") {
      grp <- self
      elements <- strsplit(name[1L], "/", fixed = TRUE)[[1L]]
      parts <- length(elements)

      # Normalize the grp and elements such that the latter are below the grp
      if (!nzchar(elements[1L])) { # first element is empty string: absolute path
        elements <- elements[-1L]
        while (grp$name != "/")
          grp <- grp$parent
      } else {
        dotdot <- which(elements == "..")
        dotdots <- length(dotdot)
        if (dotdots > 0L) {
          if (range(dotdot)[2L] > dotdots)
            stop("Malformed group path:", name[1L])
          for (i in seq_len(dotdots))
            grp <- grp$parent
          elements <- elements[-dotdot]
        }
      }

      # Traverse down the groups until 1 element is left
      if (length(elements) > 1L)
        for (i in 1L:(length(elements) - 1L)) {
          grp <- grp$subgroups[[ elements[i] ]]
          if (is.null(grp))
            stop("Path not found in the resource:", name[1L])
        }

      nm <- elements[length(elements)]

      # Helper function to find a named object in the current group, either an
      # CF or NC object, depending on the `scope` argument
      .find_here <- function(g) {
        if (scope == "CF") {
          idx <- which(names(g$CFvars) == nm)
          if (length(idx)) return(g$CFvars[[idx]])

          idx <- which(names(g$CFaxes) == nm)
          if (length(idx)) return(g$CFaxes[[idx]])
        } else {
          idx <- which(names(g$NCvars) == nm)
          if (length(idx)) return(g$NCvars[[idx]])
        }

        NULL
      }

      # Find the object in the current group
      obj <- .find_here(grp)
      if (!is.null(obj)) return(obj)

      # If the named object was not qualified, search higher groups
      if (parts == 1L)
        while (grp$name != "/") {
          grp <- grp$parent
          obj <- .find_here(grp)
          if (!is.null(obj)) return(obj)
        }

      # Give up
      NULL
    },

    #' List the CF data variables in this group
    #'
    #' This method lists the CF data variables located in this group,
    #' optionally including data variables in subgroups.
    #'
    #' @param recursive Should subgroups be scanned for CF data variables too
    #' (default is `TRUE`)?
    #'
    #' @returns A list of `CFVariable`.
    variables = function(recursive = TRUE) {
      if (recursive)
        subvars <- lapply(self$subgroups, function(g) g$variables(recursive))
      else subvars <- list()
      c(self$CFvars, subvars)
    },

    #' List the axes of CF data variables in this group
    #'
    #' This method lists the axes located in this group, optionally including
    #' axes in subgroups.
    #'
    #' @param recursive Should subgroups be scanned for axes too (default is
    #'   `TRUE`)?
    #'
    #' @returns A list of `CFAxis` descendants.
    axes = function(recursive = TRUE) {
      if (recursive)
        subaxes <- lapply(self$subgroups, function(g) g$axes(recursive))
      else subaxes <- list()
      c(self$CFaxes, subaxes)
    }
  ),
  active = list(
    #' @field handle Get the handle to the netCDF resource for the group
    #'
    #' @returns The netCDF handle. This is the file-level handle for the root
    #' group and the group handle for subgroups.
    handle = function(value) {
      if (missing(value))
        self$resource$group_handle(self$fullname)
      else
        stop("Can't assign a value to a netCDF resource handle", call. = FALSE)
    }
  )
)
