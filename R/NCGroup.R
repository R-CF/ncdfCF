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
#' @field CFaux List of auxiliary variables. These could be [CFAxisScalar] or
#'   [CFAuxiliaryLongLat] that hold longitude and latitude values for every grid
#'   point in the data variable that references them.
#' @docType class
#' @name NCGroup
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
    CFaux     = list(), # of CFAuxiliaryLongLat
    CFcrs     = list(), # of CFGridMapping

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
        cat("<", self$friendlyClassName, "> [", self$id, "] ", self$name, "\n", sep = "")
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

      # Axes
      if (length(self$CFaxes) > 0L) {
        ax <- paste(sapply(self$CFaxes, function(x) x$shard()), collapse = ", ")
        hier <- c(hier, paste0(sep, "Axes     : ", ax, "\n"))
      }

      # Variables
      if (length(self$CFvars) > 0L) {
        vars <- lapply(self$CFvars, function(v) v$shard())
        vars <- unlist(vars[lengths(vars) > 0])
        v <- paste(vars, collapse = ", ")
        hier <- c(hier, paste0(sep, "Variables: ", v, "\n"))
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
    #' Given the name of an object, possibly preceded by an absolute or relative
    #' group path, return the object to the caller. Typically, this method is
    #' called programmatically; similar interactive use is provided through the
    #' `[[.CFDataset()` function.
    #'
    #' @param name The name of an object, with an optional absolute or relative
    #'   group path from the calling group. The object must either an CF
    #'   construct (data variable, axis, auxiliary axis, or grid mapping) or an
    #'   NC group, dimension or variable.
    #' @param scope Either "CF" (default) for a CF construct, or "NC" for a
    #'   netCDF group, dimension or variable.
    #'
    #' @return The object with the provided name in the requested scope. If the
    #'   object is not found, returns `NULL`.
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

      # Helper function to find a named object in the group `g`, either an
      # CF or NC object, depending on the `scope` argument
      .find_here <- function(g) {
        if (scope == "CF") {
          idx <- which(names(g$CFvars) == nm)
          if (length(idx)) return(g$CFvars[[idx]])

          idx <- which(names(g$CFaxes) == nm)
          if (length(idx)) return(g$CFaxes[[idx]])

          idx <- which(names(g$CFaux) == nm)
          if (length(idx)) return(g$CFaux[[idx]])

          idx <- which(names(g$CFcrs) == nm)
          if (length(idx)) return(g$CFcrs[[idx]])
        } else {
          idx <- which(names(g$NCvars) == nm)
          if (length(idx)) return(g$NCvars[[idx]])

          idx <- which(names(g$NCdims) == nm)
          if (length(idx)) return(g$NCdims[[idx]])

          idx <- which(names(g$subgroups) == nm)
          if (length(idx)) return(g$subgroups[[idx]])
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

    #' Add an auxiliary long-lat variable to the group
    #'
    #' This method creates a [CFAuxiliaryLongLat] from the arguments and adds it
    #' to the group `CFaux` list, but only if the combination of `lon`, `lat`
    #' isn't already present.
    #'
    #' @param lon,lat Instances of [NCVariable] having a two-dimensional grid of
    #' longitude and latitude values, respectively.
    #' @param bndsLong,bndsLat Instances of [CFBounds] with the 2D bounds of the
    #' longitude and latitude grid values, respectively, or `NULL` when not set.
    #'
    #' @return `self` invisibly.
    addAuxiliaryLongLat = function(lon, lat, bndsLong, bndsLat) {
      nm <- paste(lon$name, lat$name, sep = "_")
      if (!length(self$CFaux)) {
        self$CFaux <- list(CFAuxiliaryLongLat$new(lon, lat, bndsLong, bndsLat))
        names(self$CFaux) <- nm
      } else {
        known <- lapply(self$CFaux, function(a) c(a$varLong$id, a$varLat$id))
        if (!any(sapply(known, function(k) k[1L] == lon$id && k[2L] == lat$id)))
          self$CFaux[[nm]] <- CFAuxiliaryLongLat$new(lon, lat, bndsLong, bndsLat)
      }
      invisible(self)
    },

    #' Fully qualified name of the group
    #'
    #' This method lists the fully qualified name of this group,
    #' optionally including names in subgroups.
    #'
    #' @param recursive Should subgroups be scanned for names too
    #' (default is `TRUE`)?
    #'
    #' @return A character vector with group names.
    fullnames = function(recursive = TRUE) {
      if (recursive && length(self$subgroups))
        c(self$fullname, sapply(self$subgroups, function(g) g$fullnames(recursive)))
      else self$fullname
    },

    #' List the CF data variables in this group
    #'
    #' This method lists the CF data variables located in this group,
    #' optionally including data variables in subgroups.
    #'
    #' @param recursive Should subgroups be scanned for CF data variables too
    #' (default is `TRUE`)?
    #'
    #' @return A list of `CFVariable`.
    variables = function(recursive = TRUE) {
      if (recursive && length(self$subgroups))
        c(self$CFvars, unlist(lapply(self$subgroups, function(g) g$variables(recursive))))
      else self$CFvars
    },

    #' List the axes of CF data variables in this group
    #'
    #' This method lists the axes located in this group, optionally including
    #' axes in subgroups.
    #'
    #' @param recursive Should subgroups be scanned for axes too (default is
    #'   `TRUE`)?
    #'
    #' @return A list of `CFAxis` descendants.
    axes = function(recursive = TRUE) {
      if (recursive && length(self$subgroups))
        subaxes <- lapply(self$subgroups, function(g) g$axes(recursive))
      else subaxes <- list()
      c(self$CFaxes, unlist(subaxes))
    },

    #' List the grid mappings of CF data variables in this group
    #'
    #' This method lists the grid mappings located in this group, optionally
    #' including grid mappings in subgroups.
    #'
    #' @param recursive Should subgroups be scanned for grid mappings too
    #'   (default is `TRUE`)?
    #'
    #' @return A list of `CFGridMapping` instances.
    grid_mappings = function(recursive = TRUE) {
      if (recursive && length(self$subgroups))
        subgm <- lapply(self$subgroups, function(g) g$grid_mappings(recursive))
      else subgm <- list()
      c(self$CFcrs, unlist(subgm))
    }
  ),
  active = list(
    #' @field friendlyClassName (read-only) A nice description of the class.
    friendlyClassName = function(value) {
      if (missing(value))
        "Group"
    },

    #' @field handle Get the handle to the netCDF resource for the group
    handle = function(value) {
      if (missing(value))
        self$resource$group_handle(self$fullname)
      else
        stop("Can't assign a value to a netCDF resource handle", call. = FALSE)
    }
  )
)
