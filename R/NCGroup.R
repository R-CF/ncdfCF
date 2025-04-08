#' NetCDF group
#'
#' @description This class represents a netCDF group, the object that holds
#'   elements like dimensions and variables of a netCDF file. This class also
#'   holds references to any CF objects based on the netCDF elements held by the
#'   group.
#'
#'   Direct access to groups is usually not necessary. The principal objects
#'   held by the group, CF data variables and axes, are accessible via other
#'   means. Only for access to the group attributes is a reference to a group
#'   required.
#'
#' @docType class
NCGroup <- R6::R6Class("NCGroup",
  inherit = NCObject,
  public = list(
    #' @field resource Access to the underlying netCDF resource. This can be
    #' `NULL` for instances created in memory.
    resource  = NULL,

    #' @field fullname The fully qualified absolute path of the group.
    fullname  = "",

    #' @field parent Parent group of this group, the owning `CFDataset` for the
    #'   root group.
    parent    = NULL,

    #' @field subgroups List of child `NCGroup` instances of this group.
    subgroups = list(),

    #' @field NCvars List of netCDF variables that are located in this group.
    NCvars    = list(),

    #' @field NCdims List of netCDF dimensions that are located in this group.
    NCdims    = list(),

    #' @field NCudts List of netCDF user-defined types that are located in this
    #'   group.
    NCudts    = list(),

    #' @field CFvars List of CF data variables in this group. There must be a
    #'   corresponding item in `NCvars` for each item in this list.
    CFvars    = list(),

    #' @field CFaxes List of axes of CF data variables in this group. There must
    #'   be a corresponding item in `NCvars` for each item in this list. Note
    #'   that the CF data variable(s) that an axis is associated with may be
    #'   located in a different group. Also, objects that further describe the
    #'   basic axis definition, such as its bounds, labels, ancillary data, may
    #'   be located in a different group; all such elements can be accessed
    #'   directly from the [CFAxis] instances that this list holds.
    CFaxes    = list(),

    #' @field CFaux List of auxiliary coordinates located in this group. These
    #' could be [CFLabel] instances or an axis.
    CFaux  = list(),

    #' @field CFlonglat List of [CFAuxiliaryLongLat] that hold longitude and
    #'   latitude values for every grid point in the data variable that
    #'   references them.
    CFlonglat = list(),

    #' @field CFmeasures List of cell measures variables in this group.
    CFmeasures = list(),

    #' @field CFcrs List of grid mappings located in this group.
    CFcrs = list(),

    #' @description Create a new instance of this class.
    #' @param id The identifier of the group.
    #' @param name The name of the group.
    #' @param fullname The fully qualified name of the group.
    #' @param parent The parent group of this group. `NULL` for the root group.
    #' @param resource Reference to the [CFResource] instance that provides
    #' access to the netCDF resource. For in-memory groups this can be `NULL`.
    initialize = function(id, name, fullname, parent, resource) {
      super$initialize(id, name)
      self$fullname <- fullname
      self$parent <- parent
      self$resource <- resource
    },

    #' @description Summary of the group printed to the console.
    #' @param stand_alone Logical to indicate if the group should be printed as
    #' an object separate from other objects (`TRUE`, default), or print as part
    #' of an enclosing object (`FALSE`).
    #' @param ... Passed on to other methods.
    print = function(stand_alone = TRUE, ...) {
      if (stand_alone || self$name != "/") {
        cat("<", self$friendlyClassName, "> [", self$id, "] ", self$name, "\n", sep = "")
        cat("Path      :", self$fullname, "\n")
      }
      if (length(self$subgroups) > 0L)
        cat("Subgroups :", paste(names(self$subgroups), collapse = ", "), "\n")

      self$print_attributes(...)
    },

    #' @description Prints the hierarchy of the group and its subgroups to the
    #'   console, with a summary of contained objects. Usually called from the
    #'   root group to display the full group hierarchy.
    #' @param idx,total Arguments to control indentation. Should both be 1 (the
    #'   default) when called interactively. The values will be updated during
    #'   recursion when there are groups below the current group.
    hierarchy = function(idx = 1L, total = 1L) {
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

    #' @description Find an object by its name. Given the name of an object,
    #'   possibly preceded by an absolute or relative group path, return the
    #'   object to the caller. Typically, this method is called
    #'   programmatically; similar interactive use is provided through the
    #'   `[[.CFDataset` operator.
    #'
    #' @param name The name of an object, with an optional absolute or relative
    #'   group path from the calling group. The object must either an CF
    #'   construct (data variable, axis, auxiliary axis, label, or grid mapping)
    #'   or an NC group, dimension or variable.
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

          idx <- which(names(g$CFlonglat) == nm)
          if (length(idx)) return(g$CFlonglat[[idx]])

          idx <- which(names(g$CFaux) == nm)
          if (length(idx)) return(g$CFaux[[idx]])

          idx <- which(names(g$CFmeasures) == nm)
          if (length(idx)) return(g$CFmeasures[[idx]])

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

    #' @description Find an NC dimension object by its id. Given the id of a
    #'   dimension, return the [NCDimension] object to the caller. The dimension
    #'   has to be found in the current group or any of its parents.
    #'
    #' @param id The id of the dimension.
    #'
    #' @return The [NCDimension] object with an identifier equal to the `id`
    #'   argument. If the object is not found, returns `NULL`.
    find_dim_by_id = function(id) {
      dims <- sapply(self$NCdims, function(d) d$id)
      if (length(dims)) {
        idx <- which(dims == id)
        if (length(idx))
          return(self$NCdims[[idx]])
      }
      if (!is.null(self$parent))
        return(self$parent$find_dim_by_id(id))
      else
        return(NULL)
    },

    #' @description Has a given name been defined in this group already?
    #'
    #' @param name Character string. The name will be searched for, regardless
    #' of case.
    #' @param scope Either "CF" for a CF construct, "NC" for a
    #'   netCDF variable, or "both" (default) to test both scopes.
    #'
    #' @return `TRUE` if `name` is present in the group, `FALSE` otherwise.
    has_name = function(name, scope = "both") {
      name <- tolower(name)
      res <- if (scope %in% c("NC", "both")) name %in% tolower(names(self$NCvars))
             else FALSE
      if (scope %in% c("CF", "both"))
        res <- res ||
               name %in% tolower(c(names(self$CFvars),
                                   names(self$CFaxes),
                                   names(self$CFlonglat),
                                   names(self$CFaux),
                                   names(self$CFcrs),
                                   names(self$CFmeasures)))
      else
        stop("Invalid 'scope' argument supplied.", call. = FALSE)

      res
    },

    #' @description Find NC variables that are not referenced by CF objects. For
    #'   debugging purposes only.
    #' @return List of [NCVariable].
    unused = function() {
      vars <- lapply(self$NCvars, function(v) { if (!length(v$CF)) v})
      vars <- vars[lengths(vars) > 0L]

      # Descend into subgroups
      if (length(self$subgroups)) {
        subvars <- lapply(self$subgroups, function(g) g$unused())
        vars <- append(vars, unlist(subvars))
      }

      vars
    },

    #' @description Add an auxiliary long-lat variable to the group. This method
    #'   creates a [CFAuxiliaryLongLat] from the arguments and adds it to the
    #'   group `CFlonglat` list, but only if the combination of `lon`, `lat` isn't
    #'   already present.
    #' @param lon,lat Instances of [NCVariable] having a two-dimensional grid of
    #'   longitude and latitude values, respectively.
    #' @param bndsLong,bndsLat Instances of [CFBounds] with the 2D bounds of the
    #'   longitude and latitude grid values, respectively, or `NULL` when not
    #'   set.
    #' @return `self` invisibly.
    addAuxiliaryLongLat = function(lon, lat, bndsLong, bndsLat) {
      nm <- paste(lon$name, lat$name, sep = "_")
      if (!length(self$CFlonglat)) {
        self$CFlonglat <- list(CFAuxiliaryLongLat$new(lon, lat, bndsLong, bndsLat))
        names(self$CFlonglat) <- nm
      } else {
        known <- lapply(self$CFlonglat, function(a) c(a$varLong$id, a$varLat$id))
        if (!any(sapply(known, function(k) k[1L] == lon$id && k[2L] == lat$id)))
          self$CFlonglat[[nm]] <- CFAuxiliaryLongLat$new(lon, lat, bndsLong, bndsLat)
      }
      invisible(self)
    },

    #' @description Add a cell measure variable to the group.
    #' @param cm Instance of [CFCellMeasure].
    #' @return `self` invisibly.
    addCellMeasure = function(cm) {
      self$CFmeasures <- append(self$CFmeasures, cm)
      names(self$CFmeasures) <- sapply(self$CFmeasures, function(m) m$name)
      invisible(self)
    },

    #' @description This method lists the fully qualified name of this group,
    #'   optionally including names in subgroups.
    #'
    #' @param recursive Should subgroups be scanned for names too (default is
    #'   `TRUE`)?
    #'
    #' @return A character vector with group names.
    fullnames = function(recursive = TRUE) {
      if (recursive && length(self$subgroups))
        c(self$fullname, sapply(self$subgroups, function(g) g$fullnames(recursive)))
      else self$fullname
    },

    #' @description List all the dimensions that are visible from this group
    #' including those that are defined in parent groups (by names not defined
    #' by any of their child groups in direct lineage to the current group).
    #' @return A vector of [NCDimension] objects.
    dimensions = function() {
      dims <- self$NCdims
      if (self$name == "/")
        dims
      else {
        pdims <- self$parent$dimensions()
        if (length(dims)) {
          local_names <- sapply(dims, function(d) d$name)
          parent_names <- sapply(pdims, function(d) d$name)
          keep <- pdims[!which(parent_names %in% local_names)]
          append(dims, keep)
        } else
          pdims
      }
    },

    #' @description This method lists the CF data variables located in this
    #'   group, optionally including data variables in subgroups.
    #'
    #' @param recursive Should subgroups be scanned for CF data variables too
    #'   (default is `TRUE`)?
    #'
    #' @return A list of [CFVariable].
    variables = function(recursive = TRUE) {
      if (recursive && length(self$subgroups))
        c(self$CFvars, unlist(lapply(self$subgroups, function(g) g$variables(recursive))))
      else self$CFvars
    },

    #' @description This method lists the axes located in this group, optionally
    #'   including axes in subgroups.
    #'
    #' @param recursive Should subgroups be scanned for axes too (default is
    #'   `TRUE`)?
    #'
    #' @return A list of [CFAxis] descendants.
    axes = function(recursive = TRUE) {
      if (recursive && length(self$subgroups))
        subaxes <- lapply(self$subgroups, function(g) g$axes(recursive))
      else subaxes <- list()
      c(self$CFaxes, unlist(subaxes))
    },

    #' @description This method lists the grid mappings located in this group,
    #'   optionally including grid mappings in subgroups.
    #'
    #' @param recursive Should subgroups be scanned for grid mappings too
    #'   (default is `TRUE`)?
    #'
    #' @return A list of [CFGridMapping] instances.
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

    #' @field handle (read-only) Get the handle to the netCDF resource for the
    #'   group
    handle = function(value) {
      if (missing(value))
        if (is.null(self$resource)) NULL
        else self$resource$group_handle(self$fullname)
      else
        stop("Can't assign a value to a netCDF resource handle", call. = FALSE)
    },

    #' @field root (read-only) Retrieve the root group.
    root = function(value) {
      if (missing(value)) {
        g <- self
        while (g$name != "/") g <- g$parent
        g
      }
    },

    #' @field data_set (read-only) Retrieve the [CFDataset] that the group
    #' belongs to.
    data_set = function(value) {
      if (missing(value)) {
        g <- self
        while (inherits(g, "NCGroup")) g <- g$parent
        g
      }
    }
  )
)
