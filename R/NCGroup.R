#' NetCDF group
#'
#' @description This class represents a netCDF group, the object that holds
#'   elements like dimensions and variables of a netCDF file.
#'
#'   Direct access to groups is usually not necessary. The principal objects
#'   of interest, CF data variables and axes, are accessible via [CFGroup].
#'   Changing the properties of a netCDF group other than its name may very
#'   well invalidate the CF objects or even the netCDF file.
#'
#' @docType class
NCGroup <- R6::R6Class("NCGroup",
  inherit = NCObject,
  private = list(
    # The name of the group. Groups manage their own names because they are not
    # compliant with CF rules when starting with a backslash.
    .name = "",

    # Access to the underlying netCDF resource. This can be `NULL` for instances
    # created in memory. Managed internally.
    .resource = NULL,

    # The CFGroup connected to this NC group
    .CF = NULL
  ),
  public = list(
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

    #' @description Create a new instance of this class.
    #' @param id The identifier of the group.
    #' @param name The name of the group.
    #' @param attributes Optional, a `data.frame` with group attributes.
    #' @param parent The parent group of this group. The owning [CFDataset] for
    #'   the root group.
    #' @param resource Reference to the [CFResource] instance that provides
    #'   access to the netCDF resource. For in-memory groups this can be `NULL`.
    #' @return An instance of this class.
    initialize = function(id, name, attributes = data.frame(), parent, resource) {
      super$initialize(id, "group", attributes)
      private$.name <- name
      self$parent <- parent
      private$.resource <- resource
    },

    #' @description Summary of the group printed to the console.
    #' @param stand_alone Logical to indicate if the group should be printed as
    #' an object separate from other objects (`TRUE`, default), or print as part
    #' of an enclosing object (`FALSE`).
    #' @param ... Passed on to other methods.
    print = function(stand_alone = TRUE, ...) {
      if (stand_alone || private$.name != "/") {
        cat("<", self$friendlyClassName, "> [", self$id, "] ", private$.name, "\n", sep = "")
        cat("Path      :", self$fullname, "\n")
      }
      if (length(self$subgroups) > 0L)
        cat("Subgroups :", paste(names(self$subgroups), collapse = ", "), "\n")

      self$print_attributes(...)
    },

    #' @description Find an object by its name. Given the name of an object,
    #'   possibly preceded by an absolute or relative group path, return the
    #'   object to the caller. Typically, this method is called
    #'   programmatically.
    #' @param name The name of an object, with an optional absolute or relative
    #'   group path from the calling group. The object must be an NC group,
    #'   dimension or variable.
    #' @return The object with the provided name. If the object is not found,
    #'   returns `NULL`.
    find_by_name = function(name) {
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
            stop("Malformed group path:", name[1L], call. = FALSE) # nocov
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
            stop("Path not found in the resource:", name[1L], call. = FALSE) # nocov
        }

      nm <- elements[length(elements)]

      # Helper function to find a named object in the group `g`
      .find_here <- function(g) {
        idx <- which(names(g$NCvars) == nm)
        if (length(idx)) return(g$NCvars[[idx]])

        idx <- which(names(g$NCdims) == nm)
        if (length(idx)) return(g$NCdims[[idx]])

        idx <- which(names(g$subgroups) == nm)
        if (length(idx)) return(g$subgroups[[idx]])

        NULL
      }

      # Find the object in the current group
      obj <- .find_here(grp)
      if (!is.null(obj)) return(obj)

      if (parts == 1L) {
        # If the named object was not qualified, search higher groups
        while (grp$name != "/") {
          grp <- grp$parent
          obj <- .find_here(grp)
          if (!is.null(obj)) return(obj)
        }

        # If still not found, try lateral search
        .traverse_subgroups <- function(g, level) {
          obj <- .find_here(g)
          if (is.null(obj))
            lapply(g$subgroups, .traverse_subgroups, level = level + 1L)
          else
            list(level = level, obj = obj)
        }

        res <- lapply(grp$subgroups, .traverse_subgroups, level = 1L)
        res <- res[lengths(res) > 0L]
        # FIXME: The below takes the first occurrence, rather than the one with the lowest level
        if (length(res))
          return(res[[1L]]$obj)
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
      if (is.null(id))
        return(NULL)

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
        vars <- append(vars, unlist(subvars, use.names = FALSE))
      }

      vars
    },

    #' @description Create a new group as a sub-group of the current group. This
    #' writes the new group to the netCDF resource, but only if it is open for
    #' writing.
    #' @param CFgroup The [CFGroup] associated with this NC group.
    #' @return The newly created group as a `NCGroup` instance, invisibly.
    create_group = function(CFgroup) {
      # fix this here
      if (length(nm) != 1L || !.is_valid_name(nm))
        stop("Argument `nm` must be a single character string with a netCDF-compliant value.", call. = FALSE)
      if (!self$can_write)
        stop("NetCDF resource is not writeable.", call. = FALSE)
      if (nm %in% names(self$subgroups))
        stop("Subgroup with the same name already exists.", call. = FALSE)

      g <- RNetCDF::grp.def.nc(self$handle, nm)
      grp <- NCGroup$new(id = as.integer(g$self), name = nm, parent = self,
                         resource = private$.resource, CFGroup = CFgroup)
      self$subgroups <- c(self$subgroups, setNames(list(grp), nm))
      invisible(grp)
    },

    #' @description This method lists the fully qualified name of this group,
    #'   optionally including names in subgroups.
    #' @param recursive Should subgroups be scanned for names too (default is
    #'   `TRUE`)?
    #' @return A character vector with group names.
    fullnames = function(recursive = TRUE) {
      if (recursive && length(self$subgroups))
        c(self$fullname, sapply(self$subgroups, function(g) g$fullnames(recursive)))
      else self$fullname
    },

    #' @description List all the dimensions that are visible from this group,
    #'   possibly including those that are defined in parent groups (by names
    #'   not defined by any of their child groups in direct lineage to the
    #'   current group).
    #' @param scope Character string that indicates if only dimensions in the
    #'   current group should be reported (`local`) or visible dimensions in
    #'   parent groups as well (`all`, default).
    #' @return A vector of [NCDimension] objects.
    dimensions = function(scope = "all") {
      dims <- self$NCdims
      if (scope == "local") return(dims)

      if (private$.name == "/")
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

    #' @description Save any unsaved changes to the netCDF resource.
    #' @return Self, invisibly.
    save = function() {
      if (self$can_write) {
        self$write_attributes(self$handle, -1L)
        # FIXME: Save other items too
      }
      invisible(self)
    }
  ),
  active = list(
    #' @field friendlyClassName (read-only) A nice description of the class.
    friendlyClassName = function(value) {
      if (missing(value))
        "Group"
    },

    #' @field resource (read-only) The RNetCDF object to the underlying netCDF
    #'   resource.
    resource = function(value) {
      if (missing(value))
        private$.resource
    },

    #' @field handle (read-only) Get the handle to the netCDF resource for the
    #'   group
    handle = function(value) {
      if (missing(value))
        if (is.null(private$.resource)) NULL
        else private$.resource$group_handle(self$fullname)
      else
        stop("Can't assign a value to a netCDF resource handle", call. = FALSE)
    },

    #' @field can_write (read-only) Is the resource writable?
    can_write = function(value) {
      if (missing(value))
        !is.null(private$.resource) && private$.resource$can_write
    },

    #' @field name Set or retrieve the name of the group. Note that the name is
    #'   always relative to the location in the hierarchy that the group is in
    #'   and it should thus not be qualified by backslashes. The name has to be
    #'   a valid CF name. The name of the root group cannot be changed.
    name = function(value) {
      if (missing(value))
        private$.name
      else if (private$.name == "/")
        stop("Cannot change the name of the root group", call. = FALSE)
      else if (.is_valid_name(value)) {
        private$.name <- value
      }
    },

    #' @field fullname (read-only) The fully qualified absolute path of the group.
    fullname = function(value) {
      if (missing(value)) {
        nm <- private$.name
        if (nm == "/") return(nm)
        g <- self
        while (g$parent$name != "/") {
          g <- g$parent
          nm <- paste(g$name, nm, sep = "/")
        }
      }
      if (nm != "/") paste0("/", nm) else nm
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
    #'   belongs to. If the group is not attached to a `CFDataset`, returns
    #'   `NULL`.
    data_set = function(value) {
      if (missing(value)) {
        g <- self
        while (inherits(g, "NCGroup")) g <- g$parent
        g
      }
    },

    #' @field CF Set or retrieve the [CFGroup] that is associated with this
    #'   NC group.
    CF = function(value) {
      if (missing(value))
        private$.CF
      else if (inherits(value, "CFGroup"))
        private$.CF <- value
      else
        stop("Bad CF group assignment.", call. = FALSE)
    }
  )
)
