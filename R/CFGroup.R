#' Group for CF objects
#'
#' @description This class represents a CF group, the object that holds
#'   elements like dimensions and variables of a [CFDataset].
#'
#'   Direct access to groups is usually not necessary. The principal objects
#'   held by the group, CF data variables and axes, are accessible via other
#'   means. Only for access to the group attributes is a reference to a group
#'   required. Changing the properties of a group other than its name may very
#'   well invalidate the CF objects or even the netCDF file.
#'
#' @docType class
CFGroup <- R6::R6Class("CFGroup",
  inherit = CFObject,
  cloneable = FALSE,
  private = list(
    # The name of the group. Groups manage their own names because they are not
    # compliant with CF rules when starting with a backslash.
    .name = "",

    # The parent group of this group, or a CFDataset for the root group.
    .parent = NULL,

    # List of child `CFGroup` instances of this group.
    .subgroups = list(),

    # List of CF objects defined for this group
    .objects = list(),

    # Set the name of the group. Cascade to the underlying NCGroup, as necessary.
    set_name = function(new_name) {
      if (.is_valid_name(new_name)) {
        if (!(!is.null(private$.parent) && !is.null(private$.parent$find_by_name(new_name)))) {
          private$.name <- if (!is.null(private$.NCobj)) {
            private$.NCobj$set_name(new_name)
            private$.NCobj$name # new_name may not have been written
          } else
            new_name
        }
      }
      invisible(self)
    }
  ),
  public = list(
    #' @description Create a new CF group instance.
    #' @param grp Either a [NCGroup] instance when opening a netCDF resource, or
    #'   a character string with a name for the group when creating a new CF
    #'   group in memory. When a character string, it should be the local name,
    #'   without any slash "/" characters. For the root group, specify an empty
    #'   string "".
    #' @param parent The parent group for this group, or a `CFDataset` for the
    #'   root group.
    #' @return An instance of this class.
    initialize = function(grp, parent) {
      if (inherits(grp, "NCGroup")) {
        super$initialize(grp)
        private$.name <- grp$name
      } else {
        if (nzchar(grp)) {
          super$initialize(grp)
          private$.name <- grp
        } else {
          super$initialize("root")
          private$.name <- "/"
        }
        private$.dirty <- TRUE
      }
      private$.parent <- parent
    },

    #' @description Summary of the group printed to the console.
    #' @param stand_alone Logical to indicate if the group should be printed as
    #' an object separate from other objects (`TRUE`, default), or print as part
    #' of an enclosing object (`FALSE`).
    #' @param ... Passed on to other methods.
    print = function(stand_alone = TRUE, ...) {
      if (stand_alone || private$.name != "/") {
        virtual <- if (is.null(private$.NCobj)) " (virtual)" else ""
        cat("<CF Group> [", self$id, "] ", private$.name, virtual, "\n", sep = "")
        cat("Path      :", self$fullname, "\n")
      }
      if (self$has_subgroups)
        cat("Subgroups :", paste(names(self$subgroups), collapse = ", "), "\n")

      self$print_attributes(width = 50L)
    },

    #' @description Prints the hierarchy of the group and its subgroups to the
    #'   console, with a summary of contained objects. Usually called from the
    #'   root group to display the full group hierarchy.
    #' @param idx,total Arguments to control indentation. Should both be 1 (the
    #'   default) when called interactively. The values will be updated during
    #'   recursion when there are groups below the current group.
    hierarchy = function(idx = 1L, total = 1L) {
      if (idx == total) sep <- "   " else sep <- "|  "
      hier <- paste0("* ", private$.name, "\n")

      # Axes
      ax <- self$objects("CFAxis", recursive = FALSE)
      if (length(ax) > 0L) {
        ax <- paste(sapply(ax, function(x) x$shard()), collapse = ", ")
        hier <- c(hier, paste0(sep, "Axes     : ", ax, "\n"))
      }

      # Variables
      vars <- self$objects("CFVariable", recursive = FALSE)
      if (length(vars) > 0L) {
        vars <- lapply(vars, function(v) v$shard())
        vars <- unlist(vars[lengths(vars) > 0], use.names = FALSE)
        v <- paste(vars, collapse = ", ")
        hier <- c(hier, paste0(sep, "Variables: ", v, "\n"))
      }

      # Subgroups
      subs <- length(self$subgroups)
      if (subs > 0L) {
        sg <- unlist(sapply(1L:subs, function(g) self$subgroups[[g]]$hierarchy(g, subs)), use.names = FALSE)
        hier <- c(hier, paste0(sep, sg))
      }
      hier
    },

    #' @description Retrieve the names of the subgroups of the current group.
    #' @param recursive Logical, default is `TRUE`. If `TRUE`, include names of
    #'   recursively through the group hierarchy.
    #' @return A character vector with the names of the subgroups of the current
    #'   group. If `recursive = TRUE`, the names will be fully qualified with
    #'   their path.
    subgroup_names = function(recursive = TRUE) {
      nms <- if (recursive) sapply(private$.subgroups, function(g) g$fullname)
             else names(private$.subgroups)

      if (recursive && self$has_subgroups) {
        subs <- lapply(private$.subgroups, function(g) g$subgroup_names(TRUE))
        c(nms, unlist(subs))
      } else
        nms
    },

    #' @description Create a new group as a subgroup of the current group.
    #' @param name The name of the new subgroup. This must be a valid CF name,
    #'   so not contain any slash '/' characters among other restrictions, and
    #'   it cannot be already present in the group.
    #' @return The newly created group, or an error.
    create_subgroup = function(name) {
      if (!.is_valid_name(name))
        stop("Specified name is not valid.", call. = FALSE)
      if (name %in% names(private$.subgroups))
        stop("Specified name is a duplicate of another object.", call. = FALSE)

      grp <- CFGroup$new(name, parent = self)
      private$.subgroups <- c(private$.subgroups, setNames(list(grp), name))
      grp
    },

    #' @description Add subgroups to the current group. These subgroups must be
    #' fully formed, including having set their parent to this group. Use the
    #' `create_subgroup()` method to add a group from scratch.
    #' @param grps A `CFGroup`, or `list` thereof.
    #' @return Self, invisibly.
    add_subgroups = function(grps) {
      if (!is.list(grps))
        grps <- setNames(list(grps), grps$name)
      private$.subgroups <- c(private$.subgroups, grps)
      invisible(self)
    },

    #' @description Add one or more CF objects to the current group. This is an
    #'   internal method that should not be invoked by the user. The objects to
    #'   be added are considered atomic and not assessed for any contained
    #'   objects. Use a method like `add_variable()` to add a CF variable to
    #'   this group as well as its composing sub-objects such as axes.
    #' @param obj An instance of a `CFObject` descendant class, or a `list`
    #'   thereof. If it is a `list`, the list elements must be named after the
    #'   CF object they contain.
    #' @param silent Logical. If `TRUE` (default), CF objects in argument `obj`
    #'   whose name is already present in the list of CF objects *and* whose
    #'   class is identical to the already present object are silently dropped;
    #'   otherwise or when the argument is `FALSE` an error is thrown.
    #' @return Self, invisibly, or an error.
    add_CF_object = function(obj, silent = TRUE) {
      if (is.list(obj)) {
        names <- sapply(obj, function(o) o$name)
        idx <- match(names, names(private$.objects))
        if (any(!is.na(idx)))
          if (silent) {
            idx <- idx[which(!is.na(idx))] # the duplicates
            if (!all(sapply(obj, function(o) class(o)[1L]) == class(private$.objects[idx])[1L]))
              stop("Object name already present in group.", call. = FALSE)
          } else
            stop("Object name already present in group.", call. = FALSE)
      } else {
        idx <- match(obj$name, names(private$.objects))
        if (is.na(idx))
          obj <- setNames(list(obj), obj$name)
        else if (silent && class(obj)[1L] == class(private$.objects[[idx]])[1L])
          return(invisible(self))
        else
          stop("Object name already present in group.", call. = FALSE)
      }

      private$.objects <- c(private$.objects, obj)
      invisible(self)
    },

    #' @description This method lists the CF objects of a certain class located
    #'   in this group, optionally including objects in subgroups.
    #' @param cls Character vector of classes whose objects to retrieve. Note
    #' that subclasses are automatically retrieved as well, so specifying
    #' `cls = "CFAxis"` will retrieve all axes defined in this group.
    #' @param recursive Should subgroups be scanned for CF objects too
    #'   (default is `TRUE`)?
    #' @return A list of [CFObject] instances.
    objects = function(cls, recursive = TRUE) {
      objs <- lapply(private$.objects, function(obj) {if (any(!is.na(match(cls, class(obj))))) obj})
      objs <- objs[lengths(objs) > 0L]

      if (recursive && self$has_subgroups) {
        subs <- lapply(private$.subgroups, function(g) g$objects(cls, recursive))
        c(objs, unlist(subs))
      } else
        objs
    },

    #' @description Find an object by its name. Given the name of an object,
    #'   possibly preceded by an absolute or relative group path, return the
    #'   object to the caller. Typically, this method is called
    #'   programmatically; similar interactive use is provided through the
    #'   `[[.CFDataset` operator.
    #' @param name The name of an object, with an optional absolute or relative
    #'   group path from the calling group. The object must be an CF construct:
    #'   group, data variable, axis, auxiliary axis, label, grid mapping, etc.
    #' @return The object with the provided name. If the object is not found,
    #'   returns `NULL`.
    find_by_name = function(name) {
      if (is.null(name)) return(NULL)

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
          if (parts == dotdots)
            return(grp)
          elements <- elements[-dotdot]
        }
      }

      # Traverse down the groups until 1 element is left
      if (length(elements) > 1L)
        for (i in 1L:(length(elements) - 1L)) {
          grp <- grp$subgroups[[ elements[i] ]]
          if (is.null(grp))
            stop("Path not found in the resource: ", name[1L], call. = FALSE) # nocov
        }

      nm <- elements[length(elements)]

      # Helper function to find a named object in the group `g`.
      .find_here <- function(g) {
        objs <- g$CFobjects
        idx <- which(names(objs) == nm)
        if (length(idx))
          objs[[idx]]
        else if (nm %in% names(g$subgroups))
          g$subgroups[[nm]]
        else NULL
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

    #' @description Add a [CFVariable] object to the group. If there is another
    #'   object with the same name in this group an error is thrown. For
    #'   associated objects (such as axes, CRS, boundary variables, etc), if
    #'   another object with the same name is otherwise identical to the
    #'   associated object then that object will be linked from the variable,
    #'   otherwise an error is thrown.
    #' @param var An instance of `CFVariable` or any of its descendants.
    #' @param locations Optional. A `list` whose named elements correspond to
    #'   the names of objects associated with the variable in argument `var`.
    #'   Each list element has a single character string indicating the group in
    #'   the hierarchy where the object should be stored. As an example, if the
    #'   variable has axes "lon" and "lat" and they should be stored in the
    #'   parent group of this group, then specify `locations = list(lon = "..",
    #'   lat = "..")`. Locations can use absolute paths or relative paths from
    #'   the current group. Associated objects that are not in the list will be
    #'   stored in this group. If the argument `locations` is not provided, all
    #'   associated objects will be stored in this group.
    #' @return Argument `var`, invisibly.
    add_variable = function(var, locations = list()) {
      var$attach_to_group(self, locations)
      invisible(var)
    },

    #' @description Write the group to file, including its attributes, if it
    #' doesn't already exist.
    #' @param recursive If `TRUE` (default), write sub-groups as well.
    #' @return Self, invisibly.
    write = function(recursive = TRUE) {
      if (is.null(private$.NCobj)) {
        if (is.null(res <- self$data_set$resource))
          stop("Can't write a virtual group.", call. = FALSE)
        parent <- if (self$name == "/") NULL else private$.parent$NC
        private$.NCobj <- NCGroup$new(id = NA, name = private$.name, attributes = self$attributes,
                                      parent = parent, resource = res)
        private$.id <- private$.NCobj$id
        private$.NCobj$CF <- self
        if (!is.null(parent))
          parent$subgroups <- c(parent$subgroups, setNames(list(private$.NCobj), private$.name))
      }
      self$write_attributes()

      if (recursive)
        lapply(private$.subgroups, function(g) g$write(recursive))

      invisible(self)
    },

    #' @description Write data variables in the group to file, including its
    #'   associated objects, if it doesn't already exist.
    #' @param pack Logical to indicate if the data should be packed. Packing is
    #'   only useful for numeric data; packing is not performed on integer
    #'   values. Packing is always to the "NC_SHORT" data type, i.e. 16-bits per
    #'   value.
    #' @param recursive If `TRUE` (default), write data variables in sub-groups
    #'   as well.
    #' @return Self, invisibly.
    write_variables = function(pack = FALSE, recursive = TRUE) {
      lapply(private$.objects, function(obj) {
        if (inherits(obj, "CFVariable"))
          obj$write(pack)
      })
      if (recursive)
        lapply(private$.subgroups, function(g) g$write_variables(pack, recursive))

      invisible(self)
    }
  ),
  active = list(
    #' @field parent (read-only) The parent group of the current group, or its
    #' owning data set for the root node.
    parent = function(value) {
      if (missing(value))
        private$.parent
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
      else
        private$set_name(value)
    },

    #' @field fullname (read-only) The fully qualified absolute path of the group.
    fullname = function(value) {
      if (missing(value)) {
        nm <- private$.name
        if (nm == "/") return(nm)
        if (!is.null(self$parent)) {
          g <- self
          while (g$parent$name != "/") {
            g <- g$parent
            nm <- paste(g$name, nm, sep = "/")
          }
        }
      }
      if (nm != "/") paste0("/", nm) else nm
    },

    #' @field root (read-only) Retrieve the root group.
    root = function(value) {
      if (missing(value)) {
        g <- self
        while (!is.null(g$parent) && g$name != "/") g <- g$parent
        g
      }
    },

    #' @field data_set (read-only) Retrieve the [CFDataset] that the group
    #'   belongs to. If the group is not attached to a `CFDataset`, returns
    #'   `NULL`.
    data_set = function(value) {
      if (missing(value)) {
        g <- self
        while (inherits(g, "CFGroup")) g <- g$parent
        g
      }
    },

    #' @field has_subgroups (read-only) Does the current group have subgroups?
    has_subgroups = function(value) {
      if (missing(value))
        length(private$.subgroups) > 0L
    },

    #' @field subgroups (read-only) Retrieve the list of the subgroups of the
    #'   current group.
    subgroups = function(value) {
      if (missing(value))
        private$.subgroups
    },

    #' @field CFobjects (read-only) Retrieve the list of CF objects of the
    #'   current group.
    CFobjects = function(value) {
      if (missing(value))
        private$.objects
    }
  )
)
