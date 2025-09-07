# Standard names used for parametric Z axes
Z_parametric_standard_names <-
  c("atmosphere_ln_pressure_coordinate",
    "atmosphere_sigma_coordinate", "atmosphere_hybrid_sigma_pressure_coordinate",
    "atmosphere_hybrid_height_coordinate", "atmosphere_sleve_coordinate",
    "ocean_sigma_coordinate", "ocean_s_coordinate", "ocean_s_coordinate_g1",
    "ocean_s_coordinate_g2", "ocean_sigma_z_coordinate", "ocean_double_sigma_coordinate")

#' Vertical CF axis object
#'
#' @description This class represents a vertical axis, which may be parametric.
#'   A regular vertical axis behaves like any other numeric axis. A parametric
#'   vertical axis, on the other hand, is defined through an index value that is
#'   contained in the axis coordinates, with additional data variables that hold
#'   ancillary "formula terms" with which to calculate dimensional axis
#'   coordinates. It is used in atmosphere and ocean data sets.
#'
#'   Parametric vertical axes can only be read from file, not created from
#'   scratch.
#'
#' @references
#' https://cfconventions.org/Data/cf-conventions/cf-conventions.html#parametric-vertical-coordinate
#' https://www.myroms.org/wiki/Vertical_S-coordinate
#'
#' @docType class
#' @export
CFAxisVertical <- R6::R6Class("CFAxisVertical",
  inherit = CFAxisNumeric,
  cloneable = FALSE,
  private = list(
    # The 'standard_name' attribute of the axis that identifies the parametric
    # form of this axis.
    .parameter_name = "",

    # The standard name for the computed values of the axis.
    .name_computed = "",

    # The unit of the computed values of the axis.
    .units_computed = "",

    # A `data.frame` with columns `terms`, `variable` and `ParamTerm` containing
    # the terms of the formula to calculate the axis values. Column `ParamTerm`
    # has the references to the variables that hold the data for each term.
    .terms = NULL,

    # The computed values of the parametric axis. This is a CFVariable instance
    # once it is computed.
    .computed_values = NULL,

    # Print some details of the parametric definition.
    print_details = function(...) {
      if (!is.null(private$.terms)) {
        cat("\nParametric definition:", private$.parameter_name)

        if (is.null(private$.computed_values)) {
          cat("\n (not calculated)\n")
        } else {
          cat("\n ", private$.name_computed, " (", private$.units_computed, ")\n", sep = "")
          cat(" Axes:", paste(sapply(private$.computed_values$axes, function(ax) ax$shard())), "\n")
        }
      }
    },

    # This function computes the actual dimensional axis values from the terms.
    compute = function() {
      if (is.null(private$.computed_values))
        switch(private$.parameter_name,
          "ocean_s_coordinate_g1" = private$ocean_s_coordinate_g1(),
          "ocean_s_coordinate_g2" = private$ocean_s_coordinate_g2()
        )
      private$.computed_values
    },

    # Helper function to get the data for a specific formula term. Can also
    # return 0 if the data is not found.
    get_data = function(term) {
      v <- private$.terms[private$.terms$term == term, ]$ParamTerm[[1L]]
      if (inherits(v, "CFVerticalParametricTerm"))
        v$values
      else 0
    },

    # Helper function to compute z(i,j,k,n) = s(i,j,k) * t(i,j,n)
    ijkn_from_ijk_times_ijn = function(ijk, ijn) {
      d1 <- dim(ijk)
      d2 <- dim(ijn)
      ijkn <- array(ijk, dim = c(d1, d2[3])) # Recycle last dimension
      ijnk <- array(ijn, dim = c(d2, d1[3])) # Same but last two dims must be reversed
      ijkn * aperm(ijnk, c(1, 2, 4, 3))
    },

    ocean_s_coordinate_g1 = function() {
      # z(n,k,j,i) = S(k,j,i) + eta(n,j,i) * (1 + S(k,j,i) / depth(j,i))
      # where S(k,j,i) = depth_c * s(k) + (depth(j,i) - depth_c) * C(k)
      s <- private$.values
      C <- private$get_data("C")
      eta <- private$get_data("eta")
      depth <- private$get_data("depth")
      depth_c <- private$get_data("depth_c")

      S <- sweep((depth - depth_c) %o% C, 3, depth_c * s, "+")

      # Construct the axes for the result. Use "depth" axes [i,j], combine with
      # self axis [k].
      ax <- private$.terms[private$.terms$term == "depth", ]$ParamTerm[[1L]]
      axes <- append(ax$axes, self)
      names(axes) <- c(names(ax$axes), self$name)

      crds <- if (identical(eta, 0))
        S
      else {
        tmp <- sweep(S, MARGIN = 1:2, depth, "/") + 1 # [k,j,i] 1 + S(k,j,i) / depth(j,i)
        d <- dim(eta)
        if (is.null(d))  # eta is a scalar
          S + eta * tmp
        else if (length(d) == 3L) {
          # eta is time-variant so add the [n] axis
          ax <- private$.terms[private$.terms$term == "eta", ]$ParamTerm[[1L]]
          axes <- append(axes, ax$axes[[3L]])
          names(axes) <- c(names(axes)[1L:3L], ax$axes[[3L]]$name)

          z <- private$ijkn_from_ijk_times_ijn(tmp, eta)
          sweep(z, MARGIN = 1:3, S, "+")
        } else
          S + sweep(tmp, MARGIN = 1:2, eta, "*")
      }
      private$.name_computed <- private$ocean_computed_name()
      private$.computed_values <- CFArray$new(private$.name_computed, crds, "NC_DOUBLE", axes, NULL, data.frame())
    },

    ocean_s_coordinate_g2 = function() {
      # z(n,k,j,i) = eta(n,j,i) + (eta(n,j,i) + depth(j,i)) * S(k,j,i)
      # where S(k,j,i) = (depth_c * s(k) + depth(j,i) * C(k)) / (depth_c + depth(j,i))
      s <- private$.values
      C <- private$get_data("C")
      eta <- private$get_data("eta")
      depth <- private$get_data("depth")
      depth_c <- private$get_data("depth_c")

      # Construct the axes for the result. Use "depth" axes [i,j], combine with
      # self axis [k].
      ax <- private$.terms[private$.terms$term == "depth", ]$ParamTerm[[1L]]
      axes <- append(ax$axes, self)
      names(axes) <- c(names(ax$axes), self$name)

      S <- sweep(depth %o% C, MARGIN = 3, s * depth_c, "+") # [k,j,i] depth_c * s(k) + depth(j,i) * C(k)
      S <- sweep(S, MARGIN = 1:2, depth + depth_c, "/")     # [k,j,i] S(k,j,i)

      crds <- if (identical(eta, 0))
        sweep(S, MARGIN = 1:2, depth, "*")
      else {
        d <- dim(eta)
        if (is.null(d))  # eta is a scalar
          sweep(S, MARGIN = 1:2, depth + eta, "*") + eta
        else if (length(d) == 3L) {
          # eta is time-variant so add the [n] axis
          ax <- private$.terms[private$.terms$term == "eta", ]$ParamTerm[[1L]]
          axes <- append(axes, ax$axes[[3L]])
          names(axes) <- c(names(axes)[1L:3L], ax$axes[[3L]]$name)

          z <- private$ijkn_from_ijk_times_ijn(S, sweep(eta, MARGIN = 1:2, depth, "+"))
          sweep(z, MARGIN = c(1, 2, 4), eta, "+")
        } else {
          z <- sweep(S, MARGIN = 1:2, eta + depth, "*")
          sweep(z, MARGIN = 1:2, eta, "+")
        }
      }
      private$.name_computed <- private$ocean_computed_name()
      private$.computed_values <- CFArray$new(private$.name_computed, crds, "NC_DOUBLE", axes, NULL, data.frame())
    },

    # Helper function to determine the computed name of ocean formulations
    ocean_computed_name = function() {
      switch(private$.terms[private$.terms$term == "depth", ]$ParamTerm[[1L]]$attribute("standard_name"),
        "sea_floor_depth_below_geoid" = "altitude",
        "sea_floor_depth_below_geopotential_datum" = "height_above_geopotential_datum",
        "sea_floor_depth_below_reference_ellipsoid" = "height_above_reference_ellipsoid",
        "sea_floor_depth_below_mean_sea_level" = "height_above_mean_sea_level",
        "non_standard_name"
        )
    }
  ),
  public = list(
    #' @description Create a new instance of this class.
    #' @param var The name of the axis when creating a new axis. When reading an
    #'   axis from file, the [NCVariable] object that describes this instance.
    #' @param values Optional. The values of the axis in a vector. The values
    #'   have to be numeric and monotonic. Ignored when argument `var` is a
    #'   `NCVariable` object.
    #' @param start Optional. Integer index where to start reading axis data
    #'   from file. The index may be `NA` to start reading data from the start.
    #' @param count Optional. Number of elements to read from file. This may be
    #'   `NA` to read to the end of the data.
    #' @param attributes Optional. A `data.frame` with the attributes of the
    #'   axis. When an empty `data.frame` (default) and argument `var` is an
    #'   `NCVariable` instance, attributes of the axis will be taken from the
    #'   netCDF resource.
    initialize = function(var, values, start = 1L, count = NA, attributes = data.frame()) {
      super$initialize(var, values = values, start = start, count = count, orientation =  "Z", attributes = attributes)

      if (!is.na(self$attribute("formula_terms")))
        private$.parameter_name <- self$attribute("standard_name")
    },

    #' @description Create a copy of this axis. The copy is completely separate
    #' from `self`, meaning that both `self` and all of its components are made
    #' from new instances. Note that the parametric terms, if any, are not
    #' copied here. If needed, copy the terms by calling the `copy_terms()`
    #' method **after** all axes that the terms refer to have been copied to, as
    #' appropriate.
    #' @param name The name for the new axis. If an empty string is passed, will
    #'   use the name of this axis.
    #' @return The newly created axis.
    copy = function(name = "") {
      if (self$has_resource) {
        ax <- CFAxisVertical$new(self$NCvar, start = private$.start_count$start,
                                 count = private$.start_count$count, attributes = self$attributes)
        if (nzchar(name))
          ax$name <- name
      } else {
        if (!nzchar(name))
          name <- self$name
        ax <- CFAxisVertical$new(name, values = private$.values, attributes = self$attributes)
      }

      if (inherits(private$.bounds, "CFBounds"))
        ax$bounds <- private$.bounds$copy()

      private$subset_coordinates(ax, c(1L, self$length))
      ax
    },

    #' @description Create a copy of this axis but using the supplied values.
    #'   The attributes are copied to the new axis. Boundary values, parametric
    #'   coordinates and auxiliary coordinates are not copied.
    #'
    #'   After this operation the attributes of the newly created axes may not
    #'   be accurate, except for the "actual_range" attribute. The calling code
    #'   should set, modify or delete attributes as appropriate.
    #' @param name The name for the new axis. If an empty string is passed, will
    #'   use the name of this axis.
    #' @param values The values to the used with the copy of this axis.
    #' @return The newly created axis.
    copy_with_values = function(name = "", values) {
      if (!nzchar(name))
        name <- self$name
      CFAxisVertical$new(name, values = values, attributes = self$attributes)
    },

    #' @description Copy the parametric terms from `from` to
    #'   `self`. The parametric terms will be configured in `self`.
    #' @param from The `NCGroup` from which references to the parametric terms
    #'   are accessible.
    #' @param original_axes List of `CFAxis` instances from the CF object that
    #'   these parametric terms are copied from.
    #' @param new_axes List of `CFAxis` instances to use with the formula term
    #'   objects.
    #' @return Self, invisibly.
    copy_terms = function(from, original_axes, new_axes) {
      ft <- self$attribute("formula_terms")
      if (!is.na(ft)) {
        ft <- trimws(strsplit(ft, " ")[[1L]], whitespace = ":")
        dim(ft) <- c(2, length(ft) * 0.5)
        rownames(ft) <- c("term", "variable")
        ft <- as.data.frame(t(ft))
        ft$ParamTerm <- lapply(ft$variable, function(v) {
          if (v == self$name) NULL
          else {
            ncvar <- from$find_by_name(v, "NC")
            if (!is.null(ncvar)) {
              xids <- lapply(original_axes, function(x) x$dimid)
              nd <- ncvar$ndims
              if (nd > 0L) {
                ax <- vector("list", nd)
                for (x in 1:nd) {
                  ndx <- which(sapply(xids, function(e) ncvar$dimids[x] %in% e))
                  if (!length(ndx)) {
                    warning(paste0("Possible variable '", ncvar$name, "' cannot be constructed because of unknown axis identifier ", ncvar$dimids[x]))
                    return(NULL)
                  }
                  ax[[x]] <- new_axes[[ndx]]
                }
                names(ax) <- sapply(ax, function(x) x$name)
              } else ax <- list()
              #ncvar$group <- self$group

              CFVerticalParametricTerm$new(ncvar, ax)
            } else
              CFVerticalParametricTerm$new(0, NULL)
          }
        })
        private$.terms <- ft
      }

      return(self)
    },

    #' @description Configure the formula terms of a parametric vertical axis.
    #'   If the vertical axis has a `formula_terms` attribute it has a
    #'   parametric coordinate space that is calculated from the formula terms.
    #'   This method sets up the axis instance to calculate the dimensional
    #'   coordinate space (but it does not do the actual calculation; access the
    #'   `parametric_coordinates` field to get the dimensional coordinates).
    #'
    #'   This method is called automatically when opening a netCDF file. It
    #'   should also be called after copying the axes that it refers to. It is
    #'   not intended to be called otherwise.
    #' @param axes List of `CFAxis` instances to use with the formula term
    #'   objects.
    #' @return Self, invisibly.
    configure_terms = function(axes) {
      ft <- self$attribute("formula_terms")
      if (!is.na(ft)) {
        ft <- trimws(strsplit(ft, " ")[[1L]], whitespace = ":")
        dim(ft) <- c(2, length(ft) * 0.5)
        rownames(ft) <- c("term", "variable")
        ft <- as.data.frame(t(ft))
        ft$ParamTerm <- lapply(ft$variable, function(v) {
          if (v == self$name) NULL
          else {
            ncvar <- self$group$find_by_name(v, "NC")
            if (!is.null(ncvar)) {
              ax <- .buildVariableAxisList(ncvar, axes)
              CFVerticalParametricTerm$new(ncvar, ax)
            } else
              CFVerticalParametricTerm$new(0, NULL)
          }
        })
        private$.terms <- ft
      }

      return(self)
    },

    #' @description This method subsets the parametric terms after the axis has
    #'   been subset. This method is called by `CFVariable$subset()` and
    #'   `$profile()` methods and should not be called otherwise.
    #' @param axes The list of dimensional axes that the data variable uses. The
    #'   axes are named after the original axes (some may have been renamed in
    #'   the preceding process).
    #' @param start A vector of indices where to start reading in the terms, in
    #'   the original axis order.
    #' @param count A vector of lengths to read along each axis, in the original
    #'   axis order.
    #' @param index Optional index vector when auxiliary coordinates are to be
    #'   used to warp rotated planar coordinates to lat-long coordinates.
    #' @param dim_in Optional list of dimensions to use with argument `index`
    #'   for the original data.
    #' @param dim_out Optional list of dimensions to use with argument `index`
    #'   for the warped data.
    #' @return Self, invisibly.
    parametric_subset = function(axes, start, count, index, dim_in, dim_out) {
      if (is.null(private$.terms)) return(invisible(self))

      # If computed before, throw out parametric coordinates
      was_computed <- !is.null(private$.computed_values)
      private$.computed_values <- NULL

      # Pass all arguments to the parametric term instances
      #continue here

      # If the parametric coordinates were computed before, recompute
      if (was_computed) private$compute()
      invisible(self)
    },

    #' @description Append a vector of values at the end of the current values
    #'   of the axis. Boundary values are appended as well but if either this
    #'   axis or the `from` axis does not have boundary values, neither will the
    #'   resulting axis.
    #' @param from An instance of `CFAxisVertical` whose values to append to the
    #'   values of this axis.
    #' @return A new `CFAxisVertical` instance with values from this axis and
    #'   the `from` axis appended.
    append = function(from) {
      if (super$can_append(from) && .c_is_monotonic(self$values, from$values)) {
        ax <- CFAxisVertical$new(self$name, values = c(private$values, from$values),
                                 attributes = self$attributes)

        if (!is.null(private$.bounds)) {
          new_bnds <- private$.bounds$append(from$bounds)
          if (!is.null(new_bnds))
            ax$bounds <- new_bnds
        }

        ax
      } else
        stop("Axis values cannot be appended.", call. = FALSE)
    }
  ),
  active = list(
    #' @field friendlyClassName (read-only) A nice description of the class.
    friendlyClassName = function(value) {
      if (missing(value))
        if (is.null(private$.terms)) "Vertical axis"
        else "Vertical axis (parametric)"
    },

    #' @field formula_terms (read-only) A `data.frame` with the "formula_terms"
    #' to calculate the parametric axis values.
    formula_terms = function(value) {
      if (missing(value))
        private$.terms
    },

    #' @field is_parametric (read-only) Logical flag that indicates if the
    #'   coordinates of the axis are parametric.
    is_parametric = function(value) {
      if (missing(value))
        !is.na(self$attribute("formula_terms"))
    },

    #' @field parametric_coordinates (read-only) Retrieve the parametric
    #' coordinates of this vertical axis as a [CFVariable].
    parametric_coordinates = function(value) {
      if (missing(value))
        private$compute()
    },

    #' @field computed_name (read-only) The name of the computed parameterised
    #' coordinates. If the parameterised coordinates have not been computed yet
    #' the computed name is an empty string.
    computed_name = function(value) {
      if (missing(value)) {
        private$.name_computed
      }
    },

    #' @field computed_units (read-only) Return the units of the computed
    #' parameterised coordinates, if computed, otherwise return `NULL`. This
    #' will access the standard names table.
    computed_units = function(value) {
      if (missing(value)) {
        if (is.null(private$.computed_values))
          NULL
        else if (private$.name_computed == "non_standard_name")
          "unknown units"
        else
          CF$standard_names$find(private$.name_computed)$units
      }
    }
  )
)
