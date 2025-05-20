#' Base ancestor of CFVariable and CFArray
#'
#' @description This class is a basic ancestor to [CFVariable] and [CFArray]. It
#' should not be instantiated directly, use the descendant classes instead.
#'
#'   This class provides access to common properties of data variables and the
#'   data they contain.
#'
#' @docType class
CFVariableBase <- R6::R6Class("CFVariableBase",
  inherit = CFObject,
  private = list(
    # The netCDF type of the unpacked data of the variable.
    values_type = NA_character_,

    # Return the R order of dimensional axes that "receive special treatment".
    # Scalar axes are not considered here.
    YXZT = function() {
      orient <- sapply(self$axes, function(x) if (x$length > 1L) x$orientation)
      match(c("Y", "X", "Z", "T"), orient, nomatch = 0L)
    },

    # Return the CF canonical order of dimensional axes that "receive special
    # treatment". Scalar axes are not considered here.
    XYZT = function() {
      orient <- sapply(self$axes, function(x) if (x$length > 1L) x$orientation)
      match(c("X", "Y", "Z", "T"), orient, nomatch = 0L)
    },

    # Return the number of "dimensional" axes, i.e. axes that are associated
    # with a dimension of the data of the variable. This may include dimensions
    # with length 1.
    num_dim_axes = function() {
      sum(sapply(self$axes, function(x) x$NCvar$ndims > 0L))
    },

    # Return the names of the dimensional axes.
    dim_names = function() {
      sapply(1:private$num_dim_axes(), function(i) self$axes[[i]]$name)
    },

    # Return the lengths of the dimensional axes.
    dim = function() {
      sapply(1:private$num_dim_axes(), function(i) self$axes[[i]]$length)
    },

    # Return a vector with the two auxiliary coordinate variable names, if they
    # are set for the variable. This is only properly implemented for CFVariable.
    aux_var_names = function() {
      NULL
    },

    # Check that names passed as arguments to $subset() and $profile() are valid.
    # This means that they must refer to an axis by name or orientation and
    # there can be no duplication. It returns an integer vector with the order
    # in which the axes are specified.
    check_names = function(nm) {
      axis_names <- names(self$axes)
      is_axis <- match(nm, axis_names)
      if (anyDuplicated(is_axis, incomparables = NA))
        stop("Duplicated axis names not allowed", call. = FALSE)
      if (!any(is.na(is_axis)))
        return(is_axis)

      orientations <- sapply(self$axes, function(a) a$orientation)
      is_orient <- match(nm, orientations)
      if (anyDuplicated(is_orient, incomparables = NA))
        stop("Duplicated axis orientations not allowed", call. = FALSE)
      if (!any(is.na(is_orient)))
        return(is_orient)

      ax_na <- which(is.na(is_axis))
      is_axis[ax_na] <- is_orient[ax_na]
      if (anyDuplicated(is_axis, incomparables = NA))
        stop("Duplicated axis names and orientations not allowed", call. = FALSE)
      if (any(is.na(is_axis)))
        stop("Arguments contain names not corresponding to an axis", call. = FALSE)

      is_axis
    },

    # Drop unwanted values in the "coordinates" attribute
    dropCoordinates = function(atts, names) {
      crd <- atts[atts$name == "coordinates", ]$value
      if (!length(crd)) return(atts)

      crds <- strsplit(crd[[1L]], " ")[[1L]]
      keep <- !(crds %in% names)
      if (!any(keep))
        return(atts[!(atts$name == "coordinates"), ])

      crds <- paste(crds[keep])
      atts[atts$name == "coordinates", ]$length <- nchar(crds)
      atts[atts$name == "coordinates", ]$value <- crds
      atts
    }
  ),
  public = list(
    #' @field axes List of instances of classes descending from [CFAxis] that
    #'   are the axes of the data object. If there are any scalar axes, they are
    #'   listed after the axes that associate with the dimensions of the data.
    #'   (In other words, axes `1..n` describe the `1..n` data dimensions, while
    #'   any axes `n+1..m` are scalar axes.)
    axes = list(),

    #' @field crs The coordinate reference system of this variable, as an
    #'   instance of [CFGridMapping]. If this field is `NULL`, the horizontal
    #'   component of the axes are in decimal degrees of longitude and latitude.
    crs = NULL,

    #' @field cell_measure The [CFCellMeasure] object of this variable, if
    #'   defined.
    cell_measure = NULL,

    #' @description Create an instance of this class.
    #' @param var The NC variable that describes this data object.
    #' @param axes A `list` of [CFAxis] descendant instances that describe the
    #'   axes of the data object.
    #' @param crs The [CFGridMapping] instance of this data object, or `NULL`
    #'   when no grid mapping is available.
    #' @return An instance of this class.
    initialize = function(var, axes, crs) {
      super$initialize(var)
      self$axes <- axes
      self$crs <- crs
    },

    #' @description Return the time object from the axis representing time.
    #' @param want Character string with value "axis" or "time", indicating
    #' what is to be returned.
    #' @return If `want = "axis"` the [CFAxisTime] axis; if `want = "time"` the
    #' `CFTime` instance of the axis, or `NULL` if the variable does not have a
    #' "time" axis.
    time = function(want = "time") {
      ndx <- sapply(self$axes, inherits, "CFAxisTime")
      if (any(ndx))
        if (want == "axis") self$axes[[which(ndx)]]
      else self$axes[[which(ndx)]]$time()
      else NULL
    },

    #' @description Summarise the temporal domain of the data, if present, to
    #'   a lower resolution, using a user-supplied aggregation function.
    #'
    #'   Attributes are copied from the input data variable or data array. Note
    #'   that after a summarisation the attributes may no longer be accurate.
    #'   This method tries to sanitise attributes (such as removing
    #'   `scale_factor` and `add_offset`, when present, as these will no longer
    #'   be appropriate in most cases) but the onus is on the calling code (or
    #'   yourself as interactive coder). Attributes like `standard_name` and
    #'   `cell_methods` likely require an update in the output of this method,
    #'   but the appropriate new values are not known to this method. Use
    #'   `CFArray$set_attribute()` on the result of this method to set or update
    #'   attributes as appropriate.
    #' @param name Character vector with a name for each of the results that
    #'   `fun` returns. So if `fun` has 2 return values, this should be a vector
    #'   of length 2. Any missing values are assigned a default name of
    #'   "result_#" (with '#' being replaced with an ordinal number).
    #' @param fun A function or a symbol or character string naming a function
    #'   that will be applied to each grouping of data. The function must return
    #'   an atomic value (such as `sum()` or `mean()`), or a vector of atomic
    #'   values (such as `range()`). Lists and other objects are not allowed and
    #'   will throw an error that may be cryptic as there is no way that this
    #'   method can assert that `fun` behaves properly so an error will pop up
    #'   somewhere, most probably in unexpected ways. The function may also be
    #'   user-defined so you could write a wrapper around a function like `lm()`
    #'   to return values like the intercept or any coefficients from the object
    #'   returned by calling that function.
    #' @param period The period to summarise to. Must be one of either "day",
    #'   "dekad", "month", "quarter", "season", "year". A "quarter" is the
    #'   standard calendar quarter such as January-March, April-June, etc. A
    #'   "season" is a meteorological season, such as December-February,
    #'   March-May, etc. (any December data is from the year preceding the
    #'   January data). The period must be of lower resolution than the
    #'   resolution of the time axis.
    #' @param era Optional, integer vector of years to summarise over by the
    #'   specified `period`. The extreme values of the years will be used. This
    #'   can also be a list of multiple such  vectors. The elements in the list,
    #'   if used, should have names as these will be used to label the results.
    #' @param ... Additional parameters passed on to `fun`.
    #' @return A `CFData` object, or a list thereof with as many `CFData`
    #'   objects as `fun` returns values.
    summarise = function(name, fun, period, era = NULL, ...) {
      if (missing(name) || missing(period) || missing(fun))
        stop("Arguments 'name', 'period' and 'fun' are required.", call. = FALSE)
      if (!(period %in% c("day", "dekad", "month", "quarter", "season", "year")))
        stop("Argument 'period' has invalid value.", call. = FALSE)
      if (!.is_valid_name(name))
        stop("Not all names are valid.", call. = FALSE)

      # Find the time object, create the factor
      tax <- self$time("axis")
      if (is.null(tax))
        stop("No 'time' axis found to summarise on.", call. = FALSE)
      f <- try(tax$time()$factor(period, era), silent = TRUE)
      if (inherits(f, "try-error"))
        stop("The 'time' axis is too short to summarise on.", call. = FALSE)
      if (is.factor(f)) f <- list(f)

      tm <- sum(private$YXZT() > 0L) # Test which oriented axes are present, T is the last one

      # Attributes
      atts <- self$attributes
      atts <- atts[!atts$name %in% c("scale_factor", "add_offset"), ]
      coords <- unlist(atts[atts$name == "coordinates", ]$value)
      if (!is.null(coords)) {
        # Remove reference to any auxiliary coordinate variables, but leave scalar variables
        if (!is.null(nm <- private$aux_var_names())) {
          crd <- gsub(paste(nm, collapse = "|"), "", coords)
          crd <- trimws(crd)
          atts[atts$name == "coordinates", "value"] <- crd
          atts[atts$name == "coordinates", "length"] <- nchar(crd)
        }
      }

      res <- lapply(f, function(fac) {
        # Create a new group for the result
        grp <- makeGroup(-1L, "/", "/")

        # Make a new time axis for the result
        new_tm <- attr(fac, "CFTime")
        var <- NCVariable$new(-1L, tax$name, grp, tax$NCvar$vtype, 1L, NULL)
        len <- length(new_tm)
        dim <- NCDimension$new(-1L, tax$name, len, FALSE)
        new_ax <- CFAxisTime$new(var, dim, new_tm)
        if (len == 1L) {
          ax <- c(self$axes[-tm], new_ax)
          names(ax) <- c(names(self$axes[-tm]), tax$name)
        } else {
          ax <- c(new_ax, self$axes[-tm])
          names(ax) <- c(tax$name, names(self$axes[-tm]))
        }
        new_ax$attributes <- tax$attributes
        if (inherits(new_tm, "CFClimatology")) {
          new_ax$delete_attribute("bounds")
          new_ax$set_attribute("climatology", "NC_CHAR", "climatology_bnds")
        }

        # Summarise
        dt <- private$process_data(tm, fac, fun, ...)

        # Create the output
        len <- length(dt)
        if (len == 1L)
          CFArray$new(name[1L], grp, dt[[1L]], private$values_type, ax, self$crs, atts)
        else {
          if (length(name) < len)
            name <- c(name, paste0("result_", (length(name)+1L):len))
          out <- lapply(1:len, function(i) CFArray$new(name[i], grp, dt[[i]], private$values_type, ax, self$crs, atts))
          names(out) <- name
          out
        }
      })
      if (length(f) == 1L)
        res[[1L]]
      else {
        names(res) <- names(f)
        res
      }
    },

    #' @description This method extracts profiles of values from the array of
    #'   the variable, with the location along each axis to extract expressed in
    #'   coordinate values of each axis.
    #'
    #' @details The coordinates along each axis to be sampled are expressed in
    #'   values of the domain of the axis. Any axes which are not passed as
    #'   arguments are extracted in whole to the result. If bounds are set on
    #'   the axis, the coordinate whose bounds envelop the requested coordinate
    #'   is selected. Otherwise, the coordinate along the axis closest to the
    #'   supplied value will be used. If the value for a specified axis falls
    #'   outside the valid range of that axis, `NULL` is returned.
    #'
    #'   A typical case is to extract the temporal profile as a 1D array for a
    #'   given location. In this case, use arguments for the latitude and
    #'   longitude on an X-Y-T data variable: `profile(lat = -24, lon = 3)`.
    #'   Other profiling options are also possible, such as a 2D zonal
    #'   atmospheric profile at a given longitude for an X-Y-Z data variable:
    #'   `profile(lon = 34)`.
    #'
    #'   Multiple profiles can be extracted in one call by supplying vectors for
    #'   the indicated axes: `profile(lat = c(-24, -23, -2), lon = c(5, 5, 6))`.
    #'   The vectors need not have the same length, unless `.as_table = TRUE`.
    #'   With unequal length vectors the result will be a `list` of [CFArray]
    #'   instances with different dimensionality and/or different axes.
    #'
    #'   ## Auxiliary coordinate variables (CFVariable only)
    #'
    #'   A special case exists for variables where the horizontal dimensions (X
    #'   and Y) are not in longitude and latitude coordinates but in some other
    #'   coordinate system. In this case the netCDF resource may have so-called
    #'   *auxiliary coordinate variables*. If the data variable has such
    #'   *auxiliary coordinate variables* then they will be used automatically
    #'   if, and only if, the axes are specified as `X` and `Y`. **Note** that
    #'   if you want to profile the data in the original grid units, you should
    #'   specify the horizontal axis names.
    #' @param ... One or more arguments of the form `axis = location`. The
    #'   "axis" part should be the name of an axis or its orientation `X`, `Y`,
    #'   `Z` or `T`. The "location" part is a vector of values representing
    #'   coordinates along the axis where to profile. A profile will be
    #'   generated for each of the elements of the "location" vectors in all
    #'   arguments.
    #' @param .names A character vector with names for the results. The names
    #'   will be used for the `CFArray` instances, or as values for the
    #'   "location" column of the `data.table` if argument `.as_table` is
    #'   `TRUE`. If the vector is shorter than the longest vector of locations
    #'   in the `...` argument, a name "location_#" will be used, with the #
    #'   replaced by the ordinal number of the vector element.
    #' @param .as_table Logical to flag if the results should be `CFArray`
    #'   instances (`FALSE`, default) or a single `data.table` (`TRUE`). If
    #'   `TRUE`, all `...` arguments must have the same number of elements, use
    #'   the same axes and the `data.table` package must be installed.
    #' @return If `.as_table = FALSE`, a list of [CFArray] instances, each
    #'   having one profile for each of the elements in the "location" vectors
    #'   of argument `...` and named with the respective `.names` value. If
    #'   `.as_table = TRUE`, a `data.table` with a row for each element along
    #'   all profiles, with a ".variable" column using the values from the
    #'   `.names` argument.
    profile = function(..., .names = NULL, .as_table = FALSE) {
      num_axes <- private$num_dim_axes()
      if (!num_axes)
        stop("Cannot profile a scalar variable", call. = FALSE)

      # Organize the selectors
      selectors <- list(...)
      sel_names <- names(selectors)
      sel_axes <- private$check_names(sel_names)
      sel_lengths <- sapply(selectors, length)
      total <- max(sel_lengths)

      # Convert domain values to indices
      if (all(c("X", "Y") %in% sel_names) && inherits(self, "CFVariable") &&
          inherits(private$llgrid, "CFAuxiliaryLongLat")) {
        # Only for CFVariable, CFArray never has an auxiliary lat-long grid
        xy <- private$llgrid$sample_index(selectors[["X"]], selectors[["Y"]])
        sel_indices <- lapply(1:length(selectors), function(x) {
          if (sel_names[x] == "X")
            idx <- xy[, 1L]
          else if (sel_names[x] == "Y")
            idx <- xy[, 2L]
          else
            idx <- self$axes[[sel_axes[x]]]$indexOf(selectors[[x]])
          idx[is.na(idx)] <- 0L
          idx
        })
      } else {
        xy <- NULL
        sel_indices <- lapply(1:length(selectors), function(x) {
          idx <- self$axes[[sel_axes[x]]]$indexOf(selectors[[x]])
          idx[is.na(idx)] <- 0L
          idx
        })
      }

      # Check .as_table
      if (.as_table) {
        if (!requireNamespace("data.table", quietly = TRUE))
          stop("Please install package 'data.table' before using this functionality", call. = FALSE)
        hasNA <- any(sapply(sel_indices, function(x) any(x == 0L)))
        if (!all(sel_lengths == sel_lengths[1L]) || hasNA)
          stop("All ... arguments must have the same length and use same axes when `.as_table = TRUE`.", call. = FALSE)
      }

      # Check .names
      len <- length(.names)
      if (len < total) {
        .names <- c(.names, sapply((len + 1L):total, function(i) paste0("location_", i)))
      } else if (len > total)
        .names <- .names[1L:total]

      # Group for results
      out_group <- NCGroup$new(-1L, "/", "/", NULL, NULL)
      out_group$set_attribute("title", "NC_CHAR", paste("Processing result of variable", self$name))
      out_group$set_attribute("history", "NC_CHAR", paste0(format(Sys.time(), "%FT%T%z"), " R package ncdfCF(", packageVersion("ncdfCF"), ")::CFVariable$profile()"))

      out <- vector("list", total)
      for (e in 1L:total) {
        indices <- sapply(sel_indices, function(x) x[e])
        if (!any(indices == 0L, na.rm = TRUE)) { # All selector coordinates are in range
          start <- rep(1L, num_axes)
          count <- rep(NA_integer_, num_axes)
          out_axes_dim <- list()
          out_axes_other <- list()

          for (ax in 1L:num_axes) {
            axis <- self$axes[[ax]]
            ax_ndx <- match(ax, sel_axes)
            if (!is.na(ax_ndx) && !is.na(indices[ax_ndx])) {
              start[ax] <- indices[ax_ndx]
              count[ax] <- 1L
              orient <- axis$orientation
              nm <- if (is.null(xy))
                axis$name
              else if (orient == "X")
                private$llgrid$varLong$name
              else
                private$llgrid$varLat$name
              x <- makeAxis(nm, out_group, orient, selectors[[ax_ndx]][e], NULL)
              out_axes_other <- append(out_axes_other, x)
            } else {
              out_axes_dim <- append(out_axes_dim, axis$subset(out_group))
            }
          }

          # Read the data
          d <- private$read_chunk(start, count)
          d <- drop(d)

          # Sanitize the attributes for the result, as required and make CRS
          if (is.null(xy)) {
            atts <- self$attributes
            crs <- self$crs
          } else {
            atts <- private$dropCoordinates(self$attributes,
                                            c(private$llgrid$varLong$name, private$llgrid$varLat$name))
            atts <- atts[!(atts$name == "grid_mapping"), ]  # drop: warped to lat-long
            crs <- NULL
          }

          # Assemble the CFArray instance
          scalars <- self$axes[-(1L:num_axes)]
          axes <- c(out_axes_dim, out_axes_other, scalars)
          names(axes) <- sapply(axes, function(a) a$name)
          arr <- CFArray$new(.names[e], out_group, d, private$values_type, axes, crs, atts)
          arr$add_coordinates(sapply(out_axes_other, function(ax) ax$name))
          out[[e]] <- if (.as_table) arr$data.table(var_as_column = TRUE)
          else arr
        }
      }

      if (.as_table) {
        atts <- attributes(out[[1L]])$value
        out <- data.table::rbindlist(out, use.names = FALSE)
        data.table::setattr(out, "value", atts)
      } else
        names(out) <- .names
      out
    }
  ),
  active = list(
    # #' @field axis_labels (read-only) Retrieve the names of any axes that have
    # #'   labels associated with them.
    # axis_labels = function(value) {
    #   ax <- sapply(self$axes, function(x) if (x$has_labels) x$name)
    #   ax <- unlist(ax[lengths(ax) > 0L])
    #   names(ax) <- NULL
    #   ax
    # }
  )
)
