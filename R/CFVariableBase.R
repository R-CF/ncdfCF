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
    # Return the R order of dimensional axes that "receive special treatment".
    # Scalar axes are not considered here.
    YXZT = function() {
      orient <- sapply(self$axes, function(x) if (!inherits(x, "CFAxisScalar")) x$orientation)
      match(c("Y", "X", "Z", "T"), orient, nomatch = 0L)
    },

    # Return the number of "dimensional" axes, i.e. axes that are associated
    # with a dimension of the data of the variable. This may include dimensions
    # with length 1, but it excludes scalar axes.
    num_dim_axes = function() {
      sum(sapply(self$axes, function(x) !inherits(x, "CFAxisScalar")))
    },

    # Return a vector with the two auxiliary coordinate variable names, if they
    # are set for the variable. This is only properly implemented for CFVariable.
    aux_var_names = function() {
      NULL
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

    #' @description Create an instance of this class.
    #' @param var The NC variable that describes this data object.
    #' @param group The group that this data object should live in.
    #' @param axes A `list` of [CFAxis] descendant instances that describe the
    #'   axes of the data object.
    #' @param crs The [CFGridMapping] instance of this data object, or `NULL`
    #'   when no grid mapping is available.
    #' @return An instance of this class.
    initialize = function(var, group, axes, crs) {
      super$initialize(var, group)
      self$axes <- axes
      self$crs <- crs
    },

    #' @description Return the time object from the axis representing time.
    #' @param want Character string with value "axis" or "time", indicating
    #' what is to be returned.
    #' @return If `want = "axis"` the [CFAxisTime] axis; if `want = "time"` the
    #' `CFTime` instance of the axis, or `NULL` if the variable does not have a
    #' "time" dimension.
    time = function(want = "time") {
      ndx <- sapply(self$axes, inherits, "CFAxisTime")
      if (any(ndx))
        if (want == "axis") self$axes[[which(ndx)]]
      else self$axes[[which(ndx)]]$time()
      else NULL
    },

    #' @description Summarise the temporal dimension of the data, if present, to
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
    #' @param period The period to summarise to. Must be one of either "day",
    #'   "dekad", "month", "quarter", "season", "year". A "quarter" is the
    #'   standard calendar quarter such as January-March, April-June, etc. A
    #'   "season" is a meteorological season, such as December-February,
    #'   March-May, etc. (any December data is from the year preceding the
    #'   January data). The period must be of lower resolution than the
    #'   resolution of the time dimension.
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
    #' @return A `CFData` object, or a list thereof with as many `CFData`
    #'   objects as `fun` returns values, created in the same group as `self`
    #'   with the summarised data.
    summarise = function(name, period, fun) {
      if (missing(name) || missing(period) || missing(fun))
        stop("Arguments 'name', 'period' and 'fun' are required.", call. = FALSE)
      if (!(period %in% c("day", "dekad", "month", "quarter", "season", "year")))
        stop("Argument 'period' has invalid value.", call. = FALSE)
      if (!.is_valid_name(name))
        stop("Not all names are valid.", call. = FALSE)

      # Find the time object, create the factor
      tax <- self$time("axis")
      if (is.null(tax))
        stop("No 'time' dimension found to summarise on.", call. = FALSE)
      fac <- try(tax$time()$factor(period), silent = TRUE)
      if (inherits(fac, "try-error"))
        stop("The 'time' dimension is too short to summarise on.", call. = FALSE)

      # Make a new time axis for the result
      new_tm <- attr(fac, "CFTime")
      var <- NCVariable$new(-1L, tax$name, self$group, "NC_DOUBLE", 1L, NULL)
      len <- length(new_tm)
      new_ax <- if (len == 1L)
        CFAxisScalar$new(self$group, var, "T", new_tm)
      else {
        dim <- NCDimension$new(-1L, tax$name, len, FALSE)
        CFAxisTime$new(self$group, var, dim, new_tm)
      }

      # Summarise
      tm <- sum(private$YXZT() > 0L) # Test which oriented axes are present, T is the last one
      dt <- private$process_data(tm, fac, fun, na.rm = TRUE)

      # Organise the axes
      ax <- c(new_ax, self$axes[-tm])
      names(ax) <- c("time", names(self$axes[-tm]))

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

      # FIXME: set cell_methods

      # Create the output
      len <- length(dt)
      if (len == 1L)
        CFArray$new(name[1L], self$group, dt[[1L]], ax, self$crs, atts)
      else {
        if (length(name) < len)
          name <- c(name, paste0("result_", (length(name)+1L):len))
        lapply(1:len, function(i) CFArray$new(name[i], self$group, dt[[i]], ax, self$crs, atts))
      }
    }
  )
)
