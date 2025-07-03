#' Vertical CF axis object
#'
#' @description This class represents a vertical axis, which may be parametric.
#'   It is defined through an index value that is contained in the axis, with
#'   additional [NCVariable] instances that hold ancillary data with which to
#'   calculate dimensional axis values. It is used in atmosphere and ocean data
#'   sets. Non-parametric vertical axes are stored in an [CFAxisNumeric]
#'   instance.
#'
#' @references
#' https://cfconventions.org/Data/cf-conventions/cf-conventions-1.12/cf-conventions.html#parametric-vertical-coordinate
#'
#' @docType class
#' @export
CFAxisVertical <- R6::R6Class("CFAxisVertical",
  inherit = CFAxisNumeric,
  private = list(
    # A `data.frame` with columns `terms`, `variable` and `NCvar` containing the
    # terms of the formula to calculate the axis values. Column `NCvar` has the
    # references to the [NCVariable]s that hold the data for the terms. Column
    # `data` is added when the data are loaded and contains an array (matrix,
    # vector) with the data for the formula term.
    terms = NULL,

    # The computed values of the parametric axis.
    computed_values = NULL,

    # Print some details of the parametric definition.
    print_details = function(...) {
      if (!is.null(private$terms)) {
        if (is.null(private$computed_values)) private$compute()

        cat("\nParametric definition\n")
      }
    },

    # This function computes the actual dimensional axis values from the terms.
    compute = function() {
      if (any(is.null(ft$NCvar))) return()

      # Load the data, as necessary

      # Call the appropriate function
      func <- try(match.fun(self$parameter_name, FALSE), silent = TRUE)
      if (inherits(func, "try-error")) return()
      private$computed_values <- func(private$terms)
    }
  ),
  public = list(
    #' @field parameter_name The 'standard_name' attribute of the [NCVariable]
    #' that identifies the parametric form of this axis.
    parameter_name = "",

    #' @field computed_name The standard name for the computed values of the
    #'   axis.
    computed_name = "",

    #' @field computed_units The unit of the computed values of the axis.
    computed_units = "",

    #' @description Create a new instance of this class.
    #' @param nc_var The netCDF variable that describes this instance.
    #' @param nc_dim The netCDF dimension that describes the dimensionality.
    #' @param values The coordinates of this axis.
    #' @param standard_name Character string with the "standard_name" that
    #' defines the meaning, and processing of coordinates, of this axis. In
    #' particular, the "standard_name may indicate that this is a parametric
    #' vertical axis.
    initialize = function(nc_var, nc_dim, values, standard_name) {
      super$initialize(nc_var, nc_dim, "Z", values)
      self$parameter_name <- standard_name
      self$set_attribute("actual_range", nc_var$vtype, range(values))

      # If formula_terms attribute is set, create the parametric bounds
      ft <- nc_var$attribute("formula_terms")
      if (!is.na(ft)) {
        ft <- trimws(strsplit(ft, " ")[[1L]], whitespace = ":")
        dim(ft) <- c(2, length(ft) * 0.5)
        rownames(ft) <- c("term", "variable")
        ft <- as.data.frame(t(ft))
        ft$NCvar <- lapply(ft$variable, function(v) {
          ncvar <- nc_var$group$find_by_name(v, "NC")
          if (!is.null(ncvar)) {
            ncvar$CF <- self
            ncvar
          }
        })
        if (any(is.null(ft$NCvar)))
          warning("Not all formula terms for the parametric vertical axis were found", call. = FALSE)
        private$terms <- ft
      }
    }
  ),
  active = list(
    #' @field friendlyClassName (read-only) A nice description of the class.
    friendlyClassName = function(value) {
      if (missing(value))
        if (is.null(private$terms)) "Vertical axis"
        else "Vertical axis (parametric)"
    },

    #' @field formula_terms (read-only) A `data.frame` with the "formula_terms"
    #' to calculate the parametric axis values.
    formula_terms = function(value) {
      if (missing(value))
        private$terms
    }
  )
)

# The functions to calculate parametric coordinate values ======================

# All functions are named after the CF "standard_name" attribute for the
# parametric coordinate values. All functions have a single argument "vars"
# which is a data.frame as in private$terms in the class above. All functions
# return an array (or matrix or vector) as appropriate for the function and its
# supplied data values.

ocean_s_coordinate_g1 <- function(vars) {
  # z(n,k,j,i) = S(k,j,i) + eta(n,j,i) * (1 + S(k,j,i) / depth(j,i))
  # where S(k,j,i) = depth_c * s(k) + (depth(j,i) - depth_c) * C(k)
  s <- vars[[vars$term == "s", "data"]]; if (is.null(s)) s <- 0
  C <- vars[[vars$term == "C", "data"]]; if (is.null(C)) C <- 0
  eta <- vars[[vars$term == "era", "data"]]; if (is.null(era)) era <- 0
  depth <- vars[[vars$term == "depth", "data"]]; if (is.null(depth)) depth <- 1 # Non-standard, avoid division by 0
  depth_c <- vars[[vars$term == "depth_c", "data"]]; if (is.null(depth_c)) depth_c <- 0

  S <- sweep((depth - depth_c) %o% C, 3, depth_c * s, "+")
  S + eta * (sweep(S, MARGIN = 1:2, depth, "/") + 1)
}
