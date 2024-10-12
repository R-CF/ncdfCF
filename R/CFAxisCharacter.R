#' CF character axis object
#'
#' @description This class represent CF axes that use categorical character
#' labels as coordinate values.
#'
#' @docType class
#'
#' @export
CFAxisCharacter <- R6::R6Class("CFAxisCharacter",
 inherit = CFAxis,
 public = list(
   #' @field values The character labels of this axis.
   values     = NULL,

   #' @description Create a new instance of this class.
   #' @param grp The group that contains the netCDF variable.
   #' @param nc_var The netCDF variable that describes this instance.
   #' @param nc_dim The netCDF dimension that describes the dimensionality.
   #' @param orientation The orientation (`X`, `Y`, `Z`, or `T`) or `""` if
   #' different or unknown.
   #' @param values The character dimension values of this axis.
   initialize = function(grp, nc_var, nc_dim, orientation, values) {
     super$initialize(grp, nc_var, nc_dim, orientation)
     self$values <- values
   },

   #' @description Some details of the axis
   #'
   #' @return A 1-row `data.frame` with some details of the axis.
   brief = function() {
    out <- super$brief()
    out$values <- if (self$length) sprintf("[%s]", paste0(self$values, collapse = ", "))
                  else "(no values)"
    out
   },

   #' @title Find indices in the axis domain
   #'
   #' @description Given a vector of character strings `x`, find their indices
   #' in the values of the axis.
   #'
   #' @param x Vector of character strings to find axis indices for.
   #' @param method Ignored.
   #'
   #' @return Numeric vector of the same length as `x`. Values of `x` outside
   #' of the range of the values in the axis are returned as `NA`.
   indexOf = function(x, method = "constant") {
     match(x, self$values)
   }
 ),
 active = list(
   #' @field friendlyClassName (read-only) A nice description of the class.
   friendlyClassName = function(value) {
     if (missing(value))
       "Character axis"
   },

   #' @field dimnames (read-only) The coordinates of the axis as a character
   #' vector.
   dimnames = function(value) {
     if (missing(value))
       self$values
   }
 )
)
