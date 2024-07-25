CFAxisCharacter <- R6::R6Class("CFAxisCharacter",
 inherit = CFAxis,
 public = list(
   values     = NULL,

   initialize = function(grp, nc_var, nc_dim, values) {
     super$initialize(grp, nc_var, nc_dim)
     self$values <- values
   },

   brief = function() {
    out <- super$brief()
    out$values <- if (self$length) sprintf("[%s]", paste0(self$values, collapse = ", "))
                  else "(no values)"
    out
   },

   indexOf = function(x, method = "constant") {
     match(x, self$values)
   }
 )
)

# Public S3 methods ------------------------------------------------------------

#' @export
dimnames.CFAxisCharacter <- function(x) {
  x$values
}
