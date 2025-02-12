#' NetCDF dimension object
#'
#' @description This class represents an netCDF dimensions. It contains the
#'   information on a dimension that is stored in an netCDF file.
#'
#' This class is not very useful for interactive use. Use the [CFAxis]
#' descendent classes instead.
#'
#' @docType class
#'
NCDimension <- R6::R6Class("NCDimension",
  inherit = NCObject,
  public = list(
    #' @field length The length of the dimension. If field `unlim = TRUE`, this
    #' field indicates the length of the data in this dimension written to file.
    length = 0L,

    #' @field unlim Logical flag to indicate if the dimension is unlimited, i.e.
    #'   that additional data may be written to file incrementing in this
    #'   dimension.
    unlim  = FALSE,

    #' @description Create a new netCDF dimension. This class should not be
    #'   instantiated directly, create CF objects instead. This class is
    #'   instantiated when opening a netCDF resource.
    #'
    #' @param id Numeric identifier of the netCDF dimension.
    #' @param name Character string with the name of the netCDF dimension.
    #' @param length Length of the dimension.
    #' @param unlim Is the dimension unlimited?
    #' @return A `NCDimension` instance.
    initialize = function(id, name, length, unlim) {
      super$initialize(id, name)
      self$length <- length
      self$unlim <- unlim
    },

    #' @description Summary of the NC dimension printed to the console.
    #' @param ... Passed on to other methods.
    print = function(...) {
      cat("<netCDF dimension> [", self$id, "] ", self$name, "\n", sep = "")
      cat("Length       :", self$length, "\n")
      cat("Unlimited    :", self$unlim, "\n")
    },

    #' @description Very concise information on the dimension. The information
    #'   returned by this function is very concise and most useful when combined
    #'   with similar information from other dimensions.
    #'
    #' @return Character string with very basic dimension information.
    shard = function() {
      unlim <- if (self$unlim) "-U" else ""
      paste0("[", self$id, ": ", self$name, " (", self$length, unlim, ")]")
    },

    #' @description Write the dimension to a netCDF file.
    #' @param h The handle to the netCDF file to write.
    write = function(h) {
      # Error will be thrown when trying to write a dimension that's already
      # defined, such as when a dimension is shared between multiple objects.
      # This error can be safely ignored.
      try(if (self$unlim)
            RNetCDF::dim.def.nc(h, self$name, unlim = TRUE)
          else
            RNetCDF::dim.def.nc(h, self$name, self$length),
          silent = TRUE)
    }
  )
)
