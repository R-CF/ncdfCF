#' NetCDF dimension object
#'
#' @description This class represents an netCDF dimensions. It contains the
#'   information on a dimension that is stored in an netCDF file. Consequently,
#'   the properties of this class are all read-only. The length of the dimension
#'   may change if data is written to an unlimited dimension, but that is
#'   managed internally.
#'
#'   This class is not very useful for interactive use. Use the [CFAxis]
#'   descendent classes instead.
#'
#' @docType class
#'
NCDimension <- R6::R6Class("NCDimension",
  inherit = NCObject,
  private = list(
    # The length of the dimension.
    .length = 0L,

    # Flag to indicate if the dimension is unlimited.
    .unlim = FALSE
  ),
  public = list(
    #' @description Create a new netCDF dimension. This class should not be
    #'   instantiated directly, create CF objects instead. This class is
    #'   instantiated when opening a netCDF resource.
    #'
    #' @param id Numeric identifier of the netCDF dimension.
    #' @param name Character string with the name of the netCDF dimension.
    #' @param length Length of the dimension.
    #' @param unlim Is the dimension unlimited?
    #' @param group The group where the dimension is located.
    #' @return A `NCDimension` instance.
    initialize = function(id, name, length, unlim, group) {
      super$initialize(id, name)
      private$.length <- length
      private$.unlim <- unlim

      # Add self to the group
      # FIXME: Must be a NCGroup method
      group$NCdims <- append(group$NCdims, setNames(list(self), name))
    },

    #' @description Summary of the NC dimension printed to the console.
    #' @param ... Passed on to other methods.
    print = function(...) {
      cat("<netCDF dimension> [", self$id, "] ", self$name, "\n", sep = "")
      cat("Length       :", private$.length, "\n")
      cat("Unlimited    :", private$.unlim, "\n")
    },

    #' @description Write the dimension to a netCDF file.
    #' @param h The handle to the netCDF file to write.
    #' @return Self, invisibly.
    write = function(h) {
      # Error will be thrown when trying to write a dimension that's already
      # defined, such as when a dimension is shared between multiple objects.
      # This error can be safely ignored.
      did <- try(if (private$.unlim)
            RNetCDF::dim.def.nc(h, self$name, unlim = TRUE)
          else
            RNetCDF::dim.def.nc(h, self$name, private$.length),
          silent = TRUE)
      if (inherits(did, "try-error"))
        did <- RNetCDF::dim.inq.nc(h, self$name)$id
      self$id <- did
      invisible(self)
    }
  ),
  active = list(
    #' @field length (read-only) The length of the dimension. If field `unlim =
    #'   TRUE`, this field indicates the length of the data in this dimension
    #'   written to file.
    length = function(value) {
      if (missing(value))
        private$.length
    },

    #' @field unlim (read-only) Logical flag to indicate if the dimension is
    #'   unlimited, i.e. that additional data may be written to file
    #'   incrementing this dimension.
    unlim = function(value) {
      if (missing(value))
        private$.unlim
    }
  )
)
