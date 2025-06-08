#' NetCDF resource object
#'
#' @description This class contains the connection details to a netCDF resource.
#'
#'   There is a single instance of this class for every netCDF resource, owned
#'   by the [CFDataset] instance. The instance is shared by other objects,
#'   specifically [NCGroup] instances, for access to the underlying resource for
#'   reading of data.
#'
#'   This class should never have to be accessed directly. All access is handled
#'   by higher-level methods.
#'
#' @docType class
#'
CFResource <- R6::R6Class("CFResource",
  private = list(
    .uri    = "",
    .handle = NULL,

    open = function() {
      if (!is.null(private$.handle)) {
        err <- try(RNetCDF::file.inq.nc(private$.handle), silent = TRUE)
        if (!inherits(err, "try-error"))
          return()
      }

      err <- try(private$.handle <- RNetCDF::open.nc(private$.uri), silent = TRUE)
      if (inherits(err, "try-error")) {
        self$error <- "Error opening netCDF resource"
        private$.handle <- NULL
      } else
        self$error <- ""
    },

    # This method is called automatically when the instance is
    # deleted, ensuring that file handles are properly closed.
    finalize = function() {
      self$close()
    }
  ),
  public = list(
    #' @field error Error message, or empty string.
    error = "",

    #' @description Create a connection to a netCDF resource. This is called by
    #'   [open_ncdf()] when opening a netCDF resource; you should never have to
    #'   call this directly.
    #'
    #' @param uri The URI to the netCDF resource.
    #' @return An instance of this class.
    initialize = function(uri) {
      private$.uri <- uri
      private$.handle <- NULL
      self$error <- ""
    },

    #' @description Closing an open netCDF resource. It should rarely be
    #'   necessary to call this method directly.
    close = function() {
      try(RNetCDF::close.nc(private$.handle), silent = TRUE)
      private$.handle <- NULL
    },

    #' @description Every group in a netCDF file has its own handle, with the
    #'   "root" group having the handle for the entire netCDF resource. The
    #'   handle returned by this method is valid only for the named group.
    #'
    #' @param group_name The absolute path to the group.
    #' @return The handle to the group.
    group_handle = function(group_name) {
      private$open()
      if (group_name == "/")
        private$.handle
      else
        RNetCDF::grp.inq.nc(private$.handle, group_name)$self
    }
  ),
  active = list(
    #' @field friendlyClassName (read-only) A nice description of the class.
    friendlyClassName = function(value) {
      if (missing(value))
        "NetCDF resource connection details"
    },

    #' @field handle (read-only) The handle to the netCDF resource.
    handle = function(value) {
      if (nzchar(self$error))
        return (self$error)

      private$open()
      if (missing(value))
        private$.handle
      else
        stop("Can't assign a value to a netCDF resource handle", call. = FALSE)
    },

    #' @field uri (read-only) The URI of the netCDF resource, either a local
    #'   filename or the location of an online resource.
    uri = function(value) {
      if (missing(value)) {
        private$.uri
      } else
        stop("Can't assign a new value to a netCDF resource URI", call. = FALSE)
    }
  )
)
