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
NCResource <- R6::R6Class("NCResource",
  private = list(
    .uri    = "",
    .handle = NULL,
    .write = FALSE,

    open = function() {
      if (!is.null(private$.handle)) {
        err <- try(RNetCDF::file.inq.nc(private$.handle), silent = TRUE)
        if (!inherits(err, "try-error"))
          return()
      }

      err <- try(private$.handle <- RNetCDF::open.nc(private$.uri, write = private$.write, share = TRUE), silent = TRUE)
      if (inherits(err, "try-error")) {
        self$error <- paste("Error opening netCDF resource:", private$.uri)
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
    #'   [open_ncdf()] when opening a netCDF resource or when saving a dataset
    #'   to file. You should never have to call this directly.
    #'
    #' @param uri The URI to the netCDF resource.
    #' @param write Logical flag to indicate if the resource should be
    #'   read-write.
    #' @return An instance of this class.
    initialize = function(uri, write) {
      private$.uri <- uri
      private$.write <- write
      private$open()
    },

    #' @description Print a summary of the netCDF resource to the console.
    #' @return Self, invisibly.
    print = function() {
      cat("<netCDF resource>", private$.uri, "\n")
      cat("Open    :", if (is.null(private$.handle)) "\u2718" else "\u2714", "\n")
      cat("Writable:", if (private$.write) "\u2714" else "\u2718", "\n")

      invisible(self)
    },

    #' @description Create a new file on disk for the netCDF resource.
    #' @return Self, invisibly.
    create = function() {
      private$.handle <- try(RNetCDF::create.nc(private$.uri, prefill = FALSE, format = "netcdf4"), silent = TRUE)
      if (!inherits(private$.handle, "NetCDF"))
        stop("Could not create the netCDF file. Please check that the location of the supplied file name is writable.", call. = FALSE)
      self$error <- ""
      invisible(self)
    },

    #' @description Closing an open netCDF resource. It should rarely be
    #'   necessary to call this method directly.
    close = function() {
      try(RNetCDF::close.nc(private$.handle), silent = TRUE)
      private$.handle <- NULL
    },

    # #' @description Write any buffered data to the netCDF resource.
    # sync = function() {
    #   # This method is superfluous as netCDF files are opened with share = TRUE
    #   private$open()
    #   try(RNetCDF::sync.nc(private$.handle), silent = TRUE)
    # },

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
    },

    #' @field can_write (read-only) Is the resource writable?
    can_write = function(value) {
      if (missing(value))
        private$.write
    }
  )
)
