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
    }
  ),
  public = list(
    error = "",

    initialize = function(uri) {
      private$.uri <- uri
      private$.handle <- NULL
      self$error <- ""
    },

    finalize = function() {
      self$close()
    },

    close = function() {
      try(RNetCDF::close.nc(private$.handle), silent = TRUE)
      private$.handle <- NULL
    },

    group_handle = function(group_name) {
      private$open()
      if (group_name == "/")
        private$.handle
      else
        RNetCDF::grp.inq.nc(private$.handle, group_name)$self
    }
  ),
  active = list(
    handle = function(value) {
      if (nzchar(self$error))
        return (self$error)

      private$open()
      if (missing(value))
        private$.handle
      else
        stop("Can't assign a value to a netCDF resource handle", call. = FALSE)
    },

    uri = function(value) {
      if (missing(value)) {
        private$.uri
      } else
        stop("Can't assign a new value to a netCDF resource URI", call. = FALSE)
    }
  )
)
