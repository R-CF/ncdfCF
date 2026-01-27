#' Create a plot object for a `CFVariable`
#' @description This is a basic function to support plotting of `ncdfCF` data
#' with the `ggplot2` package. Specifically, this function creates a `geo_ncdf`
#' object which can be used like a `geom_raster`. The `geom_ncdf` takes a
#' `CFVariable` instance as its `data`. The `CFVariable` should be properly
#' pre-processed to make it suitable for plotting. The `$subset()` method is
#' well suited for this task. Note that currently only map plotting works, e.g.
#' the `CFVariable` should have `X` and `Y` axes.
#' @param mapping As in \link[ggplot2:geom_tile]{geom_raster}. If the argument
#' is not provided, a mapping is constructed from the properties of the `data`
#' argument, which is usually the right way.
#' @param data A [CFVariable] instance. This will override any `data` setting of
#' the `ggplot()` function.
#' @param ... Arguments passed on to `geom_raster()`.
#' @return A `geom_*` object that can be used in `ggplot2` plot composition.
#' @export
#' @examples
#' library(ggplot2)
#' fn <- system.file("extdata", "tasmax_NAM-44_day_20410701-vncdfCF.nc", package = "ncdfCF")
#' ds <- open_ncdf(fn)
#' tasmax <- ds[["tasmax"]]
#' ggplot() + geom_ncdf(data = tasmax) + coord_equal() + scale_fill_viridis_c()
geom_ncdf <- function(mapping = NULL, data, ...) {
  if (!requireNamespace("ggplot2", quietly = TRUE))
    stop("Please install package 'ggplot2' before using this functionality")

  if (!inherits(data, "CFVariable"))
    stop("`data` argument must be a CFVariable object.", call. = FALSE)

  # Analyse the axes
  ax_crds <- lapply(data$axes, function(ax) if (ax$length > 1L) ax$coordinates)
  ax_crds <- ax_crds[lengths(ax_crds) > 0L]
  if (!length(ax_crds))
    stop("Not enough data to plot.", call. = FALSE)

  ax_names <- names(data$axes)
  ax_orient <- sapply(data$axes, function(ax)  ax$orientation)
  ax_order <- match(c("X", "Y", "Z", "T"), ax_orient, nomatch = 0L)
  XY <- ax_order[1L] > 0L && ax_order[2L] > 0L

  # Create a data.frame
  plot_df <- expand.grid(ax_crds, KEEP.OUT.ATTRS = FALSE, stringsAsFactors = FALSE)
  dt <- data$values
  dim(dt) <- NULL
  plot_df <- cbind(plot_df, setNames(list(dt), data$name))
  plot_df <- stats::na.omit(plot_df)

  # Set the mapping
  if (is.null(mapping)) {
    # Figure out what x and y should be
    mapping <- if (XY)
      ggplot2::aes(x = !!ggplot2::sym(ax_names[ax_order[1L]]),
                   y = !!ggplot2::sym(ax_names[ax_order[2L]]),
                   fill = !!ggplot2::sym(data$name))
    else {
      names <- names(plot_df)
      ggplot2::aes(x = !!ggplot2::sym(names[1L]),
                   y = !!ggplot2::sym(names[2L]),
                   fill = !!ggplot2::sym(data$name))
    }
  }

  ggplot2::geom_raster(mapping = mapping, data = plot_df, ...)
}
