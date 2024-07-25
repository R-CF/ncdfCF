#' ncdfCF: Easy Access to NetCDF Files and Interpreting with CF Metadata Conventions
#'
#' Support for accessing and interpreting netCDF datasets in a familiar R style.
#' Built on top of the [`RNetCDF` package](https://cran.r-project.org/package=RNetCDF),
#' built and maintained by the developers of the `netcdf` library, package
#' `ncdfCF` provides high-level access to netCDF resources. Resources are matched
#' against the
#' [Climate and Forecast (CF) Metadata Conventions](https://cfconventions.org/)
#' for climate and forecasting data, current version 1.11. The CF Metadata
#' Conventions is widely used for distributing files with climate observations
#' or projections, including the Coupled Model Intercomparison Project (CMIP)
#' data used by climate change scientists and the Intergovernmental Panel on
#' Climate Change (IPCC).
#'
#' This package currently supports group traversal with scoping rules, axis
#' determination, time interpretation with all 9 defined calendars, use of
#' bounds data, and search for and use of standard names.
#'
#' Properties of the netCDF resource objects are easily examined using common
#' R commands. Access to the data in the variables can be had using similarly
#' known patterns.
#'
#' **Open, inquire**
#' * [open_ncdf()]: Open a netCDF resource, either in a local file system or on
#' a THREDDS server. Note that resources are automatically closed.
#' * [show()], `brief()`, and `shard()`: Print (increasingly more compact)
#' information to the console for a data set, variable, or axis.
#' * [dimnames()]: Vector of names of the axes in the data set or variable,
#' or a vector of coordinate values for an axis.
#' * [dim()], [length()]: Vector of the dimension lengths for a data set or
#' variable, or the length of a single axis
#' * [axis()]: The orientation of an axis.
#' * [time()]: Return the [CFtime](https://cran.r-project.org/web//packages//CFtime/index.html)
#' instance of the axis, or `NULL` if not a time axis
#'
#' **Filtering and selection**
#' * [`[]`][bracket_select]: Select the entire variable or a part thereof
#' using index values.
#' * [subset()]: Select a subset from a variable by specifying extents in
#' real-world coordinates for the axes.
#' * [indexOf()]: Index values into the axis from real-world coordinates,
#' possibly with fractional part for interpolation.
#' @keywords internal
#' @aliases ncdfCF-package
"_PACKAGE"

## usethis namespace: start
## usethis namespace: end
NULL
