#' ncdfCF: Easy Access to NetCDF Files and Interpreting with CF Metadata Conventions
#'
#' Support for accessing and interpreting NetCDF datasets in a familiar R style.
#' Built on top of the [`RNetCDF` package](https://cran.r-project.org/package=RNetCDF),
#' built and maintained by the developers of the `netcdf` library, package
#' `ncdfCF` provides high-level access to NetCDF resources. Resources are matched
#' against the
#' [Climate and Forecast (CF) Metadata Conventions](https://cfconventions.org/)
#' for climate and forecasting data. The CF Metadata Conventions is widely used
#' for distributing files with climate observations or projections, including
#' the Coupled Model Intercomparison Project (CMIP) data used by climate change
#' scientists and the Intergovernmental Panel on Climate Change (IPCC). This
#' package currently supports axis determination, time interpretation with all
#' 9 defined calendars, and use of bounds data.
#'
#' Properties of the NetCDF resource objects are easily examined using common
#' R commands. Access to the data in the variables can be had using similarly
#' known patterns.
#'
#' **Open, inquire**
#' * [open_ncdf()]: Open a NetCDF resource, either in a local file system or on
#' a THREDDS server. Note that resources are automatically closed.
#' * [show()], [brief()], and [shard()]: Print (increasingly more compact)
#' information to the console for a dataset, variable, or dimension.
#' * [dimnames()]: Vector of names of the dimensions in the dataset or variable,
#' or a vector of coordinate values for a dimension.
#' * [dim()], [length()]: Vector of the dimension lengths for a dataset or
#' variable, or the length of a single dimension.
#' * [axis()]: The axis of the dimension.
#' * [has_bounds()]: Does the dimension have bounds set?
#' * [time()]: Return the [CFtime](https://cran.r-project.org/web//packages//CFtime/index.html)
#' instance of the dimension, or `NULL` if not a time dimension.
#'
#' **Filtering and selection**
#' * [`[]`][bracket_select]: Select the entire variable or a part thereof
#' using index values.
#' * [subset()]: Select a subset from a variable by specifying extents in
#' real-world coordinates for the dimensions.
#' * [indexOf()]: Index values into the dimension from real-world coordinates,
#' possibly with fractional part for interpolation.
#' @keywords internal
#' @aliases ncdfCF-package
"_PACKAGE"

## usethis namespace: start
## usethis namespace: end
NULL
