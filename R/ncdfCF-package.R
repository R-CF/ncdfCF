#' ncdfCF: Easy Access to NetCDF Files and Interpreting with CF Metadata
#' Conventions
#'
#' Support for accessing and interpreting netCDF datasets in a familiar R style.
#' Built on top of the [`RNetCDF`
#' package](https://cran.r-project.org/package=RNetCDF), built and maintained by
#' the developers of the `netcdf` library, package `ncdfCF` provides high-level
#' access to netCDF resources. Resources are matched against the [Climate and
#' Forecast (CF) Metadata Conventions](https://cfconventions.org/) for climate
#' and forecasting data, current version 1.12. The CF Metadata Conventions is
#' widely used for distributing files with climate observations or projections,
#' including the Coupled Model Intercomparison Project (CMIP) data used by
#' climate change scientists and the Intergovernmental Panel on Climate Change
#' (IPCC), as well as large collections of satellite imagery, including from
#' Landsat and MODIS.
#'
#' This package currently supports most common features of the CF conventions,
#' including group traversal with scoping rules, auxiliary axes, time axis
#' interpretation with all defined calendars, grid mapping, use of bounds data,
#' manipulating and interpreting attributes of groups (including global
#' attributes) and variables, search for and use of standard names. Some
#' specific constructs in CF are also supported:
#'  * Axes can be oriented to access the data in the familiar R arrangement
#' (CF allows any data arrangement and most data sets indeed use an arrangement
#' that is not immediately useful in R).
#'  * The `CFVariable::subset()` function allows one to select subsets of data
#' using coordinate values along the axes (such as latitude values, or points in
#' time) rather than index values into the array.
#'  * Use of auxiliary grids to warp data variables using a non-default grid to
#' a regular latitude-longitude grid.
#'  * Calculate coordinate fields for parametric vertical axes.
#'
#' Properties of the netCDF resource objects are easily examined using common R
#' commands. Access to the data in the variables can be had using similarly
#' known patterns, and data can be exported to a variety of formats.
#'
#' This package is intended to access data from netCDF resources in a format
#' that is easily integrated with other R coding patterns and packages, with
#' full support for the CF Metadata Conventions that define the data properties.
#' This package does very little beyond that; specifically, there is no support
#' for spatial analysis, mosaicing, changing the coordinate reference system
#' (i.e. projection), or any significant form of data analysis at all. The user
#' is directed to other packages for such functionality.
#'
#' **Global functions**
#'
#' * [open_ncdf()]: Open a netCDF resource, either in a local file system or on
#' a THREDDS server, or through any other means supported by the `RNetCDF`
#' package. The return value is a `CFDataset`. Note that resources are
#' automatically closed.
#' * [peek_ncdf()]: Rapid inspection of objects in a netCDF resource, returning
#' information on variables, axes and global attributes with which intelligent
#' inferences can be made about a netCDF resource.
#'
#' **Data set**
#'
#' A [CFDataset] is the object that contains a netCDF resource. This is the main
#' class that you need to access netCDF data from a file or an online resource.
#'
#' * `show()`: Print information about the data set to the console. This will
#' print some information on the resource, as well as all identified data
#' variables, axes, and the global attributes.
#' * `name`: The name of the data set.
#' * `conventions`: The conventions that this data set complies with. There
#' could be multiple conventions listed in a single character string. CF
#' Metadata Conventions use a "CF-1.*" label.
#' * `variables()`, `axes()`, `attributes()`: Return a `list` or `data.frame` with
#' all objects of the specific type found in the data set.
#' * `find_by_name()`: Find a named object in the data set. This can be a data
#' variable, an axis, or any other named object. A short-hand method to achieve
#' the same is the `[[` operator. This also supports scanning for objects in
#' hierarchical groups in `netcdf4` resources.
#' * `objects_by_standard_name()`: Find objects that use a specific value for
#' the "standard_name" attribute, or return all objects that have such the
#' "standard_name" attribute irrespective of its value.
#' * `has_subgroups()`: Indicates if the data set has subgroups below the root
#' group.
#' * `hierarchy()`: Prints the group hierarchy in the data set to the console.
#'
#' ***S3 methods for CFDataset***
#'
#' * [names()]: Vector of names of the data variables in the data set.
#' * [dimnames()]: Vector of names of the axes in the data set.
#'
#' **Data variable**
#'
#' A [CFVariable] contains a single data variable from a data set. It contains
#' detailed information on the data variable and there are functions to access
#' the data, with different selection methods.
#'
#' ***Properties***
#'
#' * `show()`, `brief()`, and `shard()`: Print to the console or return to the
#' caller (increasingly more compact) information on a data variable.
#' * `name`, `id`: Basic properties of the data variable.
#' * `axes()`: List of `CFAxis` objects representing the axes that the data
#' variable uses.
#' * `attributes`: `data.frame` with the attributes of the data variable.
#' * `attribute()`: Retrieve the value of an attribute of the data variable.
#' * `crs`: The so-called grid-mapping object that contains information
#' on the coordinate reference system (CRS) of the data variable.
#' * `crs_wkt2`: The CRS of the data variable, in WKT2 format. This is derived
#' from the data in the grid-mapping object.
#'
#' ***Data extraction***
#'
#' * `subset()`: Select a subset of data from a variable by specifying extents
#' in real-world coordinates for the axes into a `CFVariable` object. This can
#' also process "auxiliary coordinate variables", if present, to warp data from
#' its native CRS to latitude-longitude.
#' * `summarise()`: Summarise the data in the data variable over the "time"
#' axis, using a user-defined function, returning a `CFVariable` object. The
#' function can be built-in (such as `min()` and `max()`) or a user-developed
#' function. The function may also return a vector with multiple values (such as
#' `range()`) and then a list of `CFVariable` objects is returned, one for each
#' result of the function.
#' * `profile()`: Extract a profile of data from the variable into a `CFVariable`
#' or a `data.table`. Profiles can be for a single location, or zonal (e.g.
#' across a longitude); multiple profiles can be extracted in a single call.
#' * `raw()`: The array of data values from the data object as read from the
#' netCDF resource.
#' * `array()`: The array of data values from the data object, oriented in the
#' standard R arrangement.
#' * `terra()`: (requires the `terra` package) The data values from the data
#' object as a `terra::SpatRaster` (2 or 3 dimensions) or
#' `terra::SpatRasterDataset` (4 dimensions), with all relevant properties set.
#' * `data.table()`: (requires the `data.table` package) The data values from
#' the data object as a `data.table` where every row consists of the permutation
#' of the axis values and a final data value.
#'
#' ***New data variables***
#'
#' New `CFVariable` objects can be constructed from R vectors, matrices or
#' arrays, optionally creating axes from dimnames on the R object, using the
#' [as_CF()] function.
#'
#' ***S3 methods for CFVariable***
#'
#' * [dim()], [dimnames()]: Vector of the lengths and coordinate values of the
#' axes of the data variable.
#' * `[]` ([bracket_select]): Select the entire data variable or a part thereof
#' using index values, returning an array of data values.
#'
#' **Axis**
#'
#' The `CFAxis` class is the common ancestor of specialized classes that
#' represent specific types of axes. These sub-classes are the ones that are
#' actually returned when retrieving an axis. These classes are:
#'
#' * [CFAxisNumeric] is a basic numeric axis, where the coordinate values
#' represent some physical property. The [CFAxisLongitude] and [CFAxisLatitude]
#' classes derive from the basic numeric class to manage the specifics of
#' geodetic coordinate systems. Class [CFAxisVertical] also derives from the
#' basic class to manage depth and height axes.
#' * [CFAxisTime] is a specialized class to deal with time axes. Under the CF
#' Metadata Conventions multiple different calendars have been defined and this
#' class deals with the complexities of all of these. Functionality is provided
#' by the `CFtime` package.
#' * [CFAxisCharacter] is for axes that use character labels as categorical
#' values.
#' * [CFAxisDiscrete] is for axes that don't have any intrinsic coordinate
#' values, instead the ordinal values along the axis are used.
#'
#' Any scalar axes that are found in a netCDF file are converted to one of the
#' above axis classes, with a length of 1.
#'
#' Any of the axis classes can have one or more coordinate sets associated with
#' them. This is most useful for `CFAxisDiscrete`. Labels of the active
#' coordinate set are used for display of axis properties, as well as for
#' selection in e.g. `CFVariable$subset()`.
#'
#' Methods for `CFAxis` instances:
#'
#' ***Properties***
#'
#' * `show()`, `brief()`, and `shard()`: Print to the console or return to the
#' caller (increasingly more compact) information on an axis.
#' * `name`, `id`: Basic properties of the axis.
#'
#' ***Extraction***
#'
#' * `indexOf()`: Retrieve the sub-range of the axis that encompasses the
#' physical values passed.
#' * `subset()`: Create a new `CFAxis` instance that spans a sub-range of the
#' axis.
#' * `time`: Retrieve the `CFTime` instance of the axis.
#'
#' ***Coordinates***
#'
#' * `coordinate_names`: Set or retrieve the names of the coordinate sets (not the
#' coordinates themselves).
#' * `active_coordinates`: Set or retrieve the coordinate set that is currently active
#' and used for display and selection.
#' * `auxiliary`: Set or retrieve the CF object that manages the active coordinate set,
#' either an instance of [CFLabel] or an [CFAxis] descendant.
#' * `coordinates`: Retrieve the coordinates of the active coordinate set. This may
#' be the coordinate values of the axis (say, longitude values) or a set of
#' auxiliary coordinates associated with the axis.
#' * `coordinate_range`: A vector with the extreme coordinate values of the axis.
#'
#' ***S3 methods for CFAxis***
#'
#' * [dim()], [dimnames()]: The length and coordinate values of the axis.
#'
#' @keywords internal
#' @aliases ncdfCF-package
"_PACKAGE"

## usethis namespace: start
## usethis namespace: end
NULL
