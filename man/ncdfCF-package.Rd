% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ncdfCF-package.R
\docType{package}
\name{ncdfCF-package}
\alias{ncdfCF}
\alias{ncdfCF-package}
\title{ncdfCF: Easy Access to NetCDF Files and Interpreting with CF Metadata
Conventions}
\description{
Support for accessing and interpreting netCDF datasets in a familiar R style.
Built on top of the \href{https://cran.r-project.org/package=RNetCDF}{\code{RNetCDF} package}, built and maintained by
the developers of the \code{netcdf} library, package \code{ncdfCF} provides high-level
access to netCDF resources. Resources are matched against the \href{https://cfconventions.org/}{Climate and Forecast (CF) Metadata Conventions} for climate
and forecasting data, current version 1.12. The CF Metadata Conventions is
widely used for distributing files with climate observations or projections,
including the Coupled Model Intercomparison Project (CMIP) data used by
climate change scientists and the Intergovernmental Panel on Climate Change
(IPCC), as well as large collections of satellite imagery, including from
Landsat and MODIS.
}
\details{
This package currently supports most common features of the CF conventions,
including group traversal with scoping rules, auxiliary axes, time axis
interpretation with all defined calendars, grid mapping, use of bounds data,
manipulating and interpreting attributes of groups (including global
attributes) and variables, search for and use of standard names. Some
specific constructs in CF are also supported:
\itemize{
\item Axes can be oriented to access the data in the familiar R arrangement
(CF allows any data arrangement and most data sets indeed use an arrangement
that is not immediately useful in R).
\item The \code{CFVariable::subset()} function allows one to select subsets of data
using coordinate values along the axes (such as latitude values, or points in
time) rather than index values into the array.
\item Use of auxiliary grids to warp data variables using a non-default grid to
a regular latitude-longitude grid.
\item Calculate coordinate fields for parametric vertical axes.
}

Properties of the netCDF resource objects are easily examined using common R
commands. Access to the data in the variables can be had using similarly
known patterns, and data can be exported to a variety of formats.

This package is intended to access data from netCDF resources in a format
that is easily integrated with other R coding patterns and packages, with
full support for the CF Metadata Conventions that define the data properties.
This package does very little beyond that; specifically, there is no support
for spatial analysis, mosaicing, changing the coordinate reference system
(i.e. projection), or any significant form of data analysis at all. The user
is directed to other packages for such functionality.

\strong{Global functions}
\itemize{
\item \code{\link[=open_ncdf]{open_ncdf()}}: Open a netCDF resource, either in a local file system or on
a THREDDS server, or through any other means supported by the \code{RNetCDF}
package. The return value is a \code{CFDataset}. Note that resources are
automatically closed.
\item \code{\link[=peek_ncdf]{peek_ncdf()}}: Rapid inspection of objects in a netCDF resource, returning
information on variables, axes and global attributes with which intelligent
inferences can be made about a netCDF resource.
}

\strong{Data set}

A \link{CFDataset} is the object that contains a netCDF resource. This is the main
class that you need to access netCDF data.
\itemize{
\item \code{show()}: Print information about the data set to the console. This will
print some information on the resource, as well as all identified data
variables, axes, and the global attributes.
\item \code{name}: The name of the data set.
\item \code{conventions}: The conventions that this data set complies with. There
could be multiple conventions listed in a single character string. CF
Metadata Conventions use a "CF-1.*" label.
\item \code{variables()}, \code{axes()}, \code{attributes()}: Return a \code{list} or \code{data.frame} with
all objects of the specific type found in the data set.
\item \code{find_by_name()}: Find a named object in the data set. This can be a data
variable, an axis, or any other named object. A short-hand method to achieve
the same is the \code{[[} operator. This also supports scanning for objects in
hierarchical groups in \code{netcdf4} resources.
\item \code{objects_by_standard_name()}: Find objects that use a specific value for
the "standard_name" attribute, or return all objects that have such the
"standard_name" attribute irrespective of its value.
\item \code{has_subgroups()}: Indicates if the data set has subgroups below the root
group.
\item \code{hierarchy()}: Prints the group hierarchy in the data set to the console.
}

\emph{\strong{S3 methods for CFDataset}}
\itemize{
\item \code{\link[=names]{names()}}: Vector of names of the data variables in the data set.
\item \code{\link[=dimnames]{dimnames()}}: Vector of names of the axes in the data set.
}

\strong{Data variable}

A \link{CFVariable} contains a single data variable from a data set. It contains
detailed information on the data variable and there are functions to access
the data, with different selection methods.

\emph{\strong{Properties}}
\itemize{
\item \code{show()}, \code{brief()}, and \code{shard()}: Print to the console or return to the
caller (increasingly more compact) information on a data variable.
\item \code{name}, \code{id}: Basic properties of the data variable.
\item \code{axes()}: List of \code{CFAxis} objects representing the axes that the data
variable uses.
\item \code{attributes}: \code{data.frame} with the attributes of the data variable.
\item \code{attribute()}: Retrieve the values of an attribute of the data variable.
\item \code{crs}: The so-called grid-mapping object that contains information
on the coordinate reference system (CRS) of the data variable.
\item \code{crs_wkt2}: The CRS of the data variable, in WKT2 format. This is derived
from the data in the grid-mapping object.
}

\emph{\strong{Data extraction}}
\itemize{
\item \code{data()}: Extract all data from a data variable into a \code{CFArray} object.
\item \code{subset()}: Select a subset of data from a variable by specifying extents
in real-world coordinates for the axes into a \code{CFArray} object. This can also
process "auxiliary coordinate variables", if present, to warp data from its
native CRS to latitude-longitude.
\item \code{summarise()}: Summarise the data in the data variable over the "time"
axis, using a user-defined function, returning a \code{CFArray} object. The
function can be built-in (such as \code{min()} and \code{max()}) or a user-developed
function. The function may also return a vector with multiple values (such as
\code{range()}) and then a list of \code{CFArray} objects is returned, one for each
result of the function.
\item \code{profile()}: Extract a profile of data from the variable into a \code{CFArray}
or a \code{data.table}. Profiles can be for a single location, or zonal (e.g.
across a longitude); multiple profiles can be extracted in a single call.
}

\emph{\strong{S3 methods for CFVariable}}
\itemize{
\item \code{\link[=dim]{dim()}}, \code{\link[=dimnames]{dimnames()}}: Vector of the lengths and coordinate values of the
axes of the data variable.
\item \verb{[]} (\link{bracket_select}): Select the entire data variable or a part thereof
using index values, returning an array of data values.
}

\strong{Axis}

The \code{CFAxis} class is the common ancestor of specialized classes that
represent specific types of axes. These sub-classes are the ones that are
actually returned when retrieving an axis. These classes are:
\itemize{
\item \link{CFAxisNumeric} is a basic numeric axis, where the coordinate values
represent some physical property. The \link{CFAxisLongitude} and \link{CFAxisLatitude}
classes derive from the basic numeric class to manage the specifics of
geodetic coordinate systems. Class \link{CFAxisVertical} also derives from the
basic class to manage depth and height axes.
\item \link{CFAxisTime} is a specialized class to deal with time axes. Under the CF
Metadata Conventions multiple different calendars have been defined and this
class deals with the complexities of all of these. Functionality is provided
by the \code{CFtime} package.
\item \link{CFAxisCharacter} is for axes that use character labels as categorical
values.
\item \link{CFAxisDiscrete} is for axes that don't have any intrinsic coordinate
values, instead the ordinal values along the axis are used.
}

Any scalar axes that are found in a netCDF file are converted to one of the
above axis classes, with a length of 1.

Any of the axis classes can have one or more coordinate sets associated with
them. This is most useful for \code{CFAxisDiscrete}. Labels of the active
coordinate set are used for display of axis properties, as well as for
selection in e.g. \code{CFVariable$subset()}.

Methods for \code{CFAxis} instances:

\emph{\strong{Properties}}
\itemize{
\item \code{show()}, \code{brief()}, and \code{shard()}: Print to the console or return to the
caller (increasingly more compact) information on an axis.
\item \code{name}, \code{id}: Basic properties of the axis.
}

\emph{\strong{Extraction}}
\itemize{
\item \code{indexOf()}: Retrieve the sub-range of the axis that encompasses the
physical values passed.
\item \code{subset}: Create a new \code{CFAxis} instance that spans a sub-range of the
axis.
\item \code{time()}: Retrieve the \code{CFTime} instance of the axis.
}

\emph{\strong{Coordinates}}
\itemize{
\item \code{coordinate_names}: Set or retrieve the names of the coordinate sets (not the
coordinates themselves).
\item \code{active_coordinates}: Set or retrieve the coordinate set that is currently active
and used for display and selection.
\item \code{auxiliary}: Set or retrieve the CF object that manages the active coordinate set,
either an instance of \link{CFLabel} or an \link{CFAxis} descendant.
\item \code{coordinates}: Retrieve the coordinates of the active coordinate set. This may
be the coordinate values of the axis (say, longitude values) or a set of
auxiliary coordinates associated with the axis.
\item \code{coordinate_range}: A vector with the extreme coordinate values of the axis.
}

\emph{\strong{S3 methods for CFAxis}}
\itemize{
\item \code{\link[=dim]{dim()}}, \code{\link[=dimnames]{dimnames()}}: The length and coordinate values of the axis.
}

\strong{Data}

The \code{CFVariable::data()}, \code{subset()} and \code{profile()} methods return a
\link{CFArray} object to the caller. The \code{CFArray} object contains the data from
the netCDF resource, as well as important metadata from the data variable:
axes, CRS and attributes. The data is easily accessed as a raw array, or
processed to some other format.

\emph{\strong{Properties}}
\itemize{
\item \code{show()}: Print information about the data object to the console, including
axes, the CRS and its attributes.
\item \code{axes()}: List of \code{CFAxis} objects representing the axes that the data
object uses.
\item \code{crs}: A WKT2-formatted string of the CRS of the data object.
}

\emph{\strong{Data extraction}}
\itemize{
\item \code{raw()}: The array of data values from the data object as read from the
netCDF resource.
\item \code{array()}: The array of data values from the data object, oriented in the
standard R arrangement.
\item \code{summarise()}: Summarise the data in the data array over the "time"
axis, using a user-defined function, returning a new \code{CFArray} object. The
function can be built-in (such as \code{min()} and \code{max()}) or a user-developed
function. The function may also return a vector with multiple values (such as
\code{range()}) and then a list of \code{CFArray} objects is returned, one for each
result of the function.
\item \code{terra()}: (requires the \code{terra} package) The data values from the data
object as a \code{terra::SpatRaster} (2 or 3 dimensions) or
\code{terra::SpatRasterDataset} (4 dimensions), with all relevant properties set.
\item \code{data.table()}: (requires the \code{data.table} package) The data values from
the data object as a \code{data.table} where every row consists of the permutation
of the axis values and a final data value.
}

\emph{\strong{New data arrays}}

New \code{CFArray} objects can be constructed from R vectors, matrices or arrays,
optionally creating axes from dimnames on the R object, using the
\code{as_CFArray()} function.
}
\seealso{
Useful links:
\itemize{
  \item \url{https://github.com/R-CF/ncdfCF}
  \item Report bugs at \url{https://github.com/R-CF/ncdfCF/issues}
}

}
\author{
\strong{Maintainer}: Patrick Van Laake \email{patrick@vanlaake.net} [copyright holder]

}
\keyword{internal}
