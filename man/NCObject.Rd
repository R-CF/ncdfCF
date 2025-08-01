% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/NCObject.R
\docType{class}
\name{NCObject}
\alias{NCObject}
\title{NetCDF base object}
\description{
This class is a basic ancestor to all classes that represent
netCDF objects, specifically groups, dimensions, variables and the
user-defined types in a netCDF file. More useful classes use this class as
ancestor.

The fields in this class are common among all netCDF objects. In addition,
this class manages the attributes for its descendent classes.
}
\section{Active bindings}{
\if{html}{\out{<div class="r6-active-bindings">}}
\describe{
\item{\code{id}}{Set or retrieve the identifier of the netCDF object. In
general, the \code{id} value is immutable so it should never be set to a new
value. Setting the \code{id} value is only useful when writing a CF object
to a new netCDF file and this is managed by the respective NC objects
once a new \code{id} value is reported by the \code{netcdf} library.}

\item{\code{name}}{Set or retrieve the name of the object. Note that the name
must comply with CF requirements: start with a letter, followed by
lettters, numbers or underscores, and having a maximum length of 255
characters. Multi-byte characters are not allowed in names.}

\item{\code{attributes}}{Read or set the attributes of the object. The
attributes are stored in a \code{data.frame} with columns "id" (integer),
"name" (character), "type" (one of the netCDF data types), "length"
(integer), and "value" (any allowed type). When setting the attributes,
all existing attributes are deleted; use method \code{set_attribute()} to
add attributes to the existing set. Upon reading, when there are no
attributes, an empty \code{data.frame} will be returned.}
}
\if{html}{\out{</div>}}
}
\section{Methods}{
\subsection{Public methods}{
\itemize{
\item \href{#method-NCObject-new}{\code{NCObject$new()}}
\item \href{#method-NCObject-print_attributes}{\code{NCObject$print_attributes()}}
\item \href{#method-NCObject-attribute}{\code{NCObject$attribute()}}
\item \href{#method-NCObject-set_attribute}{\code{NCObject$set_attribute()}}
\item \href{#method-NCObject-append_attribute}{\code{NCObject$append_attribute()}}
\item \href{#method-NCObject-delete_attribute}{\code{NCObject$delete_attribute()}}
\item \href{#method-NCObject-write_attributes}{\code{NCObject$write_attributes()}}
\item \href{#method-NCObject-add_coordinates}{\code{NCObject$add_coordinates()}}
\item \href{#method-NCObject-clone}{\code{NCObject$clone()}}
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-NCObject-new"></a>}}
\if{latex}{\out{\hypertarget{method-NCObject-new}{}}}
\subsection{Method \code{new()}}{
Create a new netCDF object. This class should not be
instantiated directly, create descendant objects instead.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{NCObject$new(id, name)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{id}}{Numeric identifier of the netCDF object.}

\item{\code{name}}{Character string with the name of the netCDF object.}
}
\if{html}{\out{</div>}}
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-NCObject-print_attributes"></a>}}
\if{latex}{\out{\hypertarget{method-NCObject-print_attributes}{}}}
\subsection{Method \code{print_attributes()}}{
This function prints the attributes of the netCDF object to
the console. Through object linkages, this also applies to the CF data
variables and axes, which each link to a netCDF object.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{NCObject$print_attributes(width = 50L)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{width}}{The maximum width of each column in the \code{data.frame} when
printed to the console.}
}
\if{html}{\out{</div>}}
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-NCObject-attribute"></a>}}
\if{latex}{\out{\hypertarget{method-NCObject-attribute}{}}}
\subsection{Method \code{attribute()}}{
This method returns an attribute of a netCDF object.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{NCObject$attribute(att, field = "value")}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{att}}{Attribute name whose value to return.}

\item{\code{field}}{The field of the attribute to return values from. This must
be "value" (default) or "type".}
}
\if{html}{\out{</div>}}
}
\subsection{Returns}{
If the \code{field} argument is "type", a character string. If \code{field}
is "value", a single value of the type of the attribute, or a vector
when the attribute has multiple values. If no attribute is named with a
value of argument \code{att} \code{NA} is returned.
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-NCObject-set_attribute"></a>}}
\if{latex}{\out{\hypertarget{method-NCObject-set_attribute}{}}}
\subsection{Method \code{set_attribute()}}{
Add an attribute. If an attribute \code{name} already exists, it
will be overwritten.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{NCObject$set_attribute(name, type, value)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{name}}{The name of the attribute. The name must begin with a letter
and be composed of letters, digits, and underscores, with a maximum
length of 255 characters. UTF-8 characters are not supported in
attribute names.}

\item{\code{type}}{The type of the attribute, as a string value of a netCDF data
type or a user-defined type.}

\item{\code{value}}{The value of the attribute. This can be of any supported
type, including a vector or list of values. Matrices, arrays and like
compound data structures should be stored as a data variable, not as an
attribute and they are thus not allowed. In general, an attribute
should be a character value, a numeric value, a logical value, or a
short vector or list of any of these. Values passed in a list will be
coerced to their common mode.}
}
\if{html}{\out{</div>}}
}
\subsection{Returns}{
Self, invisibly.
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-NCObject-append_attribute"></a>}}
\if{latex}{\out{\hypertarget{method-NCObject-append_attribute}{}}}
\subsection{Method \code{append_attribute()}}{
Append the text value of an attribute. If an attribute
\code{name} already exists, the \code{value} will be appended to the existing
value of the attribute. If the attribute \code{name} does not exist it will
be created. The attribute must be of "NC_CHAR" or "NC_STRING" type; in
the latter case having only a single string value.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{NCObject$append_attribute(name, value, sep = "; ", prepend = FALSE)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{name}}{The name of the attribute. The name must begin with a letter
and be composed of letters, digits, and underscores, with a maximum
length of 255 characters. UTF-8 characters are not supported in
attribute names.}

\item{\code{value}}{The character value of the attribute to append. This must be
a character string.}

\item{\code{sep}}{The separator to use. Default is \code{"; "}.}

\item{\code{prepend}}{Logical to flag if the supplied \code{value} should be placed
before the existing value. Default is \code{FALSE}.}
}
\if{html}{\out{</div>}}
}
\subsection{Returns}{
Self, invisibly.
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-NCObject-delete_attribute"></a>}}
\if{latex}{\out{\hypertarget{method-NCObject-delete_attribute}{}}}
\subsection{Method \code{delete_attribute()}}{
Delete attributes. If an attribute \code{name} is not present
this method simply returns.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{NCObject$delete_attribute(name)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{name}}{Vector of names of the attributes to delete.}
}
\if{html}{\out{</div>}}
}
\subsection{Returns}{
Self, invisibly.
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-NCObject-write_attributes"></a>}}
\if{latex}{\out{\hypertarget{method-NCObject-write_attributes}{}}}
\subsection{Method \code{write_attributes()}}{
Write the attributes of this object to a netCDF file.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{NCObject$write_attributes(nc, nm)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{nc}}{The handle to the netCDF file opened for writing.}

\item{\code{nm}}{The NC variable name or "NC_GLOBAL" to write the attributes to.}
}
\if{html}{\out{</div>}}
}
\subsection{Returns}{
Self, invisibly.
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-NCObject-add_coordinates"></a>}}
\if{latex}{\out{\hypertarget{method-NCObject-add_coordinates}{}}}
\subsection{Method \code{add_coordinates()}}{
Add names of axes to the "coordinates" attribute, avoiding
duplicates and retaining previous values.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{NCObject$add_coordinates(crds)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{crds}}{Vector of axis names to add to the attribute.}
}
\if{html}{\out{</div>}}
}
\subsection{Returns}{
Self, invisibly.
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-NCObject-clone"></a>}}
\if{latex}{\out{\hypertarget{method-NCObject-clone}{}}}
\subsection{Method \code{clone()}}{
The objects of this class are cloneable with this method.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{NCObject$clone(deep = FALSE)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{deep}}{Whether to make a deep clone.}
}
\if{html}{\out{</div>}}
}
}
}
