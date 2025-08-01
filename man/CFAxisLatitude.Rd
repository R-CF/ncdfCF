% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/CFAxisLatitude.R
\docType{class}
\name{CFAxisLatitude}
\alias{CFAxisLatitude}
\title{Latitude CF axis object}
\description{
This class represents a latitude axis. Its values are numeric.
This class adds some logic that is specific to latitudes, such as their
range, orientation and meaning.
}
\section{Super classes}{
\code{\link[ncdfCF:CFObject]{ncdfCF::CFObject}} -> \code{\link[ncdfCF:CFAxis]{ncdfCF::CFAxis}} -> \code{\link[ncdfCF:CFAxisNumeric]{ncdfCF::CFAxisNumeric}} -> \code{CFAxisLatitude}
}
\section{Active bindings}{
\if{html}{\out{<div class="r6-active-bindings">}}
\describe{
\item{\code{friendlyClassName}}{(read-only) A nice description of the class.}
}
\if{html}{\out{</div>}}
}
\section{Methods}{
\subsection{Public methods}{
\itemize{
\item \href{#method-CFAxisLatitude-new}{\code{CFAxisLatitude$new()}}
\item \href{#method-CFAxisLatitude-clone}{\code{CFAxisLatitude$clone()}}
}
}
\if{html}{\out{
<details><summary>Inherited methods</summary>
<ul>
<li><span class="pkg-link" data-pkg="ncdfCF" data-topic="CFObject" data-id="add_coordinates"><a href='../../ncdfCF/html/CFObject.html#method-CFObject-add_coordinates'><code>ncdfCF::CFObject$add_coordinates()</code></a></span></li>
<li><span class="pkg-link" data-pkg="ncdfCF" data-topic="CFObject" data-id="append_attribute"><a href='../../ncdfCF/html/CFObject.html#method-CFObject-append_attribute'><code>ncdfCF::CFObject$append_attribute()</code></a></span></li>
<li><span class="pkg-link" data-pkg="ncdfCF" data-topic="CFObject" data-id="attribute"><a href='../../ncdfCF/html/CFObject.html#method-CFObject-attribute'><code>ncdfCF::CFObject$attribute()</code></a></span></li>
<li><span class="pkg-link" data-pkg="ncdfCF" data-topic="CFObject" data-id="delete_attribute"><a href='../../ncdfCF/html/CFObject.html#method-CFObject-delete_attribute'><code>ncdfCF::CFObject$delete_attribute()</code></a></span></li>
<li><span class="pkg-link" data-pkg="ncdfCF" data-topic="CFObject" data-id="print_attributes"><a href='../../ncdfCF/html/CFObject.html#method-CFObject-print_attributes'><code>ncdfCF::CFObject$print_attributes()</code></a></span></li>
<li><span class="pkg-link" data-pkg="ncdfCF" data-topic="CFObject" data-id="set_attribute"><a href='../../ncdfCF/html/CFObject.html#method-CFObject-set_attribute'><code>ncdfCF::CFObject$set_attribute()</code></a></span></li>
<li><span class="pkg-link" data-pkg="ncdfCF" data-topic="CFObject" data-id="write_attributes"><a href='../../ncdfCF/html/CFObject.html#method-CFObject-write_attributes'><code>ncdfCF::CFObject$write_attributes()</code></a></span></li>
<li><span class="pkg-link" data-pkg="ncdfCF" data-topic="CFAxis" data-id="can_append"><a href='../../ncdfCF/html/CFAxis.html#method-CFAxis-can_append'><code>ncdfCF::CFAxis$can_append()</code></a></span></li>
<li><span class="pkg-link" data-pkg="ncdfCF" data-topic="CFAxis" data-id="configure_terms"><a href='../../ncdfCF/html/CFAxis.html#method-CFAxis-configure_terms'><code>ncdfCF::CFAxis$configure_terms()</code></a></span></li>
<li><span class="pkg-link" data-pkg="ncdfCF" data-topic="CFAxis" data-id="peek"><a href='../../ncdfCF/html/CFAxis.html#method-CFAxis-peek'><code>ncdfCF::CFAxis$peek()</code></a></span></li>
<li><span class="pkg-link" data-pkg="ncdfCF" data-topic="CFAxis" data-id="shard"><a href='../../ncdfCF/html/CFAxis.html#method-CFAxis-shard'><code>ncdfCF::CFAxis$shard()</code></a></span></li>
<li><span class="pkg-link" data-pkg="ncdfCF" data-topic="CFAxis" data-id="time"><a href='../../ncdfCF/html/CFAxis.html#method-CFAxis-time'><code>ncdfCF::CFAxis$time()</code></a></span></li>
<li><span class="pkg-link" data-pkg="ncdfCF" data-topic="CFAxis" data-id="write"><a href='../../ncdfCF/html/CFAxis.html#method-CFAxis-write'><code>ncdfCF::CFAxis$write()</code></a></span></li>
<li><span class="pkg-link" data-pkg="ncdfCF" data-topic="CFAxisNumeric" data-id="append"><a href='../../ncdfCF/html/CFAxisNumeric.html#method-CFAxisNumeric-append'><code>ncdfCF::CFAxisNumeric$append()</code></a></span></li>
<li><span class="pkg-link" data-pkg="ncdfCF" data-topic="CFAxisNumeric" data-id="brief"><a href='../../ncdfCF/html/CFAxisNumeric.html#method-CFAxisNumeric-brief'><code>ncdfCF::CFAxisNumeric$brief()</code></a></span></li>
<li><span class="pkg-link" data-pkg="ncdfCF" data-topic="CFAxisNumeric" data-id="identical"><a href='../../ncdfCF/html/CFAxisNumeric.html#method-CFAxisNumeric-identical'><code>ncdfCF::CFAxisNumeric$identical()</code></a></span></li>
<li><span class="pkg-link" data-pkg="ncdfCF" data-topic="CFAxisNumeric" data-id="indexOf"><a href='../../ncdfCF/html/CFAxisNumeric.html#method-CFAxisNumeric-indexOf'><code>ncdfCF::CFAxisNumeric$indexOf()</code></a></span></li>
<li><span class="pkg-link" data-pkg="ncdfCF" data-topic="CFAxisNumeric" data-id="print"><a href='../../ncdfCF/html/CFAxisNumeric.html#method-CFAxisNumeric-print'><code>ncdfCF::CFAxisNumeric$print()</code></a></span></li>
<li><span class="pkg-link" data-pkg="ncdfCF" data-topic="CFAxisNumeric" data-id="range"><a href='../../ncdfCF/html/CFAxisNumeric.html#method-CFAxisNumeric-range'><code>ncdfCF::CFAxisNumeric$range()</code></a></span></li>
<li><span class="pkg-link" data-pkg="ncdfCF" data-topic="CFAxisNumeric" data-id="subset"><a href='../../ncdfCF/html/CFAxisNumeric.html#method-CFAxisNumeric-subset'><code>ncdfCF::CFAxisNumeric$subset()</code></a></span></li>
</ul>
</details>
}}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-CFAxisLatitude-new"></a>}}
\if{latex}{\out{\hypertarget{method-CFAxisLatitude-new}{}}}
\subsection{Method \code{new()}}{
Create a new instance of this class.

Creating a new latitude axis is more easily done with the
\code{\link[=makeLatitudeAxis]{makeLatitudeAxis()}} function.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{CFAxisLatitude$new(nc_var, nc_dim, values)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{nc_var}}{The netCDF variable that describes this instance.}

\item{\code{nc_dim}}{The netCDF dimension that describes the dimensionality.}

\item{\code{values}}{The coordinates of this axis.}
}
\if{html}{\out{</div>}}
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-CFAxisLatitude-clone"></a>}}
\if{latex}{\out{\hypertarget{method-CFAxisLatitude-clone}{}}}
\subsection{Method \code{clone()}}{
The objects of this class are cloneable with this method.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{CFAxisLatitude$clone(deep = FALSE)}\if{html}{\out{</div>}}
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
