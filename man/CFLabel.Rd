% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/CFLabel.R
\docType{class}
\name{CFLabel}
\alias{CFLabel}
\title{CF label object}
\description{
This class represent CF labels, i.e. an NC variable of character
type that provides a textual label for a discrete or general numeric axis.
See also \link{CFAxisCharacter}, which is an axis with character labels.
}
\section{Super class}{
\code{\link[ncdfCF:CFObject]{ncdfCF::CFObject}} -> \code{CFLabel}
}
\section{Public fields}{
\if{html}{\out{<div class="r6-fields">}}
\describe{
\item{\code{NCdim}}{The \link{NCDimension} that stores the netCDF dimension details.}
}
\if{html}{\out{</div>}}
}
\section{Active bindings}{
\if{html}{\out{<div class="r6-active-bindings">}}
\describe{
\item{\code{friendlyClassName}}{(read-only) A nice description of the class.}

\item{\code{coordinates}}{(read-only) The label set as a vector.}

\item{\code{length}}{(read-only) The number of labels in the set.}

\item{\code{dimid}}{(read-only) The netCDF dimension id of this label set.}
}
\if{html}{\out{</div>}}
}
\section{Methods}{
\subsection{Public methods}{
\itemize{
\item \href{#method-CFLabel-new}{\code{CFLabel$new()}}
\item \href{#method-CFLabel-print}{\code{CFLabel$print()}}
\item \href{#method-CFLabel-subset}{\code{CFLabel$subset()}}
\item \href{#method-CFLabel-write}{\code{CFLabel$write()}}
\item \href{#method-CFLabel-clone}{\code{CFLabel$clone()}}
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
</ul>
</details>
}}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-CFLabel-new"></a>}}
\if{latex}{\out{\hypertarget{method-CFLabel-new}{}}}
\subsection{Method \code{new()}}{
Create a new instance of this class.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{CFLabel$new(nc_var, nc_dim, values)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{nc_var}}{The netCDF variable that describes this instance.}

\item{\code{nc_dim}}{The netCDF dimension that describes the dimensionality.}

\item{\code{values}}{Character vector of the label values.}
}
\if{html}{\out{</div>}}
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-CFLabel-print"></a>}}
\if{latex}{\out{\hypertarget{method-CFLabel-print}{}}}
\subsection{Method \code{print()}}{
Prints a summary of the labels to the console.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{CFLabel$print(...)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{...}}{Arguments passed on to other functions. Of particular interest
is \verb{width = } to indicate a maximum width of attribute columns.}
}
\if{html}{\out{</div>}}
}
\subsection{Returns}{
\code{self}, invisibly.
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-CFLabel-subset"></a>}}
\if{latex}{\out{\hypertarget{method-CFLabel-subset}{}}}
\subsection{Method \code{subset()}}{
Retrieve a subset of the labels.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{CFLabel$subset(grp, rng)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{grp}}{The group to create the new label object in.}

\item{\code{rng}}{The range of indices to retrieve.}
}
\if{html}{\out{</div>}}
}
\subsection{Returns}{
A \code{CFLabel} instance, or \code{NULL} if the \code{rng} values are invalid.
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-CFLabel-write"></a>}}
\if{latex}{\out{\hypertarget{method-CFLabel-write}{}}}
\subsection{Method \code{write()}}{
Write the labels to a netCDF file, including its attributes.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{CFLabel$write(nc)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{nc}}{The handle of the netCDF file opened for writing or a group in
the netCDF file. If \code{NULL}, write to the file or group where the labels
were read from (the file must have been opened for writing). If not
\code{NULL}, the handle to a netCDF file or a group therein.}
}
\if{html}{\out{</div>}}
}
\subsection{Returns}{
Self, invisibly.
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-CFLabel-clone"></a>}}
\if{latex}{\out{\hypertarget{method-CFLabel-clone}{}}}
\subsection{Method \code{clone()}}{
The objects of this class are cloneable with this method.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{CFLabel$clone(deep = FALSE)}\if{html}{\out{</div>}}
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
