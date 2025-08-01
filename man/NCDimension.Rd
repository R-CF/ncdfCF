% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/NCDimension.R
\docType{class}
\name{NCDimension}
\alias{NCDimension}
\title{NetCDF dimension object}
\description{
This class represents an netCDF dimensions. It contains the
information on a dimension that is stored in an netCDF file.

This class is not very useful for interactive use. Use the \link{CFAxis}
descendent classes instead.
}
\section{Super class}{
\code{\link[ncdfCF:NCObject]{ncdfCF::NCObject}} -> \code{NCDimension}
}
\section{Public fields}{
\if{html}{\out{<div class="r6-fields">}}
\describe{
\item{\code{length}}{The length of the dimension. If field \code{unlim = TRUE}, this
field indicates the length of the data in this dimension written to file.}

\item{\code{unlim}}{Logical flag to indicate if the dimension is unlimited, i.e.
that additional data may be written to file incrementing in this
dimension.}
}
\if{html}{\out{</div>}}
}
\section{Methods}{
\subsection{Public methods}{
\itemize{
\item \href{#method-NCDimension-new}{\code{NCDimension$new()}}
\item \href{#method-NCDimension-print}{\code{NCDimension$print()}}
\item \href{#method-NCDimension-write}{\code{NCDimension$write()}}
\item \href{#method-NCDimension-clone}{\code{NCDimension$clone()}}
}
}
\if{html}{\out{
<details><summary>Inherited methods</summary>
<ul>
<li><span class="pkg-link" data-pkg="ncdfCF" data-topic="NCObject" data-id="add_coordinates"><a href='../../ncdfCF/html/NCObject.html#method-NCObject-add_coordinates'><code>ncdfCF::NCObject$add_coordinates()</code></a></span></li>
<li><span class="pkg-link" data-pkg="ncdfCF" data-topic="NCObject" data-id="append_attribute"><a href='../../ncdfCF/html/NCObject.html#method-NCObject-append_attribute'><code>ncdfCF::NCObject$append_attribute()</code></a></span></li>
<li><span class="pkg-link" data-pkg="ncdfCF" data-topic="NCObject" data-id="attribute"><a href='../../ncdfCF/html/NCObject.html#method-NCObject-attribute'><code>ncdfCF::NCObject$attribute()</code></a></span></li>
<li><span class="pkg-link" data-pkg="ncdfCF" data-topic="NCObject" data-id="delete_attribute"><a href='../../ncdfCF/html/NCObject.html#method-NCObject-delete_attribute'><code>ncdfCF::NCObject$delete_attribute()</code></a></span></li>
<li><span class="pkg-link" data-pkg="ncdfCF" data-topic="NCObject" data-id="print_attributes"><a href='../../ncdfCF/html/NCObject.html#method-NCObject-print_attributes'><code>ncdfCF::NCObject$print_attributes()</code></a></span></li>
<li><span class="pkg-link" data-pkg="ncdfCF" data-topic="NCObject" data-id="set_attribute"><a href='../../ncdfCF/html/NCObject.html#method-NCObject-set_attribute'><code>ncdfCF::NCObject$set_attribute()</code></a></span></li>
<li><span class="pkg-link" data-pkg="ncdfCF" data-topic="NCObject" data-id="write_attributes"><a href='../../ncdfCF/html/NCObject.html#method-NCObject-write_attributes'><code>ncdfCF::NCObject$write_attributes()</code></a></span></li>
</ul>
</details>
}}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-NCDimension-new"></a>}}
\if{latex}{\out{\hypertarget{method-NCDimension-new}{}}}
\subsection{Method \code{new()}}{
Create a new netCDF dimension. This class should not be
instantiated directly, create CF objects instead. This class is
instantiated when opening a netCDF resource.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{NCDimension$new(id, name, length, unlim)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{id}}{Numeric identifier of the netCDF dimension.}

\item{\code{name}}{Character string with the name of the netCDF dimension.}

\item{\code{length}}{Length of the dimension.}

\item{\code{unlim}}{Is the dimension unlimited?}
}
\if{html}{\out{</div>}}
}
\subsection{Returns}{
A \code{NCDimension} instance.
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-NCDimension-print"></a>}}
\if{latex}{\out{\hypertarget{method-NCDimension-print}{}}}
\subsection{Method \code{print()}}{
Summary of the NC dimension printed to the console.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{NCDimension$print(...)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{...}}{Passed on to other methods.}
}
\if{html}{\out{</div>}}
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-NCDimension-write"></a>}}
\if{latex}{\out{\hypertarget{method-NCDimension-write}{}}}
\subsection{Method \code{write()}}{
Write the dimension to a netCDF file.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{NCDimension$write(h)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{h}}{The handle to the netCDF file to write.}
}
\if{html}{\out{</div>}}
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-NCDimension-clone"></a>}}
\if{latex}{\out{\hypertarget{method-NCDimension-clone}{}}}
\subsection{Method \code{clone()}}{
The objects of this class are cloneable with this method.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{NCDimension$clone(deep = FALSE)}\if{html}{\out{</div>}}
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
