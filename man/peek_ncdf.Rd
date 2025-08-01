% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/readCF.R
\name{peek_ncdf}
\alias{peek_ncdf}
\title{Examine a netCDF resource}
\usage{
peek_ncdf(resource)
}
\arguments{
\item{resource}{The name of the netCDF resource to open, either a local file
name or a remote URI.}
}
\value{
A list with elements "variables", "axes" and global "attributes",
each a \code{data.frame}.
}
\description{
This function will read a netCDF resource and return a list of identifying
information, including data variables, axes and global attributes. Upon
returning the netCDF resource is closed.
}
\details{
If you find that you need other information to be included in the result,
\href{https://github.com/R-CF/ncdfCF/issues}{open an issue}.
}
\examples{
fn <- system.file("extdata",
  "pr_day_EC-Earth3-CC_ssp245_r1i1p1f1_gr_20230101-20231231_vncdfCF.nc",
  package = "ncdfCF")
peek_ncdf(fn)
}
