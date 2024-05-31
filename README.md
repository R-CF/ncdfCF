
<!-- README.md is generated from README.Rmd. Please edit that file -->

# ncdfCF

<!-- badges: start -->

[![Lifecycle:
Experimental](https://img.shields.io/badge/Lifecycle-Experimental-red.svg)](https://lifecycle.r-lib.org/articles/stages.html)
[![License: GPL
v3](https://img.shields.io/badge/License-MIT-blue.svg)](https://mit-license.org)
[![Last
commit](https://img.shields.io/github/last-commit/pvanlaake/ncdfCF)](https://github.com/pvanlaake/ncdfCF/commits/main)
<!-- badges: end -->

The `ncdfCF` package provides an easy to use interface to NetCDF
resources in R, either in local files or remotely on a THREDDS server.

It is built on the `RNetCDF` package which provides a basic interface to
the NetCDF library, but which lacks an intuitive user experience -
effectively it is a transliteration of the `netcdf` library API into R
and some tweaks to adjust to the R environment (e.g. data types and
internal representation). Package `ncdf4` does one better by performing
the tedious task of reading the structural metadata from the resource
that is needed for a basic understanding of the contents, such as
dimension and variable details, but the essential library API concept
remains with functions that directly map to the `netcdf` library
functions. One would really need to understand the NetCDF data model and
implementation details to effectively use these packages. (For instance,
most data describing a dimension is stored as a variable! So to read the
`dimnames()` of a dimension you’d have to call `var.get.nc()` or
`ncvar_get()`.) Neither package loads the attributes of the dimensions,
variables and the dataset (“global” variables), which is essential to
*understand* what the dimensions and variables represent.

Package `ncdfCF` provides a high-level interface using functions and
methods that are familiar to the R user. It reads the structural
metadata and also the attributes upon opening the resource. In the
process, the `ncdfCF` package also applies CF Metadata Conventions to
interpret the data. This currently applies to:

- The **axis designation**. The three mechanisms to identify the axis
  each dimension represents are applied until an axis is determined.
- The **time dimension**. Time is usually encoded as an offset from a
  datum. Using the `CFtime` package these offsets can be turned into
  intelligible dates and times, for all 9 defined calendars.
- **Bounds** information. When present, bounds are read and used in
  analyses.

##### Basic usage

Opening and inspecting the contents of a NetCDF resource is very
straightforward:

``` r
library(ncdfCF)
#> 
#> Attaching package: 'ncdfCF'
#> The following object is masked from 'package:graphics':
#> 
#>     axis

# Get any NetCDF file
fn <- system.file("extdata", "ERA5land_Rwanda_20160101.nc", package = "ncdfCF")

# Open the file, all metadata is read
ds <- ncdfDataset(fn)

# Easy access in understandable format to all the details
ds
#> Dataset   : /Library/Frameworks/R.framework/Versions/4.4-arm64/Resources/library/ncdfCF/extdata/ERA5land_Rwanda_20160101.nc 
#> 
#> Variables :
#>  id name long_name             units dimensions               
#>  3  t2m  2 metre temperature   K     longitude, latitude, time
#>  4  pev  Potential evaporation m     longitude, latitude, time
#>  5  tp   Total precipitation   m     longitude, latitude, time
#> 
#> Dimensions:
#>  id axis name      dims                                              unlim
#>  1  X    longitude [31: 28 ... 31]                                        
#>  2  Y    latitude  [21: -1 ... -3]                                        
#>  0  T    time      [24: 2016-01-01 00:00:00 ... 2016-01-01 23:00:00] U    
#> 
#> Attributes:
#>  id name        type    length
#>  0  CDI         NC_CHAR  64   
#>  1  Conventions NC_CHAR   6   
#>  2  history     NC_CHAR 482   
#>  3  CDO         NC_CHAR  64   
#>  value                                             
#>  Climate Data Interface version 2.4.1 (https://m...
#>  CF-1.6                                            
#>  Tue May 28 18:39:12 2024: cdo seldate,2016-01-0...
#>  Climate Data Operators version 2.4.1 (https://m...

# Variables can be accessed through standard list-type extraction syntax
t2m <- ds[["t2m"]]
t2m
#> Variable: [3] t2m | 2 metre temperature
#> 
#> Dimensions:
#>  id axis      name                                              dims unlim
#>   1    X longitude                                   [31: 28 ... 31]      
#>   2    Y  latitude                                   [21: -1 ... -3]      
#>   0    T      time [24: 2016-01-01 00:00:00 ... 2016-01-01 23:00:00]     U
#> 
#> Attributes:
#>  id name          type      length value              
#>  0  long_name     NC_CHAR   19     2 metre temperature
#>  1  units         NC_CHAR    1     K                  
#>  2  add_offset    NC_DOUBLE  1     292.664569285614   
#>  3  scale_factor  NC_DOUBLE  1     0.00045127252204996
#>  4  _FillValue    NC_SHORT   1     -32767             
#>  5  missing_value NC_SHORT   1     -32767

# Same with dimensions, but now without first putting the object in a variable
ds[["longitude"]]
#> Dimension: [1] longitude
#> Axis     : X 
#> Length   : 31  
#> Range    : 28 ... 31 degrees_east 
#> Bounds   : (not set) 
#> 
#> Attributes:
#>  id name          type    length value       
#>  0  standard_name NC_CHAR  9     longitude   
#>  1  long_name     NC_CHAR  9     longitude   
#>  2  units         NC_CHAR 12     degrees_east
#>  3  axis          NC_CHAR  1     X

# Regular base R operations simplify life further
dimnames(ds[["pev"]]) # A variable: list of dimension names
#>   longitude    latitude        time 
#> "longitude"  "latitude"      "time"
dimnames(ds[["longitude"]]) # A dimension: vector of dimension element values
#>  [1] 28.0 28.1 28.2 28.3 28.4 28.5 28.6 28.7 28.8 28.9 29.0 29.1 29.2 29.3 29.4
#> [16] 29.5 29.6 29.7 29.8 29.9 30.0 30.1 30.2 30.3 30.4 30.5 30.6 30.7 30.8 30.9
#> [31] 31.0

# Access attributes
attribute(ds[["pev"]], "long_name")
#> [1] "Potential evaporation"
```

##### Extracting data

One of the perpetual headaches of users of NetCDF files is to extract
the data. If you want to get all the data for a variable then neither
`RNetCDF` nor `ncdf4` are particularly troublesome:

``` r
library(ncdf4)
nc <- nc_open(fn)
vars <- names(nc$vars)
d <- ncvar_get(nc, vars[[1]])
```

But what if you are interested in only a small area or a month of data
while the resource has global data spanning multiple years? In both
`RNetCDF` and `ncdf4` packages you’d have to work out how your
real-world boundaries translate to indices into the variable array of
interest and then populate `start` and `count` arguments to pass on to
`var.get.nc()` or `ncvar_get()`. Many R users default to simply reading
the entire array and then extracting the area of interest using standard
R tools. That is wasteful at best (lots of I/O, RAM usage, CPU cycles)
and practically impossible with some larger NetCDF resources that have
variables upwards of 1GB in size.

Enter `ncdfCF`. With `ncdfCF` you have two options to extract data for a
variable:

- **`[]`**: Using R’s standard extraction operator `[` you work directly
  with the index values into the array dimensions:
  `d <- t2m[3:5, 1:4, 1:10]`. You can leave out dimensions to extract
  everything from that dimension (but you have to indicate the position,
  just like in regular arrays). So to get the first 5 “time” slices from
  `t2m`: `d <- t2m[, , 1:5]`. Not specifying anything gets you the whole
  array: `d <- t2m[]`. This works for any number of dimensions, you
  simply have to adjust the number of positions that you specify. You
  still need to know the indices into the arrays but `ncdfCF` has some
  helper functions to get you those.
- **`subset()`**: The `subset()` method is more flexible than `[]`
  because it requires less knowledge of how the data in the variable is
  structured, particularly the order of the dimensions. While many
  NetCDF resources “behave” in their dimension order, there is no
  guarantee. With `subset()` you supply a list with items for each
  dimension (by axis or dimension name, in any order) and each item
  containing a vector of real-world coordinates to extract. As an
  example, to extract values of a variable `x` for Australia for the
  year 2020 you call
  `subset(x, list(X = 112:154, Y = -9:-44, T = c("2020-01-01", "2021-01-01")))`.

Both approaches lend themselves well to the `apply()` family of
functions for processing. Importantly, these functions are to access
data from the NetCDF resource so you can tweak the size of your request
to the capacity of the computer, without exhausting RAM.

## Development plan

Package `ncdfCF` is in the early phases of development. It supports
reading of dimensions, variables, attributes and data from NetCDF
resources in “classic” and “NetCDF4” formats. From the CF Metadata
Conventions it supports identification of dimension axes, interpretation
of the “time” dimension, and reading of “bounds” information.

Development plans for the near future focus on supporting the below
features:

##### NetCDF

- Support for writing.
- Support for “group” information in “NetCDF4” formatted resources.

##### CF Metadata Conventions

- Full support for discrete or categorical dimensions.
- Interface to “standard_name” libraries and other “defined
  vocabularies”.
- Compliance with CMIP5 / CMIP6 requirements.

## Installation

**CAUTION:** Package `ncdfCF` is still in the early phases of
development. While extensively tested on multiple well-structured
datasets, errors may still occur, particularly in datasets that do not
adhere to the CF Metadata Conventions.

Package `ncdfCF` has not yet been submitted to CRAN.

You can install the development version of `ncdfCF` from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("pvanlaake/ncdfCF")
```
