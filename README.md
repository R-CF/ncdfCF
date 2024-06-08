
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
It is built on the `RNetCDF` package which, like package `ncdf4`,
provides a basic interface to the `netcdf` library, but which lacks an
intuitive user interface. Package `ncdfCF` provides a high-level
interface using functions and methods that are familiar to the R user.
It reads the structural metadata and also the attributes upon opening
the resource. In the process, the `ncdfCF` package also applies CF
Metadata Conventions to interpret the data. This currently applies to:

- The **axis designation**. The three mechanisms to identify the axis
  each dimension represents are applied until an axis is determined.
- The **time dimension**. Time is usually encoded as an offset from a
  datum. Using the `CFtime` package these offsets can be turned into
  intelligible dates and times, for all 9 defined calendars.
- **Bounds** information. When present, bounds are read and used in
  analyses.
- **Discrete dimensions** with character labels.

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
```

``` r

# Get any NetCDF file
fn <- system.file("extdata", "ERA5land_Rwanda_20160101.nc", package = "ncdfCF")

# Open the file, all metadata is read
ds <- open_ncdf(fn)

# Easy access in understandable format to all the details
ds
#> Dataset   : /private/var/folders/gs/s0mmlczn4l7bjbmwfrrhjlt80000gn/T/Rtmp4NMDYW/temp_libpath133b9482f742d/ncdfCF/extdata/ERA5land_Rwanda_20160101.nc 
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
#>  id name        type    length value                                   
#>  0  CDI         NC_CHAR  64    Climate Data Interface version 2.4.1 ...
#>  1  Conventions NC_CHAR   6    CF-1.6                                  
#>  2  history     NC_CHAR 482    Tue May 28 18:39:12 2024: cdo seldate...
#>  3  CDO         NC_CHAR  64    Climate Data Operators version 2.4.1 ...
```

``` r

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
```

``` r

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
```

``` r

# Regular base R operations simplify life further
dimnames(ds[["pev"]]) # A variable: list of dimension names
#>   longitude    latitude        time 
#> "longitude"  "latitude"      "time"
```

``` r
dimnames(ds[["longitude"]]) # A dimension: vector of dimension element values
#>  [1] 28.0 28.1 28.2 28.3 28.4 28.5 28.6 28.7 28.8 28.9 29.0 29.1 29.2 29.3 29.4
#> [16] 29.5 29.6 29.7 29.8 29.9 30.0 30.1 30.2 30.3 30.4 30.5 30.6 30.7 30.8 30.9
#> [31] 31.0
```

``` r

# Access attributes
attribute(ds[["pev"]], "long_name")
#> [1] "Potential evaporation"
```

##### Extracting data

There are two ways to read data for a variable from the resource:

- **`[]`:** The usual R array operator. This uses index values into the
  dimensions and requires you to know the order in which the dimensions
  are specified for the variable. With a bit of tinkering and some
  helper functions in `ncdfCF` this is still very easy to do.
- **`subset()`:** The `subset()` method lets you specify what you want
  to extract from each dimension in real-world coordinates and
  timestamps, in whichever order.

``` r
# Extract a timeseries for a specific location
ts <- t2m[5, 4, ]
str(ts)
#>  num [1, 1, 1:24] 293 292 292 291 291 ...
#>  - attr(*, "dimnames")=List of 3
#>   ..$ : chr "28.4"
#>   ..$ : chr "-1.3"
#>   ..$ : chr [1:24] "2016-01-01 00:00:00" "2016-01-01 01:00:00" "2016-01-01 02:00:00" "2016-01-01 03:00:00" ...
#>  - attr(*, "axis")= Named chr [1:3] "X" "Y" "T"
#>   ..- attr(*, "names")= chr [1:3] "longitude" "latitude" "time"
#>  - attr(*, "time")=List of 1
#>   ..$ time:Formal class 'CFtime' [package "CFtime"] with 4 slots
#>   .. .. ..@ datum     :Formal class 'CFdatum' [package "CFtime"] with 5 slots
#>   .. .. .. .. ..@ definition: chr "hours since 1900-01-01 00:00:00.0"
#>   .. .. .. .. ..@ unit      : int 3
#>   .. .. .. .. ..@ origin    :'data.frame':   1 obs. of  8 variables:
#>   .. .. .. .. .. ..$ year  : int 1900
#>   .. .. .. .. .. ..$ month : num 1
#>   .. .. .. .. .. ..$ day   : num 1
#>   .. .. .. .. .. ..$ hour  : num 0
#>   .. .. .. .. .. ..$ minute: num 0
#>   .. .. .. .. .. ..$ second: num 0
#>   .. .. .. .. .. ..$ tz    : chr "+0000"
#>   .. .. .. .. .. ..$ offset: num 0
#>   .. .. .. .. ..@ calendar  : chr "gregorian"
#>   .. .. .. .. ..@ cal_id    : int 1
#>   .. .. ..@ resolution: num 1
#>   .. .. ..@ offsets   : num [1:24] 1016832 1016833 1016834 1016835 1016836 ...
#>   .. .. ..@ bounds    : logi FALSE
```

``` r

# Extract the full spatial extent for one time step
ts <- t2m[, , 12]
str(ts)
#>  num [1:31, 1:21, 1] 300 300 300 300 300 ...
#>  - attr(*, "dimnames")=List of 3
#>   ..$ : chr [1:31] "28" "28.1" "28.2" "28.3" ...
#>   ..$ : chr [1:21] "-1" "-1.1" "-1.2" "-1.3" ...
#>   ..$ : chr "2016-01-01 11:00:00"
#>  - attr(*, "axis")= Named chr [1:3] "X" "Y" "T"
#>   ..- attr(*, "names")= chr [1:3] "longitude" "latitude" "time"
#>  - attr(*, "time")=List of 1
#>   ..$ time:Formal class 'CFtime' [package "CFtime"] with 4 slots
#>   .. .. ..@ datum     :Formal class 'CFdatum' [package "CFtime"] with 5 slots
#>   .. .. .. .. ..@ definition: chr "hours since 1900-01-01 00:00:00.0"
#>   .. .. .. .. ..@ unit      : int 3
#>   .. .. .. .. ..@ origin    :'data.frame':   1 obs. of  8 variables:
#>   .. .. .. .. .. ..$ year  : int 1900
#>   .. .. .. .. .. ..$ month : num 1
#>   .. .. .. .. .. ..$ day   : num 1
#>   .. .. .. .. .. ..$ hour  : num 0
#>   .. .. .. .. .. ..$ minute: num 0
#>   .. .. .. .. .. ..$ second: num 0
#>   .. .. .. .. .. ..$ tz    : chr "+0000"
#>   .. .. .. .. .. ..$ offset: num 0
#>   .. .. .. .. ..@ calendar  : chr "gregorian"
#>   .. .. .. .. ..@ cal_id    : int 1
#>   .. .. ..@ resolution: num NA
#>   .. .. ..@ offsets   : num 1016843
#>   .. .. ..@ bounds    : logi FALSE
```

Note that the results contain degenerate dimensions (of length 1). This
by design because it allows attributes to be attached in a consistent
manner.

``` r
# Extract a specific region, full time dimension
ts <- subset(t2m, list(X = 29:30, Y = -1:-2))
str(ts)
#>  num [1:10, 1:10, 1:24] 290 291 291 292 293 ...
#>  - attr(*, "dimnames")=List of 3
#>   ..$ : chr [1:10] "29" "29.1" "29.2" "29.3" ...
#>   ..$ : chr [1:10] "-1" "-1.1" "-1.2" "-1.3" ...
#>   ..$ : chr [1:24] "2016-01-01 00:00:00" "2016-01-01 01:00:00" "2016-01-01 02:00:00" "2016-01-01 03:00:00" ...
#>  - attr(*, "axis")= Named chr [1:3] "X" "Y" "T"
#>   ..- attr(*, "names")= chr [1:3] "longitude" "latitude" "time"
#>  - attr(*, "time")=List of 1
#>   ..$ time:Formal class 'CFtime' [package "CFtime"] with 4 slots
#>   .. .. ..@ datum     :Formal class 'CFdatum' [package "CFtime"] with 5 slots
#>   .. .. .. .. ..@ definition: chr "hours since 1900-01-01 00:00:00.0"
#>   .. .. .. .. ..@ unit      : int 3
#>   .. .. .. .. ..@ origin    :'data.frame':   1 obs. of  8 variables:
#>   .. .. .. .. .. ..$ year  : int 1900
#>   .. .. .. .. .. ..$ month : num 1
#>   .. .. .. .. .. ..$ day   : num 1
#>   .. .. .. .. .. ..$ hour  : num 0
#>   .. .. .. .. .. ..$ minute: num 0
#>   .. .. .. .. .. ..$ second: num 0
#>   .. .. .. .. .. ..$ tz    : chr "+0000"
#>   .. .. .. .. .. ..$ offset: num 0
#>   .. .. .. .. ..@ calendar  : chr "gregorian"
#>   .. .. .. .. ..@ cal_id    : int 1
#>   .. .. ..@ resolution: num 1
#>   .. .. ..@ offsets   : num [1:24] 1016832 1016833 1016834 1016835 1016836 ...
#>   .. .. ..@ bounds    : logi FALSE
```

``` r

# Extract specific time slices for a specific region
# Note that the dimensions are specified out of order and using alternative
# specifications: only the extreme values are used.
ts <- subset(t2m, list(T = c("2016-01-01 09:00", "2016-01-01 15:00"),
                       X = c(29.6, 28.8),
                       Y = seq(-2, -1, by = 0.05)))
str(ts)
#>  num [1:8, 1:10, 1:6] 297 296 296 298 299 ...
#>  - attr(*, "dimnames")=List of 3
#>   ..$ : chr [1:8] "28.8" "28.9" "29" "29.1" ...
#>   ..$ : chr [1:10] "-1" "-1.1" "-1.2" "-1.3" ...
#>   ..$ : chr [1:6] "2016-01-01 09:00:00" "2016-01-01 10:00:00" "2016-01-01 11:00:00" "2016-01-01 12:00:00" ...
#>  - attr(*, "axis")= Named chr [1:3] "X" "Y" "T"
#>   ..- attr(*, "names")= chr [1:3] "longitude" "latitude" "time"
#>  - attr(*, "time")=List of 1
#>   ..$ time:Formal class 'CFtime' [package "CFtime"] with 4 slots
#>   .. .. ..@ datum     :Formal class 'CFdatum' [package "CFtime"] with 5 slots
#>   .. .. .. .. ..@ definition: chr "hours since 1900-01-01 00:00:00.0"
#>   .. .. .. .. ..@ unit      : int 3
#>   .. .. .. .. ..@ origin    :'data.frame':   1 obs. of  8 variables:
#>   .. .. .. .. .. ..$ year  : int 1900
#>   .. .. .. .. .. ..$ month : num 1
#>   .. .. .. .. .. ..$ day   : num 1
#>   .. .. .. .. .. ..$ hour  : num 0
#>   .. .. .. .. .. ..$ minute: num 0
#>   .. .. .. .. .. ..$ second: num 0
#>   .. .. .. .. .. ..$ tz    : chr "+0000"
#>   .. .. .. .. .. ..$ offset: num 0
#>   .. .. .. .. ..@ calendar  : chr "gregorian"
#>   .. .. .. .. ..@ cal_id    : int 1
#>   .. .. ..@ resolution: num 6
#>   .. .. ..@ offsets   : num [1:2] 1016841 1016847
#>   .. .. ..@ bounds    : logi FALSE
```

Both of these methods will read data from the NetCDF resource, but only
as much as is requested.

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

**<span style="color: red;">CAUTION:</span>** Package `ncdfCF` is still
in the early phases of development. While extensively tested on multiple
well-structured datasets, errors may still occur, particularly in
datasets that do not adhere to the CF Metadata Conventions.

Package `ncdfCF` has not yet been submitted to CRAN.

You can install the development version of `ncdfCF` from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("pvanlaake/ncdfCF")
```
