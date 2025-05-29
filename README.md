
<!-- README.md is generated from README.Rmd. Please edit that file -->

# ncdfCF

<!-- badges: start -->

[![Lifecycle:
Experimental](https://img.shields.io/badge/Lifecycle-Experimental-green.svg)](https://lifecycle.r-lib.org/articles/stages.html)
[![CRAN
Status](https://www.r-pkg.org/badges/version/ncdfCF)](https://cran.r-project.org/package=ncdfCF)
[![CRAN
Downloads](https://cranlogs.r-pkg.org/badges/grand-total/ncdfCF)](https://cran.r-project.org/package=ncdfCF)
[![License: GPL
v3](https://img.shields.io/badge/License-MIT-blue.svg)](https://mit-license.org)
[![Last
commit](https://img.shields.io/github/last-commit/pvanlaake/ncdfCF)](https://github.com/pvanlaake/ncdfCF/commits/main)
[![R-CMD-check](https://github.com/pvanlaake/ncdfCF/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/pvanlaake/ncdfCF/actions/workflows/R-CMD-check.yaml)
<!-- badges: end -->

The `ncdfCF` package provides an easy to use interface to netCDF
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
- The **time coordinate**. Time is usually encoded as an offset from an
  origin. Using the `CFtime` package these offsets can be turned into
  intelligible dates and times, for all defined calendars.
- **Bounds** information. When present, bounds are read and used in
  analyses.
- **Discrete coordinates**, optionally with character labels. When
  labels are provided, these will be used as `dimnames` for the axis.
  (Note that this also applies to generic numeric axes with labels
  defined.)
- **Parametric vertical coordinates** are read, including variables
  listed in the `formula_terms` attribute.
- **Auxiliary coordinates** are identified and read. This applies also
  to **scalar axes** and **auxiliary longitude-latitude grids**.
  Auxiliary coordinates can be activated by the user and then used in
  display, selection and processing. Data on non-Cartesian grids can be
  automatically rectified to a longitude-latitude grid if an auxiliary
  grid is present in the resource.
- The **cell measure variables** are read and linked to any data
  variables referencing them. Cell measure variables that are external
  to the netCDF resource with the referring data variable can be linked
  to the data set and then they are immediately available to the
  referring data variables.
- **Labels**, as separate variables identified through the `coordinates`
  attribute of axes, are read, including when multiple sets of labels
  are defined for a single axis. Users can select which set of labels to
  make active for display, selection and processing.
- The **grid_mapping** variables, providing the coordinate reference
  system (CRS) of the data, with support for all defined objects in the
  latest EPSG database as well as “manual” construction of CRSs.

##### Basic usage

Opening and inspecting the contents of a netCDF resource is very
straightforward:

``` r
library(ncdfCF)

# Get any netCDF file
fn <- system.file("extdata", "ERA5land_Rwanda_20160101.nc", package = "ncdfCF")

# Open the file, all metadata is read
(ds <- open_ncdf(fn))
#> <Dataset> ERA5land_Rwanda_20160101 
#> Resource   : /Library/Frameworks/R.framework/Versions/4.5-arm64/Resources/library/ncdfCF/extdata/ERA5land_Rwanda_20160101.nc 
#> Format     : offset64 
#> Collection : Generic netCDF data 
#> Conventions: CF-1.6 
#> Keep open  : FALSE 
#> 
#> Variables:
#>  name long_name             units data_type axes                     
#>  t2m  2 metre temperature   K     NC_SHORT  longitude, latitude, time
#>  pev  Potential evaporation m     NC_SHORT  longitude, latitude, time
#>  tp   Total precipitation   m     NC_SHORT  longitude, latitude, time
#> 
#> Attributes:
#>  name        type    length value                                             
#>  CDI         NC_CHAR  64    Climate Data Interface version 2.4.1 (https://m...
#>  Conventions NC_CHAR   6    CF-1.6                                            
#>  history     NC_CHAR 482    Tue May 28 18:39:12 2024: cdo seldate,2016-01-0...
#>  CDO         NC_CHAR  64    Climate Data Operators version 2.4.1 (https://m...

# ...or very brief details
ds$var_names
#> [1] "t2m" "pev" "tp"
ds$axis_names
#> [1] "time"      "longitude" "latitude"

# Variables and axes can be accessed through standard list-type extraction syntax
(t2m <- ds[["t2m"]])
#> <Variable> t2m 
#> Long name: 2 metre temperature 
#> 
#> Axes:
#>  axis name      length unlim values                                       
#>  X    longitude 31           [28 ... 31]                                  
#>  Y    latitude  21           [-1 ... -3]                                  
#>  T    time      24     U     [2016-01-01T00:00:00 ... 2016-01-01T23:00:00]
#>  unit                             
#>  degrees_east                     
#>  degrees_north                    
#>  hours since 1900-01-01 00:00:00.0
#> 
#> Attributes:
#>  name      type    length value              
#>  long_name NC_CHAR 19     2 metre temperature
#>  units     NC_CHAR  1     K

ds[["longitude"]]
#> <Longitude axis> [1] longitude
#> Length     : 31
#> Axis       : X 
#> Coordinates: 28, 28.1, 28.2 ... 30.8, 30.9, 31 (degrees_east)
#> Bounds     : (not set)
#> 
#> Attributes:
#>  name          type     length value       
#>  standard_name NC_CHAR   9     longitude   
#>  long_name     NC_CHAR   9     longitude   
#>  units         NC_CHAR  12     degrees_east
#>  axis          NC_CHAR   1     X           
#>  actual_range  NC_FLOAT  2     28, 31

# Regular base R operations simplify life further
dimnames(ds[["pev"]]) # A variable: list of axis names
#> [1] "longitude" "latitude"  "time"
dimnames(ds[["longitude"]]) # An axis: vector of axis coordinates
#>  [1] 28.0 28.1 28.2 28.3 28.4 28.5 28.6 28.7 28.8 28.9 29.0 29.1 29.2 29.3 29.4
#> [16] 29.5 29.6 29.7 29.8 29.9 30.0 30.1 30.2 30.3 30.4 30.5 30.6 30.7 30.8 30.9
#> [31] 31.0

# Access attributes
ds[["pev"]]$attribute("long_name")
#> [1] "Potential evaporation"
```

If you just want to inspect what data is included in the netCDF
resource, use the `peek_ncdf()` function:

``` r
peek_ncdf(fn)
#> $uri
#> [1] "/Library/Frameworks/R.framework/Versions/4.5-arm64/Resources/library/ncdfCF/extdata/ERA5land_Rwanda_20160101.nc"
#> 
#> $type
#> [1] "Generic netCDF data"
#> 
#> $variables
#>     id name             long_name standard_name units                      axes
#> t2m  3  t2m   2 metre temperature            NA     K longitude, latitude, time
#> pev  4  pev Potential evaporation            NA     m longitude, latitude, time
#> tp   5   tp   Total precipitation            NA     m longitude, latitude, time
#> 
#> $axes
#>                     class id axis      name long_name standard_name
#> time           CFAxisTime  0    T      time      time          time
#> longitude CFAxisLongitude  1    X longitude longitude     longitude
#> latitude   CFAxisLatitude  2    Y  latitude  latitude      latitude
#>                                       units length unlimited
#> time      hours since 1900-01-01 00:00:00.0     24      TRUE
#> longitude                      degrees_east     31     FALSE
#> latitude                      degrees_north     21     FALSE
#>                                                  values has_bounds
#> time      [2016-01-01T00:00:00 ... 2016-01-01T23:00:00]      FALSE
#> longitude                                   [28 ... 31]      FALSE
#> latitude                                    [-1 ... -3]      FALSE
#>           coordinate_sets
#> time                    1
#> longitude               1
#> latitude                1
#> 
#> $attributes
#>   id        name    type length
#> 1  0         CDI NC_CHAR     64
#> 2  1 Conventions NC_CHAR      6
#> 3  2     history NC_CHAR    482
#> 4  3         CDO NC_CHAR     64
#>                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                 value
#> 1                                                                                                                                                                                                                                                                                                                                                                                                                                    Climate Data Interface version 2.4.1 (https://mpimet.mpg.de/cdi)
#> 2                                                                                                                                                                                                                                                                                                                                                                                                                                                                                              CF-1.6
#> 3 Tue May 28 18:39:12 2024: cdo seldate,2016-01-01,2016-01-01 /Users/patrickvanlaake/CC/ERA5land/Rwanda/ERA5land_Rwanda_t2m-pev-tp_2016-2018.nc ERA5land_Rwanda_20160101.nc\n2021-12-22 07:00:24 GMT by grib_to_netcdf-2.23.0: /opt/ecmwf/mars-client/bin/grib_to_netcdf -S param -o /cache/data5/adaptor.mars.internal-1640155821.967082-25565-12-0b19757d-da4e-4ea4-b8aa-d08ec89caf2c.nc /cache/tmp/0b19757d-da4e-4ea4-b8aa-d08ec89caf2c-adaptor.mars.internal-1640142203.3196251-25565-10-tmp.grib
#> 4                                                                                                                                                                                                                                                                                                                                                                                                                                    Climate Data Operators version 2.4.1 (https://mpimet.mpg.de/cdo)
```

##### Extracting data

There are four ways to read data for a data variable from the resource:

- **`data():`** The `data()` method returns all data of a variable,
  including its metadata, in a `CFArray` instance.
- **`[]`:** The usual R array operator gives you access to the raw,
  non-interpreted data in the netCDF resource. This uses index values
  into the dimensions and requires you to know the order in which the
  dimensions are specified for the variable. With a bit of tinkering and
  some helper functions in `ncdfCF` this is still very easy to do.
- **`subset()`:** The `subset()` method lets you specify what you want
  to extract from each dimension in real-world coordinates and
  timestamps, in whichever order. This can also rectify non-Cartesian
  grids to regular longitude-latitude grids.
- **`profile()`:** Extract “profiles” from the data variable. This can
  take different forms, such as a temporal or depth profile for a single
  location, but it could also be a zonal field (such as a transect in
  latitude - atmospheric depth for a given longitude) or some other
  profile in the physical space of the data variable.

``` r
# Extract a timeseries for a specific location
ts <- t2m[5, 4, ]
str(ts)
#>  num [1, 1, 1:24] 293 292 292 291 291 ...
#>  - attr(*, "dimnames")=List of 3
#>   ..$ : chr "28.4"
#>   ..$ : chr "-1.3"
#>   ..$ : chr [1:24] "2016-01-01T00:00:00" "2016-01-01T01:00:00" "2016-01-01T02:00:00" "2016-01-01T03:00:00" ...
#>  - attr(*, "axis")= Named list()
#>  - attr(*, "time")=List of 1
#>   ..$ time:CFTime with origin [hours since 1900-01-01 00:00:00.0] using calendar [standard] having 24 offset values

# Extract the full spatial extent for one time step
ts <- t2m[, , 12]
str(ts)
#>  num [1:31, 1:21, 1] 300 300 300 300 300 ...
#>  - attr(*, "dimnames")=List of 3
#>   ..$ : chr [1:31] "28" "28.1" "28.200001" "28.299999" ...
#>   ..$ : chr [1:21] "-1" "-1.1" "-1.2" "-1.3" ...
#>   ..$ : chr "2016-01-01T11:00:00"
#>  - attr(*, "axis")= Named list()
#>  - attr(*, "time")=List of 1
#>   ..$ time:CFTime with origin [hours since 1900-01-01 00:00:00.0] using calendar [standard] having 1 offset values
```

Note that the results contain degenerate dimensions (of length 1). This
by design when using basic `[]` data access because it allows attributes
to be attached in a consistent manner. When using the `subset()` method,
the data is returned as an instance of `CFArray`, including axes and
attributes:

``` r
# Extract a specific region, full time dimension
(ts <- t2m$subset(list(X = 29:30, Y = -1:-2)))
#> <Data array> t2m 
#> Long name: 2 metre temperature 
#> 
#> Values: [283.0182 ... 302.0447] K
#>     NA: 0 (0.0%)
#> 
#> Axes:
#>  axis name      length unlim values                                       
#>  X    longitude 31           [28 ... 31]                                  
#>  Y    latitude  21           [-1 ... -3]                                  
#>  T    time      24     U     [2016-01-01T00:00:00 ... 2016-01-01T23:00:00]
#>  unit                             
#>  degrees_east                     
#>  degrees_north                    
#>  hours since 1900-01-01 00:00:00.0
#> 
#> Attributes:
#>  name         type      length value                
#>  long_name    NC_CHAR   19     2 metre temperature  
#>  units        NC_CHAR    1     K                    
#>  actual_range NC_DOUBLE  2     283.018168, 302.04472

# Extract specific time slices for a specific region
# Note that the dimensions are specified out of order and using alternative
# specifications: only the extreme values are used.
(ts <- t2m$subset(list(T = c("2016-01-01 09:00", "2016-01-01 15:00"),
                       X = c(29.6, 28.8),
                       Y = seq(-2, -1, by = 0.05))))
#> <Data array> t2m 
#> Long name: 2 metre temperature 
#> 
#> Values: [283.0182 ... 302.0447] K
#>     NA: 0 (0.0%)
#> 
#> Axes:
#>  axis name      length unlim values                                       
#>  X    longitude 31           [28 ... 31]                                  
#>  Y    latitude  21           [-1 ... -3]                                  
#>  T    time      24     U     [2016-01-01T00:00:00 ... 2016-01-01T23:00:00]
#>  unit                             
#>  degrees_east                     
#>  degrees_north                    
#>  hours since 1900-01-01 00:00:00.0
#> 
#> Attributes:
#>  name         type      length value                
#>  long_name    NC_CHAR   19     2 metre temperature  
#>  units        NC_CHAR    1     K                    
#>  actual_range NC_DOUBLE  2     283.018168, 302.04472
```

The latter two methods will read only as much data from the netCDF
resource as is requested.

##### Summarising data over time

With the `summarise()` method, available for both `CFVariable` and
`CFArray`, you can apply a function over the data to generate summaries.
You could, for instance, summarise daily data to monthly means. These
methods use the specific calendar of the “time” axis. The return value
is a new `CFArray` object.

``` r
# Summarising hourly temperature data to calculate the daily maximum temperature
t2m$summarise("tmax", max, "day")
#> <Data array> tmax 
#> Long name: 2 metre temperature 
#> 
#> Values: [290.0364 ... 302.0447] K
#>     NA: 0 (0.0%)
#> 
#> Axes:
#>  axis name      length values                unit                             
#>  X    longitude 31     [28 ... 31]           degrees_east                     
#>  Y    latitude  21     [-1 ... -3]           degrees_north                    
#>  T    time       1     [2016-01-01T12:00:00] hours since 1900-01-01 00:00:00.0
#> 
#> Attributes:
#>  name         type      length value                
#>  long_name    NC_CHAR   19     2 metre temperature  
#>  units        NC_CHAR    1     K                    
#>  actual_range NC_DOUBLE  2     290.036358, 302.04472
```

A function may also return a vector of multiple values, in which case a
list is returned with a new `CFArray` object for each return value of
the function. This allows you to calculate multiple results with a
single call. You could write your own function to tailor the
calculations to your needs. Rather than just calculating the daily
maximum, you could get the daily maximum, minimum and diurnal range in
one go:

``` r
# Function to calculate multiple daily stats
# It is good practice to include a `na.rm` argument in all your functions
daily_stats <- function(x, na.rm = TRUE) {
  # x is the vector of values for one day
  minmax <- range(x, na.rm = na.rm)
  diurnal <- minmax[2L] - minmax[1L]
  c(minmax, diurnal)
}

# Call summarise() with your own function
# The `name` argument should have as many names as the function returns results
(stats <- t2m$summarise(c("tmin", "tmax", "diurnal_range"), daily_stats, "day"))
#> $tmin
#> <Data array> tmin 
#> Long name: 2 metre temperature 
#> 
#> Values: [283.0182 ... 293.8659] K
#>     NA: 0 (0.0%)
#> 
#> Axes:
#>  axis name      length values                unit                             
#>  X    longitude 31     [28 ... 31]           degrees_east                     
#>  Y    latitude  21     [-1 ... -3]           degrees_north                    
#>  T    time       1     [2016-01-01T12:00:00] hours since 1900-01-01 00:00:00.0
#> 
#> Attributes:
#>  name         type      length value                 
#>  long_name    NC_CHAR   19     2 metre temperature   
#>  units        NC_CHAR    1     K                     
#>  actual_range NC_DOUBLE  2     283.018168, 293.865857
#> 
#> $tmax
#> <Data array> tmax 
#> Long name: 2 metre temperature 
#> 
#> Values: [290.0364 ... 302.0447] K
#>     NA: 0 (0.0%)
#> 
#> Axes:
#>  axis name      length values                unit                             
#>  X    longitude 31     [28 ... 31]           degrees_east                     
#>  Y    latitude  21     [-1 ... -3]           degrees_north                    
#>  T    time       1     [2016-01-01T12:00:00] hours since 1900-01-01 00:00:00.0
#> 
#> Attributes:
#>  name         type      length value                
#>  long_name    NC_CHAR   19     2 metre temperature  
#>  units        NC_CHAR    1     K                    
#>  actual_range NC_DOUBLE  2     290.036358, 302.04472
#> 
#> $diurnal_range
#> <Data array> diurnal_range 
#> Long name: 2 metre temperature 
#> 
#> Values: [1.819982 ... 11.27369] K
#>     NA: 0 (0.0%)
#> 
#> Axes:
#>  axis name      length values                unit                             
#>  X    longitude 31     [28 ... 31]           degrees_east                     
#>  Y    latitude  21     [-1 ... -3]           degrees_north                    
#>  T    time       1     [2016-01-01T12:00:00] hours since 1900-01-01 00:00:00.0
#> 
#> Attributes:
#>  name         type      length value              
#>  long_name    NC_CHAR   19     2 metre temperature
#>  units        NC_CHAR    1     K                  
#>  actual_range NC_DOUBLE  2     1.819982, 11.27369
```

Note that you may have to update some attributes after calling
`summarise()`. You can use the `set_attribute()` method on the `CFArray`
objects to do that.

##### Exporting and saving data

A `CFData` object can be exported to a `data.table` or to a
`terra::SpatRaster` (3D) or `terra::SpatRasterDataset` (4D) for further
processing. Obviously, these packages need to be installed to utilise
these methods.

``` r
# install.packages("data.table")
library(data.table)
head(dt <- ts$data.table())
#>    longitude latitude                time      t2m
#>        <num>    <num>              <char>    <num>
#> 1:      28.0       -1 2016-01-01T00:00:00 293.8875
#> 2:      28.1       -1 2016-01-01T00:00:00 294.4015
#> 3:      28.2       -1 2016-01-01T00:00:00 294.4972
#> 4:      28.3       -1 2016-01-01T00:00:00 293.9426
#> 5:      28.4       -1 2016-01-01T00:00:00 293.6339
#> 6:      28.5       -1 2016-01-01T00:00:00 293.0206

#install.packages("terra")
suppressMessages(library(terra))
(r <- stats[["diurnal_range"]]$terra())
#> class       : SpatRaster 
#> dimensions  : 21, 31, 1  (nrow, ncol, nlyr)
#> resolution  : 0.1, 0.1  (x, y)
#> extent      : 27.95, 31.05, -3.05, -0.95  (xmin, xmax, ymin, ymax)
#> coord. ref. : lon/lat WGS 84 (EPSG:4326) 
#> source(s)   : memory
#> name        : 2016-01-01T12:00:00 
#> min value   :            1.819982 
#> max value   :           11.273690
terra::plot(r)
```

<img src="man/figures/README-export-1.png" width="100%" />

A `CFData` object can also be written back to a netCDF file. The object
will have all its relevant attributes and properties written together
with the actual data: axes, bounds, attributes, CRS.

``` r
# Save a CFData instance to a netCDF file on disk
stats[["diurnal_range"]]$save("~/path/file.nc")
```

##### A note on Discrete Sampling Geometries

Discrete Sampling Geometries (DSG) map almost directly to the venerable
`data.frame` in R (with several exceptions). In that sense, they are
rather distinct from array-based data sets. At the moment there is no
specific code for DSG, but the simplest layouts can currently already be
read (without any warranty). Various methods, such as
`CFVariable::subset()` or `CFData::array()` will fail miserably, and you
are well-advised to try no more than the empty array indexing operator
`CFVariable::[]` which will yield the full data variable with column and
row names set as an array, of `CFVariable::data()` to get the whole data
variable as a `CFData` object for further processing. You can identify a
DSG data set by the `featureType` attribute of the `CFDataset`.

More comprehensive support for DSG is in the development plan.

## Development plan

Package `ncdfCF` is in the early phases of development. It supports
reading of all data objects from netCDF resources in “classic” and
“netcdf4” formats; and can write single data arrays back to a netCDF
file. From the CF Metadata Conventions it supports identification of
axes, interpretation of the “time” axis, name resolution when using
groups, reading of “bounds” information, auxiliary coordinate variables,
labels, cell measures, attributes and grid mapping information.

Development plans for the near future focus on supporting the below
features:

##### netCDF

- Support for writing of data sets (`CFArray` instances can already be
  written to file).

##### CF Metadata Conventions

- Calculate parametric vertical coordinates.
- Aggregation, using the CFA convention.
- Support for discrete sampling geometries.
- Interface to “standard_name” libraries and other “defined
  vocabularies”.
- Compliance with CMIP5 / CMIP6 requirements.

## Installation

Package `ncdfCF` is still in the early phases of development. While
extensively tested on multiple well-structured datasets, errors may
still occur, particularly in datasets that do not adhere to the CF
Metadata Conventions. The API may still change and although care is
taken not to make breaking changes, sometimes this is unavoidable.

Installation from CRAN of the latest release:

    install.packages("ncdfCF")

You can install the development version of `ncdfCF` from
[GitHub](https://github.com/) with:

    # install.packages("devtools")
    devtools::install_github("pvanlaake/ncdfCF")
