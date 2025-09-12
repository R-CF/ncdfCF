
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
commit](https://img.shields.io/github/last-commit/R-CF/ncdfCF)](https://github.com/R-CF/ncdfCF/commits/main)
[![R-CMD-check](https://github.com/R-CF/ncdfCF/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/R-CF/ncdfCF/actions/workflows/R-CMD-check.yaml)
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
- **Parametric vertical coordinates** are computed using the
  `formula_terms` attribute, for two ocean formulations.
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

## Basic usage

Opening and inspecting the contents of a netCDF resource is very
straightforward:

``` r
library(ncdfCF)

# Get any netCDF file
fn <- system.file("extdata", "ERA5land_Rwanda_20160101.nc", package = "ncdfCF")

# Open the file, all metadata is read
(ds <- open_ncdf(fn))
#> <Dataset> ERA5land_Rwanda_20160101 
#> Resource   : /private/var/folders/gs/s0mmlczn4l7bjbmwfrrhjlt80000gn/T/RtmpWtgooh/temp_libpath49db3636d842/ncdfCF/extdata/ERA5land_Rwanda_20160101.nc 
#> Format     : offset64 
#> Collection : Generic netCDF data 
#> Conventions: CF-1.6 
#> 
#> Variables:
#>  name long_name             units data_type axes                     
#>  t2m  2 metre temperature   K     NC_DOUBLE longitude, latitude, time
#>  pev  Potential evaporation m     NC_DOUBLE longitude, latitude, time
#>  tp   Total precipitation   m     NC_DOUBLE longitude, latitude, time
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
#> Values: (not loaded)
#> 
#> Axes:
#>  axis name      length values                                       
#>  X    longitude 31     [28 ... 31]                                  
#>  Y    latitude  21     [-1 ... -3]                                  
#>  T    time      24-U   [2016-01-01T00:00:00 ... 2016-01-01T23:00:00]
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
dimnames(ds[["longitude"]]) # An axis: vector of axis coordinate values
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
#> [1] "/private/var/folders/gs/s0mmlczn4l7bjbmwfrrhjlt80000gn/T/RtmpWtgooh/temp_libpath49db3636d842/ncdfCF/extdata/ERA5land_Rwanda_20160101.nc"
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

## Extracting data

There are three ways to read data for a data variable from the resource:

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
# Extract a timeseries for a specific location - see also the `profile()` method
ts <- t2m[5, 4, ]
str(ts)
#>  num [1, 1, 1:24] 293 292 292 291 291 ...
#>  - attr(*, "dimnames")=List of 3
#>   ..$ longitude: chr "28.4"
#>   ..$ latitude : chr "-1.3"
#>   ..$ time     : chr [1:24] "2016-01-01T00:00:00" "2016-01-01T01:00:00" "2016-01-01T02:00:00" "2016-01-01T03:00:00" ...
#>  - attr(*, "axis")= Named chr [1:3] "X" "Y" "T"
#>   ..- attr(*, "names")= chr [1:3] "longitude" "latitude" "time"
#>  - attr(*, "time")=List of 1
#>   ..$ time:CFTime with origin [hours since 1900-01-01 00:00:00.0] using calendar [standard] having 24 offset values

# Extract the full spatial extent for one time step
ts <- t2m[, , 12]
str(ts)
#>  num [1:31, 1:21, 1] 300 300 300 300 300 ...
#>  - attr(*, "dimnames")=List of 3
#>   ..$ longitude: chr [1:31] "28" "28.1" "28.200001" "28.299999" ...
#>   ..$ latitude : chr [1:21] "-1" "-1.1" "-1.2" "-1.3" ...
#>   ..$ time     : chr "2016-01-01T11:00:00"
#>  - attr(*, "axis")= Named chr [1:3] "X" "Y" "T"
#>   ..- attr(*, "names")= chr [1:3] "longitude" "latitude" "time"
#>  - attr(*, "time")=List of 1
#>   ..$ time:CFTime with origin [hours since 1900-01-01 00:00:00.0] using calendar [standard] having 1 offset values
```

Note that the results contain degenerate dimensions (of length 1). This
by design when using basic `[]` data access because it allows attributes
to be attached in a consistent manner. When using the `subset()` method,
the data is returned as an instance of `CFVariable`, including axes and
attributes:

``` r
# Extract a specific region, full time dimension
(ts <- t2m$subset(list(X = 29:30, Y = -1:-2)))
#> <Variable> t2m 
#> Long name: 2 metre temperature 
#> 
#> Values: (not loaded)
#> 
#> Axes:
#>  axis name      length values                                       
#>  X    longitude 11     [29 ... 30]                                  
#>  Y    latitude  11     [-1 ... -2]                                  
#>  T    time      24-U   [2016-01-01T00:00:00 ... 2016-01-01T23:00:00]
#>  unit                             
#>  degrees_east                     
#>  degrees_north                    
#>  hours since 1900-01-01 00:00:00.0
#> 
#> Attributes:
#>  name      type    length value              
#>  long_name NC_CHAR 19     2 metre temperature
#>  units     NC_CHAR  1     K

# Extract specific time slices for a specific region
# Note that the dimensions are specified out of order and using alternative
# specifications: only the extreme values are used.
(ts <- t2m$subset(list(T = c("2016-01-01 09:00", "2016-01-01 15:00"),
                       X = c(29.6, 28.8),
                       Y = seq(-2, -1, by = 0.05))))
#> <Variable> t2m 
#> Long name: 2 metre temperature 
#> 
#> Values: (not loaded)
#> 
#> Axes:
#>  axis name      length values                                       
#>  X    longitude 7      [28.9 ... 29.5]                              
#>  Y    latitude  11     [-1 ... -2]                                  
#>  T    time      6-U    [2016-01-01T09:00:00 ... 2016-01-01T14:00:00]
#>  unit                             
#>  degrees_east                     
#>  degrees_north                    
#>  hours since 1900-01-01 00:00:00.0
#> 
#> Attributes:
#>  name      type    length value              
#>  long_name NC_CHAR 19     2 metre temperature
#>  units     NC_CHAR  1     K
```

The latter two methods will read only as much data from the netCDF
resource as is requested.

##### Make a profile of data

It is often useful to extract a “profile” of data for a given location
or zone, such as a timeseries of data. The `profile()` method has some
flexible options to support this:

- Profile specific locations, with multiple locations specified per
  call, returning the data as a (set of) `CFVariable` instance(s) or as
  a single `data.table`.
- Profile zones, such as a latitude band or an atmospheric level. Data
  is returned as a new `CFVariable` instance(s).

In all cases, you can profile over any of the axes and over any number
of axes.

Note that the `profile()` method returns data for the grid cells closest
to the specified location. That is different from the `subset()` method,
which will return data as it is recorded in the netCDF resource.

``` r
rwa <- t2m$profile(longitude = c(30.07, 30.07, 29.74), latitude = c(-1.94, -1.58, -2.60), 
                   .names = c("Kigali", "Byumba", "Butare"), .as_table = TRUE)
head(rwa)
#>                   time longitude latitude .variable   .value
#>                 <char>     <num>    <num>    <char>    <num>
#> 1: 2016-01-01T00:00:00     30.07    -1.94    Kigali 290.4055
#> 2: 2016-01-01T01:00:00     30.07    -1.94    Kigali 290.0088
#> 3: 2016-01-01T02:00:00     30.07    -1.94    Kigali 289.3608
#> 4: 2016-01-01T03:00:00     30.07    -1.94    Kigali 288.8414
#> 5: 2016-01-01T04:00:00     30.07    -1.94    Kigali 288.4713
#> 6: 2016-01-01T05:00:00     30.07    -1.94    Kigali 289.9276
attr(rwa, "value")
#> $name
#> [1] "2 metre temperature"
#> 
#> $units
#> [1] "K"
```

Some critical metadata is recorded in the “value” attribute: original
long name and the physical unit.

When you provide coordinates for all axes but one, you get a profile of
values along the remaining axis, as shown above. If you provide fewer
axis coordinates you get progressively higher-order results. To get a
latitudinal transect, for instance, provide only a longitude coordinate:

``` r
(trans29_74 <- t2m$profile(longitude = 29.74, .names = "lon_29_74"))
#> <Variable> lon_29_74 
#> Long name: 2 metre temperature 
#> 
#> Values: [286.5394 ... 298.963] K
#>     NA: 0 (0.0%)
#> 
#> Axes:
#>  axis name      length values                                       
#>  Y    latitude  21     [-1 ... -3]                                  
#>  T    time      24-U   [2016-01-01T00:00:00 ... 2016-01-01T23:00:00]
#>  X    longitude 1      [29.74]                                      
#>  unit                             
#>  degrees_north                    
#>  hours since 1900-01-01 00:00:00.0
#>  degrees_east                     
#> 
#> Attributes:
#>  name         type      length value                
#>  long_name    NC_CHAR   19     2 metre temperature  
#>  units        NC_CHAR    1     K                    
#>  actual_range NC_DOUBLE  2     286.539447, 298.96298
#>  coordinates  NC_CHAR    9     longitude
```

Note that there is only a single longitude coordinate left, at exactly
the specified longitude.

## Summarising data over time

With the `summarise()` method you can apply a function over the data to
generate summaries. You could, for instance, summarise daily data to
monthly means. These methods use the specific calendar of the “time”
axis. The return value is a new `CFVariable` object.

``` r
# Summarising hourly temperature data to calculate the daily maximum temperature
t2m$summarise("tmax", max, "day")
#> <Variable> tmax 
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
list is returned with a new `CFVariable` object for each return value of
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
#> <Variable> tmin 
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
#> <Variable> tmax 
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
#> <Variable> diurnal_range 
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
`summarise()`. You can use the `set_attribute()` method on the
`CFVariable` objects to do that.

## Create new netCDF objects

You can convert a suitable R object into a `CFVariable` instance quite
easily. R objects that are supported include arrays, matrices and
vectors of type logical, integer, numeric or logical.

``` r
arr <- array(rnorm(120), dim = c(6, 5, 4))
as_CF("my_first_CF_object", arr)
#> <Variable> my_first_CF_object 
#> 
#> Values: [-3.054023 ... 2.954466] 
#>     NA: 0 (0.0%)
#> 
#> Axes:
#>  name   length values   
#>  axis_1 6      [1 ... 6]
#>  axis_2 5      [1 ... 5]
#>  axis_3 4      [1 ... 4]
#> 
#> Attributes:
#>  name         type      length value              
#>  actual_range NC_DOUBLE 2      -3.054023, 2.954466
```

Usable but not very impressive. The axes have dull names without any
meaning and the coordinates are just a sequence along the axis.

If the R object has `dimnames` set, these will be used to create more
informed axes. More interestingly, if your array represents some spatial
data you can give your `dimnames` appropriate names (“lat”, “lon”,
“latitude”, “longitude”, case-insensitive) and the corresponding axis
will be created (if the coordinate values in the `dimnames` are within
the domain of the axis type). For “time” coordinates, these are
automatically detected irrespective of the name.

``` r
# Note the use of named dimnames here - these will become the names of the axes
dimnames(arr) <- list(lat = c(45, 44, 43, 42, 41, 40), lon = c(0, 1, 2, 3, 4), 
                      time = c("2025-07-01", "2025-07-02", "2025-07-03", "2025-07-04"))

(obj <- as_CF("a_better_CF_object", arr))
#> <Variable> a_better_CF_object 
#> 
#> Values: [-3.054023 ... 2.954466] 
#>     NA: 0 (0.0%)
#> 
#> Axes:
#>  axis name length values                      unit                          
#>  Y    lat  6      [45 ... 40]                 degrees_north                 
#>  X    lon  5      [0 ... 4]                   degrees_east                  
#>  T    time 4      [2025-07-01 ... 2025-07-04] days since 1970-01-01T00:00:00
#> 
#> Attributes:
#>  name         type      length value              
#>  actual_range NC_DOUBLE 2      -3.054023, 2.954466

# Axes are of a specific type and have basic attributes set
obj$axes[["lat"]]
#> <Latitude axis> [-22] lat
#> Length     : 6
#> Axis       : Y 
#> Coordinates: 45, 44, 43, 42, 41, 40 (degrees_north)
#> Bounds     : (not set)
#> 
#> Attributes:
#>  name          type      length value        
#>  actual_range  NC_DOUBLE  2     40, 45       
#>  axis          NC_CHAR    1     Y            
#>  standard_name NC_CHAR    8     latitude     
#>  units         NC_CHAR   13     degrees_north

obj$axes[["time"]]
#> <Time axis> [-24] time
#> Length     : 4
#> Axis       : T 
#> Calendar   : standard 
#> Range      : 2025-07-01 ... 2025-07-04 (days) 
#> Bounds     : (not set) 
#> 
#> Attributes:
#>  name          type      length value                         
#>  actual_range  NC_DOUBLE  2     20270, 20273                  
#>  axis          NC_CHAR    1     T                             
#>  standard_name NC_CHAR    4     time                          
#>  units         NC_CHAR   30     days since 1970-01-01T00:00:00
#>  calendar      NC_CHAR    8     standard
```

You can further modify the resulting `CFVariable` by setting other
properties, such as attributes or a coordinate reference system. Once
the object is complete, you can export or save it.

## Exporting and saving data

A `CFVariable` object can be exported to a `data.table` or to a
`terra::SpatRaster` (3D) or `terra::SpatRasterDataset` (4D) for further
processing. Obviously, these packages need to be installed to utilise
these methods.

``` r
# install.packages("data.table")
library(data.table)
head(dt <- ts$data.table())
#>    longitude latitude                time
#>        <num>    <num>              <char>
#> 1:      28.9       -1 2016-01-01T09:00:00
#> 2:      29.0       -1 2016-01-01T09:00:00
#> 3:      29.1       -1 2016-01-01T09:00:00
#> 4:      29.2       -1 2016-01-01T09:00:00
#> 5:      29.3       -1 2016-01-01T09:00:00
#> 6:      29.4       -1 2016-01-01T09:00:00

#install.packages("terra")
suppressMessages(library(terra))
(r <- stats[["diurnal_range"]]$terra())
#> class       : SpatRaster 
#> size        : 21, 31, 1  (nrow, ncol, nlyr)
#> resolution  : 0.1, 0.1  (x, y)
#> extent      : 27.95, 31.05, -3.05, -0.95  (xmin, xmax, ymin, ymax)
#> coord. ref. :  
#> source(s)   : memory
#> name        : 2016-01-01T12:00:00 
#> min value   :            1.819982 
#> max value   :           11.273690
terra::plot(r)
```

<img src="man/figures/README-export-1.png" width="100%" />

A `CFVariable` object can also be written back to a netCDF file. The
object will have all its relevant attributes and properties written
together with the actual data: axes, bounds, attributes, CRS. The netCDF
file is of version “netcdf4” and will have the axes oriented in such a
way that the file has maximum portability (specifically, data will be
stored in row-major order with increasing Y values).

``` r
# Save a CFVariable instance to a netCDF file on disk
stats[["diurnal_range"]]$save("~/path/file.nc")
```

##### A note on Discrete Sampling Geometries

Discrete Sampling Geometries (DSG) map almost directly to the venerable
`data.frame` in R (with several exceptions). In that sense, they are
rather distinct from array-based data sets. At the moment there is no
specific code for DSG, but the simplest layouts can currently already be
read (without any warranty). Various methods, such as
`CFVariable::subset()` or `CFVariable::array()` will fail miserably, and
you are well-advised to try no more than the empty array indexing
operator `CFVariable::[]` which will yield the full data variable with
column and row names set as an array, of `CFVariable::data.table` for a
format that matches the structure of a typical table closest. You can
identify a DSG data set by the `featureType` attribute of the
`CFDataset`.

More comprehensive support for DSG is in the development plan.

## Development plan

Package `ncdfCF` is still being developed. It supports reading of all
data objects from netCDF resources in “classic” and “netcdf4” formats;
and can write single data variables back to a netCDF file. From the CF
Metadata Conventions it supports identification of axes, interpretation
of the “time” axis, name resolution when using groups, cell boundary
information, auxiliary coordinate variables, labels, cell measures,
attributes and grid mapping information, among others.

Development plans for the near future focus on supporting the below
features:

##### netCDF

- Support for writing of complex data sets (single `CFVariable`
  instances can already be written to file).
- Writing data to an unlimited dimension of a data variable.

##### CF Metadata Conventions

- Cell methods
- Aggregation, using the CFA convention.
- Support for discrete sampling geometries.
- Compliance with CMIP5 / CMIP6 requirements.

## Installation

Package `ncdfCF` is still being developed. While extensively tested on
multiple well-structured data sets, errors may still occur, particularly
in data sets that do not adhere to the CF Metadata Conventions. The API
may still change and although care is taken not to make breaking
changes, sometimes this is unavoidable.

Installation from CRAN of the latest release:

    install.packages("ncdfCF")

You can install the development version of `ncdfCF` from
[GitHub](https://github.com/) with:

    # install.packages("devtools")
    devtools::install_github("R-CF/ncdfCF")
