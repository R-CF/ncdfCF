# 1. Creating new data sets and variables

## Creating a new CF data set

Creating a new CF data set is as easy as `my_ds <- create_ncdf()`. This
will create an empty data set (duh!) but it will have a root group and
some basic attributes:

``` r
library(ncdfCF)

(my_ds <- create_ncdf())
#> <Dataset> CF_dataset 
#> Resource   : (virtual) 
#> Format     : netcdf4 
#> Collection : Generic netCDF data 
#> Conventions: CF-1.13 
#> Has groups : FALSE 
#> 
#> Attributes:
#>  name        type    length value                                             
#>  Conventions NC_CHAR  7     CF-1.13                                           
#>  history     NC_CHAR 67     Created with R package ncdfCF 0.7.0.9000 on 202...
```

The data set is created in memory (`Resource : (virtual)`) and you can
save it to disk when you are done adding data variables to it with
`my_ds$save(fn = "~/path/to/netcdf/data/my_data.nc")`. If you prefer,
you can also provide the file name to
`create_ncdf(fn = "~/path/to/netcdf/data/my_data.nc")` and then save
your edits with `my_ds$save()`. This alternative pattern is easier and
less error-prone when you add many objects to the data set and want to
do intermediate saving of your work to the netCDF file.

## Creating a new CF data variable

There are different ways that you can create a CFVariable: - Convert a
suitable R object such as an array or a matrix. - Process data from
existing netCDF resources into a new CFVariable.

### Vector, matrix, array

Any vector, matrix or array of a suitable type (numeric, integer,
logical, character) can be converted to a `CFVariable` with the
[`as_CF()`](https://r-cf.github.io/ncdfCF/reference/as_CF.md) generic S3
method.

``` r
arr <- array(rnorm(120), dim = c(6, 5, 4))
as_CF("my_first_CF_object", arr)
#> <Variable> my_first_CF_object 
#> 
#> Values: [-2.612334 ... 2.755418] 
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
#>  actual_range NC_DOUBLE 2      -2.612334, 2.755418
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
#> Values: [-2.612334 ... 2.755418] 
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
#>  actual_range NC_DOUBLE 2      -2.612334, 2.755418

# Axes are of a specific type and have basic attributes set
obj$axes[["lat"]]
#> <Latitude axis> [-8] lat
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
#> <Time axis> [-10] time
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

You can use the
[`as_CF()`](https://r-cf.github.io/ncdfCF/reference/as_CF.md) generic
method also to convert a
[`terra::SpatRaster`](https://rspatial.github.io/terra/reference/SpatRaster-class.html)
into a `CFVariable`. Keep in mind, though, that `terra` has limited
support for multi-dimensional data and very limited support specifically
for vertical dimensions (`depth` in `terra`) and calendars other than
`standard` or `proleptic_gregorian`. You are therefore advised to
carefully review the properties of the data variable derived from a
[`terra::SpatRaster`](https://rspatial.github.io/terra/reference/SpatRaster-class.html).

### Processing data

The `ncdfCF` package support processing of data through extraction of
subsets or profiles of the data in the netCDF resource, or summarising
data over the “time” axis of a data variable. All of these operations
return a `CFVariable` instance to the caller. You can also perform
arithmetical and mathematical operations on a `CFVariable` which also
returns a new `CFVariable` to the caller.

### Managing your new CFVariables
