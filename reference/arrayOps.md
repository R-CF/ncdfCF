# Operations on CFVariable objects

Basic arithmetic, mathematical and logical operations can be applied on
the data of
[CFVariable](https://r-cf.github.io/ncdfCF/reference/CFVariable.md)
objects having a suitable data type, specifically the base R functions
from the Ops and Math groups of the S3
[groupGeneric](https://rdrr.io/r/base/groupGeneric.html) functions.

## Usage

``` r
# S3 method for class 'CFVariable'
Ops(e1, e2)

# S3 method for class 'CFVariable'
Math(x, ...)
```

## Arguments

- e1, e2:

  `CFVariable` objects, or a single numeric value.

- x:

  A CFVariable object.

- ...:

  Additional arguments passed on to the math functions.

## Value

A new `CFVariable` object. The object will have the same coordinate
space as the `CFVariable` object used as argument. Arguments are not
copied and the new object will only have the "actual_range" attribute
set.

Results that are logical (see the examples) are stored using the
`NC_SHORT` data type because netCDF does not have a native logical data
type.

## Details

The functions always return a new `CFVariable` object. Functions can
thus be concatenated to create more complex expressions. The data type
of the new object is determined by the base R function; its name is
concatenated from the names in the argument object(s). The result will
be assigned to a private group and is thus completely disjoint from
other CF objects.

For the Ops functions with two arguments, if both arguments are a
`CFVariable` object they have to be compatible: same shape, axis
coordinate values and coordinate reference system. The resulting
`CFVariable` object will use the same axes as the `CFVariable` object(s)
used as argument.

The attributes of the resulting `CFVariable` object should be updated to
reflect its contents, in particular the "name", "long_name",
"standard_name" and "units" attributes. Attributes are not copied over
from the `CFVariable` objects in the arguments.

## Examples

``` r
fn <- system.file("extdata", "ERA5land_Rwanda_20160101.nc", package = "ncdfCF")
ds <- open_ncdf(fn)

# Temperature data in K
t2m <- ds[["t2m"]]

# Convert to degrees_Celsius
t2mC <- t2m - 273.15
t2mC$name <- "t2m_Celsius"
t2mC$set_attribute("units", "NC_CHAR", "degrees_Celsius")
t2mC
#> <Variable> t2m_Celsius 
#> 
#> Values: [9.868168 ... 28.89472] degrees_Celsius
#>     NA: 0 (0.0%)
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
#>  name         type      length value             
#>  actual_range NC_DOUBLE  2     9.868168, 28.89472
#>  units        NC_CHAR   15     degrees_Celsius   

hot <- t2mC > 20
hot$name <- "t2m_Celsius_over_20"
hot$set_attribute("long_name", "NC_CHAR", "Flag to indicate where temperature is 20C or hotter")
hot$set_attribute("units", "NC_CHAR", "1")
hot
#> <Variable> t2m_Celsius_over_20 
#> Long name: Flag to indicate where temperature is 20C or hotter 
#> 
#> Values: [0 ... 1] 1
#>     NA: 0 (0.0%)
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
#>  name         type     length value                         
#>  actual_range NC_SHORT  2     0, 1                          
#>  long_name    NC_CHAR  51     Flag to indicate where temp...
#>  units        NC_CHAR   1     1                             
```
