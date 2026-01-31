# Get a CF object from a data set

This method can be used to retrieve a variable or axis from the data set
by name.

## Usage

``` r
# S3 method for class 'CFDataset'
x[[i]]
```

## Arguments

- x:

  An `CFDataset` to extract a variable or axis from.

- i:

  The name of a variable or axis in `x`. If data set `x` has groups, `i`
  should be an absolute path to the object to retrieve.

## Value

An instance of `CFVariable` or an `CFAxis` descendant class, or `NULL`
if the name is not found.

## Details

If the data set has groups, the name `i` of the variable or axis should
be fully qualified with the path to the group where the object is
located. This fully qualified name can be retrieved with the
[`names()`](https://rdrr.io/r/base/names.html) and
[`dimnames()`](https://r-cf.github.io/ncdfCF/reference/dimnames.md)
functions, respectively.

## Examples

``` r
fn <- system.file("extdata", "ERA5land_Rwanda_20160101.nc", package = "ncdfCF")
ds <- open_ncdf(fn)
v1 <- ds$var_names[1]
var <- ds[[v1]]
var
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
```
