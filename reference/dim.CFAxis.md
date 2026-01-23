# Axis length

This method returns the lengths of the axes of a variable or axis.

## Usage

``` r
# S3 method for class 'CFAxis'
dim(x)
```

## Arguments

- x:

  A `CFVariable` instance or a descendant of `CFAxis`.

## Value

For a `CFVariable` instance in argument `x`, a named vector of axis
lengths, excluding any scalar axes. For a `CFAxis` descendant instance
in argument `x`, the length of the axis.

## Examples

``` r
fn <- system.file("extdata", "ERA5land_Rwanda_20160101.nc", package = "ncdfCF")
ds <- open_ncdf(fn)
t2m <- ds[["t2m"]]
dim(t2m)
#> longitude  latitude      time 
#>        31        21        24 
dim(t2m$axes[["time"]])
#> [1] 24
```
