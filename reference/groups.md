# List the groups in the CF object, recursively.

List the groups in the CF object, recursively.

## Usage

``` r
groups(x)

# S3 method for class 'CFDataset'
groups(x)
```

## Arguments

- x:

  A `CFDataset` instance.

## Value

A character vector with group names in the object.

## Examples

``` r
fn <- system.file("extdata", "ERA5land_Rwanda_20160101.nc", package = "ncdfCF")
ds <- open_ncdf(fn)
groups(ds)
#> [1] "/"
```
