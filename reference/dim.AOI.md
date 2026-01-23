# The dimensions of the grid of an AOI

This method returns the dimensions of the grid that would be created for
the AOI.

## Usage

``` r
# S3 method for class 'AOI'
dim(x)
```

## Arguments

- x:

  An instance of the `AOI` class.

## Value

A vector of two values giving the longitude and latitude dimensions of
the grid that would be created for the AOI.

## Examples

``` r
a <- aoi(30, 40, 10, 30, 0.1)
dim(a)
#> [1] 200 100
```
