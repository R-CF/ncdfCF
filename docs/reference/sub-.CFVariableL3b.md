# Extract data for a variable

Extract data from a `CFVariableL3b` instance, optionally sub-setting the
axes to load only data of interest.

## Usage

``` r
# S3 method for class 'CFVariableL3b'
x[i, j, ..., drop = FALSE]
```

## Arguments

- x:

  An `CFVariableL3b` instance to extract the data of.

- i, j, ...:

  Expressions, one for each of the two axes of `x`, that select a number
  of elements along each axis. `i` is for the longitude axis, `j` for
  the latitude axis, `...` (additional named arguments) is invalid as
  there are only two axes to subset from. If either expression is
  missing, the entire axis is extracted. The values for the arguments
  may be an integer vector or a function that returns an integer vector.
  The range of the values in the vector will be used. See examples,
  below.

- drop:

  Logical, ignored. Axes are never dropped. Any degenerate dimensions of
  the array are returned as such, with dimnames and appropriate
  attributes set.

## Value

An array with dimnames and other attributes set.

## Details

If all the data of the variable in `x` is to be extracted, simply use
`[]` (unlike with regular arrays, this is required, otherwise the
details of the variable are printed on the console).

The indices into the axes to be subset can be specified in a variety of
ways; in practice it should (resolve to) be a vector of integers. A
range (e.g. `100:200`), an explicit vector (`c(23, 46, 3, 45, 17`), a
sequence (`seq(from = 78, to = 100, by = 2`), all work. Note, however,
that only a single range is generated from the vector so these examples
resolve to `100:200`, `3:46`, and `78:100`, respectively. It is also
possible to use a custom function as an argument.

This method works with "bare" indices into the axes of the array. If you
want to use domain values of the axes (e.g. longitude values or
timestamps) to extract part of the variable array, use the
`CFVariableL3b$subset()` method.

Scalar axes should not be included in the indexing as they do not
represent a dimension into the data array.

## Examples

``` r
fn <- system.file("extdata",
  "pr_day_EC-Earth3-CC_ssp245_r1i1p1f1_gr_20230101-20231231_vncdfCF.nc",
  package = "ncdfCF")
ds <- open_ncdf(fn)
pr <- ds[["pr"]]

# How are the dimensions organized?
dimnames(pr)
#> [1] "lon"  "lat"  "time"

# Precipitation data for March for a single location
x <- pr[5, 12, 61:91]
str(x)
#>  num [1, 1, 1:31] 2.17e-05 2.68e-06 1.04e-04 8.62e-05 7.09e-05 ...
#>  - attr(*, "dimnames")=List of 3
#>   ..$ lon : chr "8.4375"
#>   ..$ lat : chr "48.07005"
#>   ..$ time: chr [1:31] "2023-03-02T12:00:00" "2023-03-03T12:00:00" "2023-03-04T12:00:00" "2023-03-05T12:00:00" ...
#>  - attr(*, "axis")= Named chr [1:3] "X" "Y" "T"
#>   ..- attr(*, "names")= chr [1:3] "lon" "lat" "time"
#>  - attr(*, "time")=List of 1
#>   ..$ time:CFTime with origin [days since 1850-01-01] using calendar [proleptic_gregorian] having 31 offset values

# Summer precipitation over the full spatial extent
summer <- pr[, , 173:263]
str(summer)
#>  num [1:14, 1:14, 1:91] -2.21e-25 -2.21e-25 -2.21e-25 -2.21e-25 -2.21e-25 ...
#>  - attr(*, "dimnames")=List of 3
#>   ..$ lon : chr [1:14] "5.625" "6.328125" "7.03125" "7.734375" ...
#>   ..$ lat : chr [1:14] "40.35078" "41.05254" "41.75429" "42.45604" ...
#>   ..$ time: chr [1:91] "2023-06-22T12:00:00" "2023-06-23T12:00:00" "2023-06-24T12:00:00" "2023-06-25T12:00:00" ...
#>  - attr(*, "axis")= Named chr [1:3] "X" "Y" "T"
#>   ..- attr(*, "names")= chr [1:3] "lon" "lat" "time"
#>  - attr(*, "time")=List of 1
#>   ..$ time:CFTime with origin [days since 1850-01-01] using calendar [proleptic_gregorian] having 91 offset values
```
