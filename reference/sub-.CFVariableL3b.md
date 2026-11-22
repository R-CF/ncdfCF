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
  of elements along each axis. `i` is for the latitude axis, `j` for the
  longitude axis, `...` (additional named arguments) is invalid as there
  are only two axes to subset from. If either expression is missing, the
  entire axis is extracted. The values for the arguments should be an
  integer vector; the range of the values in the vector will be used.
  See examples, below.

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
if (FALSE) { # \dontrun{
# A NASA Ocean Color level-3 binned file
ds <- open_ncdf("AQUA_MODIS.20030101.L3b.DAY.CHL.nc")
chl <- ds[["/level-3_binned_data/chlor_a"]]

# The dimensions of the grid: latitude, longitude
chl$dim()

# Chlorophyll concentration over part of the tropical Atlantic Ocean
x <- chl[1712:1752, 4000:4300]
str(x)

# A band of latitudes over the full longitude extent
band <- chl[1712:1752, ]
str(band)
} # }
```
