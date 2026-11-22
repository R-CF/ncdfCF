# Copy data from a `CFVariable` to a `geozarr_array`

This function will take a
[CFVariable](https://r-cf.github.io/ncdfCF/reference/CFVariable.md) and
copy the data into a GeoZarr array in a Zarr store.

## Usage

``` r
copy_to_geozarr(var, geozarr, verbose = FALSE)
```

## Arguments

- var:

  A `CFVariable` instance to copy the data from.

- geozarr:

  A `geozarr_array` instance to copy data to.

- verbose:

  Logical. If `TRUE`, the progress of copying is printed to the console,
  per chunk of the `geozarr_array` that is written. Default is `FALSE`.

## Value

`var`, invisibly.

## Details

The GeoZarr array must have been initialised before calling this
function. The
[`create_geozarr_array()`](https://rdrr.io/pkg/geozarr/man/create_geozarr_array.html)
can be used for that purpose.

Missing data in the `CFVariable` are translated to the GeoZarr
conventions, see the "Exporting data" vignette for details.

A scalar axis of the `CFVariable` can be an axis of the GeoZarr array,
with a length of 1 for the variable. This is how variables from multiple
files, each with its own scalar axis value such as an ensemble member or
a height, are combined along a new dimension.

The `CFVariable` axes have to be compatible with those of the GeoZarr
array. Specifically, the values of the (active) coordinates of each axis
have to be a contiguous subset of the corresponding axis (by position)
of the GeoZarr array.
