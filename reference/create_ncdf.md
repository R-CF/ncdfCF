# Create a new data set

This function creates a new, empty data set with an associated netCDF
file in "netcdf4" format.

## Usage

``` r
create_ncdf(fn)
```

## Arguments

- fn:

  Optional. The fully qualified file name of the netCDF file if that
  should be created. The file cannot already exist. It is recommended
  that the filename uses an extension of ".nc". If the argument is not
  provided, a virtual data set will be created.
