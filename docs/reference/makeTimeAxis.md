# Create a time axis

With this method you can create a time axis to use with new
[CFVariable](https://r-cf.github.io/ncdfCF/reference/CFVariable.md)
instances.

## Usage

``` r
makeTimeAxis(name, values, attributes = data.frame(), group = NULL)
```

## Arguments

- name:

  Name of the axis.

- values:

  A `CFTime` or `CFClimatology` instance with time values and optionally
  bounds set.

- attributes:

  `data.frame` with the attributes of the axis to create. Attributes
  "standard_name", "units", "calendar", "actual_range" and "axis" will
  be set or updated.

- group:

  [CFGroup](https://r-cf.github.io/ncdfCF/reference/CFGroup.md) instance
  where the axis will be located. If `NULL` (default), a private group
  will be created for the axis.

## Value

A [CFAxisTime](https://r-cf.github.io/ncdfCF/reference/CFAxisTime.md)
instance.
