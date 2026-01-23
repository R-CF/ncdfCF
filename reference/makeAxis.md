# Create an axis

With this method you can create an axis to use with new
[CFVariable](https://r-cf.github.io/ncdfCF/reference/CFVariable.md)
instances. Depending on the `orientation` argument and the type of the
`values` argument an instance of a class descending from
[CFAxis](https://r-cf.github.io/ncdfCF/reference/CFAxis.md) will be
returned.

## Usage

``` r
makeAxis(
  name,
  orientation,
  values,
  bounds = NULL,
  attributes = data.frame(),
  group = NULL
)
```

## Arguments

- name:

  Name of the axis.

- orientation:

  The orientation of the axis. Must be one of "X", "Y", "Z", or "T" for
  longitude, latitude, height or depth, and time axes, respectively. For
  any other axis, indicate an empty string ""

- values:

  The coordinate values. In the case of an axis with `orientation = "T"`
  this must be a `CFTime` or `CFClimatology` instance.

- bounds:

  The boundary values of the coordinates, or `NULL` if not available.

- attributes:

  `data.frame` with the attributes of the axis to create. Depending on
  which axis is created one or more attributes may be added or amended.

- group:

  [CFGroup](https://r-cf.github.io/ncdfCF/reference/CFGroup.md) instance
  where the axis will be located. If `NULL` (default), a private group
  will be created for the axis.

## Value

An instance of a class descending from
[CFAxis](https://r-cf.github.io/ncdfCF/reference/CFAxis.md).

## Details

There are several restrictions on the combination of `orientation` and
`values` arguments. Longitude, latitude and depth axes (`orientation` of
"X", "Y" or "Z") must have numeric `values`. For a time axis
(`orientation` of "T") the `values` argument must be an instance of
`CFTime` or `CFClimatology`.

## See also

[`makeLongitudeAxis()`](https://r-cf.github.io/ncdfCF/reference/makeLongitudeAxis.md),
[`makeLatitudeAxis()`](https://r-cf.github.io/ncdfCF/reference/makeLatitudeAxis.md),
[`makeTimeAxis()`](https://r-cf.github.io/ncdfCF/reference/makeTimeAxis.md),
[`makeDiscreteAxis()`](https://r-cf.github.io/ncdfCF/reference/makeDiscreteAxis.md)
