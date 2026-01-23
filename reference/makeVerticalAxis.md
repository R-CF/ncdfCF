# Create a vertical axis

With this method you can create a vertical axis to use with new
[CFVariable](https://r-cf.github.io/ncdfCF/reference/CFVariable.md)
instances. Note that you should set the "positive" attribute after
creating the axis to indicate if values are increasing going upwards
(positive = "up") or downwards (positive = "down").

## Usage

``` r
makeVerticalAxis(
  name,
  values,
  bounds = NULL,
  attributes = data.frame(),
  group = NULL
)
```

## Arguments

- name:

  Name of the axis.

- values:

  The coordinate values.

- bounds:

  The bounds of the coordinate values, or `NULL` if not available.

- attributes:

  `data.frame` with the attributes of the axis to create. Attributes
  "actual_range" and "axis" will be set or updated.

- group:

  [CFGroup](https://r-cf.github.io/ncdfCF/reference/CFGroup.md) instance
  where the axis will be located. If `NULL` (default), a private group
  will be created for the axis.

## Value

A
[CFAxisVertical](https://r-cf.github.io/ncdfCF/reference/CFAxisVertical.md)
instance.
