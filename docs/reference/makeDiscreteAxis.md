# Create a discrete axis

With this method you can create a discrete axis to use with new
[CFVariable](https://r-cf.github.io/ncdfCF/reference/CFVariable.md)
instances.

## Usage

``` r
makeDiscreteAxis(name, length, group = NULL)
```

## Arguments

- name:

  Name of the axis.

- length:

  The length of the axis.

- group:

  [CFGroup](https://r-cf.github.io/ncdfCF/reference/CFGroup.md) instance
  where the axis will be located. If `NULL` (default), a private group
  will be created for the axis.

## Value

A
[CFAxisDiscrete](https://r-cf.github.io/ncdfCF/reference/CFAxisDiscrete.md)
instance. The values will be a sequence of size `length`.
