# Create a character axis

With this method you can create a character axis to use with new
[CFVariable](https://r-cf.github.io/ncdfCF/reference/CFVariable.md)
instances.

## Usage

``` r
makeCharacterAxis(name, values, attributes = data.frame(), group = NULL)
```

## Arguments

- name:

  Name of the axis.

- values:

  The character coordinate values of the axis.

- attributes:

  `data.frame` with the attributes of the axis to create.

- group:

  [CFGroup](https://r-cf.github.io/ncdfCF/reference/CFGroup.md) instance
  where the axis will be located. If `NULL` (default), a private group
  will be created for the axis.

## Value

A
[CFAxisCharacter](https://r-cf.github.io/ncdfCF/reference/CFAxisCharacter.md)
instance.
