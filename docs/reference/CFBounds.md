# CF boundary variable

This class represents the boundaries of an axis or an auxiliary
longitude-latitude grid.

The class manages the boundary information for an axis (2 vertices per
element) or an auxiliary longitude-latitude grid (4 vertices per
element).

## Super classes

[`ncdfCF::CFObject`](https://r-cf.github.io/ncdfCF/reference/CFObject.md)
-\>
[`ncdfCF::CFData`](https://r-cf.github.io/ncdfCF/reference/CFData.md)
-\> `CFBounds`

## Active bindings

- `friendlyClassName`:

  (read-only) A nice description of the class.

- `length`:

  (read-only) The length of the second dimension of the data, i.e. the
  number of boundary values.

- `vertices`:

  (read-only) The length of the first dimension of the data, i.e. the
  number of vertices that make up a boundary.

- `values`:

  Set or retrieve the boundary values of this object. Upon retrieval,
  values are read from the netCDF resource, if there is one, upon first
  access and cached thereafter. Upon setting values, if there is a
  linked netCDF resource, this object will be detached from it.

## Methods

### Public methods

- [`CFBounds$new()`](#method-CFBounds-new)

- [`CFBounds$print()`](#method-CFBounds-print)

- [`CFBounds$range()`](#method-CFBounds-range)

- [`CFBounds$copy()`](#method-CFBounds-copy)

- [`CFBounds$subset()`](#method-CFBounds-subset)

- [`CFBounds$append()`](#method-CFBounds-append)

- [`CFBounds$write()`](#method-CFBounds-write)

Inherited methods

- [`ncdfCF::CFObject$append_attribute()`](https://r-cf.github.io/ncdfCF/reference/CFObject.html#method-append_attribute)
- [`ncdfCF::CFObject$attach_to_group()`](https://r-cf.github.io/ncdfCF/reference/CFObject.html#method-attach_to_group)
- [`ncdfCF::CFObject$attribute()`](https://r-cf.github.io/ncdfCF/reference/CFObject.html#method-attribute)
- [`ncdfCF::CFObject$attributes_identical()`](https://r-cf.github.io/ncdfCF/reference/CFObject.html#method-attributes_identical)
- [`ncdfCF::CFObject$delete_attribute()`](https://r-cf.github.io/ncdfCF/reference/CFObject.html#method-delete_attribute)
- [`ncdfCF::CFObject$print_attributes()`](https://r-cf.github.io/ncdfCF/reference/CFObject.html#method-print_attributes)
- [`ncdfCF::CFObject$set_attribute()`](https://r-cf.github.io/ncdfCF/reference/CFObject.html#method-set_attribute)
- [`ncdfCF::CFObject$write_attributes()`](https://r-cf.github.io/ncdfCF/reference/CFObject.html#method-write_attributes)
- [`ncdfCF::CFData$detach()`](https://r-cf.github.io/ncdfCF/reference/CFData.html#method-detach)
- [`ncdfCF::CFData$dim()`](https://r-cf.github.io/ncdfCF/reference/CFData.html#method-dim)

------------------------------------------------------------------------

### Method `new()`

Create an instance of this class.

#### Usage

    CFBounds$new(
      var,
      group,
      values,
      start = NA,
      count = NA,
      attributes = data.frame(),
      owner_dims = 1L
    )

#### Arguments

- `var`:

  The name of the boundary variable when creating a new boundary
  variable. When reading a boundary variable from file, the
  [NCVariable](https://r-cf.github.io/ncdfCF/reference/NCVariable.md)
  object that describes this instance.

- `group`:

  The [CFGroup](https://r-cf.github.io/ncdfCF/reference/CFGroup.md) that
  this instance will live in.

- `values`:

  Optional. The values of the boundary variable. This must be a numeric
  matrix whose first dimension has a length equal to the number of
  vertices for each boundary, and the second dimension is as long as the
  `CFObject` instances that use these boundary values. Ignored when
  argument `var` is a `NCVariable` object.

- `start`:

  Optional. Vector of indices where to start reading boundary data along
  the dimensions of the data. The vector must be `NA` to read all data,
  otherwise it must have a length equal to the dimensionality of the
  owning object + 1.

- `count`:

  Optional. Vector of number of elements to read along each dimension of
  the boundary data. The vector must be `NA` to read to the end of each
  dimension, otherwise it must have a length equal to the dimensionality
  of the owning object + 1.

- `attributes`:

  Optional. A `data.frame` with the attributes of the boundary object.
  When an empty `data.frame` (default) and argument `var` is an
  `NCVariable` instance, attributes of the bounds object will be taken
  from the netCDF resource.

- `owner_dims`:

  Optional, the number of dimensions of the object that these boundary
  values pertain to. Default is 1.

#### Returns

A new instance of this class.

------------------------------------------------------------------------

### Method [`print()`](https://rdrr.io/r/base/print.html)

Print a summary of the object to the console.

#### Usage

    CFBounds$print(attributes = TRUE, ...)

#### Arguments

- `attributes`:

  Default `TRUE`, flag to indicate if the attributes of the boundary
  values should be printed.

- `...`:

  Arguments passed on to other functions. Of particular interest is
  `width = ` to indicate a maximum width of attribute columns.

------------------------------------------------------------------------

### Method [`range()`](https://rdrr.io/r/base/range.html)

Retrieve the lowest and highest value in the bounds.

#### Usage

    CFBounds$range()

------------------------------------------------------------------------

### Method `copy()`

Create a copy of this bounds object The copy is completely separate from
`self`, meaning that both `self` and all of its components are made from
new instances.

#### Usage

    CFBounds$copy(name = "", group)

#### Arguments

- `name`:

  The name for the new bounds object. If an empty string is passed, will
  use the name of this bounds object.

- `group`:

  The [CFGroup](https://r-cf.github.io/ncdfCF/reference/CFGroup.md)
  where the copy of this axis will live.

#### Returns

The newly created bounds object.

------------------------------------------------------------------------

### Method [`subset()`](https://rdrr.io/r/base/subset.html)

Return a boundary variable spanning a smaller coordinate range. This
currently only applies to 1-D axes.

This method returns boundary values which span the range of indices
given by the `rng` argument.

#### Usage

    CFBounds$subset(group, rng)

#### Arguments

- `group`:

  The [CFGroup](https://r-cf.github.io/ncdfCF/reference/CFGroup.md)
  where the copy of these bounds will live.

- `rng`:

  The range of values from this bounds object to include in the returned
  object.

#### Returns

A `CFBounds` instance covering the indicated range of indices.

------------------------------------------------------------------------

### Method [`append()`](https://rdrr.io/r/base/append.html)

Append boundary values at the end of the current values of the boundary
variable.

#### Usage

    CFBounds$append(from, group)

#### Arguments

- `from`:

  An instance of `CFBounds` whose values to append to the values of this
  boundary variable.

- `group`:

  The [CFGroup](https://r-cf.github.io/ncdfCF/reference/CFGroup.md)
  where the copy of these bounds will live.

#### Returns

A new `CFBounds` instance with values from this boundary variable and
the `from` boundary variable appended. If argument `from` is `NULL`,
return `NULL`.

------------------------------------------------------------------------

### Method [`write()`](https://rdrr.io/r/base/write.html)

Write the boundary variable to a netCDF file. This method should not be
called directly; instead, `CFVariable$save()` will call this method
automatically.

#### Usage

    CFBounds$write(object_name)

#### Arguments

- `object_name`:

  The name of the object that uses these boundary values, usually an
  axis but could also be an auxiliary CV or a parametric Z axis.
