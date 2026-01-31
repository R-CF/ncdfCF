# Parametric formula term for a vertical CF axis object

This class represents a formula term for a parametric vertical axis.

## Super classes

[`ncdfCF::CFObject`](https://r-cf.github.io/ncdfCF/reference/CFObject.md)
-\>
[`ncdfCF::CFData`](https://r-cf.github.io/ncdfCF/reference/CFData.md)
-\>
[`ncdfCF::CFVariable`](https://r-cf.github.io/ncdfCF/reference/CFVariable.md)
-\> `CFVerticalParametricTerm`

## Active bindings

- `has_data`:

  Logical flag that indicates of the instance has an associated data
  variable. If not, the instance will report `0` as its data.

- `values`:

  (read-only) The values of the parametric term. Depending on the
  definition of the term, this could be a large array or a simple
  scalar. Specifically, if the term is defined but no data is included
  in the netCDF resource, this method will return `0`, as per the CF
  Metadata Conventions.

## Methods

### Public methods

- [`CFVerticalParametricTerm$new()`](#method-CFVerticalParametricTerm-new)

- [`CFVerticalParametricTerm$print()`](#method-CFVerticalParametricTerm-print)

- [`CFVerticalParametricTerm$subset()`](#method-CFVerticalParametricTerm-subset)

Inherited methods

- [`ncdfCF::CFObject$append_attribute()`](https://r-cf.github.io/ncdfCF/reference/CFObject.html#method-append_attribute)
- [`ncdfCF::CFObject$attribute()`](https://r-cf.github.io/ncdfCF/reference/CFObject.html#method-attribute)
- [`ncdfCF::CFObject$attributes_identical()`](https://r-cf.github.io/ncdfCF/reference/CFObject.html#method-attributes_identical)
- [`ncdfCF::CFObject$delete_attribute()`](https://r-cf.github.io/ncdfCF/reference/CFObject.html#method-delete_attribute)
- [`ncdfCF::CFObject$print_attributes()`](https://r-cf.github.io/ncdfCF/reference/CFObject.html#method-print_attributes)
- [`ncdfCF::CFObject$set_attribute()`](https://r-cf.github.io/ncdfCF/reference/CFObject.html#method-set_attribute)
- [`ncdfCF::CFObject$write_attributes()`](https://r-cf.github.io/ncdfCF/reference/CFObject.html#method-write_attributes)
- [`ncdfCF::CFData$dim()`](https://r-cf.github.io/ncdfCF/reference/CFData.html#method-dim)
- [`ncdfCF::CFVariable$add_ancillary_variable()`](https://r-cf.github.io/ncdfCF/reference/CFVariable.html#method-add_ancillary_variable)
- [`ncdfCF::CFVariable$add_auxiliary_coordinate()`](https://r-cf.github.io/ncdfCF/reference/CFVariable.html#method-add_auxiliary_coordinate)
- [`ncdfCF::CFVariable$add_cell_measure()`](https://r-cf.github.io/ncdfCF/reference/CFVariable.html#method-add_cell_measure)
- [`ncdfCF::CFVariable$append()`](https://r-cf.github.io/ncdfCF/reference/CFVariable.html#method-append)
- [`ncdfCF::CFVariable$array()`](https://r-cf.github.io/ncdfCF/reference/CFVariable.html#method-array)
- [`ncdfCF::CFVariable$attach_to_group()`](https://r-cf.github.io/ncdfCF/reference/CFVariable.html#method-attach_to_group)
- [`ncdfCF::CFVariable$brief()`](https://r-cf.github.io/ncdfCF/reference/CFVariable.html#method-brief)
- [`ncdfCF::CFVariable$data.table()`](https://r-cf.github.io/ncdfCF/reference/CFVariable.html#method-data.table)
- [`ncdfCF::CFVariable$detach()`](https://r-cf.github.io/ncdfCF/reference/CFVariable.html#method-detach)
- [`ncdfCF::CFVariable$is_coincident()`](https://r-cf.github.io/ncdfCF/reference/CFVariable.html#method-is_coincident)
- [`ncdfCF::CFVariable$peek()`](https://r-cf.github.io/ncdfCF/reference/CFVariable.html#method-peek)
- [`ncdfCF::CFVariable$profile()`](https://r-cf.github.io/ncdfCF/reference/CFVariable.html#method-profile)
- [`ncdfCF::CFVariable$raw()`](https://r-cf.github.io/ncdfCF/reference/CFVariable.html#method-raw)
- [`ncdfCF::CFVariable$save()`](https://r-cf.github.io/ncdfCF/reference/CFVariable.html#method-save)
- [`ncdfCF::CFVariable$shard()`](https://r-cf.github.io/ncdfCF/reference/CFVariable.html#method-shard)
- [`ncdfCF::CFVariable$summarise()`](https://r-cf.github.io/ncdfCF/reference/CFVariable.html#method-summarise)
- [`ncdfCF::CFVariable$terra()`](https://r-cf.github.io/ncdfCF/reference/CFVariable.html#method-terra)
- [`ncdfCF::CFVariable$time()`](https://r-cf.github.io/ncdfCF/reference/CFVariable.html#method-time)
- [`ncdfCF::CFVariable$write()`](https://r-cf.github.io/ncdfCF/reference/CFVariable.html#method-write)

------------------------------------------------------------------------

### Method `new()`

Create an instance of this class.

#### Usage

    CFVerticalParametricTerm$new(
      var,
      axes,
      values = values,
      start = NA,
      count = NA,
      attributes = data.frame()
    )

#### Arguments

- `var`:

  The
  [NCVariable](https://r-cf.github.io/ncdfCF/reference/NCVariable.md)
  instance upon which this CF variable is based when read from a netCDF
  resource, or the name for the new CF variable to be created.

- `axes`:

  A `list` of
  [CFAxis](https://r-cf.github.io/ncdfCF/reference/CFAxis.md) descendant
  instances that describe the axes of the data object.

- `values`:

  Optional. The values of the variable in an array.

- `start`:

  Optional. Vector of indices where to start reading data along the
  dimensions of the array on file. The vector must be `NA` to read all
  data, otherwise it must have agree with the dimensions of the array on
  file. Ignored when argument `var` is not an `NCVariable` instance.

- `count`:

  Optional. Vector of number of elements to read along each dimension of
  the array on file. The vector must be `NA` to read to the end of each
  dimension, otherwise its value must agree with the corresponding
  `start` value and the dimension of the array on file. Ignored when
  argument `var` is not an `NCVariable` instance.

- `attributes`:

  Optional. A `data.frame` with the attributes of the object. When
  argument `var` is an `NCVariable` instance and this argument is an
  empty `data.frame` (default), arguments will be read from the
  resource.

#### Returns

An instance of this class.

------------------------------------------------------------------------

### Method [`print()`](https://rdrr.io/r/base/print.html)

Prints a summary of the parametric formula term to the console.

#### Usage

    CFVerticalParametricTerm$print(...)

#### Arguments

- `...`:

  Arguments passed on to other functions. Of particular interest is
  `width = ` to indicate a maximum width of attribute columns.

#### Returns

`self`, invisibly.

------------------------------------------------------------------------

### Method [`subset()`](https://rdrr.io/r/base/subset.html)

Subset the indices to read a smaller portion of the data from the netCDF
file. The passed indices should be named after the axes that they refer
to. There may be more indices than axes and they may be in a different
order than the axes of the term.

#### Usage

    CFVerticalParametricTerm$subset(
      original_axis_names,
      new_axes,
      start,
      count,
      aux = NULL,
      ZT_dim = NULL
    )

#### Arguments

- `original_axis_names`:

  Character vector of names of the axes prior to a modifying operation
  in the owning data variable.

- `new_axes`:

  List of `CFAxis` instances to use for the subsetting.

- `start`:

  The indices to start reading data from the file, as an integer vector
  at least as long as the number of axis for the term.

- `count`:

  The number of values to read from the file, as an integer vector at
  least as long as the number of axis for the term.

- `aux`:

  Optional. List with the parameters for an auxiliary grid
  transformation. Default is `NULL`.

- `ZT_dim`:

  Optional. Dimensions of the non-grid axes when an auxiliary grid
  transformation is specified.

#### Returns

The new parametric term object.
