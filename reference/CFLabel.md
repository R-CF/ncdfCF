# CF label object

This class represent CF labels, i.e. a variable of character type that
provides a textual label for a discrete or general numeric axis. See
also
[CFAxisCharacter](https://r-cf.github.io/ncdfCF/reference/CFAxisCharacter.md),
which is an axis with character labels.

## Super classes

[`ncdfCF::CFObject`](https://r-cf.github.io/ncdfCF/reference/CFObject.md)
-\>
[`ncdfCF::CFData`](https://r-cf.github.io/ncdfCF/reference/CFData.md)
-\> `CFLabel`

## Active bindings

- `friendlyClassName`:

  (read-only) A nice description of the class.

- `values`:

  Set or retrieve the labels of this object. In general you should use
  the `coordinates` field rather than this one. Upon setting values, if
  there is a linked netCDF resource, this object will be detached from
  it.

- `coordinates`:

  (read-only) Retrieve the labels of this object. Upon retrieval, label
  values are read from the netCDF resource, if there is one, upon first
  access and cached thereafter.

- `length`:

  (read-only) The number of labels in the set.

- `dimid`:

  The netCDF dimension id of this label set. Setting this value to
  anything other than the correct value will lead to disaster.

## Methods

### Public methods

- [`CFLabel$new()`](#method-CFLabel-new)

- [`CFLabel$print()`](#method-CFLabel-print)

- [`CFLabel$identical()`](#method-CFLabel-identical)

- [`CFLabel$copy()`](#method-CFLabel-copy)

- [`CFLabel$slice()`](#method-CFLabel-slice)

- [`CFLabel$subset()`](#method-CFLabel-subset)

- [`CFLabel$write()`](#method-CFLabel-write)

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

### Method [`new()`](https://rdrr.io/r/methods/new.html)

Create a new instance of this class.

#### Usage

    CFLabel$new(var, group, values = NA, start = NA, count = NA)

#### Arguments

- `var`:

  The
  [NCVariable](https://r-cf.github.io/ncdfCF/reference/NCVariable.md)
  instance upon which this CF object is based when read from a netCDF
  resource, or the name for the object new CF object to be created.

- `group`:

  The [CFGroup](https://r-cf.github.io/ncdfCF/reference/CFGroup.md) that
  this instance will live in.

- `values`:

  Optional. The labels of the CF object. Ignored when argument `var` is
  a `NCVariable` object.

- `start`:

  Optional. Integer index value indicating where to start reading data
  from the file. The value may be `NA` (default) to read all data,
  otherwise it must not be larger than the number of labels. Ignored
  when argument `var` is not an `NCVariable` instance.

- `count`:

  Optional. Integer value indicating the number of labels to read from
  file. The value may be `NA` to read to the end of the labels,
  otherwise its value must agree with the corresponding `start` value
  and the number of labels on file. Ignored when argument `var` is not
  an `NCVariable` instance.

#### Returns

A `CFLabel` instance.

------------------------------------------------------------------------

### Method [`print()`](https://rdrr.io/r/base/print.html)

Prints a summary of the labels to the console.

#### Usage

    CFLabel$print(...)

#### Arguments

- `...`:

  Arguments passed on to other functions. Of particular interest is
  `width = ` to indicate a maximum width of attribute columns.

------------------------------------------------------------------------

### Method [`identical()`](https://rspatial.github.io/terra/reference/identical.html)

Tests if the object passed to this method is identical to `self`.

#### Usage

    CFLabel$identical(lbl)

#### Arguments

- `lbl`:

  The `CFLabel` instance to test.

#### Returns

`TRUE` if the two label sets are identical, `FALSE` if not.

------------------------------------------------------------------------

### Method [`copy()`](https://rdrr.io/pkg/data.table/man/copy.html)

Create a copy of this label set. The copy is completely separate from
`self`, meaning that both `self` and all of its components are made from
new instances.

#### Usage

    CFLabel$copy(name = "", group)

#### Arguments

- `name`:

  The name for the new label set. If an empty string is passed, will use
  the name of this label set.

- `group`:

  The [CFGroup](https://r-cf.github.io/ncdfCF/reference/CFGroup.md)
  where the copy of this axis will live.

#### Returns

The newly created label set.

------------------------------------------------------------------------

### Method `slice()`

Given a range of domain coordinate values, returns the indices into the
axis that fall within the supplied range.

#### Usage

    CFLabel$slice(rng)

#### Arguments

- `rng`:

  A character vector whose extreme (alphabetic) values indicate the
  indices of coordinates to return.

#### Returns

An integer vector of length 2 with the lower and higher indices into the
axis that fall within the range of coordinates in argument `rng`.
Returns `NULL` if no values of the axis fall within the range of
coordinates.

------------------------------------------------------------------------

### Method [`subset()`](https://rspatial.github.io/terra/reference/subset.html)

Retrieve a subset of the labels.

#### Usage

    CFLabel$subset(name, group, rng)

#### Arguments

- `name`:

  The name for the new label set, optional.

- `group`:

  The [CFGroup](https://r-cf.github.io/ncdfCF/reference/CFGroup.md)
  where the copy of this label set will live.

- `rng`:

  The range of indices whose values from this axis to include in the
  returned axis.

#### Returns

A `CFLabel` instance, or `NULL` if the `rng` values are invalid.

------------------------------------------------------------------------

### Method [`write()`](https://rdrr.io/r/base/write.html)

Write the labels to a netCDF file, including its attributes.

#### Usage

    CFLabel$write()

#### Returns

Self, invisibly.
