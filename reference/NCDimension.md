# NetCDF dimension object

This class represents an netCDF dimension. It contains the information
on a dimension that is stored in an netCDF file. Consequently, the
properties of this class are all read-only. The length of the dimension
may change if data is written to an unlimited dimension, but that is
managed internally.

This class is not very useful for interactive use. Use the
[CFAxis](https://r-cf.github.io/ncdfCF/reference/CFAxis.md) descendent
classes instead.

## Super class

[`ncdfCF::NCObject`](https://r-cf.github.io/ncdfCF/reference/NCObject.md)
-\> `NCDimension`

## Active bindings

- `length`:

  (read-only) The length of the dimension. If field `unlim = TRUE`, this
  field indicates the length of the data in this dimension written to
  file.

- `unlim`:

  (read-only) Logical flag to indicate if the dimension is unlimited,
  i.e. that additional data may be written to file incrementing this
  dimension.

## Methods

### Public methods

- [`NCDimension$new()`](#method-NCDimension-new)

- [`NCDimension$print()`](#method-NCDimension-print)

- [`NCDimension$write()`](#method-NCDimension-write)

- [`NCDimension$clone()`](#method-NCDimension-clone)

Inherited methods

- [`ncdfCF::NCObject$attribute()`](https://r-cf.github.io/ncdfCF/reference/NCObject.html#method-attribute)
- [`ncdfCF::NCObject$print_attributes()`](https://r-cf.github.io/ncdfCF/reference/NCObject.html#method-print_attributes)
- [`ncdfCF::NCObject$write_attributes()`](https://r-cf.github.io/ncdfCF/reference/NCObject.html#method-write_attributes)

------------------------------------------------------------------------

### Method [`new()`](https://rdrr.io/r/methods/new.html)

Create a new netCDF dimension. This class should not be instantiated
directly, create CF objects instead. This class is instantiated when
opening a netCDF resource.

#### Usage

    NCDimension$new(id, name, length = 1L, unlim = FALSE, group)

#### Arguments

- `id`:

  Numeric identifier of the netCDF dimension.

- `name`:

  Character string with the name of the netCDF dimension.

- `length`:

  Length of the dimension. Default is 1.

- `unlim`:

  Is the dimension unlimited? Default is `FALSE`.

- `group`:

  The NC group where the dimension is located.

#### Returns

A `NCDimension` instance.

------------------------------------------------------------------------

### Method [`print()`](https://rdrr.io/r/base/print.html)

Summary of the NC dimension printed to the console.

#### Usage

    NCDimension$print(...)

#### Arguments

- `...`:

  Passed on to other methods.

------------------------------------------------------------------------

### Method [`write()`](https://rdrr.io/r/base/write.html)

Write the dimension to a netCDF file.

#### Usage

    NCDimension$write(h)

#### Arguments

- `h`:

  The handle to the netCDF file to write.

#### Returns

Self, invisibly.

------------------------------------------------------------------------

### Method `clone()`

The objects of this class are cloneable with this method.

#### Usage

    NCDimension$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.
