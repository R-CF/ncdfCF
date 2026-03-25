# CF data object

This class is a basic ancestor to all classes that contain data from a
netCDF resource, specifically data variables and axes. More useful
classes use this class as ancestor.

## Super class

[`ncdfCF::CFObject`](https://r-cf.github.io/ncdfCF/reference/CFObject.md)
-\> `CFData`

## Active bindings

- `data_type`:

  Set or retrieve the data type of the data in the object. Setting the
  data type to a wrong value can have unpredictable and mostly
  catastrophic consequences.

- `ndims`:

  (read-only) Retrieve the dimensionality of the data in the array.

- `NC_map`:

  Returns a list with columns "start" and "count" giving the indices for
  reading the data of this object from a netCDF resource. The list is
  empty if this object is not backed by a netCDF resource.

## Methods

### Public methods

- [`CFData$new()`](#method-CFData-new)

- [`CFData$detach()`](#method-CFData-detach)

- [`CFData$dim()`](#method-CFData-dim)

- [`CFData$read_data()`](#method-CFData-read_data)

- [`CFData$read_chunk()`](#method-CFData-read_chunk)

Inherited methods

- [`ncdfCF::CFObject$append_attribute()`](https://r-cf.github.io/ncdfCF/reference/CFObject.html#method-append_attribute)
- [`ncdfCF::CFObject$attach_to_group()`](https://r-cf.github.io/ncdfCF/reference/CFObject.html#method-attach_to_group)
- [`ncdfCF::CFObject$attribute()`](https://r-cf.github.io/ncdfCF/reference/CFObject.html#method-attribute)
- [`ncdfCF::CFObject$attributes_identical()`](https://r-cf.github.io/ncdfCF/reference/CFObject.html#method-attributes_identical)
- [`ncdfCF::CFObject$delete_attribute()`](https://r-cf.github.io/ncdfCF/reference/CFObject.html#method-delete_attribute)
- [`ncdfCF::CFObject$print_attributes()`](https://r-cf.github.io/ncdfCF/reference/CFObject.html#method-print_attributes)
- [`ncdfCF::CFObject$set_attribute()`](https://r-cf.github.io/ncdfCF/reference/CFObject.html#method-set_attribute)
- [`ncdfCF::CFObject$write_attributes()`](https://r-cf.github.io/ncdfCF/reference/CFObject.html#method-write_attributes)

------------------------------------------------------------------------

### Method `new()`

Create a new `CFData` instance. This method is called upon creating CF
objects, such as when opening a netCDF resource or creating a new CF
object. It is rarely, if ever, useful to call this constructor directly.
Instead, use the methods from higher-level classes such as
[CFVariable](https://r-cf.github.io/ncdfCF/reference/CFVariable.md).

#### Usage

    CFData$new(
      obj,
      group,
      values,
      start = 1L,
      count = NA,
      attributes = data.frame()
    )

#### Arguments

- `obj`:

  The
  [NCVariable](https://r-cf.github.io/ncdfCF/reference/NCVariable.md)
  instance upon which this CF object is based when read from a netCDF
  resource, or the name for the new CF object to be created.

- `group`:

  The [CFGroup](https://r-cf.github.io/ncdfCF/reference/CFGroup.md) that
  this instance will live in.

- `values`:

  Optional. The values of the object in an array. Ignored when argument
  `obj` is an `NCVariable` instance.

- `start`:

  Optional. Vector of indices where to start reading data along the
  dimensions of the array on file. The vector must be `NA` to read all
  data, otherwise it must have agree with the dimensions of the array on
  file. Default value is `1`, i.e. start from the beginning of the 1-D
  NC variable. Ignored when argument `obj` is not an `NCVariable`
  instance.

- `count`:

  Optional. Vector of number of elements to read along each dimension of
  the array on file. The vector must be `NA` to read to the end of each
  dimension, otherwise its value must agree with the corresponding
  `start` value and the dimension of the array on file. Default is `NA`.
  Ignored when argument `obj` is not an `NCVariable` instance.

- `attributes`:

  Optional. A `data.frame` with the attributes of the object.

#### Returns

A `CFData` instance.

------------------------------------------------------------------------

### Method [`detach()`](https://rdrr.io/r/base/detach.html)

Detach the current object from its underlying netCDF resource. If
necessary, data is read from the resource before detaching.

#### Usage

    CFData$detach()

------------------------------------------------------------------------

### Method [`dim()`](https://rdrr.io/r/base/dim.html)

Retrieve the dimensions of the data of this object.

#### Usage

    CFData$dim(dimension)

#### Arguments

- `dimension`:

  Optional. The index of the dimension to retrieve the length for. If
  omitted, retrieve the lengths of all dimensions.

#### Returns

Integer vector with the length of each requested dimension. Read the
data of the CF object from the NC file. The data is cached by `self` so
repeated calls do not access the netCDF resource, unless argument
`refresh` is `TRUE`. This method will not assess how big the data is
before reading it so there is a chance that memory will be exhausted.
The calling code should check for this possibility and break up the
reading of data into chunks.

------------------------------------------------------------------------

### Method `read_data()`

#### Usage

    CFData$read_data(refresh = FALSE)

#### Arguments

- `refresh`:

  Should the data be read from file if the object is linked? This will
  replace current values, if previously loaded. Default `FALSE`.

#### Returns

An array of data, invisibly, as prescribed by the `start` and `count`
values used to create this object. If the object is not backed by a
netCDF resource, returns `NULL`. Read a chunk of raw data of the CF
object, as defined by the `start` and `count` vectors. Note that these
vectors are relative to any subset of the data variable that this CF
object refers to. The data read by this method will not be stored in
`self` so the calling code must take a reference to it.

------------------------------------------------------------------------

### Method `read_chunk()`

#### Usage

    CFData$read_chunk(start, count)

#### Arguments

- `start`:

  Vector of indices where to start reading data along the dimensions of
  the array. The vector must be `NA` to read all data, otherwise it must
  agree with the dimensions of the array.

- `count`:

  Vector of number of elements to read along each dimension of the
  array. The vector must be `NA` to read to the end of each dimension,
  otherwise its value must agree with the corresponding `start` value
  and the dimension of the array.

#### Returns

An array of data, as prescribed by the `start` and `count` arguments, or
`NULL` if there is no data.
