# NetCDF variable

This class represents a netCDF variable, the object that holds the
properties and data of elements like dimensions and variables of a
netCDF file.

Direct access to netCDF variables is usually not necessary. NetCDF
variables are linked from CF data variables and axes and all relevant
properties are thus made accessible.

## Super class

[`ncdfCF::NCObject`](https://r-cf.github.io/ncdfCF/reference/NCObject.md)
-\> `NCVariable`

## Active bindings

- `group`:

  (read-only) NetCDF group where this variable is located.

- `handle`:

  (read-only) Get the handle to the netCDF resource for the variable.

- `vtype`:

  (read-only) The netCDF data type of this variable. This could be the
  packed type. Don't check this field but use the appropriate method in
  the class of the object whose data type you are looking for.

- `ndims`:

  (read-only) Number of dimensions that this variable uses.

- `dimids`:

  (read-only) Vector of dimension identifiers that this NCVariable uses.

- `netcdf4`:

  (read-only) Additional properties for a `netcdf4` resource.

- `CF`:

  Register CF objects that use this netCDF variable, or retrieve the
  list of registered CF objects.

- `fullname`:

  (read-only) Name of this netCDF variable including the group path from
  the root group.

- `is_packed`:

  (read-only) Flag that indicates if the data on file is packed.

## Methods

### Public methods

- [`NCVariable$new()`](#method-NCVariable-new)

- [`NCVariable$print()`](#method-NCVariable-print)

- [`NCVariable$shard()`](#method-NCVariable-shard)

- [`NCVariable$detach()`](#method-NCVariable-detach)

- [`NCVariable$get_data()`](#method-NCVariable-get_data)

- [`NCVariable$write_data()`](#method-NCVariable-write_data)

- [`NCVariable$set_name()`](#method-NCVariable-set_name)

- [`NCVariable$dimension()`](#method-NCVariable-dimension)

- [`NCVariable$dim()`](#method-NCVariable-dim)

- [`NCVariable$clone()`](#method-NCVariable-clone)

Inherited methods

- [`ncdfCF::NCObject$attribute()`](https://r-cf.github.io/ncdfCF/reference/NCObject.html#method-attribute)
- [`ncdfCF::NCObject$print_attributes()`](https://r-cf.github.io/ncdfCF/reference/NCObject.html#method-print_attributes)
- [`ncdfCF::NCObject$write_attributes()`](https://r-cf.github.io/ncdfCF/reference/NCObject.html#method-write_attributes)

------------------------------------------------------------------------

### Method `new()`

Create a new netCDF variable. This class should not be instantiated
directly, they are created automatically when opening a netCDF resource.

#### Usage

    NCVariable$new(
      id,
      name,
      group,
      vtype,
      dimids,
      attributes = data.frame(),
      netcdf4 = list()
    )

#### Arguments

- `id`:

  Numeric identifier of the netCDF object.

- `name`:

  Character string with the name of the netCDF object.

- `group`:

  The [NCGroup](https://r-cf.github.io/ncdfCF/reference/NCGroup.md) this
  variable is located in.

- `vtype`:

  The netCDF data type of the variable.

- `dimids`:

  The identifiers of the dimensions this variable uses.

- `attributes`:

  Optional, `data.frame` with the attributes of the object.

- `netcdf4`:

  Optional, `netcdf4`-specific arguments in the format of RNetCDF.

#### Returns

An instance of this class.

------------------------------------------------------------------------

### Method [`print()`](https://rdrr.io/r/base/print.html)

Summary of the NC variable printed to the console.

#### Usage

    NCVariable$print(...)

#### Arguments

- `...`:

  Passed on to other methods.

------------------------------------------------------------------------

### Method `shard()`

Very concise information on the variable. The information returned by
this function is very concise and most useful when combined with similar
information from other variables.

#### Usage

    NCVariable$shard()

#### Returns

Character string with very basic variable information.

------------------------------------------------------------------------

### Method [`detach()`](https://rdrr.io/r/base/detach.html)

Detach the passed object from this NC variable.

#### Usage

    NCVariable$detach(obj)

#### Arguments

- `obj`:

  The CFObject instance to detach from this NC variable.

#### Returns

`obj`, invisibly.

------------------------------------------------------------------------

### Method `get_data()`

Read (a chunk of) data from the netCDF file. Degenerate dimensions are
maintained and data is always returned in its smallest type.

#### Usage

    NCVariable$get_data(start = NA, count = NA)

#### Arguments

- `start`:

  A vector of indices specifying the element where reading starts along
  each dimension of the data. When `NA`, all data are read from the
  start of the array.

- `count`:

  An integer vector specifying the number of values to read along each
  dimension of the data. Any `NA` value in vector count indicates that
  the corresponding dimension should be read from the start index to the
  end of the dimension.

#### Returns

An array, matrix or vector with the requested data, or an error object.

------------------------------------------------------------------------

### Method `write_data()`

Write (a chunk of) data to the netCDF file.

#### Usage

    NCVariable$write_data(d, start = NA, count = NA, ...)

#### Arguments

- `d`:

  The data to write. This must have appropriate dimensions.

- `start`:

  A vector of indices specifying the element where writing starts along
  each dimension of the data. When `NA`, all data are written from the
  start of the array.

- `count`:

  An integer vector specifying the number of values to write along each
  dimension of the data. Any `NA` value in vector count indicates that
  the corresponding dimension should be written from the start index to
  the end of the dimension.

- `...`:

  Other parameters passed on to
  [`RNetCDF::var.put.nc()`](https://rdrr.io/pkg/RNetCDF/man/var.put.nc.html).

#### Returns

Self, invisibly.

------------------------------------------------------------------------

### Method `set_name()`

Change the name of the NC variable. The new name must be valid in the
indicated group, it can not already exist in the group. The netCDF file
must be open for writing to change the name.

#### Usage

    NCVariable$set_name(new_name)

#### Arguments

- `new_name`:

  The new name for the NC variable

#### Returns

Self, invisibly.

------------------------------------------------------------------------

### Method `dimension()`

Get the
[NCDimension](https://r-cf.github.io/ncdfCF/reference/NCDimension.md)
object(s) that this variable uses.

#### Usage

    NCVariable$dimension(id)

#### Arguments

- `id`:

  The index of the dimension. If missing, all dimensions of this
  variable are returned.

#### Returns

A `NCDimension` object or a list thereof. If no `NCDimension`s were
found, return `NULL`.

------------------------------------------------------------------------

### Method [`dim()`](https://rdrr.io/r/base/dim.html)

The lengths of the data dimensions of this object.

#### Usage

    NCVariable$dim(dimension)

#### Arguments

- `dimension`:

  Optional. The index of the dimension to retrieve the length for. If
  omitted, retrieve the lengths of all dimensions.

------------------------------------------------------------------------

### Method `clone()`

The objects of this class are cloneable with this method.

#### Usage

    NCVariable$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.
