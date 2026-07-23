# NetCDF resource object

This class contains the connection details to a netCDF resource.

There is a single instance of this class for every netCDF resource,
owned by the
[CFDataset](https://r-cf.github.io/ncdfCF/reference/CFDataset.md)
instance. The instance is shared by other objects, specifically
[NCGroup](https://r-cf.github.io/ncdfCF/reference/NCGroup.md) instances,
for access to the underlying resource for reading of data.

This class should never have to be accessed directly. All access is
handled by higher-level methods.

## Public fields

- `error`:

  Error message, or empty string.

## Active bindings

- `friendlyClassName`:

  (read-only) A nice description of the class.

- `handle`:

  (read-only) The handle to the netCDF resource.

- `uri`:

  (read-only) The URI of the netCDF resource, either a local filename or
  the location of an online resource.

- `can_write`:

  (read-only) Is the resource writable?

## Methods

### Public methods

- [`NCResource$new()`](#method-NCResource-initialize)

- [`NCResource$print()`](#method-NCResource-print)

- [`NCResource$create()`](#method-NCResource-create)

- [`NCResource$close()`](#method-NCResource-close)

- [`NCResource$group_handle()`](#method-NCResource-group_handle)

- [`NCResource$clone()`](#method-NCResource-clone)

------------------------------------------------------------------------

### `NCResource$new()`

Create a connection to a netCDF resource. This is called by
[`open_ncdf()`](https://r-cf.github.io/ncdfCF/reference/open_ncdf.md)
when opening a netCDF resource or when saving a dataset to file. You
should never have to call this directly.

#### Usage

    NCResource$new(uri, write)

#### Arguments

- `uri`:

  The URI to the netCDF resource.

- `write`:

  Logical flag to indicate if the resource should be read-write.

#### Returns

An instance of this class.

------------------------------------------------------------------------

### `NCResource$print()`

Print a summary of the netCDF resource to the console.

#### Usage

    NCResource$print()

#### Returns

Self, invisibly.

------------------------------------------------------------------------

### `NCResource$create()`

Create a new file on disk for the netCDF resource.

#### Usage

    NCResource$create()

#### Returns

Self, invisibly.

------------------------------------------------------------------------

### `NCResource$close()`

Closing an open netCDF resource. It should rarely be necessary to call
this method directly.

#### Usage

    NCResource$close()

------------------------------------------------------------------------

### `NCResource$group_handle()`

Every group in a netCDF file has its own handle, with the "root" group
having the handle for the entire netCDF resource. The handle returned by
this method is valid only for the named group.

#### Usage

    NCResource$group_handle(group_name)

#### Arguments

- `group_name`:

  The absolute path to the group.

#### Returns

The handle to the group.

------------------------------------------------------------------------

### `NCResource$clone()`

The objects of this class are cloneable with this method.

#### Usage

    NCResource$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.
