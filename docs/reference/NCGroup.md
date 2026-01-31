# NetCDF group

This class represents a netCDF group, the object that holds elements
like dimensions and variables of a netCDF file.

Direct access to groups is usually not necessary. The principal objects
of interest, CF data variables and axes, are accessible via
[CFGroup](https://r-cf.github.io/ncdfCF/reference/CFGroup.md). Changing
the properties of a netCDF group other than its name may very well
invalidate the CF objects or even the netCDF file.

## Super class

[`ncdfCF::NCObject`](https://r-cf.github.io/ncdfCF/reference/NCObject.md)
-\> `NCGroup`

## Public fields

- `parent`:

  Parent group of this group, the owning `CFDataset` for the root group.

- `subgroups`:

  List of child `NCGroup` instances of this group.

- `NCvars`:

  List of netCDF variables that are located in this group.

- `NCdims`:

  List of netCDF dimensions that are located in this group.

- `NCudts`:

  List of netCDF user-defined types that are located in this group.

## Active bindings

- `friendlyClassName`:

  (read-only) A nice description of the class.

- `resource`:

  (read-only) The RNetCDF object to the underlying netCDF resource.

- `handle`:

  (read-only) Get the handle to the netCDF resource for the group

- `can_write`:

  (read-only) Is the resource writable?

- `name`:

  Set or retrieve the name of the group. Note that the name is always
  relative to the location in the hierarchy that the group is in and it
  should thus not be qualified by backslashes. The name has to be a
  valid CF name. The name of the root group cannot be changed.

- `fullname`:

  (read-only) The fully qualified absolute path of the group.

- `root`:

  (read-only) Retrieve the root group.

- `CF`:

  Set or retrieve the
  [CFGroup](https://r-cf.github.io/ncdfCF/reference/CFGroup.md) that is
  associated with this NC group.

## Methods

### Public methods

- [`NCGroup$new()`](#method-NCGroup-new)

- [`NCGroup$print()`](#method-NCGroup-print)

- [`NCGroup$find_by_name()`](#method-NCGroup-find_by_name)

- [`NCGroup$find_dim_by_id()`](#method-NCGroup-find_dim_by_id)

- [`NCGroup$has_name()`](#method-NCGroup-has_name)

- [`NCGroup$set_name()`](#method-NCGroup-set_name)

- [`NCGroup$unused()`](#method-NCGroup-unused)

- [`NCGroup$create_group()`](#method-NCGroup-create_group)

- [`NCGroup$append()`](#method-NCGroup-append)

- [`NCGroup$fullnames()`](#method-NCGroup-fullnames)

- [`NCGroup$dimensions()`](#method-NCGroup-dimensions)

- [`NCGroup$clone()`](#method-NCGroup-clone)

Inherited methods

- [`ncdfCF::NCObject$attribute()`](https://r-cf.github.io/ncdfCF/reference/NCObject.html#method-attribute)
- [`ncdfCF::NCObject$print_attributes()`](https://r-cf.github.io/ncdfCF/reference/NCObject.html#method-print_attributes)
- [`ncdfCF::NCObject$write_attributes()`](https://r-cf.github.io/ncdfCF/reference/NCObject.html#method-write_attributes)

------------------------------------------------------------------------

### Method `new()`

Create a new instance of this class.

#### Usage

    NCGroup$new(id, name, attributes = data.frame(), parent, resource)

#### Arguments

- `id`:

  The identifier of the group. If `NA`, the new group will be created in
  the netCDF resource, unless argument `parent == NULL`, i.e. the root
  group which already exists.

- `name`:

  The name of the group.

- `attributes`:

  Optional, a `data.frame` with group attributes.

- `parent`:

  The parent group of this group. If `NULL` then argument `resource`
  must be a valid instance of `NCResource`.

- `resource`:

  Optional. Reference to the
  [NCResource](https://r-cf.github.io/ncdfCF/reference/NCResource.md)
  instance that provides access to the netCDF resource.

#### Returns

An instance of this class.

------------------------------------------------------------------------

### Method [`print()`](https://rdrr.io/r/base/print.html)

Summary of the group printed to the console.

#### Usage

    NCGroup$print(stand_alone = TRUE, ...)

#### Arguments

- `stand_alone`:

  Logical to indicate if the group should be printed as an object
  separate from other objects (`TRUE`, default), or print as part of an
  enclosing object (`FALSE`).

- `...`:

  Passed on to other methods.

------------------------------------------------------------------------

### Method `find_by_name()`

Find an object by its name. Given the name of an object, possibly
preceded by an absolute or relative group path, return the object to the
caller. Usually this method is called programmatically.

#### Usage

    NCGroup$find_by_name(name)

#### Arguments

- `name`:

  The name of an object, with an optional absolute or relative group
  path from the calling group. The object must be an NC group, dimension
  or variable.

#### Returns

The object with the provided name. If the object is not found, returns
`NULL`.

------------------------------------------------------------------------

### Method `find_dim_by_id()`

Find an NC dimension object by its id. Given the id of a dimension,
return the
[NCDimension](https://r-cf.github.io/ncdfCF/reference/NCDimension.md)
object to the caller. The dimension has to be found in the current group
or any of its parents.

#### Usage

    NCGroup$find_dim_by_id(id)

#### Arguments

- `id`:

  The id of the dimension.

#### Returns

The
[NCDimension](https://r-cf.github.io/ncdfCF/reference/NCDimension.md)
object with an identifier equal to the `id` argument. If the object is
not found, returns `NULL`.

------------------------------------------------------------------------

### Method `has_name()`

Has a given name been defined in this group already?

#### Usage

    NCGroup$has_name(name)

#### Arguments

- `name`:

  Character string. The name will be searched for, regardless of case.

#### Returns

`TRUE` if `name` is present in the group, `FALSE` otherwise.

------------------------------------------------------------------------

### Method `set_name()`

Change the name of the NC group. The new name must be valid and should
not duplicate a sibling group.

#### Usage

    NCGroup$set_name(new_name)

#### Arguments

- `new_name`:

  The new name for the NC group.

#### Returns

Self, invisibly.

------------------------------------------------------------------------

### Method `unused()`

Find NC variables that are not referenced by CF objects. For debugging
purposes only.

#### Usage

    NCGroup$unused()

#### Returns

List of
[NCVariable](https://r-cf.github.io/ncdfCF/reference/NCVariable.md).

------------------------------------------------------------------------

### Method `create_group()`

Create a new group as a sub-group of the current group. This writes the
new group to the netCDF resource, but only if it is open for writing.

#### Usage

    NCGroup$create_group(CFgroup)

#### Arguments

- `CFgroup`:

  The [CFGroup](https://r-cf.github.io/ncdfCF/reference/CFGroup.md)
  associated with this NC group.

#### Returns

The newly created group as a `NCGroup` instance, invisibly.

------------------------------------------------------------------------

### Method [`append()`](https://rdrr.io/r/base/append.html)

Append an object to this group.

#### Usage

    NCGroup$append(obj)

#### Arguments

- `obj`:

  The object to append. This must be an `NCVariable` or `NCDimension`
  instance. Any other type of object will generate a warning.

#### Returns

Self, invisible.

------------------------------------------------------------------------

### Method `fullnames()`

This method lists the fully qualified name of this group, optionally
including names in subgroups.

#### Usage

    NCGroup$fullnames(recursive = TRUE)

#### Arguments

- `recursive`:

  Should subgroups be scanned for names too (default is `TRUE`)?

#### Returns

A character vector with group names.

------------------------------------------------------------------------

### Method `dimensions()`

List all the dimensions that are visible from this group, possibly
including those that are defined in parent groups (by names not defined
by any of their child groups in direct lineage to the current group).

#### Usage

    NCGroup$dimensions(scope = "all")

#### Arguments

- `scope`:

  Character string that indicates if only dimensions in the current
  group should be reported (`local`) or visible dimensions in parent
  groups as well (`all`, default).

#### Returns

A vector of
[NCDimension](https://r-cf.github.io/ncdfCF/reference/NCDimension.md)
objects.

------------------------------------------------------------------------

### Method `clone()`

The objects of this class are cloneable with this method.

#### Usage

    NCGroup$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.
