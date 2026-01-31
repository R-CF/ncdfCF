# Group for CF objects

This class represents a CF group, the object that holds elements like
dimensions and variables of a
[CFDataset](https://r-cf.github.io/ncdfCF/reference/CFDataset.md).

Direct access to groups is usually not necessary. The principal objects
held by the group, CF data variables and axes, are accessible via other
means. Only for access to the group attributes is a reference to a group
required. Changing the properties of a group other than its name may
very well invalidate the CF objects or even the netCDF file.

## Super class

[`ncdfCF::CFObject`](https://r-cf.github.io/ncdfCF/reference/CFObject.md)
-\> `CFGroup`

## Active bindings

- `parent`:

  (read-only) The parent group of the current group, or its owning data
  set for the root node.

- `name`:

  Set or retrieve the name of the group. Note that the name is always
  relative to the location in the hierarchy that the group is in and it
  should thus not be qualified by backslashes. The name has to be a
  valid CF name. The name of the root group cannot be changed.

- `fullname`:

  (read-only) The fully qualified absolute path of the group.

- `root`:

  (read-only) Retrieve the root group.

- `data_set`:

  (read-only) Retrieve the
  [CFDataset](https://r-cf.github.io/ncdfCF/reference/CFDataset.md) that
  the group belongs to. If the group is not attached to a `CFDataset`,
  returns `NULL`.

- `has_subgroups`:

  (read-only) Does the current group have subgroups?

- `subgroups`:

  (read-only) Retrieve the list of the subgroups of the current group.

- `CFobjects`:

  (read-only) Retrieve the list of CF objects of the current group.

## Methods

### Public methods

- [`CFGroup$new()`](#method-CFGroup-new)

- [`CFGroup$print()`](#method-CFGroup-print)

- [`CFGroup$hierarchy()`](#method-CFGroup-hierarchy)

- [`CFGroup$subgroup_names()`](#method-CFGroup-subgroup_names)

- [`CFGroup$create_subgroup()`](#method-CFGroup-create_subgroup)

- [`CFGroup$add_subgroups()`](#method-CFGroup-add_subgroups)

- [`CFGroup$add_CF_object()`](#method-CFGroup-add_CF_object)

- [`CFGroup$objects()`](#method-CFGroup-objects)

- [`CFGroup$find_by_name()`](#method-CFGroup-find_by_name)

- [`CFGroup$add_variable()`](#method-CFGroup-add_variable)

- [`CFGroup$write()`](#method-CFGroup-write)

- [`CFGroup$write_variables()`](#method-CFGroup-write_variables)

Inherited methods

- [`ncdfCF::CFObject$append_attribute()`](https://r-cf.github.io/ncdfCF/reference/CFObject.html#method-append_attribute)
- [`ncdfCF::CFObject$attach_to_group()`](https://r-cf.github.io/ncdfCF/reference/CFObject.html#method-attach_to_group)
- [`ncdfCF::CFObject$attribute()`](https://r-cf.github.io/ncdfCF/reference/CFObject.html#method-attribute)
- [`ncdfCF::CFObject$attributes_identical()`](https://r-cf.github.io/ncdfCF/reference/CFObject.html#method-attributes_identical)
- [`ncdfCF::CFObject$delete_attribute()`](https://r-cf.github.io/ncdfCF/reference/CFObject.html#method-delete_attribute)
- [`ncdfCF::CFObject$detach()`](https://r-cf.github.io/ncdfCF/reference/CFObject.html#method-detach)
- [`ncdfCF::CFObject$print_attributes()`](https://r-cf.github.io/ncdfCF/reference/CFObject.html#method-print_attributes)
- [`ncdfCF::CFObject$set_attribute()`](https://r-cf.github.io/ncdfCF/reference/CFObject.html#method-set_attribute)
- [`ncdfCF::CFObject$write_attributes()`](https://r-cf.github.io/ncdfCF/reference/CFObject.html#method-write_attributes)

------------------------------------------------------------------------

### Method `new()`

Create a new CF group instance.

#### Usage

    CFGroup$new(grp, parent)

#### Arguments

- `grp`:

  Either a [NCGroup](https://r-cf.github.io/ncdfCF/reference/NCGroup.md)
  instance when opening a netCDF resource, or a character string with a
  name for the group when creating a new CF group in memory. When a
  character string, it should be the local name, without any slash "/"
  characters. For the root group, specify an empty string "".

- `parent`:

  The parent group for this group, or a `CFDataset` for the root group.

#### Returns

An instance of this class.

------------------------------------------------------------------------

### Method [`print()`](https://rdrr.io/r/base/print.html)

Summary of the group printed to the console.

#### Usage

    CFGroup$print(stand_alone = TRUE, ...)

#### Arguments

- `stand_alone`:

  Logical to indicate if the group should be printed as an object
  separate from other objects (`TRUE`, default), or print as part of an
  enclosing object (`FALSE`).

- `...`:

  Passed on to other methods.

------------------------------------------------------------------------

### Method `hierarchy()`

Prints the hierarchy of the group and its subgroups to the console, with
a summary of contained objects. Usually called from the root group to
display the full group hierarchy.

#### Usage

    CFGroup$hierarchy(idx = 1L, total = 1L)

#### Arguments

- `idx, total`:

  Arguments to control indentation. Should both be 1 (the default) when
  called interactively. The values will be updated during recursion when
  there are groups below the current group.

------------------------------------------------------------------------

### Method `subgroup_names()`

Retrieve the names of the subgroups of the current group.

#### Usage

    CFGroup$subgroup_names(recursive = TRUE)

#### Arguments

- `recursive`:

  Logical, default is `TRUE`. If `TRUE`, include names of recursively
  through the group hierarchy.

#### Returns

A character vector with the names of the subgroups of the current group.
If `recursive = TRUE`, the names will be fully qualified with their
path.

------------------------------------------------------------------------

### Method `create_subgroup()`

Create a new group as a subgroup of the current group.

#### Usage

    CFGroup$create_subgroup(name)

#### Arguments

- `name`:

  The name of the new subgroup. This must be a valid CF name, so not
  contain any slash '/' characters among other restrictions, and it
  cannot be already present in the group.

#### Returns

The newly created group, or an error.

------------------------------------------------------------------------

### Method `add_subgroups()`

Add subgroups to the current group. These subgroups must be fully
formed, including having set their parent to this group. Use the
`create_subgroup()` method to add a group from scratch.

#### Usage

    CFGroup$add_subgroups(grps)

#### Arguments

- `grps`:

  A `CFGroup`, or `list` thereof.

#### Returns

Self, invisibly.

------------------------------------------------------------------------

### Method `add_CF_object()`

Add one or more CF objects to the current group. This is an internal
method that should not be invoked by the user. The objects to be added
are considered atomic and not assessed for any contained objects. Use a
method like `add_variable()` to add a CF variable to this group as well
as its composing sub-objects such as axes.

#### Usage

    CFGroup$add_CF_object(obj, silent = TRUE)

#### Arguments

- `obj`:

  An instance of a `CFObject` descendant class, or a `list` thereof. If
  it is a `list`, the list elements must be named after the CF object
  they contain.

- `silent`:

  Logical. If `TRUE` (default), CF objects in argument `obj` whose name
  is already present in the list of CF objects *and* whose class is
  identical to the already present object are silently dropped;
  otherwise or when the argument is `FALSE` an error is thrown.

#### Returns

Self, invisibly, or an error.

------------------------------------------------------------------------

### Method [`objects()`](https://rdrr.io/r/base/ls.html)

This method lists the CF objects of a certain class located in this
group, optionally including objects in subgroups.

#### Usage

    CFGroup$objects(cls, recursive = TRUE)

#### Arguments

- `cls`:

  Character vector of classes whose objects to retrieve. Note that
  subclasses are automatically retrieved as well, so specifying
  `cls = "CFAxis"` will retrieve all axes defined in this group.

- `recursive`:

  Should subgroups be scanned for CF objects too (default is `TRUE`)?

#### Returns

A list of
[CFObject](https://r-cf.github.io/ncdfCF/reference/CFObject.md)
instances.

------------------------------------------------------------------------

### Method `find_by_name()`

Find an object by its name. Given the name of an object, possibly
preceded by an absolute or relative group path, return the object to the
caller. Typically, this method is called programmatically; similar
interactive use is provided through the `[[.CFDataset` operator.

#### Usage

    CFGroup$find_by_name(name)

#### Arguments

- `name`:

  The name of an object, with an optional absolute or relative group
  path from the calling group. The object must be an CF construct:
  group, data variable, axis, auxiliary axis, label, grid mapping, etc.

#### Returns

The object with the provided name. If the object is not found, returns
`NULL`.

------------------------------------------------------------------------

### Method `add_variable()`

Add a
[CFVariable](https://r-cf.github.io/ncdfCF/reference/CFVariable.md)
object to the group. If there is another object with the same name in
this group an error is thrown. For associated objects (such as axes,
CRS, boundary variables, etc), if another object with the same name is
otherwise identical to the associated object then that object will be
linked from the variable, otherwise an error is thrown.

#### Usage

    CFGroup$add_variable(var, locations = list())

#### Arguments

- `var`:

  An instance of `CFVariable` or any of its descendants.

- `locations`:

  Optional. A `list` whose named elements correspond to the names of
  objects associated with the variable in argument `var`. Each list
  element has a single character string indicating the group in the
  hierarchy where the object should be stored. As an example, if the
  variable has axes "lon" and "lat" and they should be stored in the
  parent group of this group, then specify
  `locations = list(lon = "..", lat = "..")`. Locations can use absolute
  paths or relative paths from the current group. Associated objects
  that are not in the list will be stored in this group. If the argument
  `locations` is not provided, all associated objects will be stored in
  this group.

#### Returns

Argument `var`, invisibly.

------------------------------------------------------------------------

### Method [`write()`](https://rdrr.io/r/base/write.html)

Write the group to file, including its attributes, if it doesn't already
exist.

#### Usage

    CFGroup$write(recursive = TRUE)

#### Arguments

- `recursive`:

  If `TRUE` (default), write sub-groups as well.

#### Returns

Self, invisibly.

------------------------------------------------------------------------

### Method `write_variables()`

Write data variables in the group to file, including its associated
objects, if it doesn't already exist.

#### Usage

    CFGroup$write_variables(pack = FALSE, recursive = TRUE)

#### Arguments

- `pack`:

  Logical to indicate if the data should be packed. Packing is only
  useful for numeric data; packing is not performed on integer values.
  Packing is always to the "NC_SHORT" data type, i.e. 16-bits per value.

- `recursive`:

  If `TRUE` (default), write data variables in sub-groups as well.

#### Returns

Self, invisibly.
