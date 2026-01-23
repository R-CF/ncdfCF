# CF base object

This class is a basic ancestor to all classes that represent CF objects.
More useful classes use this class as ancestor.

## Active bindings

- `friendlyClassName`:

  (read-only) A nice description of the class.

- `id`:

  (read-only) Retrieve the identifier of the CF object.

- `name`:

  Set or retrieve the name of the CF object. The name must be a valid
  netCDF name: start with a character, use only characters, numbers and
  the underscore, and be at most 255 characters long.

- `fullname`:

  (read-only) The fully-qualified name of the CF object.

- `group`:

  Set or retrieve the
  [CFGroup](https://r-cf.github.io/ncdfCF/reference/CFGroup.md) that
  this object is located in, possibly `NULL`.

- `attributes`:

  (read-only) Retrieve a `data.frame` with the attributes of the CF
  object.

- `has_resource`:

  (read-only) Flag that indicates if this object has an underlying
  netCDF resource.

- `NC`:

  (read-only) The NC object that links to an underlying netCDF resource,
  or `NULL` if not linked.

- `is_dirty`:

  Flag to indicate if the object has any unsaved changes.

## Methods

### Public methods

- [`CFObject$new()`](#method-CFObject-new)

- [`CFObject$attach_to_group()`](#method-CFObject-attach_to_group)

- [`CFObject$detach()`](#method-CFObject-detach)

- [`CFObject$attribute()`](#method-CFObject-attribute)

- [`CFObject$print_attributes()`](#method-CFObject-print_attributes)

- [`CFObject$set_attribute()`](#method-CFObject-set_attribute)

- [`CFObject$attributes_identical()`](#method-CFObject-attributes_identical)

- [`CFObject$append_attribute()`](#method-CFObject-append_attribute)

- [`CFObject$delete_attribute()`](#method-CFObject-delete_attribute)

- [`CFObject$write_attributes()`](#method-CFObject-write_attributes)

- [`CFObject$clone()`](#method-CFObject-clone)

------------------------------------------------------------------------

### Method [`new()`](https://rdrr.io/r/methods/new.html)

Create a new `CFobject` instance in memory or from an object in a netCDF
resource when this method is called upon opening a netCDF resource. It
is rarely, if ever, useful to call this constructor directly. Instead,
use the methods from higher-level classes such as
[CFVariable](https://r-cf.github.io/ncdfCF/reference/CFVariable.md).

#### Usage

    CFObject$new(obj, attributes = data.frame(), group = NULL)

#### Arguments

- `obj`:

  The [NCObject](https://r-cf.github.io/ncdfCF/reference/NCObject.md)
  instance upon which this CF object is based when read from a netCDF
  resource, or the name for the new CF object to be created.

- `attributes`:

  Optional. A `data.frame` with the attributes of the object. When
  argument `obj` is an `NCGroup` instance and this argument is an empty
  `data.frame` (default), arguments will be read from the resource.

- `group`:

  The [CFGroup](https://r-cf.github.io/ncdfCF/reference/CFGroup.md)
  instance that this object will live in. The default is `NULL` but this
  is only useful for `CFGroup` instance.

#### Returns

A `CFObject` instance.

------------------------------------------------------------------------

### Method `attach_to_group()`

Attach this CF object to a group. If there is another object with the
same name in this group an error is thrown. This is the basic method
that may be overridden by descendant classes.

#### Usage

    CFObject$attach_to_group(grp, locations = list())

#### Arguments

- `grp`:

  An instance of
  [CFGroup](https://r-cf.github.io/ncdfCF/reference/CFGroup.md).

- `locations`:

  Optional. A `list` whose named elements correspond to the names of
  objects, possibly including this object. Each list element has a
  single character string indicating the group in the hierarchy where
  the object should be stored. As an example, if a data variable has
  axes "lon" and "lat" and they should be stored in the parent group of
  `grp`, then specify `locations = list(lon = "..", lat = "..")`.
  Locations can use absolute paths or relative paths from group `grp`.
  If the argument `locations` is not provided or the name of the object
  is not in the list, the object will be stored in group `grp`.

#### Returns

Self, invisibly.

------------------------------------------------------------------------

### Method [`detach()`](https://rdrr.io/r/base/detach.html)

Detach the current object from its underlying netCDF resource.

#### Usage

    CFObject$detach()

------------------------------------------------------------------------

### Method `attribute()`

Retrieve an attribute of a CF object.

#### Usage

    CFObject$attribute(att, field = "value")

#### Arguments

- `att`:

  Single character string of attribute to return.

- `field`:

  The field of the attribute to return values from. This must be "value"
  (default) or "type".

#### Returns

If the `field` argument is "type", a character string. If `field` is
"value", a single value of the type of the attribute, or a vector when
the attribute has multiple values. If no attribute is named with a value
of argument `att` `NA` is returned.

------------------------------------------------------------------------

### Method `print_attributes()`

Print the attributes of the CF object to the console.

#### Usage

    CFObject$print_attributes(width = 30L)

#### Arguments

- `width`:

  The maximum width of each column in the `data.frame` when printed to
  the console.

------------------------------------------------------------------------

### Method `set_attribute()`

Add an attribute. If an attribute `name` already exists, it will be
overwritten.

#### Usage

    CFObject$set_attribute(name, type, value)

#### Arguments

- `name`:

  The name of the attribute. The name must begin with a letter and be
  composed of letters, digits, and underscores, with a maximum length of
  255 characters. UTF-8 characters are not supported in attribute names.

- `type`:

  The type of the attribute, as a string value of a netCDF data type.

- `value`:

  The value of the attribute. This can be of any supported type,
  including a vector or list of values. Matrices, arrays and like
  compound data structures should be stored as a data variable, not as
  an attribute and they are thus not allowed. In general, an attribute
  should be a character value, a numeric value, a logical value, or a
  short vector or list of any of these. Values passed in a list will be
  coerced to their common mode.

#### Returns

Self, invisibly.

------------------------------------------------------------------------

### Method `attributes_identical()`

Test if the supplied attributes are identical to the attributes of this
instance. The order of the attributes may differ but the names, types
and values must coincide.

#### Usage

    CFObject$attributes_identical(cmp)

#### Arguments

- `cmp`:

  `data.frame` with attributes to compare to the attributes of this
  instance.

#### Returns

`TRUE` if attributes in argument `cmp` are identical to the attributes
of this instance, `FALSE` otherwise.

------------------------------------------------------------------------

### Method `append_attribute()`

Append the text value of an attribute. If an attribute `name` already
exists, the `value` will be appended to the existing value of the
attribute. If the attribute `name` does not exist it will be created.
The attribute must be of "NC_CHAR" or "NC_STRING" type; in the latter
case having only a single string value.

#### Usage

    CFObject$append_attribute(name, value, sep = "; ", prepend = FALSE)

#### Arguments

- `name`:

  The name of the attribute. The name must begin with a letter and be
  composed of letters, digits, and underscores, with a maximum length of
  255 characters. UTF-8 characters are not supported in attribute names.

- `value`:

  The character value of the attribute to append. This must be a
  character string.

- `sep`:

  The separator to use. Default is `"; "`.

- `prepend`:

  Logical to flag if the supplied `value` should be placed before the
  existing value. Default is `FALSE`.

#### Returns

Self, invisibly.

------------------------------------------------------------------------

### Method `delete_attribute()`

Delete attributes. If an attribute `name` is not present this method
simply returns.

#### Usage

    CFObject$delete_attribute(name)

#### Arguments

- `name`:

  Vector of names of the attributes to delete.

#### Returns

Self, invisibly.

------------------------------------------------------------------------

### Method `write_attributes()`

Write the attributes of this object to a netCDF file.

#### Usage

    CFObject$write_attributes()

#### Returns

Self, invisibly.

------------------------------------------------------------------------

### Method `clone()`

The objects of this class are cloneable with this method.

#### Usage

    CFObject$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.
