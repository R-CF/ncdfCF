# NetCDF base object

This class is a basic ancestor to all classes that represent netCDF
objects, specifically groups, dimensions, variables and the user-defined
types in a netCDF file. More useful classes use this class as ancestor.

The fields in this class are common among all netCDF objects. In
addition, this class manages the attributes for its descendent classes.

## Active bindings

- `id`:

  (read-only) Retrieve the identifier of the netCDF object.

- `name`:

  Set or retrieve the name of the NC object. The netCDF file must be
  open for writing to change the name.

- `attributes`:

  (read-only) Read the attributes of the object. When there are no
  attributes, an empty `data.frame` will be returned.

- `CF`:

  Register CF object that uses this netCDF object, or retrieve the list
  of registered CF objects.

## Methods

### Public methods

- [`NCObject$new()`](#method-NCObject-new)

- [`NCObject$print_attributes()`](#method-NCObject-print_attributes)

- [`NCObject$attribute()`](#method-NCObject-attribute)

- [`NCObject$write_attributes()`](#method-NCObject-write_attributes)

- [`NCObject$clone()`](#method-NCObject-clone)

------------------------------------------------------------------------

### Method `new()`

Create a new netCDF object. This class should not be instantiated
directly, create descendant objects instead.

#### Usage

    NCObject$new(id, name, attributes = data.frame())

#### Arguments

- `id`:

  Numeric identifier of the netCDF object.

- `name`:

  Character string with the name of the netCDF object.

- `attributes`:

  Optional, `data.frame` with attributes of the object.

------------------------------------------------------------------------

### Method `print_attributes()`

This function prints the attributes of the netCDF object to the console.

#### Usage

    NCObject$print_attributes(width = 50L)

#### Arguments

- `width`:

  The maximum width of each column in the `data.frame` when printed to
  the console.

------------------------------------------------------------------------

### Method `attribute()`

Retrieve an attribute of a NC object.

#### Usage

    NCObject$attribute(att, field = "value")

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

### Method `write_attributes()`

Write the attributes of this object to a netCDF file. This will retain
existing attributes, update modified attributes, and delete and add
missing attributes from the passed in argument.

#### Usage

    NCObject$write_attributes(nm, new_atts)

#### Arguments

- `nm`:

  The NC variable name or "NC_GLOBAL" to write the attributes to.

- `new_atts`:

  The attributes to write.

#### Returns

Self, invisibly.

------------------------------------------------------------------------

### Method `clone()`

The objects of this class are cloneable with this method.

#### Usage

    NCObject$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.
