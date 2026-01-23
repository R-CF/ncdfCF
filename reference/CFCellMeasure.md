# CF cell measure variable

This class represents a CF cell measure variable, the object that
indicates the area or volume of every grid cell in referencing data
variables.

If a cell measure variable is external to the current file, an instance
will still be created for it, but the user must link the external file
to this instance before it can be used in analysis.

## Active bindings

- `measure`:

  (read-only) Retrieve the measure of this instance. Either "area" or
  "volume".

- `name`:

  The name of this instance, which must refer to a NC variable or an
  external variable.

## Methods

### Public methods

- [`CFCellMeasure$new()`](#method-CFCellMeasure-new)

- [`CFCellMeasure$print()`](#method-CFCellMeasure-print)

- [`CFCellMeasure$data()`](#method-CFCellMeasure-data)

- [`CFCellMeasure$register()`](#method-CFCellMeasure-register)

- [`CFCellMeasure$link()`](#method-CFCellMeasure-link)

- [`CFCellMeasure$detach()`](#method-CFCellMeasure-detach)

- [`CFCellMeasure$clone()`](#method-CFCellMeasure-clone)

------------------------------------------------------------------------

### Method [`new()`](https://rdrr.io/r/methods/new.html)

Create an instance of this class.

#### Usage

    CFCellMeasure$new(measure, name, nc_var = NULL, axes = NULL)

#### Arguments

- `measure`:

  The measure of this object. Must be either of "area" or "volume".

- `name`:

  The name of the cell measure variable. Ignored if argument `nc_var` is
  specified.

- `nc_var`:

  The netCDF variable that defines this CF cell measure object. `NULL`
  for an external variable.

- `axes`:

  List of [CFAxis](https://r-cf.github.io/ncdfCF/reference/CFAxis.md)
  instances that describe the dimensions of the cell measure object.
  `NULL` for an external variable.

#### Returns

An instance of this class.

------------------------------------------------------------------------

### Method [`print()`](https://rdrr.io/r/base/print.html)

Print a summary of the cell measure variable to the console.

#### Usage

    CFCellMeasure$print(...)

#### Arguments

- `...`:

  Arguments passed on to other functions. Of particular interest is
  `width = ` to indicate a maximum width of attribute columns.

------------------------------------------------------------------------

### Method [`data()`](https://rdrr.io/r/utils/data.html)

Retrieve the values of the cell measure variable.

#### Usage

    CFCellMeasure$data()

#### Returns

The values of the cell measure as a
[CFVariable](https://r-cf.github.io/ncdfCF/reference/CFVariable.md)
instance.

------------------------------------------------------------------------

### Method `register()`

Register a
[CFVariable](https://r-cf.github.io/ncdfCF/reference/CFVariable.md)
which is using this cell measure variable. A check is performed on the
compatibility between the data variable and this cell measure variable.

#### Usage

    CFCellMeasure$register(var)

#### Arguments

- `var`:

  A `CFVariable` instance to link to this instance.

#### Returns

Self, invisibly.

------------------------------------------------------------------------

### Method `link()`

Link the cell measure variable to an external netCDF resource. The
resource will be opened and the appropriate data variable will be linked
to this instance. If the axes or other properties of the external
resource are not compatible with this instance, an error will be raised.

#### Usage

    CFCellMeasure$link(resource)

#### Arguments

- `resource`:

  The name of the netCDF resource to open, either a local file name or a
  remote URI.

#### Returns

Self, invisibly.

------------------------------------------------------------------------

### Method [`detach()`](https://rdrr.io/r/base/detach.html)

Detach the internal data variable from an underlying netCDF resource.

#### Usage

    CFCellMeasure$detach()

#### Returns

Self, invisibly.

------------------------------------------------------------------------

### Method `clone()`

The objects of this class are cloneable with this method.

#### Usage

    CFCellMeasure$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.
