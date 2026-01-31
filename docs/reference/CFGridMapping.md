# CF grid mapping object

This class contains the details for a coordinate reference system, or
grid mapping in CF terms, of a data variable.

When reporting the coordinate reference system to the caller, a
character string in WKT2 format is returned, following the OGC standard.

## References

https://docs.ogc.org/is/18-010r11/18-010r11.pdf
https://cfconventions.org/cf-conventions/cf-conventions.html#appendix-grid-mappings

## Super class

[`ncdfCF::CFObject`](https://r-cf.github.io/ncdfCF/reference/CFObject.md)
-\> `CFGridMapping`

## Active bindings

- `friendlyClassName`:

  (read-only) A nice description of the class.

## Methods

### Public methods

- [`CFGridMapping$new()`](#method-CFGridMapping-new)

- [`CFGridMapping$print()`](#method-CFGridMapping-print)

- [`CFGridMapping$brief()`](#method-CFGridMapping-brief)

- [`CFGridMapping$wkt2()`](#method-CFGridMapping-wkt2)

- [`CFGridMapping$write()`](#method-CFGridMapping-write)

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

Create a new instance of this class.

Note that when a new grid mapping object is created (as opposed to
reading from a netCDF resource), only the `grid_mapping_name` attribute
will be set. The caller must set all other parameters through their
respective attributes, following the CF Metadata Conventions.

#### Usage

    CFGridMapping$new(var, group, grid_mapping_name)

#### Arguments

- `var`:

  When creating a new grid mapping object, the name of the object. When
  reading from a netCDF resource, the netCDF variable that describes
  this instance.

- `group`:

  The [CFGroup](https://r-cf.github.io/ncdfCF/reference/CFGroup.md) that
  this instance will live in.

- `grid_mapping_name`:

  Optional. When creating a new grid mapping object, the formal name of
  the grid mapping, as specified in the CF Metadata Conventions. This
  value is stored in the new object as attribute "grid_mapping_name".
  Ignored when argument `var` is a NC object.

------------------------------------------------------------------------

### Method [`print()`](https://rdrr.io/r/base/print.html)

Prints a summary of the grid mapping to the console.

#### Usage

    CFGridMapping$print()

------------------------------------------------------------------------

### Method `brief()`

Retrieve a 1-row `data.frame` with some information on this grid
mapping.

#### Usage

    CFGridMapping$brief()

------------------------------------------------------------------------

### Method `wkt2()`

Retrieve the CRS string for a specific variable.

#### Usage

    CFGridMapping$wkt2(axis_info)

#### Arguments

- `axis_info`:

  A list with information that describes the axes of the `CFVariable`
  instance to describe.

#### Returns

A character string with the CRS in WKT2 format.

------------------------------------------------------------------------

### Method [`write()`](https://rdrr.io/r/base/write.html)

Write the CRS object to a netCDF file.

#### Usage

    CFGridMapping$write()

#### Returns

Self, invisibly.
