# CF Standard names table

The CF Metadata Conventions define a large number of standard names for
physical parameters, including axes and data variables. This class
accesses the standard names table. For each of the entries in the table
two properties are provided: the canonical unit and a description. These
properties are retrieved when searching for a given name.

Access to this class is through the `CF` environment. Use the
`CF$standard_names$find("name_of_interest")` method to access a
particular standard name. It is strongly recommended not to instantiate
this class manually as that may introduce problems with accessing the
underlying XML file.

The XML table is retrieved from the CF Metadata Conventions web site
[here](https://cfconventions.org/vocabularies.html) and stored locally
in the cache of the `ncdfCF` package. A check is performed periodically
for an updated version, which will then be downloaded automatically. The
frequency of the update check can be controlled with the
`CF.options$cache_stale_days` option.

## References

https://cfconventions.org/cf-conventions/cf-conventions.html#standard-name

## Active bindings

- `is_loaded`:

  (read-only) Flag to determine if the standard names table is
  available.

## Methods

### Public methods

- [`CFStandardNames$new()`](#method-CFStandardNames-new)

- [`CFStandardNames$print()`](#method-CFStandardNames-print)

- [`CFStandardNames$find()`](#method-CFStandardNames-find)

- [`CFStandardNames$load()`](#method-CFStandardNames-load)

- [`CFStandardNames$clone()`](#method-CFStandardNames-clone)

------------------------------------------------------------------------

### Method `new()`

Initialize an instance of this class. This is done automatically when
the package is loaded.

#### Usage

    CFStandardNames$new()

------------------------------------------------------------------------

### Method [`print()`](https://rdrr.io/r/base/print.html)

Print the version number of the standard names table in use, if it is
loaded. The table is loaded automatically when it is first used.

#### Usage

    CFStandardNames$print()

------------------------------------------------------------------------

### Method [`find()`](https://rdrr.io/r/utils/apropos.html)

Retrieve the information on the specified names.

#### Usage

    CFStandardNames$find(names)

#### Arguments

- `names`:

  A character vector with the names to search the standard names table
  for.

#### Returns

If an entry with a value in `names` is found, returns a `data.frame`
with with with the canonical units and a description of the name. If no
`names` are found in the table `NULL` is returned.

------------------------------------------------------------------------

### Method [`load()`](https://rdrr.io/r/base/load.html)

Load the standard names table so that it's contents may be used in
display and analysis. Note that the table may be downloaded (4.3MB at
version 91) if not available or stale.

#### Usage

    CFStandardNames$load()

#### Returns

Self, invisibly.

------------------------------------------------------------------------

### Method `clone()`

The objects of this class are cloneable with this method.

#### Usage

    CFStandardNames$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.
