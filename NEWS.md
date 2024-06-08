# ncdfCF 0.1.1

# ncdfCF 0.1.0

* Initial CRAN submission. This is a WORK IN PROGRESS and the package is not
 yet fit for a production environment.
* This version supports reading from NetCDF resources. CF Metadata Conventions
 are used to set properties on axis orientation, time dimensions and bounds.
* Standard R commands can be used to inspect properties of the NetCDF resource,
 such as `dimnames()` and `length()`.
* Access to data uses the R standard `[` selection operator for use with
 dimension indices. Use real-world coordinates with `subset()`.
