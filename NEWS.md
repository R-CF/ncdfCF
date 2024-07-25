# ncdfCF (development version)

* NetCDF groups are now supported, including traversing group hierarchies with
absolute or relative paths.
* Information on
the structure of UDTs can now be examined. This is effectively only supported
for the "compound" sub-type, for scalar values only.
* Better support for discrete axis dimensions, with a separate class.
* Data is read into the most compact form possible. This saves a significant 
amount of memory when large integer variables are read as they would remain
integers rather than the default numeric type.
* Improvements in printing object details.
* Code refactored to R6.
* GHA enabled

# ncdfCF 0.1.1

* `objects_by_standard_name()` will list objects in the netCDF resource that
have a "standard_name" attribute.

# ncdfCF 0.1.0

* Initial CRAN submission. This is a WORK IN PROGRESS and the package is not
 yet fit for a production environment.
* This version supports reading from netCDF resources. CF Metadata Conventions
 are used to set properties on axis orientation, time dimensions and bounds.
* Standard R commands can be used to inspect properties of the netCDF resource,
 such as `dimnames()` and `length()`.
* Access to data uses the R standard `[` selection operator for use with
 dimension indices. Use real-world coordinates with `subset()`.
