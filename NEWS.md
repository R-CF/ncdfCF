# ncdfCF (development version)

* NetCDF groups are now fully supported, including traversing group hierarchies 
with absolute or relative paths.
* All axis types are identified correctly. This includes:
  * Regular coordinate axes (a.k.a. NUG coordinate variables).
  * Discrete axes, even when no coordinate variable is present - an extension to 
  the CF conventions.
  * Parametric vertical axes, but values are not yet computed.
  * Scalar axes, linked to variables through the "coordinates" attribute of the 
  latter.
  * Auxiliary longitude-latitude grids, the horizontal component of the grid of
  a variable that was not defined as a Cartesian product of latitude and 
  longitude, again using the "coordinates" attribute of the variable. When
  subsetting a data variable, resampling is automatically performed.
  * The four axes that *"receive special treatment"* by the Conventions each 
  have a separate class to deal with their specific nature: CFAxisLongitude,
  CFAxisLatitude, CFAxisVertical, and CFAxisTime.
* Bounds are read and interpreted on all axes except the vertical axis, 
including the auxiliary long-lat grids.
* Information on UDTs is captured in a separate class. This is effectively only 
supported for the "compound" sub-type, for scalar values only.
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
