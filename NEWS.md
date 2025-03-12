# ncdfCF 0.4.0

-   `CFData` has been renamed `CFArray` to more accurately describe its contents.
-   `CFArray` objects can now be written to a netCDF file.
-   Methods `CFVariable$summarise()` and `CFArray$summarise()` summarise the 
    temporal dimension of a data object to a lower resolution using a user-supplied
    function, using the specific calendar of the temporal dimension and returning
    a new `CFArray` object with the summarised data for every return value of a 
    call to the function, i.e. the function may have multiple return values. The 
    `CFArray` version is much faster (because all data has been read already), 
    but the `CFVariable` version can also summarise data variables that are too 
    big to fit into the available memory entirely. In either case, code is
    optimized compared to the R base version so an operation over the "time"
    dimension of a data array is about twice as fast as using the base R
    `apply(X, MARGIN, tapply, INDEX, FUN, ...)` call.
-   Method `CFArray$data.table()` exports a data object to a `data.table`.
-   `CFVariable` and `CFArray` classes now have `time()` method to retrieve the
    "time" axis or its `CFTime` instance, if present.
-   `CFAxis` has new `coordinates` field with which to retrieve the coordinates
    along the axis.
-   New attributes can be defined on any object that supports attributes, or
    deleted.
-   Fixed error on reading bounds for auxiliary coordinate variables. Various 
    other minor code fixes.
-   `CFResource` fixed to conform to new `R6` version.
-   Documentation updated, including vignettes.

# ncdfCF 0.3.0

-   Function `peek_ncdf()` returns quick-view information on a netCDF resource.
-   String-valued labels for discrete and generic numeric axes are now
    supported, including multiple label sets per axis. The labels are
    associated with an axis rather than a data variable (as the CF
    documents imply) and the axis must be explicitly defined (as the CF
    documents imply but not explicitly state, and it is missing from the
    example given).
-   Functions `makeMemoryGroup()`, `makeLongitudeAxis()` and
    `makeLatitudeAxis()` added to create scaffolding for new CF objects.
-   `NCGroup::unused()` method identifies unused `NCVariable`s to aid in
    finding issues with netCDF resources.
-   `print()` method for `NCVariable` and `NCDimension`.
-   Method `CFObject$fullname` added, giving fully-qualified CF object name.
-   "Axis" associated with bounds variable is no longer created.
-   NASA level-3 binned data (L3b) is now supported.
-   Reference to a containing `NCGroup` moved down to `CFObject` for CF 
    objects.
-   Minor code fixes.
-   Documentation extended and formatting fixed, new vignette.

# ncdfCF 0.2.1

-   NetCDF groups are now fully supported, including traversing group
    hierarchies with absolute or relative paths.
-   All axis types are identified correctly. This includes:
    -   Regular coordinate axes (a.k.a. NUG coordinate variables).
    -   Discrete axes, even when no coordinate variable is present - an
        extension to the CF conventions.
    -   Parametric vertical axes, but values are not yet computed.
    -   Scalar axes, linked to variables through the "coordinates"
        attribute of the latter.
    -   Auxiliary longitude-latitude grids, the horizontal component of
        the grid of a variable that was not defined as a Cartesian
        product of latitude and longitude, using the "coordinates"
        attribute of the variable. When subsetting a data variable,
        resampling is automatically performed.
    -   The four axes that *"receive special treatment"* by the
        Conventions each have a separate class to deal with their
        specific nature: CFAxisLongitude, CFAxisLatitude,
        CFAxisVertical, and CFAxisTime.
-   Bounds are read and interpreted on all axes except the vertical
    axis, including any auxiliary long-lat grids.
-   Information on UDTs is captured in a separate class. This is
    effectively only supported for the "compound" sub-type, for scalar
    values only.
-   Data is read into the most compact form possible. This saves a
    significant amount of memory when large integer variables are read
    as they would remain integers rather than the default numeric type.
-   Data is returned from CFVariable\$subset() as an instance of the
    CFData class, with associated objects such as axes and the
    attributes from the variable. Data can be read out in a variety of
    forms, currently as a raw array, an oriented array or as a
    `terra::SpatRaster` or `terra::SpatRasterDataset`.
-   Full support for grid mapping variables. As a significant extension
    over CF Metadata requirements, CRS strings are produced in the OGC
    WKT2 format, using the latest EPSG database of geodetic objects.
-   Improvements in printing object details.
-   Code refactored to R6.
-   GHA enabled

# ncdfCF 0.1.1

-   `objects_by_standard_name()` will list objects in the netCDF
    resource that have a "standard_name" attribute.

# ncdfCF 0.1.0

-   Initial CRAN submission. This is a WORK IN PROGRESS and the package
    is not yet fit for a production environment.
-   This version supports reading from netCDF resources. CF Metadata
    Conventions are used to set properties on axis orientation, time
    dimensions and bounds.
-   Standard R commands can be used to inspect properties of the netCDF
    resource, such as `dimnames()` and `length()`.
-   Access to data uses the R standard `[` selection operator for use
    with dimension indices. Use real-world coordinates with `subset()`.
