# CF auxiliary longitude-latitude variable

This class represents the longitude and latitude variables that compose
auxiliary coordinate variable axes for X-Y grids that are not
longitude-latitude.

The class provides access to the data arrays for longitude and latitude
from the netCDF resource, as well as all the details that have been
associated with both axes. Additionally, this class can generate the
index to extract values on a long-lat grid of the associated X-Y grid
data variable using a user-selectable extent and resolution.

Auxiliary longitude-latitude grids are only supported for reading from a
netCDF resource. Creating an instance of this class manually therefore
has no practical purpose.

## Active bindings

- `friendlyClassName`:

  (read-only) A nice description of the class.

- `name`:

  (read-only) The name of the auxiliary lon-lat grid.

- `grid_names`:

  (read-only) Read the names of the longitude and latitude grid as a
  vector of length 2.

- `dimids`:

  (read-only) Retrieve the dimension ids used by the longitude and
  latitude grids.

- `aoi`:

  Set or retrieve the AOI for the long-lat grid.

- `lon`:

  (read-only) Retrieve the longitude grid.

- `lat`:

  (read-only) Retrieve the latitude grid.

- `lon_bounds`:

  (read-only) Retrieve the boundary values of the longitude grid.

- `lat_bounds`:

  (read-only) Retrieve the boundary values of the latitude grid.

- `extent`:

  (read-only) Retrieve the extent of the longitude and latitude grids,
  including bounds if they have been set. The extent is reported as a
  numeric vector of the four elements minimum and maximum longitude and
  minimum and maximum latitude.

- `dim`:

  (read-only) The dimensions of the longitude and latitude grids.

## Methods

### Public methods

- [`CFAuxiliaryLongLat$new()`](#method-CFAuxiliaryLongLat-new)

- [`CFAuxiliaryLongLat$print()`](#method-CFAuxiliaryLongLat-print)

- [`CFAuxiliaryLongLat$brief()`](#method-CFAuxiliaryLongLat-brief)

- [`CFAuxiliaryLongLat$sample_index()`](#method-CFAuxiliaryLongLat-sample_index)

- [`CFAuxiliaryLongLat$grid_index()`](#method-CFAuxiliaryLongLat-grid_index)

- [`CFAuxiliaryLongLat$clear_cache()`](#method-CFAuxiliaryLongLat-clear_cache)

- [`CFAuxiliaryLongLat$attach_to_group()`](#method-CFAuxiliaryLongLat-attach_to_group)

- [`CFAuxiliaryLongLat$detach()`](#method-CFAuxiliaryLongLat-detach)

- [`CFAuxiliaryLongLat$clone()`](#method-CFAuxiliaryLongLat-clone)

------------------------------------------------------------------------

### Method [`new()`](https://rdrr.io/r/methods/new.html)

Creating a new instance. It should normally not be useful to create an
instance of this class other than upon reading a netCDF resource.

#### Usage

    CFAuxiliaryLongLat$new(varLong, varLat, boundsLong = NULL, boundsLat = NULL)

#### Arguments

- `varLong, varLat`:

  The
  [CFVariable](https://r-cf.github.io/ncdfCF/reference/CFVariable.md)
  instances with the longitude and latitude grid values, respectively.

- `boundsLong, boundsLat`:

  The [CFBounds](https://r-cf.github.io/ncdfCF/reference/CFBounds.md)
  instances of the grid cells for the longitude and latitude,
  respectively, if set. Defaults to `NULL`.

------------------------------------------------------------------------

### Method [`print()`](https://rdrr.io/r/base/print.html)

Summary of the auxiliary longitude-latitude variable printed to the
console.

#### Usage

    CFAuxiliaryLongLat$print()

------------------------------------------------------------------------

### Method `brief()`

Some details of the auxiliary longitude-latitude grid.

#### Usage

    CFAuxiliaryLongLat$brief()

#### Returns

A 2-row `data.frame` with some details of the grid components.

------------------------------------------------------------------------

### Method `sample_index()`

Return the indexes into the X (longitude) and Y (latitude) axes of the
original data grid of the points closest to the supplied longitudes and
latitudes, up to a maximum distance.

#### Usage

    CFAuxiliaryLongLat$sample_index(x, y, maxDist = NULL)

#### Arguments

- `x, y`:

  Vectors of longitude and latitude values in decimal degrees,
  respectively.

- `maxDist`:

  Numeric value in decimal degrees of the maximum distance between the
  sampling point and the closest grid cell. If omitted (default), the
  distance is calculated from the nominal resolution of the grids.

#### Returns

A matrix with two columns `X` and `Y` and as many rows as arguments `x`
and `y`. The `X` and `Y` columns give the index into the grid of the
sampling points, or `c(NA, NA)` is no grid point is located within the
`maxDist` distance from the sampling point.

------------------------------------------------------------------------

### Method `grid_index()`

Compute the indices for the AOI into the data grid.

#### Usage

    CFAuxiliaryLongLat$grid_index()

#### Returns

An integer matrix with the dimensions of the AOI, where each grid cell
gives the linear index value into the longitude and latitude grids.

------------------------------------------------------------------------

### Method `clear_cache()`

Clears the cache of pre-computed grid index values if an AOI has been
set.

#### Usage

    CFAuxiliaryLongLat$clear_cache()

------------------------------------------------------------------------

### Method `attach_to_group()`

Attach the auxiliary long-lat grids and any bounds to a group. If there
is another object with the same name in this group an error is thrown.

#### Usage

    CFAuxiliaryLongLat$attach_to_group(grp, locations = list())

#### Arguments

- `grp`:

  An instance of
  [CFGroup](https://r-cf.github.io/ncdfCF/reference/CFGroup.md).

- `locations`:

  Optional. A `list` whose named elements correspond to the names of
  objects associated with these auxiliary grids. Each list element has a
  single character string indicating the group in the hierarchy where
  the object should be stored. As an example, if the variable has axes
  "lon" and "lat" and they should be stored in the parent group of
  `grp`, then specify `locations = list(lon = "..", lat = "..")`.
  Locations can use absolute paths or relative paths from group `grp`.
  The auxiliary grids and bounds that are not in the list will be stored
  in group `grp`. If the argument `locations` is not provided, all
  objects will be stored in this group.

#### Returns

Self, invisibly.

------------------------------------------------------------------------

### Method [`detach()`](https://rdrr.io/r/base/detach.html)

Detach the latitude and longitude from an underlying netCDF resource.

#### Usage

    CFAuxiliaryLongLat$detach()

#### Returns

Self, invisibly.

------------------------------------------------------------------------

### Method `clone()`

The objects of this class are cloneable with this method.

#### Usage

    CFAuxiliaryLongLat$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.
