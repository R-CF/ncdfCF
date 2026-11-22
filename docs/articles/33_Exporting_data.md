# 3. Exporting data

Data in `CFDataset` or `CFVariable` instances can be exported to several
other formats for further processing. The examples below use a data set
included with the package:

``` r

library(ncdfCF)
  
# Get a netCDF file, here hourly data for 2016-01-01 over Rwanda
fn <- system.file("extdata", "ERA5land_Rwanda_20160101.nc", package = "ncdfCF")
  
# Open the file and get a variable from the data set
ds <- open_ncdf(fn)
t2m <- ds[["t2m"]]
```

If you would like to see other export formats supported, please open an
[issue](https://github.com/R-CF/ncdfCF/issues) or, better yet, provide
the code through a PR.

## Exporting to a `data.table`

A `CFVariable` can be converted to a `data.table` which will turn every
element in the `CFVariable` data array into a row in the `data.table`.
The axes of the `CFVariable` will become columns in the `data.table`.
Two attributes are added to the `data.table`: `name` indicates the long
name of the `CFVariable`, `units` indicates the physical unit of the
data values. Other attributes of the `CFVariable` are not preserved.

``` r

# install.packages("data.table")
library(data.table)
#> 
#> Attaching package: 'data.table'
#> The following object is masked from 'package:base':
#> 
#>     %notin%
head(t2m$data.table())
#>    longitude latitude                time      t2m
#>        <num>    <num>              <char>    <num>
#> 1:      28.0       -1 2016-01-01T00:00:00 293.8875
#> 2:      28.1       -1 2016-01-01T00:00:00 294.4015
#> 3:      28.2       -1 2016-01-01T00:00:00 294.4972
#> 4:      28.3       -1 2016-01-01T00:00:00 293.9426
#> 5:      28.4       -1 2016-01-01T00:00:00 293.6339
#> 6:      28.5       -1 2016-01-01T00:00:00 293.0206
```

The `CFVariable$profile()` method can produce a `data.table` directly,
avoiding the intermediate step of producing a new `CFVariable`.

``` r

rwa <- t2m$profile(longitude = c(30.07, 30.07, 29.74), latitude = c(-1.94, -1.58, -2.60), 
                   .names = c("Kigali", "Byumba", "Butare"), .as_table = TRUE)
head(rwa)
#>    longitude latitude                time .variable   .value
#>        <num>    <num>              <char>    <char>    <num>
#> 1:     30.07    -1.94 2016-01-01T00:00:00    Kigali 290.4055
#> 2:     30.07    -1.94 2016-01-01T01:00:00    Kigali 290.0088
#> 3:     30.07    -1.94 2016-01-01T02:00:00    Kigali 289.3608
#> 4:     30.07    -1.94 2016-01-01T03:00:00    Kigali 288.8414
#> 5:     30.07    -1.94 2016-01-01T04:00:00    Kigali 288.4713
#> 6:     30.07    -1.94 2016-01-01T05:00:00    Kigali 289.9276
attr(rwa, "value")
#> $name
#> [1] "2 metre temperature"
#> 
#> $unit
#> [1] "K"
```

## Exporting to `terra` or `stars`

Packages `terra` and `stars` are widely used in the R community for
their versatility in working with geospatial data. It should be noted
that they both have a very narrow concept of the coordinate system of
data variables. In neither package is it possible to store attributes
with the axes, for instance, and they do not support important CF
features like boundaries around coordinate values, model calendars,
auxiliary latitude-longitude grids, auxiliary coordinate labels, scalar
axes or parametric vertical coordinates. They do support significant
spatial analysis features, including coordinate transformations, that
can be very useful when analysing multi-dimensional data, which is why
the export functions are included in this package.

A `CFVariable` can be exported to a
[`terra::SpatRaster`](https://rspatial.github.io/terra/reference/SpatRaster-class.html)
(3D) or
[`terra::SpatRasterDataset`](https://rspatial.github.io/terra/reference/SpatRaster-class.html)
(4D), depending on the number of axes in the `CFVariable`.

``` r

#install.packages("terra")
suppressMessages(library(terra))
t2m$terra()
#> class       : SpatRaster
#> size        : 21, 31, 24  (nrow, ncol, nlyr)
#> resolution  : 0.1, 0.1  (x, y)
#> extent      : 27.95, 31.05, -3.05, -0.95  (xmin, xmax, ymin, ymax)
#> coord. ref. : 
#> source(s)   : memory
#> names       : 2016-~00:00, 2016-~00:00, 2016-~00:00, 2016-~00:00, 2016-~00:00, 2016-~00:00, ...
#> min values  :  284.288049,  284.124237,  283.655365,  283.282614,  283.018168,  283.751937, ...
#> max values  :  296.313559,  294.994941,  294.432655,  294.589247,  293.936255,  294.377149, ...
```

The function to export a `CFVariable` or a `CFDataset` with multiple
variables to a `stars` object is included in the `stars` package. No
attributes from the CF object(s) will be preserved.

``` r

# install.packages("stars")
suppressMessages(library(stars))
st_as_stars(t2m)
#> stars object with 3 dimensions and 1 attribute
#> attribute(s):
#>             Min.  1st Qu.   Median    Mean  3rd Qu.     Max.
#> t2m [K] 283.0182 291.1691 293.6186 293.642 296.2347 302.0447
#> dimension(s):
#>           from to         offset   delta    refsys x/y
#> longitude    1 31          27.95     0.1 OGC:CRS84 [x]
#> latitude     1 21          -0.95    -0.1 OGC:CRS84 [y]
#> time         1 24 2016-01-01 UTC 1 hours   POSIXct
```

## Exporting to `zarr` or `geozarr`

[Zarr](https://zarr.dev) is a storage format for n-dimensional data that
is optimised for use over the internet or through the cloud, although it
may also be used efficiently on a local file system. Data is stored in a
Zarr store, a hierarchical structure of groups and arrays.
[GeoZarr](https://geozarr.org) is a set of community conventions for
describing geospatial data in Zarr stores. The combination of Zarr and
GeoZarr is comparable to netCDF and CF Metadata conventions.

The [`zarr` package](https://cran.r-project.org/package=zarr) is a
R-native implementation of the Zarr specification and the [`geozarr`
package](https://cran.r-project.org/package=geozarr) implements the
GeoZarr conventions. In combination they can ingest netCDF data while
preserving the semantic meaning of the CF conventions, although in a
different encoding. This package allows you to save an entire
`CFDataset` or a single `CFVariable` to a Zarr store.

``` r

z <- ds$geozarr()
z
#> ── Zarr ────────────────────────────────────────────────────────────────────────
#> ── Memory store - version 3 ────────────────────────────────────────────────────
#> Capabilities: ✔ Listing ✔ Writing ✔ Deleting ✖ Consolidated metadata
#> ── Contents ────────────────────────────────────────────────────────────────────
#> ℹ Arrays: 4
#> ── Attributes ──────────────────────────────────────────────────────────────────
#> • Conventions: CF-1.6
#> • history: Attributes simplified for example.
z$hierarchy()
#> ── Zarr hierarchy  ─────────────────────────────────────────────────────────────
#> ☰ / (root group)
#>   ☰ coordinates
#>     ⌗ longitude [float32] [31]
#>   ⌖ t2m [float64] [longitude = 31, latitude = 21, time = 24]
#>   ⌖ pev [float64] [longitude = 31, latitude = 21, time = 24]
#>   ⌖ tp [float64] [longitude = 31, latitude = 21, time = 24]
```

### Aggregating netCDF data

NetCDF files have size limitations, due to limitations in various file
system drivers. Most netCDF files that are published for wide-scale
distribution are therefore limited to about 2GB. For CMIP6 data and
other large data sets this implies that a single run of data is broken
up into multiple netCDF files. That leads to problems during analysis:
separate chunks of data from multiple files have to be stitched together
again.

Zarr stores do not have the same size limitation; stores are effectively
boundless in size because a single data set is broken up into multiple
chunks and reassembled before presenting it back to the user, all
behind-the-scenes.

The
[`aggregate_geozarr()`](https://r-cf.github.io/ncdfCF/reference/aggregate_geozarr.md)
function of this package allows you to aggregate multiple compatible
netCDF files into a single Zarr store, such as the 3 to 16 netCDF files
that make up a single daily run of CMIP6 data. While the aggregation can
take place over any axes (with the exception of parametric vertical
axes), aggregating over the temporal dimension is the most common
pattern.

    # Not run dynamically: CMIP6 data used here is 3GB+ in size

    # Get a list of compatible files, here 4 CMIP6 daily ACCESS-CM2 data for precipitation
    > fn <- list.files(path = "~/my_data/CMIP6/day", pattern = "^pr_day_ACCESS-CM2", full.names = TRUE)

    # Create a Zarr store to hold the data, with a group for the data variable
    > z <- zarr::create_zarr("~/ACCESS-CM.zarr")
    > grp <- z$add_group("/", "variables")

    # Aggregate the netCDF data from the 4 files in a single Zarr array
    > gza <- aggregate_geozarr(fn, grp)
    Opening file 1 of 4: pr_day_ACCESS-CM2_ssp245_r1i1p1f1_gn_20150101-20391003_v20191108.nc
    Opening file 2 of 4: pr_day_ACCESS-CM2_ssp245_r1i1p1f1_gn_20391004-20640705_v20191108.nc
    Opening file 3 of 4: pr_day_ACCESS-CM2_ssp245_r1i1p1f1_gn_20640706-20890407_v20191108.nc
    Opening file 4 of 4: pr_day_ACCESS-CM2_ssp245_r1i1p1f1_gn_20890408-20991231_v20191108.nc
    Creating the GeoZarr array, aggregating 4 data variables along 'time'
    Array 'pr' with shape (192, 144, 31046)
    Copying data variable 1 of 4: pr
      Chunk 54 of 54 (100%)
    Copying data variable 2 of 4: pr
      Chunk 54 of 54 (100%)
    Copying data variable 3 of 4: pr
      Chunk 54 of 54 (100%)
    Copying data variable 4 of 4: pr
      Chunk 24 of 24 (100%)
    Done.

    # The resulting Zarr array, using GeoZarr conventions
    # Note how the "time" dimension encompasses the entire CMIP6 run
    > gza
    ── Zarr array ⌖ pr ─────────────────────────────────────────────────────────────
    ℹ Path      : /variables/pr
    ℹ Domain    : GeoZarr
    ℹ Data type : float32
    ℹ Shape     : 192, 144, 31046 [lon, lat, time]
    ℹ Chunk grid: 1, 1, 183 [192 x 144 x 170]
    ── Coordinate system ───────────────────────────────────────────────────────────
     abbr name direction length values                                        unit  
     X    lon  EAST        192  [0.9375 ... 359.0625]                         degree
     Y    lat  NORTH       144  [-89.375 ... 89.375]                          degree
     T    time FUTURE    31046  [2015-01-01T12:00:00 ... 2099-12-31T12:00:00] days  
    ── Attributes ──────────────────────────────────────────────────────────────────
    • standard_name: precipitation_flux
    • long_name: Precipitation
    • comment: includes both liquid and solid phases
    • units: kg m-2 s-1
    • cell_methods: area: time: mean
    • cell_measures: area: areacella
    • history: 2019-11-08T10:43:20Z altered by CMOR: replaced missing value flag (-1.07374e+09) with standard missing value (1e+20).
    • missing_value: 1.00000002004088e+20

### Supported features

The `zarr` and `geozarr` packages do not fully support all CF
constructs. The CF constructs supported by `ncdfCF` can all be exported
successfully to `geozarr`, with some caveats.

- All coordinate system constructs are exported to `geozarr`, including
  auxiliary lat-long grids. Support for parametric vertical coordinates
  will be added in the near future. For regularly-spaced coordinates no
  separate Zarr arrays (like coordinate variables in netCDF) are
  created - the values are compressed and stored inline with the
  attributes of the referring data variable.
- All attributes are exported, including global attributes and group
  attributes. Attributes that contain structural information for netCDF
  encoding, like “coordinates” or scale/offset, are not copied.
  Attributes that denote valid data are handled separately - see next
  section. The “history” attribute is converted to a JSON array.
- Referenced objects, like ancillary variables and external variables
  (when data is linked), must be exported separately. `geozarr`
  currently has no mechanism to establish the linkages between objects,
  but the CF attributes establishing these linkages are copied over.

Discrete sampling geometries are currently not supported in `geozarr`.

### Handling missing data

CF-encoded netCDF files describe missing data with a handful of
overlapping attributes: `_FillValue`, `missing_value`, `valid_min`,
`valid_max` and `valid_range`. Zarr has a single, narrowly defined
concept: the `fill_value` of an array indicates data that has never been
written. When data are copied from CF variables to GeoZarr arrays, for
instance with
[`copy_to_geozarr()`](https://r-cf.github.io/ncdfCF/reference/copy_to_geozarr.md)
or `CFVariable$write_geozarr()`, the CF attributes for missing data are
translated into a small, unambiguous encoding. This section describes
that translation.

#### Principle

The Zarr `fill_value` is never derived from CF attributes. It always has
the Zarr default for the data type of the array and it means one thing
only: the element has never been written. For the numeric types the
default values are the same as the netCDF library default fill values,
except for 64-bit integers.

Cells that were written but are missing are encoded separately, with an
optional `missing_value` attribute on the array. A reader of the Zarr
store that wants to find all missing data must therefore honour both the
`fill_value` and the `missing_value` attribute. The `zarr` and `geozarr`
packages apply the `fill_value` only.

#### What is read from the CF file

Data are read through package RNetCDF, which sets cells to `NA` that are
equal to the `_FillValue` or `missing_value` attributes, or that are
outside the valid range. Packed data are checked for validity before
they are unpacked, as the CF Conventions (section 2.5.1) require. The
data that are copied therefore have `NA` for every missing cell,
regardless of how the producer encoded it.

#### Translation rules

The rules apply to every data variable and to auxiliary coordinate
variables, but not to character variables, for which no attributes are
set.

- The `fill_value` of the Zarr array is the Zarr default for its data
  type.
- The `missing_value` of the Zarr array is a single value that is
  determined as follows. A packed variable (with a `scale_factor` or
  `add_offset`) has none: the sentinel belongs to the packed encoding
  and CF prescribes that it is not transformed, so missing cells are
  `NA` in the unpacked data. Otherwise, if the variable has a
  `missing_value` attribute, its first element is used. Otherwise, if
  the variable has a `_FillValue` that is different from the netCDF
  library default for the data type, that value is used. A `_FillValue`
  that equals the library default does not denote missing data, it
  merely repeats the default. If none of these apply there is no
  `missing_value`.
- If there is a `missing_value`, then every `NA` in the data is written
  as that value. Several sentinels in the source, such as a vector
  `missing_value` or a `missing_value` and a different `_FillValue`, are
  thus collapsed into one. If there is no `missing_value` then `NA` is
  written as the `fill_value`.
- The `valid_range` of the Zarr array is a vector of two values, in
  unpacked units. The `valid_range` attribute takes precedence over
  `valid_min` and `valid_max`. When only one of the latter is present,
  the other bound is the largest finite double. For packed variables the
  bounds are unpacked and put in increasing order. The range is recorded
  for documentation: the data have already been masked by it when they
  are read.
- No other CF attributes for missing data are written. In particular
  `_FillValue`, `valid_min`, `valid_max` and `valid_range` are not
  carried over.

#### State of the cells

After conversion every cell of the Zarr array is in one of three states:

1.  Valid data.
2.  Written but missing: the cell holds the `missing_value`, or the
    `fill_value` if the array has no `missing_value`.
3.  Never written, for instance in a part of the array that is not
    covered by any of the aggregated files: the cell holds the
    `fill_value`.

#### Caveats

- A real value in the data that is equal to the `fill_value` is read as
  `NA` by the `zarr` package. A real `NaN` in the source data is treated
  as missing and is written as the `missing_value` or the `fill_value`.
- The comparison with the netCDF library default fill value is done on
  double values with a small tolerance. For 64-bit integer data types
  this is approximate, which is sufficient to recognise the default.
- When data from multiple netCDF files are copied into one Zarr array,
  the `missing_value` and `valid_range` attributes are set again for
  each file and the last one wins. The files are assumed to be
  consistent, as they would be when produced by a single producer.
