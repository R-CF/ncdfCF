# Create a `CFDataset` or `CFVariable` instance from an R object

With this function you can convert an R object into a
[CFDataset](https://r-cf.github.io/ncdfCF/reference/CFDataset.md) or
[CFVariable](https://r-cf.github.io/ncdfCF/reference/CFVariable.md),
depending on the characteristics of the argument `obj`. The object to
convert can be an array, matrix or vector of type `logical`, `integer`,
`numeric` or `character`, or a
[`terra::SpatRaster`](https://rspatial.github.io/terra/reference/SpatRaster-class.html).

## Usage

``` r
as_CF(name, obj)

# Default S3 method
as_CF(name, obj)

# S3 method for class 'SpatRaster'
as_CF(name, obj)
```

## Arguments

- name:

  The name of the `CFDataset` or `CFVariable` to create.

- obj:

  The object to convert. This can be an array, matrix or vector of type
  `logical`, `integer`, `numeric` or `character`, or a
  [`terra::SpatRaster`](https://rspatial.github.io/terra/reference/SpatRaster-class.html).

## Value

An instance of class
[CFDataset](https://r-cf.github.io/ncdfCF/reference/CFDataset.md) or
[CFVariable](https://r-cf.github.io/ncdfCF/reference/CFVariable.md).

## Details

Dimnames on the R object will be converted to instances of a
[CFAxis](https://r-cf.github.io/ncdfCF/reference/CFAxis.md) descendant
class, depending on their values. If the dimnames along a dimension of
the R object can be converted to `numeric`, then it will be an instance
of
[CFAxisNumeric](https://r-cf.github.io/ncdfCF/reference/CFAxisNumeric.md).
If the dimnames are `character`, a first attempt is made to create a
[CFAxisTime](https://r-cf.github.io/ncdfCF/reference/CFAxisTime.md)
(i.e. the dimnames have to represent timestamps), failing that a
[CFAxisCharacter](https://r-cf.github.io/ncdfCF/reference/CFAxisCharacter.md)
will be created. If no dimnames are set, an instance of
[CFAxisDiscrete](https://r-cf.github.io/ncdfCF/reference/CFAxisDiscrete.md)
is generated.

The axes of the `CFVariable` instance(s) are oriented as in the object.
Note that this is different from standard practice in the netCDF
community and the portability of saved data sets is thus limited. You
can improve this situation by setting the orientation of the axes and by
adding attributes.

After creation of the `CFDataset` or `CFVariable`, it is recommended to
set other properties, such as attributes or a coordinate reference
system.
