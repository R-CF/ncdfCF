test_that("L3b", {
  l3b <- "../testdata/AQUA_MODIS.20030101.L3b.DAY.CHL.nc"
  if (file.exists(l3b)) {
    ds <- open_ncdf(l3b)
    expect_equal(ds$file_type, "NASA level-3 binned data")
    chl <- ds[["/level-3_binned_data/chlor_a"]]
    expect_true(inherits(chl, "CFVariable"))
    expect_equal(chl$name, "chlor_a")
    expect_equal(chl$crs$attribute("grid_mapping_name"), "latitude_longitude")
    expect_equal(names(chl$axes), c("latitude", "longitude", "time"))
    expect_equal(sapply(chl$axes, function(ax) ax$length), c(latitude = 3001, longitude = 8432, time = 1))

    raw <- chl$raw()
    expect_equal(dim(raw), c(3001, 8432, 1))
    expect_equal(names(dimnames(raw)), c("latitude", "longitude", "time"))

    sub <- chl$subset(latitude = 0:30, longitude = 30:50)
    expect_true(inherits(sub, "CFVariable"))
    expect_equal(sub$name, "chlor_a")
    expect_equal(sub$crs$attribute("grid_mapping_name"), "latitude_longitude")
    expect_equal(names(sub$axes), c("latitude", "longitude", "time"))
    expect_equal(sapply(sub$axes, function(ax) ax$length), c(latitude = 722, longitude = 482, time = 1))

    raw <- sub$raw()
    expect_equal(dim(raw), c(latitude = 722, longitude = 482, time = 1))
    expect_equal(names(dimnames(raw)), c("latitude", "longitude", "time"))
  }
})
