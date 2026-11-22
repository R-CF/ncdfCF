l3b <- "../testdata/AQUA_MODIS.20030101.L3b.DAY.CHL.nc"

test_that("L3b: window read from file", {
  skip_if_not(file.exists(l3b))
  chl <- open_ncdf(l3b)[["/level-3_binned_data/chlor_a"]]
  win <- chl$read_chunk(c(1712L, 4000L), c(41L, 301L))
  expect_equal(dim(win), c(41L, 301L))
  expect_equal(sum(!is.na(win)), 1447L)
})

test_that("L3b: window from file equals slice of the full grid", {
  skip_if_not(file.exists(l3b))
  chl <- open_ncdf(l3b)[["/level-3_binned_data/chlor_a"]]
  win <- chl$read_window(c(1712L, 4000L), c(41L, 301L))
  full <- chl$raw()
  expect_equal(win, unname(full[1712:1752, 4000:4300, drop = FALSE]))
})

test_that("L3b: rows without data are NA", {
  skip_if_not(file.exists(l3b))
  chl <- open_ncdf(l3b)[["/level-3_binned_data/chlor_a"]]
  expect_true(all(is.na(chl$read_chunk(c(179L, 1L), c(1L, NA)))))
})

test_that("L3b", {
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
    expect_equal(dim(raw), c(3001, 8432))
    expect_equal(names(dimnames(raw)), c("latitude", "longitude"))

    sub <- chl$subset(latitude = 0:30, longitude = 30:50)
    expect_true(inherits(sub, "CFVariable"))
    expect_equal(sub$name, "chlor_a")
    expect_equal(sub$crs$attribute("grid_mapping_name"), "latitude_longitude")
    expect_equal(names(sub$axes), c("latitude", "longitude", "time"))
    expect_equal(sapply(sub$axes, function(ax) ax$length), c(latitude = 722, longitude = 482, time = 1))

    raw <- sub$raw()
    expect_equal(dim(raw), c(722, 482))
    expect_equal(names(dimnames(raw)), c("latitude", "longitude"))
  }
})

test_that("L3b: `[` with indices", {
  skip_if_not(file.exists(l3b))
  chl <- open_ncdf(l3b)[["/level-3_binned_data/chlor_a"]]
  x <- chl[1712:1752, 4000:4300]
  expect_equal(dim(x), c(41L, 301L))
  expect_equal(names(dimnames(x)), c("latitude", "longitude"))
  expect_equal(sum(!is.na(x)), 1447L)
})

test_that("L3b: `[` with missing indices", {
  skip_if_not(file.exists(l3b))
  chl <- open_ncdf(l3b)[["/level-3_binned_data/chlor_a"]]
  expect_equal(dim(chl[1712:1752, ]), c(41L, 8432L))
  expect_equal(dim(chl[, 4000:4300]), c(3001L, 301L))
  expect_equal(dim(chl[]), c(3001L, 8432L))
})
