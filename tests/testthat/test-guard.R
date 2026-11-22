# Guard for the functionality that needs the suggested package `geozarr`. These
# tests do not need that package.

test_that(".check_geozarr_package() passes when the package is installed", {
  expect_true(.check_geozarr_package("stats"))
})

test_that(".check_geozarr_package() names the missing package", {
  expect_error(.check_geozarr_package("notinstalledpackage"), "Package 'notinstalledpackage' must be installed")
})

test_that("the functions that need zarr and geozarr call the guard", {
  for (f in list(aggregate_geozarr, copy_to_geozarr))
    expect_true(any(grepl(".check_geozarr", deparse(body(f)), fixed = TRUE)))
  expect_true(any(grepl(".check_geozarr", deparse(body(CFVariable$public_methods$write_geozarr)), fixed = TRUE)))
  expect_true(any(grepl(".check_geozarr", deparse(body(CFDataset$public_methods$geozarr)), fixed = TRUE)))
})
